use syntax::ast::{self, NodeId, ExprKind, Expr, Stmt, StmtKind, Block};
use syntax::visit;

use std::ascii::AsciiExt;
use std::collections::HashMap;

macro_rules! x_counter {
    ($indextype:ident, $name:ident) => (
        pub struct $name {
            counter: u32,
        }

        impl $name {
            fn generate_new_id(&mut self) -> $indextype {
                self.counter += 1;
                $indextype(self.counter - 1)
            }
        }
    )
}

x_counter!(ScopeId, ScopeCounter);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct BlockId(ScopeId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct IfId(ScopeId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct MatchId(ScopeId);

impl ScopeId {
    fn next_id(self) -> ScopeId {
        ScopeId(self.0 + 1)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeclId(u32);

#[derive(Debug)]
pub struct Decl {
    ident: ast::Ident,
    id: NodeId,
    is_initialized: bool,
    // late_init: Option
}

pub enum BlockType {
    Normal,
    Loop,
}

pub struct ScopeTracker {
    scopes: Vec<Scope>,
    pub scope_map: HashMap<NodeId, ScopeId>,
    pub current_scope: ScopeId,
}

impl ScopeTracker {

    pub fn new() -> ScopeTracker {
        ScopeTracker {
            scopes: vec![Scope::Block(BlockScope {
                id: ScopeId(0),
                parent_id: None,
                children: vec![],
                decls: vec![],
            })],
            scope_map: HashMap::new(),
            current_scope: ScopeId(0),
        }
    }

    pub fn from_expr(e: &Expr) -> ScopeTracker {
        let mut tracker = ScopeTracker::new();
        tracker.mutate_from_expr(e);
        tracker
    }

    pub fn from_block(b: &Block) -> ScopeTracker {
        let mut tracker = ScopeTracker::new();
        tracker.mutate_from_block(b);
        tracker
    }

    fn mutate_from_expr(&mut self, e: &Expr) {
        match e.node {
            ExprKind::If(_, ref then_expr, ref else_expr) => {
                let prev_id = self.current_scope;
                let (then_id, else_id) = self.current_block_id()
                                             .create_child_if(self, else_expr.is_some());
                self.set_current_block(then_id);
                self.mutate_from_block(then_expr);

                if let (Some(id), &Some(ref else_expr)) = (else_id, else_expr) {
                    self.set_current_block(id);
                    self.mutate_from_expr(else_expr);
                }

                self.set_current_block(prev_id);
            }
            ExprKind::Loop(ref body, _) => {
                self.mutate_from_block(body);
            }
            ExprKind::While(_, ref body, _) => {
                // cond will not contain a transition / decl
                self.mutate_from_block(body);
            }
            ExprKind::Match(_, ref arms) => {
                let prev_id = self.current_scope;
                let match_id = self.current_block_id().create_child_match(self);

                for arm in arms.iter() {
                    let arm_scope_id =  match_id.new_arm(self);

                    self.set_current_block(arm_scope_id);
                    self.mutate_from_expr(&arm.body);
                }
                self.set_current_block(prev_id);
            }
            ExprKind::Block(ref block) => {
                self.mutate_from_block(block);
            }
            _ => {}
        }
    }

    fn mutate_from_stmt(&mut self, stmt: &Stmt) {
        self.current_block().push_node(stmt.id);
        let current_scope = self.current_scope;
        self.scope_map.insert(stmt.id, current_scope);

        match stmt.node {
            StmtKind::Semi(ref expr) | StmtKind::Expr(ref expr) => {
                self.mutate_from_expr(expr);
            }
            StmtKind::Local(ref local) => {
                self.current_block().decls.extend(
                    get_decls_from_pat(&local.pat)
                    .into_iter()
                    .map(|(id, identifier)| {
                        Decl {
                            ident: identifier,
                            id: id,
                            is_initialized: local.init.is_some(),
                        }
                    })
                );
            }
            _ => {}
        }
    }

    fn mutate_from_block(&mut self, node: &Block) {
        let prev_id = self.current_scope;
        let block_id = self.current_block_id().create_child_block(self);
        self.set_current_block(block_id.0);

        for stmt in &node.stmts {
            self.mutate_from_stmt(stmt);
        }

        self.set_current_block(prev_id)
    }

    #[allow(unknown_lints, needless_lifetimes)]
    pub fn find_scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.scopes[id.0 as usize]
    }

    pub fn current_block(&mut self) -> &mut BlockScope {
        let current_scope = self.current_scope;
        self.get_mut_block(BlockId(current_scope))
    }

    fn current_block_id(&self) -> BlockId {
        BlockId(self.current_scope)
    }

    pub fn current_scope(&mut self) -> &mut Scope {
        let current_id = self.current_scope;
        self.find_scope_mut(current_id)
    }

    pub fn set_current_block(&mut self, id: ScopeId) {
        self.current_scope = id;
    }

    fn next_scope_id(&self) -> ScopeId {
        ScopeId(self.scopes.len() as u32)
    }

    fn get_mut_match(&mut self, id: MatchId) -> &mut MatchScope {
        match *self.find_scope_mut(id.0) {
            Scope::Match(ref mut m) => m,
            ref x => panic!("Wrong type - {:?}", x),
        }
    }

    fn get_mut_block(&mut self, id: BlockId) -> &mut BlockScope {
        match *self.find_scope_mut(id.0) {
            Scope::Block(ref mut b) => b,
            ref x => panic!("Wrong type - {:?}", x),
        }
    }

    fn get_mut_if(&mut self, id: IfId) -> &mut IfScope {
        match *self.find_scope_mut(id.0) {
            Scope::If(ref mut i) => i,
            ref x => panic!("Wrong type - {:?}", x),
        }
    }

    pub fn is_decl_live(&self, scope: ScopeId, decl_ident: ast::Ident) -> bool {
        let mut current_scope_id = scope;

        loop {
            let current_scope = &self.scopes[current_scope_id.0 as usize];
            match *current_scope {
                Scope::Block(ref b) => {
                    if b.decls.iter().map(|d| d.ident).any(|d| d == decl_ident) {
                        return true;
                    }
                },
                _ => {}
            }

            if let Some(id) = current_scope.parent_id() {
                current_scope_id = id;
            } else {
                return false;
            }
        }
    }

}

#[derive(Debug)]
pub struct IfScope {
    pub id: ScopeId,
    pub parent_id: ScopeId,
    pub then: ScopeId,
    pub else_: Option<ScopeId>,
}


#[derive(Debug)]
pub struct MatchScope {
    pub id: ScopeId,
    pub parent_id: ScopeId,
    pub arms: Vec<ScopeId>,
}

impl MatchId {
    pub fn new_arm(self, tracker: &mut ScopeTracker) -> ScopeId {
        let arm_id = tracker.next_scope_id();

        let new_block = BlockScope::new(arm_id, self.0);

        tracker.get_mut_match(self).arms.push(arm_id);
        tracker.scopes.push(Scope::Block(new_block));

        arm_id
    }
}

#[derive(Debug)]
pub struct BlockScope {
    pub id: ScopeId,
    pub parent_id: Option<ScopeId>,
    pub children: Vec<ScopeOrNode>,
    pub decls: Vec<Decl>,
}

impl BlockScope {

    fn new(id: ScopeId, parent_id: ScopeId) -> BlockScope {
        BlockScope {
            id: id,
            parent_id: Some(parent_id),
            children: vec![],
            decls: vec![],
        }
    }

    fn push_child(&mut self, scope_id: ScopeId) {
        self.children.push(ScopeOrNode::Scope(scope_id));
    }

    pub fn push_node(&mut self, id: NodeId) {
        self.children.push(ScopeOrNode::Node(id));
    }

    #[allow(unknown_lints, needless_lifetimes)]
    fn scopes(&self) -> Vec<ScopeId> {
        self.children.iter().filter_map(
            |s: &ScopeOrNode| {
                if let ScopeOrNode::Scope(inner) = *s {
                    Some(inner)
                }
                else {
                    None
                }
            }
        ).collect()
    }
}

impl BlockId {

    pub fn create_child_if(self, tracker: &mut ScopeTracker, has_else: bool) -> (ScopeId, Option<ScopeId>) {
        let if_id = tracker.next_scope_id();
        let then_id = if_id.next_id();

        let then_block = Scope::Block(BlockScope::new(then_id, if_id));

        let (else_block, else_id) = if has_else {
            let block = BlockScope::new(then_id.next_id(), if_id);
            let new_id = block.id;
            (Some(Scope::Block(block)), Some(new_id))
        } else {
            (None, None)
        };

        let parent_id = self.0;
        tracker.scopes.push(
            Scope::If(
                IfScope {
                    id: if_id,
                    parent_id: parent_id,
                    then: then_id,
                    else_: else_id,
                }
        ));
        tracker.scopes.push(then_block);

        if let Some(b) = else_block {
            tracker.scopes.push(b);
        }

        tracker.get_mut_block(self).push_child(if_id);

        (then_id, else_id)
    }

    pub fn create_child_match(self, tracker: &mut ScopeTracker) -> MatchId {
        let match_id = tracker.next_scope_id();
        tracker.get_mut_block(self).push_child(match_id);

        let parent_id = self.0;

        tracker.scopes.push(Scope::Match(
            MatchScope {
                id: match_id,
                parent_id: parent_id,
                arms: vec![],
            }
        ));

        MatchId(match_id)
    }

    pub fn create_child_block(self, tracker: &mut ScopeTracker) -> BlockId {
        let block_id = tracker.next_scope_id();
        let new_block = BlockScope::new(block_id, self.0);

        tracker.get_mut_block(self).push_child(block_id);
        tracker.scopes.push(Scope::Block(new_block));

        BlockId(block_id)
    }

}

#[derive(Debug)]
pub enum Scope {
    Block(BlockScope),
    If(IfScope),
    Match(MatchScope),
}

impl Scope {
    fn id(&self) -> ScopeId {
        match *self {
            Scope::Block(ref s) => s.id,
            Scope::If(ref s) => s.id,
            Scope::Match(ref s) => s.id,
        }
    }

    pub fn as_match(&mut self) -> &mut MatchScope {
        match *self {
            Scope::Match(ref mut m) => m,
            _ => panic!("Wrong type: {:?}", self)
        }
    }

    pub fn as_block(&mut self) -> &mut BlockScope {
        match *self {
            Scope::Block(ref mut b) => b,
            _ => panic!("Wrong type: {:?}", self)
        }
    }

    pub fn parent_id(&self) -> Option<ScopeId> {
        match *self {
            Scope::Block(ref s) => s.parent_id,
            Scope::If(ref s) => Some(s.parent_id),
            Scope::Match(ref s) => Some(s.parent_id),
        }
    }

}

#[derive(Debug)]
pub enum ScopeOrNode {
    Scope(ScopeId),
    Node(NodeId),
}

impl ScopeOrNode {

    fn as_scope(&self) -> ScopeId {
        match *self {
            ScopeOrNode::Scope(s) => s,
            _ => panic!("Wrong type: {:?}", self)
        }
    }

    fn as_node(&self) -> NodeId {
        match *self {
            ScopeOrNode::Node(n) => n,
            _ => panic!("Wrong type: {:?}", self)
        }
    }
}


// fn get_decls_from_pat(cx: &ExtCtxt, pat: &ast::Pat) -> Vec<(NodeId, ast::Ident)> {
fn get_decls_from_pat(pat: &ast::Pat) -> Vec<(NodeId, ast::Ident)> {
    // struct Visitor<'a, 'b: 'a> {
    struct Visitor {
        // cx: &'a ExtCtxt<'b>,
        var_decls: Vec<(NodeId, ast::Ident)>,
    }

    // impl<'a, 'b: 'a> visit::Visitor for Visitor<'a, 'b> {
    impl visit::Visitor for Visitor {
        fn visit_pat(&mut self, pat: &ast::Pat) {
            match pat.node {
                ast::PatKind::Ident(ast::BindingMode::ByValue(_), id, _) => {
                    // Consider only lower case identities as a variable.
                    let id_str = id.node.name.as_str();
                    let first_char = id_str.chars().next().unwrap();

                    if first_char == first_char.to_ascii_lowercase() {
                        self.var_decls.push((pat.id, id.node));
                    }
                }
                ast::PatKind::Ident(..) => {
                    panic!("Canot handle pat {:?}", pat);
                    // self.cx.span_bug(pat.span, &format!("Canot handle pat {:?}", pat))
                }
                _ => { }
            }

            visit::walk_pat(self, pat);
        }

        fn visit_mac(&mut self, _mac: &ast::Mac) { }
    }

    let mut visitor = Visitor {
        // cx: cx,
        var_decls: Vec::new(),
    };

    visit::Visitor::visit_pat(&mut visitor, pat);

    visitor.var_decls
}
