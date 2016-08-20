use syntax::ast::{self, NodeId, ExprKind, Expr, Stmt, StmtKind, Block};
use syntax::visit;
use std::ascii::AsciiExt;

use mar::build::let_tracking::AstMap;

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
pub struct DeclId(u32);

#[derive(Debug)]
pub struct Decl {
    pub ident: ast::Ident,
    pub path: NodePath,
    pub initializations: Vec<NodePath>,
    pub usages: Vec<NodePath>,
    pub is_initialized: bool,
}

#[derive(Debug)]
pub enum BlockType {
    Normal,
    Loop,
}

pub struct ScopeTracker {
    pub root: Scope,
    pub current_scope: ScopeId,
}

#[derive(Debug)]
pub struct NodePath {
    pub scope: ScopeId,
    pub index: usize,
    pub id: NodeId,
}


impl ScopeTracker {

    pub fn new() -> (ScopeTracker, ScopeCounter) {
        let mut counter = ScopeCounter{ counter: 0 };
        let new_id = counter.generate_new_id();
        (ScopeTracker {
            root: Scope::Block(BlockScope {
                id: new_id,
                parent_id: None,
                children: vec![],
                decls: vec![],
                block_type: BlockType::Normal,
            }),
            current_scope: new_id,
        }, counter)
    }

    pub fn from_expr(e: &Expr, map: &AstMap) -> ScopeTracker {
        let (mut tracker, mut counter) = ScopeTracker::new();
        tracker.mutate_from_expr(&mut counter, e);
        tracker.root.find_decl_inits(map);
        tracker
    }

    fn mutate_from_expr(&mut self, counter: &mut ScopeCounter, e: &Expr) {
        match e.node {
            ExprKind::If(_, ref then_expr, ref else_expr) => {
                let prev_id = self.current_scope;
                let (then_id, else_id) = self.current_block()
                                             .create_child_if(counter,
                                                              else_expr.is_some());
                self.set_current_block(then_id);
                self.mutate_from_block(counter, then_expr, BlockType::Normal);

                if let (Some(id), &Some(ref else_expr)) = (else_id, else_expr) {
                    self.set_current_block(id);
                    self.mutate_from_expr(counter, else_expr);
                }

                self.set_current_block(prev_id);
            }
            ExprKind::While(_, ref body, _) | ExprKind::Loop(ref body, _)=> {
                // cond will not contain a transition / decl
                self.mutate_from_block(counter, body, BlockType::Loop);
            }
            ExprKind::Match(_, ref arms) => {
                let prev_id = self.current_scope;
                let match_id = self.current_block().create_child_match(counter);

                for arm in arms.iter() {
                    let arm_scope_id = self.find_scope(match_id)
                                           .as_match()
                                           .new_arm(counter).id;

                    self.set_current_block(arm_scope_id);

                    for pat in &arm.pats {
                        let index = self.current_block().children.len() - 1;
                        let current_scope = self.current_scope;
                        self.current_block().decls.extend(
                            get_decls_from_pat(pat)
                            .into_iter()
                            .map(|(id, identifier)| {
                                Decl {
                                    ident: identifier,
                                    path: NodePath {
                                        id: id,
                                        index: index,
                                        scope: current_scope,
                                    },
                                    initializations: vec![],
                                    usages: vec![],
                                    is_initialized: true,
                                }
                            })
                        );
                    }

                    self.mutate_from_expr(counter, &arm.body);
                    self.set_current_block(match_id);
                }
                self.set_current_block(prev_id);
            }
            ExprKind::Block(ref block) => {
                self.mutate_from_block(counter, block, BlockType::Normal);
            }
            _ => {}
        }
    }

    fn mutate_from_stmt(&mut self, counter: &mut ScopeCounter, stmt: &Stmt) {
        self.current_block().push_node(stmt.id);
        match stmt.node {
            StmtKind::Semi(ref expr) | StmtKind::Expr(ref expr) => {
                self.mutate_from_expr(counter, expr);
            }
            StmtKind::Local(ref local) => {
                let current_scope = self.current_block().id;
                let index = self.current_block().children.len();
                self.current_block().decls.extend(
                    get_decls_from_pat(&local.pat)
                    .into_iter()
                    .map(|(id, identifier)| {
                        Decl {
                            ident: identifier,
                            path: NodePath {
                                id: id,
                                index: index,
                                scope: current_scope,
                            },
                            initializations: vec![],
                            usages: vec![],
                            is_initialized: local.init.is_some(),
                        }
                    })
                );
            }
            _ => {}
        }
    }

    fn mutate_from_block(&mut self, counter: &mut ScopeCounter, node: &Block, block_type: BlockType) {
        let prev_id = self.current_scope;
        let block_id = self.current_block().create_child_block(counter, block_type);
        self.set_current_block(block_id);

        for stmt in &node.stmts {
            self.mutate_from_stmt(counter, stmt);
        }

        self.set_current_block(prev_id)
    }

    #[allow(unknown_lints, needless_lifetimes)]
    pub fn find_scope<'p>(&'p mut self, id: ScopeId) -> &'p mut Scope {
        self.root.walk(&mut |scope: &'p mut Scope| {
            if scope.id() == id {
                Some(scope)
            } else {
                None
            }
        }).unwrap()
    }

    pub fn current_block(&mut self) -> &mut BlockScope {
        self.current_scope().as_block()
    }

    pub fn current_scope(&mut self) -> &mut Scope {
        let current_id = self.current_scope;
        self.find_scope(current_id)
    }

    pub fn set_current_block(&mut self, id: ScopeId) {
        self.current_scope = id;
    }
}

#[derive(Debug)]
pub struct IfScope {
    pub id: ScopeId,
    pub parent_id: ScopeId,
    pub then: Box<BlockScope>,
    pub else_: Option<Box<BlockScope>>,
}

impl IfScope {
    #[allow(unknown_lints, needless_lifetimes)]
    fn walk<'s, T: 's, F: FnMut(&'s mut Scope) -> Option<T> >(&'s mut self, f: &mut F) -> Option<T> {
        if let Some(v) = self.then.walk(f) {
            return Some(v);
        }

        if let Some(ref mut e) = self.else_ {
            if let Some(v) = e.walk(f) {
                return Some(v);
            }
        }

        None
    }
}

#[derive(Debug)]
pub struct MatchScope {
    pub id: ScopeId,
    pub parent_id: ScopeId,
    pub arms: Vec<Scope>,
}

impl MatchScope {
    #[allow(unknown_lints, needless_lifetimes)]
    fn walk<'s, T: 's, F: FnMut(&'s mut Scope) -> Option<T> >(&'s mut self, f: &mut F) -> Option<T> {
        for scope in &mut self.arms {
            if let Some(v) = scope.walk(f) {
                return Some(v);
            }
        }

        None
    }

    pub fn new_arm(&mut self, counter: &mut ScopeCounter) -> &mut BlockScope {
        let new_block = BlockScope::new(counter, self.id, BlockType::Normal);
        self.arms.push(Scope::Block(new_block));
        self.arms.last_mut().unwrap().as_block()
    }
}

#[derive(Debug)]
pub struct BlockScope {
    pub id: ScopeId,
    pub parent_id: Option<ScopeId>,
    pub children: Vec<ScopeOrNode>,
    pub decls: Vec<Decl>,
    pub block_type: BlockType,
}

impl BlockScope {

    fn new(counter: &mut ScopeCounter, parent_id: ScopeId, block_type: BlockType) -> BlockScope {
        BlockScope {
            id: counter.generate_new_id(),
            parent_id: Some(parent_id),
            children: vec![],
            decls: vec![],
            block_type: block_type,
        }
    }

    fn push_child(&mut self, scope: Scope) -> &mut Scope {
        self.children.push(ScopeOrNode::Scope(scope));
        self.children.last_mut().unwrap().as_scope()
    }

    pub fn push_node(&mut self, id: NodeId) {
        self.children.push(ScopeOrNode::Node(id));
    }

    pub fn create_child_if(&mut self, counter: &mut ScopeCounter, has_else: bool) -> (ScopeId, Option<ScopeId>) {
        let new_id = counter.generate_new_id();
        let then_block = Box::new(BlockScope::new(counter, new_id, BlockType::Normal));
        let then_id = then_block.id;

        let (else_block, else_id) = if has_else {
            let block = Box::new(BlockScope::new(counter, new_id, BlockType::Normal));
            let new_id = block.id;
            (Some(block), Some(new_id))
        } else {
            (None, None)
        };

        let parent_id = self.id;

        self.push_child(Scope::If(
            IfScope {
                id: new_id,
                parent_id: parent_id,
                then: then_block,
                else_: else_block,
            }
        ));

        (then_id, else_id)
    }

    pub fn create_child_match(&mut self, counter: &mut ScopeCounter) -> ScopeId {
        let new_id = counter.generate_new_id();
        let parent_id = self.id;

        self.push_child(Scope::Match(
            MatchScope {
                id: new_id,
                parent_id: parent_id,
                arms: vec![],
            }
        ));

        new_id
    }

    pub fn create_child_block(&mut self, counter: &mut ScopeCounter, block_type: BlockType) -> ScopeId {
        let new_block = BlockScope::new(counter, self.id, block_type);
        let new_id = new_block.id;
        self.push_child(Scope::Block(new_block));

        new_id
    }

    #[allow(unknown_lints, needless_lifetimes)]
    fn scopes<'s>(&'s mut self) -> Vec<&'s mut Scope> {
        self.children.iter_mut().filter_map(
            |s: &'s mut ScopeOrNode| {
                if let ScopeOrNode::Scope(ref mut inner) = *s {
                    Some(inner)
                }
                else {
                    None
                }
            }
        ).collect()
    }

    #[allow(unknown_lints, needless_lifetimes)]
    fn nodes<'s>(&'s mut self) -> Vec<(usize, NodeId)> {
        self.children.iter_mut().enumerate().filter_map(
            |(idx, s): (usize, &'s mut ScopeOrNode)| {
                if let ScopeOrNode::Node(inner) = *s {
                    Some((idx, inner))
                }
                else {
                    None
                }
            }
        ).collect()
    }

    #[allow(unknown_lints, needless_lifetimes)]
    pub fn find_scope<'p>(&'p mut self, id: ScopeId) -> &'p mut Scope {
        self.walk(&mut |scope: &'p mut Scope| {
            if scope.id() == id {
                Some(scope)
            } else {
                None
            }
        }).unwrap()
    }

    #[allow(unknown_lints, needless_lifetimes)]
    fn walk<'s, T: 's, F: FnMut(&'s mut Scope) -> Option<T> >(&'s mut self, f: &mut F) -> Option<T> {
        for scope in self.scopes() {
            if let Some(v) = scope.walk(f) {
                return Some(v);
            }
        }

        None
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

    #[allow(unknown_lints, needless_lifetimes)]
    pub fn walk<'s, T: 's, F: FnMut(&'s mut Scope) -> Option<T> >(&'s mut self, f: &mut F) -> Option<T> {
        let _self = self as * mut _;

        if let Some(v) = f(unsafe { &mut *_self }) {
            return Some(v)
        };

        match *self {
            Scope::Block(ref mut b) => {
                b.walk(f)
            }
            Scope::If(ref mut i) => {
                i.walk(f)
            }
            Scope::Match(ref mut m) => {
                m.walk(f)
            }
        }
    }

    #[allow(unknown_lints, needless_lifetimes)]
    pub fn find_scope<'p>(&'p mut self, id: ScopeId) -> &'p mut Scope {
        self.walk(&mut |scope: &'p mut Scope| {
            if scope.id() == id {
                Some(scope)
            } else {
                None
            }
        }).unwrap()
    }

    fn find_decl_inits(&mut self, ast_map: &AstMap) {
        self.walk(&mut |s| {
            if let Scope::Block(ref mut b) = *s {
                let it = b.nodes().into_iter()
                        .map(|(idx, x)| (idx, x, ast_map.lookup(x)))
                        .filter_map(|(idx, id, e)| {
                            if let ExprKind::Assign(ref lv, _) = e.node {
                                if let ExprKind::Path(_, ref p) = lv.node {
                                    if p.segments.len() == 1 {
                                        return Some((idx, id, p.segments.first().unwrap().identifier))
                                    }
                                }
                            }
                            None
                        }
                );
                for (idx, id, ident) in it {
                    let scope_id = b.id;
                    let was_decl = if let Some(ref mut decl) = b.decls.iter_mut().find(|d| d.ident.name == ident.name) {
                        if decl.is_initialized {
                            continue;
                        }

                        decl.initializations.push(NodePath {
                            id: id,
                            index: idx,
                            scope: scope_id,
                        });
                        true
                    } else { false };
                    if !was_decl {
                        if let Some(id) = b.parent_id {
                            b.find_scope(id).find_decl_inits(ast_map);
                        }
                    }
                    // if let ExprKind::Assign()
                }
            }
            Some(())
        });
    }

}

#[derive(Debug)]
pub enum ScopeOrNode {
    Scope(Scope),
    Node(NodeId),
}

impl ScopeOrNode {

    fn as_scope(&mut self) -> &mut Scope {
        match *self {
            ScopeOrNode::Scope(ref mut s) => s,
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
