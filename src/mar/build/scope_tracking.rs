use syntax::ast::{self, NodeId, ExprKind, Expr, Stmt, StmtKind, Block};
use syntax::visit;
use std::ascii::AsciiExt;

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
    ident: ast::Ident,
    id: NodeId,
    is_initialized: bool,
}

pub struct ScopeTracker {
    root: Scope,
    pub current_scope: ScopeId,
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
            }),
            current_scope: new_id,
        }, counter)
    }

    pub fn from_expr(e: Expr) -> ScopeTracker{
        let (mut tracker, mut counter) = ScopeTracker::new();
        tracker.mutate_from_expr(&mut counter, &e);
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
                self.mutate_from_block(counter, then_expr);

                if let (Some(id), &Some(ref else_expr)) = (else_id, else_expr) {
                    self.set_current_block(id);
                    self.mutate_from_expr(counter, else_expr);
                }

                self.set_current_block(prev_id);
            }
            ExprKind::Loop(ref body, _) => {
                self.mutate_from_block(counter, body);
            }
            ExprKind::While(_, ref body, _) => {
                // cond will not contain a transition / decl
                self.mutate_from_block(counter, body);
            }
            ExprKind::Match(_, ref arms) => {
                let prev_id = self.current_scope;
                let match_id = self.current_block().create_child_match(counter);

                for arm in arms.iter() {
                    let arm_scope_id = self.find_scope(match_id)
                                           .as_match()
                                           .new_arm(counter).id;

                    self.set_current_block(arm_scope_id);
                    self.mutate_from_expr(counter, &arm.body);
                    self.set_current_block(match_id);
                }
                self.set_current_block(prev_id);
            }
            ExprKind::Block(ref block) => {
                self.mutate_from_block(counter, block);
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

    fn mutate_from_block(&mut self, counter: &mut ScopeCounter, node: &Block) {
        for stmt in &node.stmts {
            self.mutate_from_stmt(counter, stmt);
        }
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
        let new_block = BlockScope::new(counter, self.id);
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
}

impl BlockScope {

    fn new(counter: &mut ScopeCounter, parent_id: ScopeId) -> BlockScope {
        BlockScope {
            id: counter.generate_new_id(),
            parent_id: Some(parent_id),
            children: vec![],
            decls: vec![],
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
        let then_block = Box::new(BlockScope::new(counter, new_id));
        let then_id = then_block.id;

        let (else_block, else_id) = if has_else {
            let block = Box::new(BlockScope::new(counter, new_id));
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

    pub fn create_child_block<'p>(&'p mut self, counter: &mut ScopeCounter) -> &'p mut BlockScope {
        let new_block = BlockScope::new(counter, self.id);

        let new_scope = self.push_child(Scope::Block(new_block));

        match *new_scope {
            Scope::Block(ref mut inner) => inner,
            _ => unreachable!(),
        }
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
