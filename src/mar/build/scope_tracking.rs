// use syntax::ast::NodeId;
pub type NodeId = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

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
            }),
            current_scope: new_id,
        }, counter)
    }

    #[allow(unknown_lints, needless_lifetimes)]
    pub fn find_scope<'p>(&'p mut self, id: ScopeId) -> &'p mut Scope {
        self.root.walk(&|scope: &'p mut Scope| {
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

pub struct ScopeCounter {
    counter: u32
}

impl ScopeCounter {

    fn generate_new_id(&mut self) -> ScopeId {
        self.counter += 1;
        ScopeId(self.counter - 1)
    }

    fn new_block(&mut self, parent_id: ScopeId) -> BlockScope {
        BlockScope {
            id: self.generate_new_id(),
            parent_id: Some(parent_id),
            children: vec![],
        }
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
    fn walk<'s, T: 's, F: Fn(&'s mut Scope) -> Option<T> >(&'s mut self, f: &F) -> Option<T> {
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
    fn walk<'s, T: 's, F: Fn(&'s mut Scope) -> Option<T> >(&'s mut self, f: &F) -> Option<T> {
        for scope in &mut self.arms {
            if let Some(v) = scope.walk(f) {
                return Some(v);
            }
        }

        None
    }

    pub fn new_arm(&mut self, counter: &mut ScopeCounter) -> &mut BlockScope {
        let new_block = counter.new_block(self.id);
        self.arms.push(Scope::Block(new_block));
        self.arms.last_mut().unwrap().as_block()
    }
}

#[derive(Debug)]
pub struct BlockScope {
    pub id: ScopeId,
    pub parent_id: Option<ScopeId>,
    pub children: Vec<ScopeOrNode>,
}

impl BlockScope {

    fn push_child(&mut self, scope: Scope) -> &mut Scope {
        self.children.push(ScopeOrNode::Scope(scope));
        self.children.last_mut().unwrap().as_scope()
    }

    pub fn push_node(&mut self, id: NodeId) {
        self.children.push(ScopeOrNode::Node(id));
    }

    pub fn create_child_if(&mut self, counter: &mut ScopeCounter, has_else: bool) -> (ScopeId, Option<ScopeId>) {
        let new_id = counter.generate_new_id();
        let then_block = Box::new(counter.new_block(new_id));
        let then_id = then_block.id;

        let (else_block, else_id) = if has_else {
            let block = Box::new(counter.new_block(new_id));
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
        let new_block = counter.new_block(self.id);

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
    fn walk<'s, T: 's, F: Fn(&'s mut Scope) -> Option<T> >(&'s mut self, f: &F) -> Option<T> {
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
    fn walk<'s, T: 's, F: Fn(&'s mut Scope) -> Option<T> >(&'s mut self, f: &F) -> Option<T> {
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
