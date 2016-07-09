
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

pub struct ScopeManager {
    counter: ScopeCounter,
    root: BlockScope,
}

impl ScopeManager {

    fn new() -> ScopeManager {
        let mut counter = ScopeCounter{ counter: 0 };
        let new_id = counter.generate_new_id();
        ScopeManager {
            counter: counter,
            root: BlockScope {
                id: new_id,
                parent_id: None,
                children: vec![],
            },
        }
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
    pub parent_id: Option<ScopeId>,
    pub then: Box<BlockScope>,
    pub else_: Option<Box<BlockScope>>,
}

#[derive(Debug)]
pub struct MatchScope {
    pub id: ScopeId,
    pub parent_id: Option<ScopeId>,
    pub arms: Vec<BlockScope>,
}

#[derive(Debug)]
pub struct BlockScope {
    pub id: ScopeId,
    pub parent_id: Option<ScopeId>,
    pub children: Vec<Scope>,
}

impl BlockScope {

    fn push_child<'p>(&'p mut self, scope: Scope) -> &'p mut Scope {
        self.children.push(scope);
        self.children.last_mut().unwrap()
    }

    pub fn create_child_if<'p>(&'p mut self, counter: &mut ScopeCounter, has_else: bool) -> &'p mut IfScope {
        let then_block = Box::new(counter.new_block(this.id));

        let else_block = if has_else { Some(Box::new(counter.new_block(this.id))) }
                         else        { None };

        let new_id = counter.generate_new_id();
        let parent_id = parent.id;

        let new_scope = self.push_child(Scope::If(
            IfScope {
                id: new_id,
                parent_id: Some(parent_id),
                then: then_block,
                else_: else_block,
            }
        ));

        match *new_scope {
            Scope::If(ref mut inner) => inner,
            _ => unreachable!(),
        }
    }

    pub fn create_child_match<'p>(&'p mut self, counter: &mut ScopeCounter) -> &'p mut MatchScope {
        let new_id = counter.generate_new_id();
        let parent_id = parent.id;

        let new_scope = self.push_child(Scope::Match(
            MatchScope {
                id: new_id,
                parent_id: Some(parent_id),
                arms: vec![],
            }
        ));

        match *new_scope {
            Scope::Match(ref mut inner) => inner,
            _ => unreachable!(),
        }
    }

    pub fn create_child_block<'p>(&'p mut self, counter: &mut ScopeCounter) -> &'p mut BlockScope {
        let parent_id = parent.id;
        let new_block = counter.new_block(parent.id);

        let new_scope = self.push_child(parent,
            Scope::Block(new_block)
        );

        match *new_scope {
            Scope::Block(ref mut inner) => inner,
            _ => unreachable!(),
        }
    }

}

#[derive(Debug)]
pub enum Scope {
    Block(BlockScope),
    If(IfScope),
    Match(MatchScope),
}
