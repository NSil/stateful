use syntax::visit;
use syntax::ast;
use syntax::visit::Visitor;
use std::iter;
use std::mem;

use mar::build::scope_tracking;

struct AstMapper<'a> {
    mapping: Vec<Option<&'a ast::Expr>>,
}

impl<'a> AstMapper<'a> {
    fn new(count: u32) -> AstMapper<'a> {
        AstMapper {
            mapping: iter::repeat(None).take(count as usize).collect(),
        }
    }

    fn insert(&mut self, id: ast::NodeId, expr: &'a ast::Expr) {
        if self.mapping[id as usize - 1].is_some() {
            panic!("Node inserted twice! : {:?} - {:?}", id, expr);
        }
        self.mapping[id as usize - 1] = Some(expr);
    }

    fn into_ast_map(self) -> AstMap<'a> {
        AstMap {
            mapping: self.mapping
        }
    }
}

impl<'a> Visitor for AstMapper<'a> {

    fn visit_expr(&mut self, expr: &ast::Expr) {
        self.insert(expr.id, unsafe{ mem::transmute(expr) });
        visit::walk_expr(self, expr);
    }
}

pub struct AstMap<'a> {
    mapping: Vec<Option<&'a ast::Expr>>,
}

impl<'a> AstMap<'a> {

    pub fn from_expr(count: u32, expr: &'a ast::Expr) -> AstMap<'a> {
        let mut mapper = AstMapper::new(count);
        mapper.visit_expr(expr);
        mapper.into_ast_map()
    }

    pub fn lookup(&self, id: ast::NodeId) -> &'a ast::Expr {
        self.mapping[id as usize - 1].unwrap()
    }
}


// struct UsageFinder<'a>(&'a mut scope_tracking::ScopeTracker);

// impl<'a> Visitor for UsageFinder<'a> {

//     fn visit_expr(&mut self, expr: &ast::Expr) {

//     }
// }


