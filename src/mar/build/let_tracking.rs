use syntax::visit;
use syntax::ast;
ust std::iter;

struct AstMapper<'a> {
    mapping: Vec<Option<&'a ast::Expr>>,
}

impl<'a> AstMapper<'a> {
    fn new(count: u32) -> AstMapper {
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

    fn into_ast_map(self) -> AstMap {
        AstMap {
            mapping: self.mapping.into_iter().map(|x| x.unwrap()).collect()
        }
    }
}

impl visit::Visitor for AstMapper {

    fn visit_expr(&mut self, expr: &ast::Expr) {
        self.insert(expr.id, expr);
        visit::walk_expr(self, expr);
    }
}

struct AstMap<'a> {
    mapping: Vec<&'a ast::Expr>,
}

impl<'a> AstMap<'a> {
    pub fn lookup(&self, id: ast::NodeId) -> &'a ast::Expr {
        self.mapping[id as usize - 1]
    }
}
