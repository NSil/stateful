use mar::repr::*;
use mar::build::scope_tracking::ScopeTracker;
use syntax::visit::{Visitor, walk_expr};
use syntax::ast::{Stmt, StmtKind, Expr, ExprKind, Path, Ident};

// - Shadowing logic needs to be moved to a different phase.

// Search for drops in the current block, per decl. If no drops exist, search for assignment.
// If no asignments exist, add it to the terminating scope's forward_decls.
// If an assignment exists, add it to the terminating scope's decls.
// Remove from forward_decls.
// Do same for successors.

#[allow(needless_range_loop)]
fn fix_forward_decls(blocks: &mut [BasicBlockData], tracker: &ScopeTracker) {
    for block_id in 0..blocks.len() {
        {
            let block = &mut blocks[block_id];
            let statements = &block.statements;
            block.forward_decls.retain(|&(_, id)| {
                statements.iter()
                    .filter_map(|s| {
                        if let Statement::Drop { ref lvalue, .. } = *s {
                            Some(*lvalue == id)
                        } else {
                            None
                        }
                    })
                    .count() == 0
            });
        }

        let statements: Vec<Stmt> = blocks[block_id].statements.iter().filter_map(|s| {
            if let Statement::Expr(ref ex) = *s {
                Some(ex.clone())
            }
            else {
                None
            }
        }).collect();

        let mut v = AssignmentVisitor {
            tracker: tracker,
            blocks: blocks,
            block_id: block_id,
        };

        for stmt in &statements {
            v.visit_stmt(stmt);
        }
    }
}


struct AssignmentVisitor<'a> {
    tracker: &'a ScopeTracker,
    blocks: &'a mut [BasicBlockData],
    block_id: usize,
}

impl<'a> Visitor for AssignmentVisitor<'a> {
    fn visit_expr(&mut self, expression: &Expr) {

        if let Some(id) = get_assign_path(expression) {
            if self.blocks[self.block_id]
                .forward_decls
                .iter()
                .map(|&(_, d)| d)
                .any(|d| d.name == id.name) {
                let successors = self.blocks[self.block_id].terminator().successors();
                for block_idx in successors {
                    let block = &mut self.blocks[block_idx.index()];
                    // block.
                }
            }
        }
        walk_expr(self, expression);
    }
}


fn get_assign_path(e: &Expr) -> Option<Ident> {
    if let ExprKind::Assign(ref left_hand_expression, _) = e.node {
        let p = if let ExprKind::Path(_, ref p) = left_hand_expression.node {
            p
        } else {
            return None;
        };
        if p.global {
            return None;
        }
        if p.segments.len() != 1 {
            return None;
        }

        Some(p.segments.first().unwrap().identifier)
    } else {
        None
    }
}
