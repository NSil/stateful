use mar::repr::*;
use mar::build::scope_tracking::ScopeTracker;
use syntax::visit::{Visitor, walk_expr, walk_mac};
use syntax::ast::{Stmt, StmtKind, Expr, ExprKind, Path, Ident, Mac, BindingMode, Mutability};
use aster;

// - Shadowing logic needs to be moved to a different phase.

// Search for drops in the current block, per decl. If no drops exist, search for assignment.
// If no asignments exist, add it to the terminating scope's forward_decls.
// If an assignment exists, add it to the terminating scope's decls.
// Remove from forward_decls.
// Do same for successors.

#[allow(needless_range_loop)]
pub fn fix_forward_decls(blocks: &mut [BasicBlockData], tracker: &ScopeTracker) {
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
        let mut new_lets: Vec<_> = {
            let sp = blocks[block_id].span;
            let mut v = AssignmentVisitor {
                tracker: tracker,
                blocks: blocks,
                block_id: block_id,
                popped: Vec::new(),
            };


            for stmt in &statements {
                v.visit_stmt(stmt);
            }

            let mut builder = aster::AstBuilder::new();
            v.popped.into_iter().map(|id| {
                Statement::Let{
                    span: sp,
                    pat: builder.pat().id(id),
                    ty: None,
                    init: None,
                }
            }).collect()
        };
        new_lets.extend(blocks[block_id].statements.clone());
        blocks[block_id].statements = new_lets;

        let fwd: Vec<_> = blocks[block_id].forward_decls.drain(..).collect();
        let successors = blocks[block_id].terminator().successors();
        for s in successors {
            blocks[s.index()].forward_decls.extend(fwd.clone());
        }

    }
}


struct AssignmentVisitor<'a> {
    tracker: &'a ScopeTracker,
    blocks: &'a mut [BasicBlockData],
    block_id: usize,
    popped: Vec<Ident>,
}

impl<'a> Visitor for AssignmentVisitor<'a> {
    fn visit_expr(&mut self, expression: &Expr) {

        if let Some(id) = get_assign_path(expression) {

            println!("found assign! {:?}", expression);

            let mut assigned_decl = None;
            let forward_decls = self.blocks[self.block_id].forward_decls.clone();
            println!("d: {:?}", forward_decls);
            for (idx, (vd, ident)) in forward_decls.into_iter().enumerate() {
                if ident.name != id.name {
                    println!("irrelevant assign {:?}", expression);
                    continue;
                }
                println!("relevant assign {:?}", expression);
                let successors = self.blocks[self.block_id].terminator().successors();
                for block_idx in successors {
                    let block = &mut self.blocks[block_idx.index()];
                    block.decls.push((vd.clone(), ident.clone()));
                }
                assigned_decl = Some(idx);
            }
            println!("ad: {:?}", assigned_decl);

            if let Some(idx) = assigned_decl {
                self.popped.push(self.blocks[self.block_id].forward_decls.remove(idx).1);
            }
        } else {
            println!("not assign {:?}", expression);
        }

        walk_expr(self, expression);
    }

    fn visit_mac(&mut self, _mac: &Mac) {
        walk_mac(self, _mac)
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
