use mar::build::Builder;
use mar::build::scope::LoopScope;
use mar::repr::*;
use syntax::ast::{self, ExprKind};
use syntax::codemap::Span;
use syntax::ptr::P;
use aster::AstBuilder;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn expr(&mut self,
                extent: CodeExtent,
                block: BasicBlock,
                expr: &P<ast::Expr>) -> BasicBlock {
        let expr = self.expand_moved(expr);

        // There's no reason for us to transform expressions if they don't contain any transitions.
        if !self.contains_transition(&expr) {
            return self.into(extent, block, expr);
        }

        match expr.node {
            ExprKind::Block(ref ast_block) => {
                self.into(extent, block, ast_block)
            }
            ExprKind::Continue(label) => {
                self.break_or_continue(expr.span,
                                       label.map(|label| label.node),
                                       block,
                                       |loop_scope| loop_scope.continue_block)
            }
            ExprKind::Break(label) => {
                self.break_or_continue(expr.span,
                                       label.map(|label| label.node),
                                       block,
                                       |loop_scope| loop_scope.break_block)
            }
            ExprKind::Ret(Some(_)) => {
                self.cx.span_fatal(expr.span, "cannot return a value");
            }
            ExprKind::Ret(None) => {
                self.exit_scope(expr.span, extent, block, END_BLOCK);
                self.start_new_block(expr.span, Some("AfterReturn"))
            }
            ExprKind::If(ref cond_expr, ref then_expr, ref else_expr) => {
                // FIXME: This does not handle the `cond_expr` containing a transition yet.

                let mut then_block = self.start_new_block(expr.span, Some("Then"));
                let mut else_block = self.start_new_block(expr.span, Some("Else"));

                self.terminate(expr.span, block, TerminatorKind::If {
                    cond: cond_expr.clone(),
                    targets: (then_block, else_block),
                });

                then_block = self.into(extent, then_block, then_expr);
                else_block = self.into(extent, else_block, else_expr);

                let join_block = self.start_new_block(expr.span, Some("IfJoin"));

                self.terminate(
                    then_expr.span,
                    then_block,
                    TerminatorKind::Goto { target: join_block });

                self.terminate(
                    match *else_expr {
                        Some(ref expr) => expr.span,
                        None => expr.span,
                    },
                    else_block,
                    TerminatorKind::Goto { target: join_block });

                join_block
            }
            ExprKind::Match(ref discriminant, ref arms) => {
                self.match_expr(extent, expr.span, block, discriminant.clone(), arms)
            }
            ExprKind::Loop(ref body, label) => {
                self.expr_loop(extent, block, None, body, label)
            }
            ExprKind::While(ref cond_expr, ref body, label) => {
                self.expr_loop(extent, block, Some(cond_expr), body, label)
            }
            ExprKind::ForLoop(..) |
            ExprKind::IfLet(..)   |
            ExprKind::WhileLet(..) => {
                panic!("{:?} Should never reach this point - `preapare` should have desugared this.", expr);
            }
            ExprKind::Assign(ref left, _) => {
                if let Some(ident) = ident_from_assign_path(left) {
                    self.assign_decl(ident);
                    let builder = AstBuilder::new();
                    self.cfg.block_data_mut(block).statements.insert(0, Statement::Expr(
                        builder.stmt().let_().id(ident).build()
                    ));
                }

                // FIXME: Don't handle yield in assign yet... (Might solve in `simplify`)
                self.into(extent, block, &expr)
            }
            _ => {
                self.cx.span_bug(expr.span,
                                 &format!("don't know how to handle {:#?} yet", expr))
            }
        }
    }

    fn expr_loop(&mut self,
                 extent: CodeExtent,
                 block: BasicBlock,
                 condition: Option<&P<ast::Expr>>,
                 body: &P<ast::Block>,
                 label: Option<ast::SpannedIdent>) -> BasicBlock {
        // [block] --> [loop_block] ~~> [loop_block_end] -1-> [exit_block]
        //                  ^                  |
        //                  |                  0
        //                  |                  |
        //                  |                  v
        //           [body_block_end] <~~~ [body_block]
        //
        // If `opt_cond_expr` is `None`, then the graph is somewhat simplified:
        //
        // [block] --> [loop_block / body_block ] ~~> [body_block_end]    [exit_block]
        //                         ^                          |
        //                         |                          |
        //                         +--------------------------+

        let loop_block = self.start_new_block(body.span, Some("Loop"));
        let exit_block = self.start_new_block(body.span, Some("LoopExit"));

        // start the loop
        self.terminate(
            body.span,
            block,
            TerminatorKind::Goto { target: loop_block });

        self.in_loop_scope(extent, label, loop_block, exit_block, |this| {
            // conduct the test, if necessary
            let body_block;
            if let Some(cond_expr) = condition {
                // FIXME: This does not yet handle the expr having a transition.

                body_block = this.start_new_block(cond_expr.span, Some("LoopBody"));

                this.terminate(
                    cond_expr.span,
                    loop_block,
                    TerminatorKind::If {
                        cond: cond_expr.clone(),
                        targets: (body_block, exit_block),
                    });
            } else {
                body_block = loop_block;
            }

            // execute the body, branching back to the test
            let body_block_end = this.into(extent, body_block, body);
            this.terminate(
                body.span,
                body_block_end,
                TerminatorKind::Goto { target: loop_block });

            // final point is exit_block
            exit_block
        })
    }

    fn break_or_continue<F>(&mut self,
                            span: Span,
                            label: Option<ast::Ident>,
                            block: BasicBlock,
                            exit_selector: F)
                            -> BasicBlock
        where F: FnOnce(&LoopScope) -> BasicBlock
    {
        let loop_scope = self.find_loop_scope(span, label);
        let exit_block = exit_selector(&loop_scope);
        self.exit_scope(span, loop_scope.extent, block, exit_block);

        // Even though we've exited `block`, there could be code following the break/continue. To
        // keep rust happy, we'll create a new block that has an edge to `block`, even though
        // control will never actually flow into this block.
        self.start_new_block(span, Some("AfterBreakOrContinue"))
    }
}

fn ident_from_assign_path(e: &ast::Expr) -> Option<ast::Ident> {
    let path = if let ExprKind::Path(_, ref path) = e.node {
        path
    } else {
        return None;
    };

    if path.global {
        return None;
    }

    if path.segments.len() != 1 {
        return None;
    }

    Some(path.segments.first().unwrap().identifier)
}
