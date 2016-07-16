use mar::build::Builder;
use mar::repr::*;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn match_expr(&mut self,
                      extent: CodeExtent,
                      span: Span,
                      block: BasicBlock,
                      discriminant: P<ast::Expr>,
                      arms: &[ast::Arm])
                      -> BasicBlock {
        let targets = arms.iter()
            .map(|arm| {
                Arm {
                    pats: arm.pats.clone(),
                    guard: arm.guard.clone(),
                    block: self.start_new_block(span, Some("Arm")),
                }
            })
            .collect::<Vec<_>>();

        let join_block = self.start_new_block(span, Some("MatchJoin"));

        let match_id = self.scope_tracker.current_block().create_child_match(&mut self.scope_counter);

        for (arm, target) in arms.iter().zip(targets.iter()) {
            let arm_block = self.in_scope(extent, span, block, |this| {
                this.add_decls_from_pats(extent,
                                         target.block,
                                         arm.pats.iter());

                let prev_id = this.scope_tracker.current_scope;
                let arm_scope_id = this.scope_tracker.find_scope(match_id)
                                                     .as_match()
                                                     .new_arm(&mut this.scope_counter).id;

                this.scope_tracker.set_current_block(arm_scope_id);
                let ret_block = this.expr(extent, target.block, &arm.body);
                this.scope_tracker.set_current_block(prev_id);

                ret_block
            });

            self.terminate(span, arm_block, TerminatorKind::Goto { target: join_block });
        }

        self.terminate(span, block, TerminatorKind::Match {
            discr: discriminant.clone(),
            targets: targets,
        });

        join_block
    }
}
