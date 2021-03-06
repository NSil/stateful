#![cfg_attr(feature = "unstable", feature(plugin, plugin_registrar, rustc_private, quote))]
#![cfg_attr(feature = "clippy", plugin(clippy))]

extern crate aster;
extern crate rustc_plugin;
extern crate rustc_errors as errors;

#[macro_use] extern crate log;
#[macro_use] extern crate syntax;

pub mod mar;

use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::base::{
    Annotatable,
    ExtCtxt,
    MultiModifier,
};
use syntax::print::pprust;
pub use mar::transform::pass::MarPass;

fn expand_generator(cx: &mut ExtCtxt,
                    _sp: Span,
                    meta_item: &ast::MetaItem,
                    annotatable: Annotatable) -> Annotatable {
    let item = match annotatable {
        Annotatable::Item(item) => item,
        _ => {
            cx.span_err(
                meta_item.span,
                "`generator` may only be applied to functions");

            return annotatable;
        }
    };

    let mut mar = match mar::build::construct(cx, item.clone()) {
        Ok(mar) => mar,
        Err(mar::build::Error) => {
            return Annotatable::Item(item);
        }
    };

    if let Some(item) = mar::translate::translate(cx, &mar) {
        debug!("{}", pprust::item_to_string(&item));
    }

    let mut pass_manager = mar::transform::pass_manager::PassManager::new();
    pass_manager.add_pass(Box::new(mar::transform::remove_dead_blocks::RemoveDeadBlocks::new()));
    pass_manager.add_pass(Box::new(mar::transform::simplify_cfg::SimplifyCfg::new()));
    pass_manager.add_pass(Box::new(mar::transform::remove_dead_blocks::RemoveDeadBlocks::new()));
    pass_manager.run(&mut mar);

    match mar::translate::translate(cx, &mar) {
        Some(item) => {
            debug!("{}", pprust::item_to_string(&item));

            Annotatable::Item(item)
        }
        None => {
            // We had an error, so just return the input item for a lack of a better option.
            Annotatable::Item(item.clone())
        }
    }
}

#[plugin_registrar]
#[doc(hidden)]
pub fn plugin_registrar(registry: &mut rustc_plugin::Registry) {
    let builder = aster::AstBuilder::new();

    registry.register_syntax_extension(builder.name("generator"),
                                       MultiModifier(Box::new(expand_generator)));
}
