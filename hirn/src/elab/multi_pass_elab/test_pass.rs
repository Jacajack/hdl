use crate::elab::ElabError;

use super::{ElabPass, full_elab::{FullElabCtx, FullElabCache}};

pub(super) struct TestPass {

}

impl ElabPass<FullElabCtx, FullElabCache> for TestPass {
	fn run(&mut self, _c: FullElabCtx) -> Result<FullElabCtx, ElabError> {
		todo!();
	}
}
