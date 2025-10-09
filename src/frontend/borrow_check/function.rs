use crate::{context::CtxtRef, frontend::thir::Body};
pub struct BorrowCheck<'a> {
    _ctxt: CtxtRef<'a>,
}
impl<'a> BorrowCheck<'a> {
    pub fn new(_: &'a Body, _ctxt: CtxtRef<'a>) -> Self {
        Self { _ctxt }
    }
    pub fn check(self) {}
}
