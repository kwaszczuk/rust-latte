use crate::instructions as LLVM;

pub trait Optimizer {
    fn run(&mut self, prog: LLVM::Program) -> LLVM::Program;
}
