mod folding;
mod propagation;

use crate::optimizations::base::{Optimizer, apply_optimizers};
use crate::instructions as LLVM;

pub struct ConstantsOptimizer {
}

impl ConstantsOptimizer {
    pub fn new() -> Self {
        ConstantsOptimizer {
        }
    }
}

impl Optimizer for ConstantsOptimizer {
    fn run(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        let mut optimizations: Vec<Box<dyn Optimizer>> = vec![
            Box::new(folding::Optimizer::new()),
            Box::new(propagation::Optimizer::new()),
        ];

        let (new_prog, runs) = apply_optimizers(prog, &mut optimizations, 1000);
        println!("constants optimizations executed {} times", runs);

        new_prog
    }
}
