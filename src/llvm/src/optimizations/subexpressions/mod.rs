mod common_subexpressions_elimination;

use crate::optimizations::base::{Optimizer, apply_optimizers};
use crate::instructions as LLVM;

pub struct SubexpressionsOptimizer {
}

impl SubexpressionsOptimizer {
    pub fn new() -> Self {
        SubexpressionsOptimizer {
        }
    }
}

impl Optimizer for SubexpressionsOptimizer {
    fn run(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        let mut optimizations: Vec<Box<dyn Optimizer>> = vec![
            Box::new(common_subexpressions_elimination::Optimizer::new()),
        ];

        let (new_prog, _runs) = apply_optimizers(prog, &mut optimizations, 100);
        // println!("subexpressions optimizations executed {} times", runs);

        new_prog
    }
}
