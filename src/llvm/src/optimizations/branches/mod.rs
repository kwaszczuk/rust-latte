mod propagate_jumps;
mod reduce_paths;

use crate::optimizations::base::{Optimizer, apply_optimizers};
use crate::instructions as LLVM;

pub struct BranchesOptimizer {
}

impl BranchesOptimizer {
    pub fn new() -> Self {
        BranchesOptimizer {
        }
    }
}

impl Optimizer for BranchesOptimizer {
    fn run(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        let mut optimizations: Vec<Box<dyn Optimizer>> = vec![
            Box::new(propagate_jumps::Optimizer::new()),
            Box::new(reduce_paths::Optimizer::new()),
        ];

        let (new_prog, _runs) = apply_optimizers(prog, &mut optimizations, 100);
        // println!("branches optimizations executed {} times", runs);

        new_prog
    }
}
