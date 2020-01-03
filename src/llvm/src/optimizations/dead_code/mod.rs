mod unreachable_blocks;
mod unreachable_instructions;
mod unused_assignments;
mod unused_statics_declares;

use crate::optimizations::base::{Optimizer, apply_optimizers};
use crate::optimizations::globals;
use crate::instructions as LLVM;

pub struct DeadCodeOptimizer {
}

impl DeadCodeOptimizer {
    pub fn new() -> Self {
        DeadCodeOptimizer {
        }
    }
}

impl Optimizer for DeadCodeOptimizer {
    fn run(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        let mut optimizations: Vec<Box<dyn Optimizer>> = vec![
            Box::new(unreachable_blocks::Optimizer::new()),
            Box::new(unreachable_instructions::Optimizer::new()),
            Box::new(unused_assignments::Optimizer::new()),
            Box::new(unused_statics_declares::Optimizer::new()),
        ];

        let (new_prog, _runs) = apply_optimizers(prog, &mut optimizations, globals::MAX_OPTIMIZATION_ITERATIONS);
        // println!("dead code optimizations executed {} times", runs);

        new_prog
    }
}
