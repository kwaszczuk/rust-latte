mod unreachable_blocks;
mod unreachable_instructions;
mod unused_assignments;

use crate::optimizations::base::{Optimizer, apply_optimizers};
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
        ];

        let (new_prog, runs) = apply_optimizers(prog, &mut optimizations, 100);
        // println!("dead code optimizations executed {} times", runs);

        new_prog
    }
}
