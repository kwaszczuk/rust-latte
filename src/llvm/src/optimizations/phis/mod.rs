mod trivial_phi;

use crate::optimizations::base::{Optimizer, apply_optimizers};
use crate::instructions as LLVM;

pub struct PhisOptimizer {
}

impl PhisOptimizer {
    pub fn new() -> Self {
        PhisOptimizer {
        }
    }
}

impl Optimizer for PhisOptimizer {
    fn run(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        let mut optimizations: Vec<Box<dyn Optimizer>> = vec![
            Box::new(trivial_phi::Optimizer::new()),
        ];

        let (new_prog, _runs) = apply_optimizers(prog, &mut optimizations, 100);
        // println!("phis optimizations executed {} times", runs);

        new_prog
    }
}
