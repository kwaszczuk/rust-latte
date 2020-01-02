use base::ast;

mod instructions;
mod operators;
mod compiler;
mod utils;
mod control_flow_graph;
mod optimizations {
    pub mod base;
    pub mod constants;
    pub mod dead_code;
    pub mod phis;
    pub mod branches;
}

use optimizations::base::{Optimizer, apply_optimizers};
use optimizations::constants::{ConstantsOptimizer};
use optimizations::dead_code::{DeadCodeOptimizer};
use optimizations::branches::{BranchesOptimizer};
use optimizations::phis::{PhisOptimizer};

pub fn compile(ast_tree: &ast::Program) -> instructions::Program {
    let mut prog = compiler::LLVMCompiler::run(&ast_tree);

    // optimize generated LLVM code
    #[cfg(not(feature="no-optimizations"))] {
        prog = optimize(prog, 1);
    }

    prog
}

fn optimize(prog: instructions::Program, opt_level: usize) -> instructions::Program {
    let mut optimizations: Vec<Box<dyn Optimizer>> = vec![];

    #[cfg(any(feature="all-optimizations", feature="optimizations-constants"))] {
        optimizations.push(Box::new(ConstantsOptimizer::new()));
    }
    #[cfg(any(feature="all-optimizations", feature="optimizations-dead-code"))] {
        optimizations.push(Box::new(DeadCodeOptimizer::new()));
    }
    #[cfg(any(feature="all-optimizations", feature="optimizations-branches"))] {
        optimizations.push(Box::new(BranchesOptimizer::new()));
    }
    #[cfg(any(feature="all-optimizations", feature="optimizations-phis"))] {
        optimizations.push(Box::new(PhisOptimizer::new()));
    }

    let (prog, _) = apply_optimizers(&prog, &mut optimizations, 100);
    prog
}
