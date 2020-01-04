use base::ast;

pub mod instructions;
mod operators;
mod compiler;
mod utils;
mod control_flow_graph;
mod mem2reg;
mod optimizations {
    pub mod base;
    pub mod constants;
    pub mod dead_code;
    pub mod phis;
    pub mod branches;
    pub mod globals;
}

use optimizations::base::{Optimizer, apply_optimizers};
use optimizations::constants::{ConstantsOptimizer};
use optimizations::dead_code::{DeadCodeOptimizer};
use optimizations::branches::{BranchesOptimizer};
use optimizations::phis::{PhisOptimizer};
use optimizations::globals;

pub fn compile(ast_tree: &ast::Program) -> instructions::Program {
    let mut prog = compiler::LLVMCompiler::run(&ast_tree);

    prog.options.insert("mem2reg".to_string(), false);
    #[cfg(not(feature="no-mem2reg"))] {
        prog.options.insert("mem2reg".to_string(), true);
        prog = mem2reg::run(&prog);
    }

    prog.options.insert("optimize".to_string(), false);
    #[cfg(not(feature="no-optimizations"))] {
        prog.options.insert("optimize".to_string(), true);
        prog = optimize(&mut prog, 1);
    }

    prog
}

fn optimize(prog: &mut instructions::Program, opt_level: usize) -> instructions::Program {
    let mut optimizations: Vec<Box<dyn Optimizer>> = vec![];

    prog.options.insert("optimizations-constants".to_string(), false);
    #[cfg(any(feature="all-optimizations", feature="optimizations-constants"))] {
        prog.options.insert("optimizations-constants".to_string(), true);
        optimizations.push(Box::new(ConstantsOptimizer::new()));
    }

    prog.options.insert("optimizations-dead-code".to_string(), false);
    #[cfg(any(feature="all-optimizations", feature="optimizations-dead-code"))] {
        prog.options.insert("optimizations-dead-code".to_string(), true);
        optimizations.push(Box::new(DeadCodeOptimizer::new()));
    }

    prog.options.insert("optimizations-branches".to_string(), false);
    #[cfg(any(feature="all-optimizations", feature="optimizations-branches"))] {
        prog.options.insert("optimizations-branches".to_string(), true);
        optimizations.push(Box::new(BranchesOptimizer::new()));
    }

    prog.options.insert("optimizations-phis".to_string(), false);
    #[cfg(any(feature="all-optimizations", feature="optimizations-phis"))] {
        prog.options.insert("optimizations-phis".to_string(), true);
        optimizations.push(Box::new(PhisOptimizer::new()));
    }

    let (prog, _) = apply_optimizers(prog, &mut optimizations, globals::MAX_OPTIMIZATION_ITERATIONS);
    prog
}
