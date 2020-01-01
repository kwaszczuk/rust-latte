use base::ast;

mod instructions;
mod operators;
mod compiler;
mod utils;
mod optimizations {
    pub mod base;
    pub mod constants;
    pub mod dead_code;
}

use optimizations::base::{Optimizer, apply_optimizers};
use optimizations::constants::{ConstantsOptimizer};
use optimizations::dead_code::{DeadCodeOptimizer};

pub fn compile(ast_tree: &ast::Program) -> instructions::Program {
    let mut prog = compiler::LLVMCompiler::run(&ast_tree);

    // optimize generated LLVM code
    prog = optimize(prog, 1);

    prog
}

fn optimize(prog: instructions::Program, opt_level: usize) -> instructions::Program {
    let mut optimizations: Vec<Box<dyn Optimizer>> = vec![
        Box::new(ConstantsOptimizer::new()),
        Box::new(DeadCodeOptimizer::new()),
    ];

    let (prog, _) = apply_optimizers(&prog, &mut optimizations, 1000);
    prog
}
