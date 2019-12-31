use base::ast;

mod instructions;
mod operators;
mod compiler;
mod utils;
mod optimizations {
    pub mod constants_folding;
    pub mod base;
}

use optimizations::base::{Optimizer};
use optimizations::constants_folding::{ConstantsFolding};

pub fn compile(ast_tree: &ast::Program) -> instructions::Program {
    let mut prog = compiler::LLVMCompiler::run(&ast_tree);

    // optimize generated LLVM code
    prog = optimize(prog, 1);

    prog
}

fn optimize(mut prog: instructions::Program, opt_level: usize) -> instructions::Program {
    let mut optimizations: Vec<Box<dyn Optimizer>> = vec![
        Box::new(ConstantsFolding::new()),
    ];

    for mut opt in optimizations {
        prog = opt.run(prog);
    }

    prog
}
