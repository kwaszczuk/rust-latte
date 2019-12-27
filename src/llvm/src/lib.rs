use base::ast;
extern crate parser;
mod instructions;
mod operators;
mod compiler;

pub fn compile(ast_tree: &ast::Program) -> instructions::Program {
    compiler::LLVMCompiler::run(&ast_tree)
}
