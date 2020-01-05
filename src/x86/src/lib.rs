use llvm::instructions as LLVM;

pub mod instructions;
mod operators;
mod compiler;

pub fn compile(llvm_program: &LLVM::Program) -> instructions::Program {
    let mut prog = compiler::X86Compiler::run(&llvm_program);

    prog
}
