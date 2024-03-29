use llvm::instructions as LLVM;

pub mod instructions;
mod operators;
mod compiler;
mod register_allocation;

pub fn compile(llvm_program: &LLVM::Program) -> instructions::Program {
    let prog = compiler::X86Compiler::run(&llvm_program);

    prog
}
