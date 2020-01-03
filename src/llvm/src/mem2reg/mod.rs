pub mod dominance;
pub mod transform;

use crate::instructions as LLVM;
use crate::mem2reg::transform::SSATransformer;

pub fn run(prog: &LLVM::Program) -> LLVM::Program {
    LLVM::Program {
        options: prog.options.clone(),
        declares: prog.declares.clone(),
        statics: prog.statics.clone(),
        functions: prog.functions.iter().map(SSATransformer::to_ssa).collect(),
    }
}
