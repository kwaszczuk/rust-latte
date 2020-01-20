mod liveness;
mod interference_graph;
mod colouring;

use llvm::instructions as LLVM;
use crate::instructions as X86;
use colouring::{Colouring};
use interference_graph::{InterferenceGraph};
use std::collections::{HashMap};

fn allocatable_registers() -> Vec<X86::Register> {
    vec![
        X86::Register::RBX,
        X86::Register::RSI,
        X86::Register::RDI,
        X86::Register::R8,
        X86::Register::R9,
        X86::Register::R10,
        X86::Register::R11,
        X86::Register::R12,
        X86::Register::R13,
        X86::Register::R14,
        X86::Register::R15,
    ]
}

pub fn allocate_registers(fun: &LLVM::Function) -> Colouring<LLVM::Register, X86::Register> {

    let mut args_precolouring = HashMap::new();
    for arg_idx in 0..fun.args.len() {
        if arg_idx < 6 {
            let reg = match arg_idx {
                0 => X86::Register::RDI,
                1 => X86::Register::RSI,
                2 => X86::Register::RDX,
                3 => X86::Register::RCX,
                4 => X86::Register::R8,
                5 => X86::Register::R9,
                _ => { panic!("should not happen") },
            };
            args_precolouring.insert(
                LLVM::Register::new("".to_string(), arg_idx),
                reg.clone()
            );
        }
    }
    let graph = InterferenceGraph::from(fun);

    let colouring = Colouring::get(&graph, &allocatable_registers(), &args_precolouring);
    colouring
}

