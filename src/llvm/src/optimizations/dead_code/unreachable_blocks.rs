
use std::collections::{HashMap};

use crate::instructions as LLVM;
use crate::optimizations::base;
use crate::utils::{blocks_to_instructions, instructions_to_blocks};

pub struct Optimizer {
    used_blocks: HashMap<LLVM::Label, bool>
}

impl Optimizer {
    pub fn new() -> Self {
        Optimizer {
            used_blocks: HashMap::new(),
        }
    }

    pub fn optimize_program(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        let new_functions = prog.functions.iter().map(|f| {
            let new_f = self.optimize_function(f);
            new_f
        }).collect();

        LLVM::Program {
            declares: prog.declares.clone(),
            statics: prog.statics.clone(),
            functions: new_functions,
        }
    }

    pub fn optimize_function(&mut self, fun: &LLVM::Function) -> LLVM::Function {
        self.used_blocks.clear();
        self.find_used_blocks(&fun.body);

        let only_used_blocks = fun.body.iter().cloned().filter(|b| {
            if let Some(block_label) = &b.label {
                if let Some(_) = self.used_blocks.get(&block_label) {
                    true
                } else {
                    false
                }
            } else {
                true
            }
        }).collect();

        let instrs = blocks_to_instructions(&only_used_blocks);
        let new_instrs = self.fix_phis(&instrs);
        let new_body = instructions_to_blocks(&new_instrs);

        LLVM::Function {
            ret_ty: fun.ret_ty.clone(),
            name: fun.name.clone(),
            args: fun.args.iter().cloned().collect(),
            body: new_body,
        }
    }

    fn find_used_blocks(&mut self, blocks: &Vec<LLVM::Block>) {
        use LLVM::Instr::*;

        for b in blocks {
            for i in &b.instrs {
                match i {
                    Branch(LLVM::Branch::Direct { label }) => {
                        self.used_blocks.insert(label.clone(), true);
                    },
                    Branch(LLVM::Branch::Conditional { ty: _, val: _, true_label, false_label }) => {
                        self.used_blocks.insert(true_label.clone(), true);
                        self.used_blocks.insert(false_label.clone(), true);
                    },
                    _ => {},
                }
            }
        }
    }

    fn fix_phis(&mut self, instrs: &Vec<LLVM::Instr>) -> Vec<LLVM::Instr> {
        use LLVM::Instr::*;

        let mut new_instrs = vec![];
        for i in instrs {
            let new_instr: LLVM::Instr;
            match i.clone() {
                Phi { dest, preds } => {
                    let new_preds = preds.iter().cloned().filter(|(_, lab)| {
                        if let Some(_) = self.used_blocks.get(&lab) {
                            true
                        } else {
                            false
                        }
                    }).collect();

                    new_instr = Phi {
                        dest,
                        preds: new_preds,
                    };
                },

                _ => {
                    new_instr = i.clone();
                },
            }
            new_instrs.push(new_instr);
        }

        new_instrs
    }
}

impl base::Optimizer for Optimizer {
    fn run(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        self.optimize_program(prog)
    }
}
