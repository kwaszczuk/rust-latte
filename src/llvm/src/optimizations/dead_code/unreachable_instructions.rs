use std::collections::{HashMap};

use crate::instructions as LLVM;
use crate::optimizations::base;

pub struct Optimizer {
}

impl Optimizer {
    pub fn new() -> Self {
        Optimizer {
        }
    }

    pub fn optimize_program(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        let new_functions = prog.functions.iter().map(|f| {
            let new_f = self.optimize_function(f);
            new_f
        }).collect();

        LLVM::Program {
            options: prog.options.clone(),
            declares: prog.declares.clone(),
            statics: prog.statics.clone(),
            functions: new_functions,
        }
    }

    pub fn optimize_function(&mut self, fun: &LLVM::Function) -> LLVM::Function {
        let mut new_body = fun.body.iter().map(|b| {
            self.remove_unused_instructions(b)
        }).collect();
        new_body = self.fix_phis(&new_body);

        LLVM::Function {
            ret_ty: fun.ret_ty.clone(),
            name: fun.name.clone(),
            args: fun.args.iter().cloned().collect(),
            body: new_body,
        }
    }

    fn remove_unused_instructions(&mut self, b: &LLVM::Block) -> LLVM::Block {
        use LLVM::Instr::*;

        let mut new_instrs = vec![];
        for i in &b.instrs {
            match i {
                Alloc { dest: _ } |
                Load { src: _, dest: _ } |
                Store { src: _, dest: _ } |
                Phi { dest: _, preds: _ } |
                Arithm { dest: _, op: _, val_lhs: _, val_rhs: _ } |
                GetElementPtr { dest: _, src: _, idx1: _, idx2: _ } |
                Compare { dest_reg: _, op: _, ty: _, val_lhs: _, val_rhs: _ } |
                Call { dest_reg: _, ret_ty: _, name: _, args: _ } |
                Unreachable |
                Branch(LLVM::Branch::Direct { label: _ }) |
                Label { val: _, preds: _ } => {
                    new_instrs.push(i.clone());
                },

                Branch(LLVM::Branch::Conditional { ty: _, val, true_label, false_label }) => {
                    match val {
                        LLVM::Value::Const(LLVM::Const::True) => {
                            new_instrs.push(Branch(LLVM::Branch::Direct {
                                label: true_label.clone(),
                            }));
                        },

                        LLVM::Value::Const(LLVM::Const::False) => {
                            new_instrs.push(Branch(LLVM::Branch::Direct {
                                label: false_label.clone(),
                            }));
                        },

                        _ => {
                            new_instrs.push(i.clone());
                        },
                    };
                    break;
                },

                Return { ty: _, val: _ } |
                ReturnVoid => {
                    new_instrs.push(i.clone());
                    break;
                },
            }
        }

        LLVM::Block {
            label: b.label.clone(),
            instrs: new_instrs,
        }
    }

    fn fix_phis(&mut self, blocks: &Vec<LLVM::Block>) -> Vec<LLVM::Block> {
        use LLVM::Instr::*;

        let mut new_blocks = vec![];
        let mut jumps: HashMap<(LLVM::Label, LLVM::Label), bool> = HashMap::new();
        for b in blocks {
            if !b.label.is_entry() {
                for i in &b.instrs {
                    match i {
                        Branch(LLVM::Branch::Direct { label }) => {
                            jumps.insert((b.label.clone(), label.clone()), true);
                        },
                        Branch(LLVM::Branch::Conditional { ty: _, val: _, true_label, false_label }) => {
                            jumps.insert((b.label.clone(), true_label.clone()), true);
                            jumps.insert((b.label.clone(), false_label.clone()), true);
                        },
                        _ => {
                        }
                    }
                }
            }
        }

        for b in blocks {
            let mut new_instrs = vec![];
            if !b.label.is_entry() {
                for i in &b.instrs {
                    match i {
                        Phi { dest, preds } => {
                            let new_preds = preds.iter().cloned().filter(|p| {
                                let k = (p.1.clone(), b.label.clone());
                                if let Some(_) = jumps.get(&k) {
                                    true
                                } else {
                                    false
                                }
                            }).collect();

                            new_instrs.push(Phi {
                                dest: dest.clone(),
                                preds: new_preds,
                            });
                        },
                        _ => {
                            new_instrs.push(i.clone());
                        }
                    }
                }
            } else {
                new_instrs = b.instrs.clone();
            }

            new_blocks.push(LLVM::Block {
                label: b.label.clone(),
                instrs: new_instrs,
            });
        }

        new_blocks
    }
}

impl base::Optimizer for Optimizer {
    fn run(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        self.optimize_program(prog)
    }
}
