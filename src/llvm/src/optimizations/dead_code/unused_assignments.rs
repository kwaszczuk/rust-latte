use std::collections::{HashMap};

use crate::instructions as LLVM;
use crate::optimizations::base;
use crate::utils::{blocks_to_instructions, instructions_to_blocks};

pub struct Optimizer {
    used_registers: HashMap<LLVM::Register, bool>
}

impl Optimizer {
    pub fn new() -> Self {
        Optimizer {
            used_registers: HashMap::new(),
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
        self.used_registers.clear();
        self.find_used_registers(&fun.body);

        let instrs = blocks_to_instructions(&fun.body);
        let new_instrs = self.remove_unused_registers(&instrs);
        let new_body = instructions_to_blocks(&new_instrs);

        LLVM::Function {
            ret_ty: fun.ret_ty.clone(),
            name: fun.name.clone(),
            args: fun.args.iter().cloned().collect(),
            body: new_body,
        }
    }

    fn remove_unused_registers(&mut self, instrs: &Vec<LLVM::Instr>) -> Vec<LLVM::Instr> {
        use LLVM::Instr::*;

        let mut new_instrs = vec![];
        for i in instrs {
            match i {
                Alloc { dest } |
                Load { src: _, dest } |
                Store { src: _, dest } |
                Phi { dest, preds: _ } |
                Arithm { dest, op: _, val_lhs: _, val_rhs: _ } |
                GetElementPtr { dest, src: _, idx1: _, idx2: _ } => {
                    if let Some(_) = self.used_registers.get(&dest.1) {
                    } else {
                        // ignore this instruction
                        continue;
                    }
                },

                Compare { dest_reg, op: _, ty: _, val_lhs: _, val_rhs: _ } => {
                    if let Some(_) = self.used_registers.get(&dest_reg) {
                    } else {
                        // ignore this instruction
                        continue;
                    }
                },

                Call { dest_reg, ret_ty, name, args } => {
                    if let Some(reg) = dest_reg {
                        if let Some(_) = self.used_registers.get(&reg) {
                        } else {
                            // we cannot ignore the call instruction completly,
                            // instead we do not save it's result
                            new_instrs.push(Call {
                                dest_reg: None,
                                ret_ty: ret_ty.clone(),
                                name: name.clone(),
                                args: args.clone(),
                            });
                            continue;
                        }
                    }
                },

                ReturnVoid |
                Unreachable |
                Branch(_) |
                Label { val: _, preds: _ } |
                Return { ty: _, val: _ } => {
                },
            }

            new_instrs.push(i.clone());
        }

        new_instrs
    }

    fn find_used_registers(&mut self, blocks: &Vec<LLVM::Block>) {
        use LLVM::Instr::*;

        for b in blocks {
            for i in &b.instrs {
                match i {
                    Load { src, dest: _ } => {
                        self.used_registers.insert(src.1.clone(), true);
                    },

                    Store { src, dest: _ }  => {
                        if let LLVM::Value::Register(r) = src.1.clone() {
                            self.used_registers.insert(r.clone(), true);
                        }
                    },

                    Arithm { dest: _, op: _, val_lhs, val_rhs } |
                    Compare { dest_reg: _, op: _, ty: _, val_lhs, val_rhs } => {
                        if let LLVM::Value::Register(r) = val_lhs {
                            self.used_registers.insert(r.clone(), true);
                        }
                        if let LLVM::Value::Register(r) = val_rhs {
                            self.used_registers.insert(r.clone(), true);
                        }
                    },

                    Phi { dest: _, preds } => {
                        for p in preds {
                            if let LLVM::Value::Register(r) = p.0.clone() {
                                self.used_registers.insert(r.clone(), true);
                            }
                        }
                    }

                    GetElementPtr { dest: _, src, idx1, idx2 } => {
                        if let LLVM::Value::Register(r) = src.1.clone() {
                            self.used_registers.insert(r.clone(), true);
                        }
                        if let LLVM::Value::Register(r) = idx1.1.clone() {
                            self.used_registers.insert(r.clone(), true);
                        }
                        if let LLVM::Value::Register(r) = idx2.1.clone() {
                            self.used_registers.insert(r.clone(), true);
                        }
                    },

                    Call { dest_reg: _, ret_ty: _, name: _, args } => {
                        for a in args {
                            if let LLVM::Value::Register(r) = a.1.clone() {
                                self.used_registers.insert(r.clone(), true);
                            }
                        }
                    },

                    Branch(LLVM::Branch::Conditional { ty: _, val, true_label: _, false_label: _ }) |
                    Return { ty: _, val } => {
                        if let LLVM::Value::Register(r) = val {
                            self.used_registers.insert(r.clone(), true);
                        }
                    },

                    Branch(LLVM::Branch::Direct { label: _ }) |
                    ReturnVoid |
                    Unreachable |
                    Label { val: _, preds: _ } |
                    Alloc { dest: _ } => {
                    },
                }
            }
        }
    }
}

impl base::Optimizer for Optimizer {
    fn run(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        self.optimize_program(prog)
    }
}
