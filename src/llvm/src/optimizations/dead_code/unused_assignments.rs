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
            options: prog.options.clone(),
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
                Load { dest, .. } |
                GetElementPtr { dest, .. } |
                Phi { dest, .. } |
                Arithm { dest, .. } |
                Sext { dest, .. } |
                Bitcast { dest, .. } => {
                    if let Some(_) = self.used_registers.get(&dest.1) {
                    } else {
                        // ignore this instruction
                        continue;
                    }
                },

                Compare { dest_reg, .. } => {
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
                Label { .. } |
                Store { .. } |
                Return { .. } => {
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
                    Load { src, .. } => {
                        self.used_registers.insert(src.1.clone(), true);
                    },

                    Sext { src, .. } |
                    Bitcast { src, .. } => {
                        if let LLVM::Value::Register(r) = src.1.clone() {
                            self.used_registers.insert(r.clone(), true);
                        }
                    },

                    Store { src, dest }  => {
                        if let LLVM::Value::Register(r) = src.1.clone() {
                            self.used_registers.insert(r.clone(), true);
                        }
                        self.used_registers.insert(dest.1.clone(), true);
                    },

                    Arithm { val_lhs, val_rhs, .. } |
                    Compare { val_lhs, val_rhs, .. } => {
                        if let LLVM::Value::Register(r) = val_lhs {
                            self.used_registers.insert(r.clone(), true);
                        }
                        if let LLVM::Value::Register(r) = val_rhs {
                            self.used_registers.insert(r.clone(), true);
                        }
                    },

                    Phi { preds, .. } => {
                        for p in preds {
                            if let LLVM::Value::Register(r) = p.0.clone() {
                                self.used_registers.insert(r.clone(), true);
                            }
                        }
                    }

                    GetElementPtr { src, args, .. } => {
                        if let LLVM::Value::Register(r) = src.1.clone() {
                            self.used_registers.insert(r.clone(), true);
                        }
                        for idx in args {
                            if let LLVM::Value::Register(r) = idx.1.clone() {
                                self.used_registers.insert(r.clone(), true);
                            }
                        }
                    },

                    Call { args, .. } => {
                        for a in args {
                            if let LLVM::Value::Register(r) = a.1.clone() {
                                self.used_registers.insert(r.clone(), true);
                            }
                        }
                    },

                    Branch(LLVM::Branch::Conditional { val, .. }) |
                    Return { val, .. } => {
                        if let LLVM::Value::Register(r) = val {
                            self.used_registers.insert(r.clone(), true);
                        }
                    },

                    Branch(LLVM::Branch::Direct { .. }) |
                    ReturnVoid |
                    Unreachable |
                    Label { .. } |
                    Alloc { .. } => {
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
