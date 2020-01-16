use std::collections::{HashMap};

use crate::instructions as LLVM;
use crate::optimizations::base;
use crate::utils::{blocks_to_instructions, instructions_to_blocks};

pub struct Optimizer {
    values: HashMap<String, LLVM::Const>
}

impl Optimizer {
    pub fn new() -> Self {
        Optimizer {
            values: HashMap::new(),
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
        self.values.clear();
        let instrs = blocks_to_instructions(&fun.body);
        self.find_constant_variables(&instrs);
        let new_instrs = self.optimize_instructions(&instrs);
        let new_body = instructions_to_blocks(&new_instrs);

        LLVM::Function {
            ret_ty: fun.ret_ty.clone(),
            name: fun.name.clone(),
            args: fun.args.iter().cloned().collect(),
            body: new_body,
        }
    }

    fn optimize_instructions(&mut self, instrs: &Vec<LLVM::Instr>) -> Vec<LLVM::Instr> {
        use LLVM::Instr::*;
        let mut new_instrs = vec![];
        for i in instrs {
            let new_instr: LLVM::Instr;
            match i.clone() {
                Alloc { .. } |
                ReturnVoid |
                Unreachable |
                Load { .. } |
                Branch(LLVM::Branch::Direct { .. }) |
                Label { .. } |
                Sext { .. } |
                Bitcast { .. } => {
                    new_instr = i.clone();
                },

                Store { src, dest } => {
                    let new_val = self.optimize_value(&src.1);
                    new_instr = Store {
                        src: (src.0, new_val),
                        dest,
                    };
                },

                Compare { dest_reg, op, ty, val_lhs, val_rhs } => {
                    let new_val_lhs = self.optimize_value(&val_lhs);
                    let new_val_rhs = self.optimize_value(&val_rhs);
                    new_instr = Compare {
                        dest_reg,
                        op,
                        ty,
                        val_lhs: new_val_lhs.clone(),
                        val_rhs: new_val_rhs.clone(),
                    };
                },

                Call { dest_reg, ret_ty, name, args } => {
                    let new_args = args.iter().cloned().map(|(ty, val)| {
                        let new_val = self.optimize_value(&val);
                        (ty, new_val)
                    }).collect();
                    new_instr = Call {
                        dest_reg,
                        ret_ty,
                        name,
                        args: new_args,
                    };
                },

                Branch(LLVM::Branch::Conditional { ty, val, true_label, false_label }) => {
                    let new_val = self.optimize_value(&val);
                    new_instr = Branch(LLVM::Branch::Conditional {
                        ty,
                        val: new_val,
                        true_label,
                        false_label,
                    });
                },

                Arithm { dest, op, val_lhs, val_rhs } => {
                    let new_val_lhs = self.optimize_value(&val_lhs);
                    let new_val_rhs = self.optimize_value(&val_rhs);

                    new_instr = Arithm {
                        dest,
                        op,
                        val_lhs: new_val_lhs.clone(),
                        val_rhs: new_val_rhs.clone(),
                    };
                },

                Phi { dest, preds } => {
                    let new_preds = preds.iter().cloned().map(|(val, lab)| {
                        let new_val = self.optimize_value(&val);
                        (new_val, lab)
                    }).collect();
                    new_instr = Phi {
                        dest,
                        preds: new_preds,
                    };
                },

                GetElementPtr { dest, src, args } => {
                    new_instr = GetElementPtr {
                        dest,
                        src,
                        args: args.iter()
                            .map(|idx| (idx.0.clone(), self.optimize_value(&idx.1)))
                            .collect(),
                    };
                },

                Return { ty, val } => {
                    let new_val = self.optimize_value(&val);
                    new_instr = Return {
                        ty,
                        val: new_val,
                    };
                },
            }
            new_instrs.push(new_instr);
        }

        new_instrs
    }

    fn optimize_value(&mut self, value: &LLVM::Value) -> LLVM::Value {
        if let LLVM::Value::Register(LLVM::Register { name, .. }) = value.clone() {
            if let Some(new_val) = self.values.get(&name) {
                return new_val.clone().into();
            }
        }
        value.clone()
    }

    fn find_constant_variables(&mut self, instrs: &Vec<LLVM::Instr>) {
        use LLVM::Instr::*;
        // assume all variables are constants so,
        // add a value from each `store` instruction
        for i in instrs {
            match i {
                Store { src, dest } => {
                    if let LLVM::Value::Const(c) = src.1.clone() {
                        self.values.insert(dest.1.name.clone(), c);
                    }
                },
                _ => {},
            }
        }

        // if a `store` value is not a constant, variable is not const
        // also if it is different than a value saved, it means distinct
        // values are assigned to the variable during exeuction of a program
        for i in instrs {
            match i {
                Store { src, dest } => {
                    if let Some(val) = self.values.get(&dest.1.name) {
                        if let LLVM::Value::Const(c) = src.1.clone() {
                            if *val != c {
                                self.values.remove(&dest.1.name);
                            }
                        } else {
                            self.values.remove(&dest.1.name);
                        }
                    }
                },
                _ => {},
            }
        }

        // process load operations and save their registers as constant
        // if they are loading constant variable
        for i in instrs {
            match i {
                Load { src, dest } => {
                    if let Some(val) = self.values.get(&src.1.name) {
                        self.values.insert(dest.1.name.clone(), val.clone());
                    }
                },
                _ => {},
            }
        }
    }
}

impl base::Optimizer for Optimizer {
    fn run(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        self.optimize_program(prog)
    }
}
