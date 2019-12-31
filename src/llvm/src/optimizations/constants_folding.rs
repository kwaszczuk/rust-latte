use std::collections::{HashMap};

use crate::instructions as LLVM;
use crate::optimizations::base::{Optimizer};
use crate::utils::{blocks_to_instructions, instructions_to_blocks};
use crate::operators::{Operator, ArithmOp, RelOp};

pub struct ConstantsFolding {
    values: HashMap<String, LLVM::Const>
}

impl ConstantsFolding {
    pub fn new() -> Self {
        ConstantsFolding {
            values: HashMap::new(),
        }
    }

    pub fn optimize_program(&mut self, prog: LLVM::Program) -> LLVM::Program {
        let new_functions = prog.functions.iter().cloned().map(|f| {
            self.values.clear();
            let new_f = self.optimize_function(f);
            new_f
        }).collect();

        LLVM::Program {
            declares: prog.declares,
            statics: prog.statics,
            functions: new_functions,
        }
    }

    pub fn optimize_function(&mut self, fun: LLVM::Function) -> LLVM::Function {
        let instrs = blocks_to_instructions(fun.body);
        let new_instrs = self.optimize_instructions(instrs);
        let new_body = instructions_to_blocks(new_instrs);

        LLVM::Function {
            ret_ty: fun.ret_ty.clone(),
            name: fun.name.clone(),
            args: fun.args.iter().cloned().collect(),
            body: new_body,
        }
    }

    fn optimize_instructions(&mut self, instrs: Vec<LLVM::Instr>) -> Vec<LLVM::Instr> {
        use LLVM::Instr::*;
        let mut new_instrs = vec![];
        for i in instrs {
            match i.clone() {
                Alloc { dest: _ } |
                ReturnVoid |
                Unreachable |
                Load { src: _, dest: _ } |
                Label { val: _, preds: _ } => {
                    new_instrs.push(i);
                },

                Store { src, dest } => {
                    let new_val = self.optimize_value(src.1);
                    new_instrs.push(
                        Store {
                            src: (src.0, new_val),
                            dest,
                        }
                    );
                },

                Compare { dest_reg, op, ty, val_lhs, val_rhs } => {
                    let new_val_lhs = self.optimize_value(val_lhs);
                    let new_val_rhs = self.optimize_value(val_rhs);

                    if let (LLVM::Value::Const(c1), LLVM::Value::Const(c2)) = (new_val_lhs.clone(), new_val_rhs.clone()) {
                        use Operator::*;
                        use RelOp::*;
                        use LLVM::Const::*;

                        let new_val = match op {
                            Rel(EQ) => match (c1, c2) {
                                (Int(i1), Int(i2)) => i1 == i2,
                                (False, False) => true,
                                (True, True) => true,
                                (False, True) => false,
                                (True, False) => false,
                                _ => { panic!("should not happen"); }
                            },

                            Rel(NE) => match (c1, c2) {
                                (Int(i1), Int(i2)) => i1 != i2,
                                (False, False) => false,
                                (True, True) => false,
                                (False, True) => true,
                                (True, False) => true,
                                _ => { panic!("should not happen"); }
                            },

                            Rel(GT) => match (c1, c2) {
                                (Int(i1), Int(i2)) => i1 > i2,
                                _ => { panic!("should not happen"); }
                            },

                            Rel(GE) => match (c1, c2) {
                                (Int(i1), Int(i2)) => i1 >= i2,
                                _ => { panic!("should not happen"); }
                            },

                            Rel(LT) => match (c1, c2) {
                                (Int(i1), Int(i2)) => i1 < i2,
                                _ => { panic!("should not happen"); }
                            },

                            Rel(LE) => match (c1, c2) {
                                (Int(i1), Int(i2)) => i1 <= i2,
                                _ => { panic!("should not happen"); }
                            },
                            _ => { panic!("should not happen"); }
                        };
                        self.values.insert(dest_reg.name, new_val.into());
                        // ignore the instruction in the result
                    } else {
                        new_instrs.push(
                            Compare {
                                dest_reg,
                                op,
                                ty,
                                val_lhs: new_val_lhs.clone(),
                                val_rhs: new_val_rhs.clone(),
                            }
                        );
                    }
                },

                Call { reg_dest, ret_ty, name, args } => {
                    let new_args = args.iter().cloned().map(|(ty, val)| {
                        let new_val = self.optimize_value(val);
                        (ty, new_val)
                    }).collect();
                    new_instrs.push(
                        Call {
                            reg_dest,
                            ret_ty,
                            name,
                            args: new_args,
                        }
                    );
                },

                Branch(b) => {
                    match b {
                        LLVM::Branch::Direct { label: _ } => {
                            new_instrs.push(i.clone());
                        },
                        LLVM::Branch::Conditional { ty, val, true_label, false_label } => {
                            let new_val = self.optimize_value(val.clone());
                            new_instrs.push(
                                Branch(LLVM::Branch::Conditional {
                                    ty,
                                    val: new_val,
                                    true_label,
                                    false_label,
                                })
                            );
                        },
                    }
                },

                Arithm { dest, op, val_lhs, val_rhs } => {
                    let new_val_lhs = self.optimize_value(val_lhs);
                    let new_val_rhs = self.optimize_value(val_rhs);
                    use LLVM::Const::*;

                    if let (LLVM::Value::Const(Int(i1)), LLVM::Value::Const(Int(i2))) = (new_val_lhs.clone(), new_val_rhs.clone()) {
                        use Operator::*;
                        use ArithmOp::*;
                        let new_val = match op {
                            Arithm(Add) => i1 + i2,
                            Arithm(Sub) => i1 - i2,
                            Arithm(Mul) => i1 * i2,
                            Arithm(Div) => i1 / i2,
                            Arithm(Mod) => i1 % i2,
                            _ => { panic!("should not happen"); }
                        };
                        self.values.insert(dest.1.name, new_val.into());
                        // ignore the instruction in the result
                    } else {
                        new_instrs.push(
                            Arithm {
                                dest,
                                op,
                                val_lhs: new_val_lhs.clone(),
                                val_rhs: new_val_rhs.clone(),
                            }
                        );
                    }
                },

                Phi { dest, preds } => {
                    let new_preds = preds.iter().cloned().map(|(val, lab)| {
                        let new_val = self.optimize_value(val);
                        (new_val, lab)
                    }).collect();
                    new_instrs.push(
                        Phi {
                            dest,
                            preds: new_preds,
                        }
                    );
                },

                GetElementPtr { dest, src, idx1, idx2 } => {
                    let new_idx11 = self.optimize_value(idx1.1);
                    let new_idx21 = self.optimize_value(idx2.1);
                    new_instrs.push(
                        GetElementPtr {
                            dest,
                            src,
                            idx1: (idx1.0, new_idx11),
                            idx2: (idx2.0, new_idx21),
                        }
                    );
                },

                Return { ty, val } => {
                    let new_val = self.optimize_value(val);
                    new_instrs.push(
                        Return {
                            ty,
                            val: new_val,
                        }
                    );
                },
            }
        }

        new_instrs
    }

    fn optimize_value(&mut self, value: LLVM::Value) -> LLVM::Value {
        if let LLVM::Value::Register(LLVM::Register { name }) = value.clone() {
            if let Some(new_val) = self.values.get(&name) {
                return new_val.clone().into();
            }
        }
        value.clone()
    }
}

impl Optimizer for ConstantsFolding {
    fn run(&mut self, prog: LLVM::Program) -> LLVM::Program {
        self.optimize_program(prog)
    }
}
