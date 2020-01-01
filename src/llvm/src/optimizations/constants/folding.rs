use std::collections::{HashMap};

use crate::instructions as LLVM;
use crate::optimizations::base;
use crate::utils::{blocks_to_instructions, instructions_to_blocks};
use crate::operators::{Operator, ArithmOp, RelOp};

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
            declares: prog.declares.clone(),
            statics: prog.statics.clone(),
            functions: new_functions,
        }
    }

    pub fn optimize_function(&mut self, fun: &LLVM::Function) -> LLVM::Function {
        self.values.clear();
        let instrs = blocks_to_instructions(&fun.body);
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
                Alloc { dest: _ } |
                ReturnVoid |
                Unreachable |
                Load { src: _, dest: _ } |
                Branch(LLVM::Branch::Direct { label: _ }) |
                Label { val: _, preds: _ } => {
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
                        self.values.insert(dest_reg.name.clone(), new_val.into());
                    }

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
                    let new_val = self.optimize_value(&val.clone());
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
                        self.values.insert(dest.1.name.clone(), new_val.into());
                    }

                    new_instr = Arithm {
                        dest,
                        op,
                        val_lhs: new_val_lhs.clone(),
                        val_rhs: new_val_rhs.clone(),
                    };
                },

                Phi { dest, preds } => {
                    let new_preds: Vec<(LLVM::Value, LLVM::Label)> = preds.iter().cloned().map(|(val, lab)| {
                        let new_val = self.optimize_value(&val);
                        (new_val, lab)
                    }).collect();
                    if new_preds.len() == 1 {
                        if let LLVM::Value::Const(c) = new_preds[0].0.clone() {
                            self.values.insert(dest.1.name.clone(), c);
                        }
                    }

                    new_instr = Phi {
                        dest,
                        preds: new_preds,
                    };
                },

                GetElementPtr { dest, src, idx1, idx2 } => {
                    let new_idx11 = self.optimize_value(&idx1.1);
                    let new_idx21 = self.optimize_value(&idx2.1);
                    new_instr = GetElementPtr {
                        dest,
                        src,
                        idx1: (idx1.0, new_idx11),
                        idx2: (idx2.0, new_idx21),
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
        if let LLVM::Value::Register(LLVM::Register { name }) = value.clone() {
            if let Some(new_val) = self.values.get(&name) {
                return new_val.clone().into();
            }
        }
        value.clone()
    }
}

impl base::Optimizer for Optimizer {
    fn run(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        self.optimize_program(prog)
    }
}
