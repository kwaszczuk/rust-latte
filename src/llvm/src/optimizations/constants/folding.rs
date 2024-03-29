use std::collections::{HashMap};

use crate::instructions as LLVM;
use crate::optimizations::base;
use crate::utils::{blocks_to_instructions, instructions_to_blocks};
use crate::operators::{Operator, ArithmOp, RelOp};

pub struct Optimizer {
    values: HashMap<String, LLVM::Value>
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

                Sext { src, dest } => {
                    let new_val = self.optimize_value(&src.1);
                    if let LLVM::Value::Const(_) = new_val {
                        self.values.insert(dest.1.name.clone(), new_val.clone());
                    }
                    new_instr = Sext {
                        src: (src.0, new_val.clone()),
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
                        self.values.insert(dest_reg.name.clone(), LLVM::Value::Const(new_val.into()));
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
                    {
                        use Operator::*;
                        use ArithmOp::*;

                        match (op.clone(), new_val_lhs.clone(), new_val_rhs.clone()) {
                            (Arithm(Mul), LLVM::Value::Const(Int(0)), _) |
                            (Arithm(Mul), _, LLVM::Value::Const(Int(0))) |
                            (Arithm(Div), LLVM::Value::Const(Int(0)), _) |
                            (Arithm(Mod), LLVM::Value::Const(Int(0)), _) |
                            (Arithm(Mod), _, LLVM::Value::Const(Int(1))) => {
                                self.values.insert(dest.1.name.clone(), LLVM::Value::Const(0.into()));
                            },

                            (Arithm(Sub), LLVM::Value::Const(Int(0)), LLVM::Value::Const(Int(x))) => {
                                self.values.insert(dest.1.name.clone(), LLVM::Value::Const((-x).into()));
                            },

                            (Arithm(Add), LLVM::Value::Const(Int(0)), x) |
                            (Arithm(Add), x, LLVM::Value::Const(Int(0))) |
                            (Arithm(Sub), x, LLVM::Value::Const(Int(0))) |
                            (Arithm(Mul), LLVM::Value::Const(Int(1)), x) |
                            (Arithm(Mul), x, LLVM::Value::Const(Int(1))) |
                            (Arithm(Div), x, LLVM::Value::Const(Int(1))) => {
                                self.values.insert(dest.1.name.clone(), x);
                            },

                            (Arithm(Add), LLVM::Value::Const(Int(x)), LLVM::Value::Const(Int(y))) => {
                                self.values.insert(dest.1.name.clone(), LLVM::Value::Const((x + y).into()));
                            },

                            (Arithm(Sub), LLVM::Value::Const(Int(x)), LLVM::Value::Const(Int(y))) => {
                                self.values.insert(dest.1.name.clone(), LLVM::Value::Const((x - y).into()));
                            },

                            (Arithm(Mul), LLVM::Value::Const(Int(x)), LLVM::Value::Const(Int(y))) => {
                                self.values.insert(dest.1.name.clone(), LLVM::Value::Const((x * y).into()));
                            },

                            (Arithm(Div), LLVM::Value::Const(Int(x)), LLVM::Value::Const(Int(y))) => {
                                self.values.insert(dest.1.name.clone(), LLVM::Value::Const((x / y).into()));
                            },

                            (Arithm(Mod), LLVM::Value::Const(Int(x)), LLVM::Value::Const(Int(y))) => {
                                self.values.insert(dest.1.name.clone(), LLVM::Value::Const((x % y).into()));
                            },

                            _ => {},
                        }
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
}

impl base::Optimizer for Optimizer {
    fn run(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        self.optimize_program(prog)
    }
}
