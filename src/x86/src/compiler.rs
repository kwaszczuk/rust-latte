use std::collections::{HashMap};
use std::collections::hash_map::Entry;
use crate::instructions as X86;
use llvm::instructions as LLVM;
use crate::operators::{ArithmOp, RelOp};
use base::types::{Labeler};

use std::panic;

static DEFAULT_WORD_SIZE: i32 = 8;
static DEFAULT_TYPE: X86::Type = X86::Type::Quad;

static DEFAULT_ARGS_OFFSET: i32 = 16;

pub struct X86Compiler {
    label_generator: Labeler<X86::Label>,
    statics_map: HashMap<LLVM::Static, X86::Static>,
    registers_map: HashMap<LLVM::Register, X86::Storage>,
    labels_map: HashMap<LLVM::Label, X86::Label>,
    function_counter: usize,
    fin_label: X86::Label,
    phis_info: HashMap<LLVM::Label, HashMap<LLVM::Label, Vec<(LLVM::Register, LLVM::Value)>>>,
}

impl X86Compiler {
    fn new() -> Self {
        X86Compiler {
            label_generator: Labeler::new("".to_string()),
            statics_map: HashMap::new(),
            labels_map: HashMap::new(),
            registers_map: HashMap::new(),
            function_counter: 0,
            fin_label: X86::Label::new("".to_string(), 0), // dummy
            phis_info: HashMap::new(),
        }
    }

    pub fn run(prog: &LLVM::Program) -> X86::Program {
        let mut compiler = X86Compiler::new();
        compiler.compile(prog)
    }

    fn compile(&mut self, prog: &LLVM::Program) -> X86::Program {
        self.compile_program(prog)
    }

    fn compile_program(&mut self, program: &LLVM::Program) -> X86::Program {
        let new_statics = program.statics.iter()
            .map(|(static_, value)| {
                self.statics_map.insert(static_.clone(), static_.clone().into());
                (static_.clone().into(), value.clone())
            })
            .collect();
        let new_functions = program.functions.iter().map(|f| self.compile_function(f)).collect();

        X86::Program {
            options: program.options.clone(),
            statics: new_statics,
            functions: new_functions,
        }
    }

    fn compile_function(&mut self, fun: &LLVM::Function) -> X86::Function {
        use X86::*;
        use X86::Instr::*;
        use X86::Register::*;

        self.function_counter += 1;
        self.label_generator = Labeler::new(format!("F{}.", self.function_counter));

        let var_cnt = self.prepare_variables(&fun);
        self.prepare_phis(&fun.body);
        self.prepare_labels(&fun.body);

        let init_block = X86::Block {
            label: X86::Label::new("".to_string(), 0),
            instrs: {
                let mut ret = vec![
                    Push { ty: DEFAULT_TYPE.clone(), src: RBP.into() },
                    Move { ty: DEFAULT_TYPE.clone(), src: RSP.into(), dest: RBP.into() },
                    Arithm {
                        ty: DEFAULT_TYPE.clone(),
                        op: ArithmOp::Sub.into(),
                        src: Value::Const(align(var_cnt * DEFAULT_WORD_SIZE, 16)),
                        dest: RSP.into()
                    },
                ];
                for i in 0..fun.args.len() {
                    if i < 6 {
                        let src = match i {
                            0 => Register::RDI.into(),
                            1 => Register::RSI.into(),
                            2 => Register::RDX.into(),
                            3 => Register::RCX.into(),
                            4 => Register::R8.into(),
                            5 => Register::R9.into(),
                            _ => { panic!("should not happen") },
                        };
                        ret.push(Move {
                            ty: DEFAULT_TYPE.clone(),
                            src,
                            dest: X86::Memory {
                                offset: -(i as i32 + 1) * DEFAULT_WORD_SIZE,
                            }.into(),
                        });
                    }
                }
                ret
            },
        };

        self.fin_label = self.label_generator.next();
        let fin_block = X86::Block {
            label: self.fin_label.clone(),
            instrs: vec![
                Arithm {
                    ty: DEFAULT_TYPE.clone(),
                    op: ArithmOp::Add.into(),
                    src: Value::Const(align(var_cnt * DEFAULT_WORD_SIZE, 16)),
                    dest: RSP.into()
                },
                Pop { ty: DEFAULT_TYPE.clone(), dest: RBP.into() },
                Return,
            ],
        };


        let mut new_body: Vec<X86::Block> = fun.body.iter()
            .filter(|b| !(b.is_entry() && b.instrs.len() == 1))
            .map(|b| self.compile_block(b))
            .collect();
        new_body.insert(0, init_block);
        new_body.push(fin_block);

        X86::Function {
            name: fun.name.clone(),
            body: new_body,
        }
    }

    fn prepare_variables(&mut self, fun: &LLVM::Function) -> i32 {
        use LLVM::Instr::*;
        self.registers_map = HashMap::new();

        let mut cnt = 0;
        for i in 0..fun.args.len() {
            let store: X86::Storage = match i {
                0..=5 => {
                    cnt += 1;
                    X86::Memory {
                        offset: -cnt * DEFAULT_WORD_SIZE,
                    }.into()
                },
                _ => X86::Memory {
                    offset: DEFAULT_ARGS_OFFSET + (i as i32 - 6) * DEFAULT_WORD_SIZE,
                }.into(),
            };
            self.registers_map.insert(
                LLVM::Register::new("".to_string(), i),
                store.into(),
            );
        }

        for b in &fun.body {
            for i in &b.instrs {
                match i {
                    Alloc { .. } |
                    ReturnVoid |
                    Unreachable |
                    Load { .. } |
                    Branch(_) |
                    Store { .. } |
                    Return { .. } |
                    Label { .. } => {},

                    Compare { dest_reg, .. } => {
                        cnt += 1;
                        self.registers_map.insert(dest_reg.clone(), X86::Memory {
                            offset: -cnt * DEFAULT_WORD_SIZE,
                        }.into());
                    },

                    Call { dest_reg, .. } => {
                        if let Some(reg) = dest_reg {
                            cnt += 1;
                            self.registers_map.insert(reg.clone(), X86::Memory {
                                offset: -cnt * DEFAULT_WORD_SIZE,
                            }.into());
                        }
                    },

                    Phi { dest, .. } |
                    GetElementPtr { dest, .. } |
                    Arithm { dest, .. } => {
                        cnt += 1;
                        self.registers_map.insert(dest.1.clone(), X86::Memory {
                            offset: -cnt * DEFAULT_WORD_SIZE,
                        }.into());
                    },
                }
            }
        }
        cnt
    }

    fn prepare_phis(&mut self, blocks: &Vec<LLVM::Block>) {
        use LLVM::Instr::*;
        self.phis_info = HashMap::new();
        for b in blocks {
            for i in &b.instrs {
                match i {
                    Phi { dest, preds } => {
                        if let None = self.phis_info.get(&b.label) {
                            let preds_map = preds.into_iter().cloned().map(|p| {
                                (p.1.clone(), vec![])
                            }).collect();
                            self.phis_info.insert(b.label.clone(), preds_map);
                        }

                        for p in preds {
                            match self.phis_info.entry(b.label.clone()) {
                                Entry::Occupied(mut v1) => {
                                    match v1.get_mut().entry(p.1.clone()) {
                                        Entry::Occupied(mut v2) => {
                                            v2.get_mut().push((dest.1.clone(), p.0.clone()));
                                        },
                                        _ => { panic!("should not happend") },
                                    }
                                },
                                _ => { panic!("should not happend") },
                            }
                        }
                    },
                    _ => {},
                }
            }
        }
    }

    fn prepare_labels(&mut self, blocks: &Vec<LLVM::Block>) {
        self.labels_map = HashMap::new();
        for b in blocks {
            if b.is_entry() && b.instrs.len() == 1 {
                continue;
            }
            self.labels_map.insert(b.label.clone(), self.label_generator.next());
        }
    }

    fn compile_block(&mut self, block: &LLVM::Block) -> X86::Block {
        use X86::*;
        use X86::Instr::*;

        let mut new_instrs = vec![];

        let replace_register = |r| self.registers_map.get(&r).unwrap_or_else(|| { println!("{:?}", r); panic!("replace") }).clone();
        let cast_value_to_value = |val: &LLVM::Value| -> X86::Value {
            match val {
                LLVM::Value::Const(c) => c.clone().into(),
                LLVM::Value::Register(r) => replace_register(r.clone()).into(),
                LLVM::Value::Static(_) => {
                    panic!("should not happen")
                },
            }
        };
        let cast_value_to_value_with_rax = |val: &LLVM::Value, instrs: &mut Vec<X86::Instr>| -> X86::Value {
            match val {
                LLVM::Value::Const(c) =>  {
                    instrs.push(Move {
                        ty: DEFAULT_TYPE.clone(),
                        src: c.clone().into(),
                        dest: Register::RAX.into(),
                    });
                    Register::RAX.clone().into()
                },
                LLVM::Value::Register(r) => {
                    instrs.push(Move {
                        ty: DEFAULT_TYPE.clone(),
                        src: replace_register(r.clone()).into(),
                        dest: Register::RAX.into(),
                    });
                    Register::RAX.clone().into()
                }
                LLVM::Value::Static(_) => {
                    panic!("should not happen")
                },
            }
        };

        for i in &block.instrs {
            match i {
                LLVM::Instr::Alloc { .. } |
                LLVM::Instr::Store { .. } |
                LLVM::Instr::Load { .. } => {
                    panic!("x86 compilation require LLVM in SSA format")
                },
                LLVM::Instr::Label { .. } => {
                    panic!("label instruction should not appear in block")
                },

                LLVM::Instr::Unreachable => {},

                LLVM::Instr::Branch(b) => {
                    let prepare_jump = |from, to, instrs: &mut Vec<X86::Instr>| {
                        if let Some(m) = self.phis_info.get(to) {
                            if let Some(v) = m.get(from) {
                                for (dest, src) in v {
                                    let src_val = cast_value_to_value_with_rax(&src, instrs);
                                    instrs.push(Move {
                                        ty: DEFAULT_TYPE.clone(),
                                        dest: replace_register(dest.clone()).into(),
                                        src: src_val,
                                    });
                                }
                            }
                        }
                    };

                    match b {
                        LLVM::Branch::Direct { label } => {
                            prepare_jump(&block.label, label, &mut new_instrs);

                            let dest = self.labels_map.get(label).unwrap_or_else(|| panic!("1")).clone();
                            new_instrs.push(Jump(X86::Jump::Direct { dest }));
                        },
                        LLVM::Branch::Conditional { val, true_label, false_label, .. } => {
                            let rhs = cast_value_to_value_with_rax(val, &mut new_instrs);
                            new_instrs.push(Compare {
                                ty: Type::Byte,
                                lhs: 1.into(),
                                rhs,
                            });

                            let mut dest;
                            prepare_jump(&block.label, true_label, &mut new_instrs);
                            dest = self.labels_map.get(true_label).unwrap_or_else(|| panic!("2")).clone();
                            new_instrs.push(Jump(X86::Jump::Conditional {
                                op: RelOp::EQ.into(),
                                dest
                            }));

                            prepare_jump(&block.label, false_label, &mut new_instrs);
                            dest = self.labels_map.get(false_label).unwrap_or_else(|| panic!("3")).clone();
                            new_instrs.push(Jump(X86::Jump::Direct { dest }));
                        },
                    }
                }

                LLVM::Instr::Compare { dest_reg, op, val_lhs, val_rhs, .. } => {
                    let lhs = cast_value_to_value(val_rhs);
                    let rhs = cast_value_to_value_with_rax(val_lhs, &mut new_instrs);

                    new_instrs.push(Compare {
                        ty: DEFAULT_TYPE.clone(),
                        lhs,
                        rhs,
                    });
                    new_instrs.push(Move {
                        ty: DEFAULT_TYPE.clone(),
                        src: 0.into(),
                        dest: replace_register(dest_reg.clone()).into(),
                    });
                    new_instrs.push(Set {
                        op: op.clone().into(),
                        dest: replace_register(dest_reg.clone()).into(),
                    });
                },

                LLVM::Instr::Call { dest_reg, name, args, .. } => {
                    for i in (0..args.len()).rev() {
                        let a = args[i].clone();
                        let src_val = cast_value_to_value(&a.1);
                        if i < 6 {
                            let dest_val = match i {
                                0 => Register::RDI.into(),
                                1 => Register::RSI.into(),
                                2 => Register::RDX.into(),
                                3 => Register::RCX.into(),
                                4 => Register::R8.into(),
                                5 => Register::R9.into(),
                                _ => { panic!("should not happen") },
                            };
                            new_instrs.push(Move {
                                ty: DEFAULT_TYPE.clone(),
                                src: src_val,
                                dest: dest_val,
                            });
                        } else {
                            match i {
                                0..=5 => {},
                                _ => {
                                    new_instrs.push(Push {
                                        ty: DEFAULT_TYPE.clone(),
                                        src: src_val,
                                    });
                                },
                            }
                        }
                    }
                    new_instrs.push(Call { name: name.clone() });
                    if let Some(reg) = dest_reg {
                        new_instrs.push(Move {
                            ty: DEFAULT_TYPE.clone(),
                            src: Register::RAX.into(),
                            dest: replace_register(reg.clone()).into(),
                        });
                    }
                    if args.len() > 6 {
                        new_instrs.push(Arithm {
                            ty: DEFAULT_TYPE.clone(),
                            op: ArithmOp::Add.into(),
                            src: ((args.len() as i32 - 6) * DEFAULT_WORD_SIZE).into(),
                            dest: Register::RSP.into(),
                        });
                    }
                },

                LLVM::Instr::Phi { .. } => {
                    // handled during the jumps
                },

                LLVM::Instr::GetElementPtr { dest, src, .. } => {
                    match src.1.clone() {
                        LLVM::Value::Static(s) => {
                            let static_ = self.statics_map.get(&s).unwrap().clone();
                            new_instrs.push(Lea {
                                ty: DEFAULT_TYPE.clone(),
                                src: static_.into(),
                                dest: Register::RAX.into(),
                            });
                            new_instrs.push(Move {
                                ty: DEFAULT_TYPE.clone(),
                                src: Register::RAX.into(),
                                dest: replace_register(dest.1.clone()).into(),
                            });
                        },
                        _ => { panic!("should not happend!") },
                    }
                },

                LLVM::Instr::Arithm { dest, op, val_lhs, val_rhs } => {
                    use llvm::operators as LLVMOperator;

                    match op {
                        LLVMOperator::Operator::Arithm(LLVMOperator::ArithmOp::Div) |
                        LLVMOperator::Operator::Arithm(LLVMOperator::ArithmOp::Mod) => {
                            new_instrs.push(Xor {
                                ty: DEFAULT_TYPE.clone(),
                                lhs: Register::RDX.into(),
                                rhs: Register::RDX.into(),
                            });
                            new_instrs.push(Move {
                                ty: DEFAULT_TYPE.clone(),
                                src: cast_value_to_value(val_lhs),
                                dest: Register::RAX.into(),
                            });
                            new_instrs.push(Move {
                                ty: DEFAULT_TYPE.clone(),
                                src: cast_value_to_value(val_rhs),
                                dest: Register::RCX.into(),
                            });
                            new_instrs.push(Arithm {
                                ty: DEFAULT_TYPE.clone(),
                                op: ArithmOp::Div.into(),
                                src: Register::RCX.into(),
                                dest: X86::Memory { offset: 0 }.into(), //dummy
                            });

                            if let LLVMOperator::Operator::Arithm(LLVMOperator::ArithmOp::Div) = op {
                                new_instrs.push(Move {
                                    ty: DEFAULT_TYPE.clone(),
                                    src: Register::RAX.into(),
                                    dest: replace_register(dest.1.clone()).into(),
                                });
                            } else {
                                new_instrs.push(Move {
                                    ty: DEFAULT_TYPE.clone(),
                                    src: Register::RDX.into(),
                                    dest: replace_register(dest.1.clone()).into(),
                                });
                            }
                        },
                        _ => {
                            cast_value_to_value_with_rax(val_lhs, &mut new_instrs); // lhs in rax
                            new_instrs.push(Arithm {
                                ty: DEFAULT_TYPE.clone(),
                                op: op.clone().into(),
                                src: cast_value_to_value(&val_rhs),
                                dest: Register::RAX.into(),
                            });

                            new_instrs.push(Move {
                                ty: DEFAULT_TYPE.clone(),
                                src: Register::RAX.into(),
                                dest: replace_register(dest.1.clone()).into(),
                            });
                        },
                    }
                },

                LLVM::Instr::Return { val, .. } => {
                    let src_val = cast_value_to_value(val);

                    new_instrs.push(Move {
                        ty: DEFAULT_TYPE.clone(),
                        src: src_val,
                        dest: Register::RAX.into(),
                    });
                    new_instrs.push(Jump(X86::Jump::Direct { dest: self.fin_label.clone() }));
                },

                LLVM::Instr::ReturnVoid => {
                    new_instrs.push(Xor {
                        ty: DEFAULT_TYPE.clone(),
                        lhs: Register::RAX.into(),
                        rhs: Register::RAX.into(),
                    });
                    new_instrs.push(Jump(X86::Jump::Direct { dest: self.fin_label.clone() }));
                },
            }
        }

        X86::Block {
            label: self.labels_map.get(&block.label).unwrap().clone(),
            instrs: new_instrs,
        }
    }
}

fn align(val: i32, align_to: i32) -> i32 {
    (val + align_to - 1) / align_to * align_to
}
