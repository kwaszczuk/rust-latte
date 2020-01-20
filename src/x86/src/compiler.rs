use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;
use crate::instructions as X86;
use llvm::instructions as LLVM;
use crate::operators::{ArithmOp, RelOp};
use base::types::{Labeler};
use X86::{DEFAULT_TYPE, DEFAULT_WORD_SIZE, DEFAULT_ARGS_OFFSET, CALLEE_SAVED_OFFSET};
use crate::register_allocation::{allocate_registers};
use std::cmp::{min};

use std::panic;

pub struct X86Compiler {
    label_generator: Labeler<X86::Label>,
    statics_map: HashMap<LLVM::Static, X86::Static>,
    registers_map: HashMap<LLVM::Register, X86::Value>,
    labels_map: HashMap<LLVM::Label, X86::Label>,
    allocated_registers: HashSet<X86::Register>,
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
            allocated_registers: HashSet::new(),
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
                let mut instrs = vec![];
                // save callee saved registers
                for reg in X86::all_registers() {
                    if reg != RSP && reg.is_callee_saved() {
                        instrs.push(Push {
                            ty: DEFAULT_TYPE.clone(),
                            src: TypedRegister::default(reg).into(),
                        });
                    }
                }
                instrs.push(Move {
                    ty: DEFAULT_TYPE.clone(),
                    src: TypedRegister::default(RSP).into(),
                    dest: TypedRegister::default(RBP).into(),
                });
                instrs.push(Arithm {
                    ty: DEFAULT_TYPE.clone(),
                    op: ArithmOp::Sub.into(),
                    src: align(var_cnt * DEFAULT_WORD_SIZE, 16).into(),
                    dest: TypedRegister::default(RSP).into(),
                });

                #[cfg(feature="no-registers-allocation")] {
                    // move function arguments to the stack variables
                    for i in 0..fun.args.len() {
                        if i < 6 {
                            let src_reg = match i {
                                0 => RDI,
                                1 => RSI,
                                2 => RDX,
                                3 => RCX,
                                4 => R8,
                                5 => R9,
                                _ => { panic!("should not happen") },
                            };
                            instrs.push(Move {
                                ty: DEFAULT_TYPE.clone(),
                                src: TypedRegister::default(src_reg).into(),
                                dest: X86::Storage::new_stack_memory(
                                    -(i as i32 + 1) * DEFAULT_WORD_SIZE
                                ).into(),
                            });
                        }
                    }
                }
                instrs
            },
        };

        self.fin_label = self.label_generator.next();
        let fin_block = X86::Block {
            label: self.fin_label.clone(),
            instrs: {
                let mut instrs = vec![];
                instrs.push(Arithm {
                    ty: DEFAULT_TYPE.clone(),
                    op: ArithmOp::Add.into(),
                    src: align(var_cnt * DEFAULT_WORD_SIZE, 16).into(),
                    dest: TypedRegister::default(RSP).into(),
                });
                // restore callee saved registers
                for reg in X86::all_registers().iter().rev() {
                    if reg != &RSP && reg.is_callee_saved() {
                        instrs.push(Pop {
                            ty: DEFAULT_TYPE.clone(),
                            dest: TypedRegister::default(reg.clone()).into(),
                        });

                    }
                }
                instrs.push(Return {});

                instrs
            }
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
        let mut cnt = 0;

        #[cfg(not(feature="no-registers-allocation"))] {
            use X86::*;


            let allocation = allocate_registers(&fun);

            self.allocated_registers = HashSet::new();
            for (llvm_reg, x86_reg) in &allocation.results {
                if let Some(reg) = x86_reg {
                    // create a mappings for allocated registers
                    self.registers_map.insert(
                        llvm_reg.clone(),
                        Storage::Register(TypedRegister::default(reg.clone())).into(),
                    );
                    self.allocated_registers.insert(reg.clone());
                } else if llvm_reg.prefix != "" {
                    // create mappings for unallocated non-argument registers
                    cnt += 1;
                    self.registers_map.insert(
                        llvm_reg.clone(),
                        Storage::new_stack_memory(-cnt * DEFAULT_WORD_SIZE).into()
                    );
                } else {
                    // create mappings for unallocated argument registers
                    let arg_idx = llvm_reg.counter.clone();
                    self.registers_map.insert(
                        llvm_reg.clone(),
                        Storage::new_stack_memory(
                            DEFAULT_ARGS_OFFSET + (arg_idx as i32 - 6 + CALLEE_SAVED_OFFSET) * DEFAULT_WORD_SIZE
                        ).into()
                    );
                }
            }
        }

        #[cfg(feature="no-registers-allocation")] {
            use LLVM::Instr::*;
            self.registers_map = HashMap::new();

            for i in 0..fun.args.len() {
                let store: X86::Storage = match i {
                    0..=5 => {
                        cnt += 1;
                        X86::Storage::new_stack_memory(-cnt * DEFAULT_WORD_SIZE).into()
                    },
                    _ => X86::Storage::new_stack_memory(
                        DEFAULT_ARGS_OFFSET + (i as i32 - 6 + CALLEE_SAVED_OFFSET) * DEFAULT_WORD_SIZE
                    ).into(),
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
                        Return { .. } |
                        ReturnVoid |
                        Unreachable |
                        Branch(_) |
                        Store { .. } |
                        Label { .. } |
                        Bitcast { .. } |
                        Sext { .. } => {},

                        Compare { dest_reg, .. } => {
                            cnt += 1;
                            self.registers_map.insert(dest_reg.clone(), X86::Storage::new_stack_memory(
                                -cnt * DEFAULT_WORD_SIZE
                            ).into());
                        },

                        Call { dest_reg, .. } => {
                            if let Some(reg) = dest_reg {
                                cnt += 1;
                                self.registers_map.insert(reg.clone(), X86::Storage::new_stack_memory(
                                    -cnt * DEFAULT_WORD_SIZE
                                ).into());
                            }
                        },

                        Phi { dest, .. } |
                        Load { dest, .. } |
                        GetElementPtr { dest, .. } |
                        Arithm { dest, .. } => {
                            cnt += 1;
                            self.registers_map.insert(dest.1.clone(), X86::Storage::new_stack_memory(
                                -cnt * DEFAULT_WORD_SIZE
                            ).into());
                        },
                    }
                }
            }
        }

        for b in &fun.body {
            for i in &b.instrs {
                match i {
                    LLVM::Instr::Bitcast { dest, src } |
                    LLVM::Instr::Sext { dest, src } => {
                        match &src.1 {
                            LLVM::Value::Const(c) => {
                                if let LLVM::Const::Int(ci) = c {
                                    self.registers_map.insert(dest.1.clone(), ci.clone().into());
                                } else {
                                    panic!("should not happen")
                                }
                            },
                            LLVM::Value::Register(r) => {
                                let val = self.registers_map.get(&r).unwrap().clone();
                                self.registers_map.insert(dest.1.clone(), val);
                            },
                            LLVM::Value::Static(_) => {
                                panic!("should not happen")
                            },
                        }
                    },
                    _ => {},
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
        use Register::*;

        let mut new_instrs = vec![];

        let replace_register = |r| self.registers_map.get(&r).unwrap().clone();
        let cast_value_to_value = |val: &LLVM::Value| -> X86::Value {
            match val {
                LLVM::Value::Const(c) => c.clone().into(),
                LLVM::Value::Register(r) => replace_register(r.clone()),
                LLVM::Value::Static(_) => {
                    panic!("should not happen")
                },
            }
        };
        let cast_value_to_value_with_reg = |ty: &Type, val: &LLVM::Value, reg: Register, instrs: &mut Vec<X86::Instr>| -> X86::Value {
            let final_reg = TypedRegister::new(ty.clone(), reg);
            match val {
                LLVM::Value::Const(c) =>  {
                    instrs.push(Move {
                        ty: ty.clone(),
                        src: c.clone().into(),
                        dest: final_reg.clone().into(),
                    });
                    final_reg.clone().into()
                },
                LLVM::Value::Register(r) => {
                    instrs.push(Move {
                        ty: ty.clone(),
                        src: replace_register(r.clone()).with_type(ty).into(),
                        dest: final_reg.clone().into(),
                    });
                    final_reg.clone().into()
                }
                LLVM::Value::Static(_) => {
                    panic!("should not happen")
                },
            }
        };

        for i in &block.instrs {
            match i {
                LLVM::Instr::Alloc { .. } => {
                    panic!("x86 compilation require LLVM in SSA format")
                },
                LLVM::Instr::Label { .. } => {
                    panic!("label instruction should not appear in block")
                },

                LLVM::Instr::Unreachable => {},

                LLVM::Instr::Bitcast { .. } => {},

                LLVM::Instr::Sext { dest, src } => {
                    if let LLVM::Value::Register(r) = &src.1 {
                        let val = self.registers_map.get(&r).unwrap().clone();
                        if let Value::Storage(Storage::Memory(_)) = &val {
                            new_instrs.push(Move {
                                ty: DEFAULT_TYPE.clone(),
                                src: val.clone(),
                                dest: TypedRegister::default(RAX).into(),
                            });
                            new_instrs.push(Move {
                                ty: DEFAULT_TYPE.clone(),
                                src: 0.into(),
                                dest: val.clone().into(),
                            });
                            new_instrs.push(Move {
                                ty: Type::Double,
                                src: TypedRegister::new(Type::Double, RAX).into(),
                                dest: val.clone().into(),
                            });
                        }
                    }
                },

                LLVM::Instr::Load { dest, src } => {
                    let x86_dest_ty: Type = dest.0.clone().into();
                    new_instrs.push(Move {
                        ty: DEFAULT_TYPE.clone(),
                        src: replace_register(src.1.clone()).with_type(&DEFAULT_TYPE),
                        dest: TypedRegister::default(RAX).into(),
                    });

                    new_instrs.push(Move {
                        ty: x86_dest_ty.clone(),
                        src: Memory::from(TypedRegister::default(RAX)).into(),
                        dest: TypedRegister::new(x86_dest_ty.clone(), RAX).into(),
                    });

                    new_instrs.push(Move {
                        ty: x86_dest_ty.clone(),
                        src: TypedRegister::new(x86_dest_ty.clone(), RAX).into(),
                        dest: replace_register(dest.1.clone()).with_type(&x86_dest_ty).into(),
                    });
                },

                LLVM::Instr::Store { dest, src } => {
                    let x86_src_ty: Type = src.0.clone().into();

                    new_instrs.push(Move {
                        ty: DEFAULT_TYPE.clone(),
                        src: replace_register(dest.1.clone()).with_type(&DEFAULT_TYPE).into(),
                        dest: TypedRegister::default(RAX).into(),
                    });

                    let val_src = cast_value_to_value_with_reg(&x86_src_ty, &src.1, RCX, &mut new_instrs);

                    new_instrs.push(Move {
                        ty: x86_src_ty.clone(),
                        src: val_src,
                        dest: X86::Memory {
                            base: TypedRegister::default(RAX).into(),
                            offset_base: None,
                            offset_mul: None,
                        }.into(),
                    });
                },

                LLVM::Instr::Branch(b) => {
                    let prepare_jump = |from, to, instrs: &mut Vec<X86::Instr>| {
                        if let Some(m) = self.phis_info.get(to) {
                            if let Some(v) = m.get(from) {
                                for (dest, src) in v {
                                    let src_val = cast_value_to_value_with_reg(&DEFAULT_TYPE, &src, RAX, instrs);
                                    instrs.push(Move {
                                        ty: DEFAULT_TYPE.clone(),
                                        dest: replace_register(dest.clone()).with_type(&DEFAULT_TYPE).into(),
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
                        LLVM::Branch::Conditional { val, true_label, false_label, ty } => {
                            let rhs = cast_value_to_value_with_reg(&ty.clone().into(), val, RAX, &mut new_instrs);
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

                LLVM::Instr::Compare { dest_reg, op, val_lhs, val_rhs, ty } => {
                    let x86_ty = ty.clone().into();
                    let lhs = cast_value_to_value(val_rhs).with_type(&x86_ty);
                    let rhs = cast_value_to_value_with_reg(&x86_ty, val_lhs, RAX, &mut new_instrs);

                    new_instrs.push(Compare {
                        ty: x86_ty.clone(),
                        lhs,
                        rhs,
                    });
                    new_instrs.push(Move {
                        ty: x86_ty.clone(),
                        src: 0.into(),
                        dest: replace_register(dest_reg.clone()).with_type(&x86_ty).into(),
                    });
                    new_instrs.push(Set {
                        op: op.clone().into(),
                        dest: replace_register(dest_reg.clone()).with_type(&Type::Byte).into(),
                    });
                },

                LLVM::Instr::Call { dest_reg, name, args, ret_ty } => {
                    let allocated_registers: Vec<X86::Register> =
                        self.allocated_registers.iter().cloned().collect();

                    // save caller-saved registers before function call
                    for reg in &allocated_registers {
                        if reg.is_caller_saved() {
                            new_instrs.push(Push {
                                ty: DEFAULT_TYPE.clone(),
                                src: Storage::Register(TypedRegister::default(reg.clone())).into(),
                            });
                        }
                    }

                    // first push non-register arguments to make sure that registers
                    // that store their values are up to date
                    for i in 6..args.len() {
                        let (arg_ty, arg_val) = args[i].clone();
                        let x86_arg_ty = X86::Type::from(arg_ty.clone());
                        let src_val = cast_value_to_value(&arg_val).with_type(&x86_arg_ty);

                        new_instrs.push(Push {
                            ty: arg_ty.into(),
                            src: src_val,
                        });
                    }

                    // setup register arguments

                    let mut moves = HashMap::new();
                    for i in 0..min(6, args.len()) {
                        let (arg_ty, arg_val) = args[i].clone();
                        let x86_arg_ty = X86::Type::from(arg_ty.clone());
                        let src_val = cast_value_to_value(&arg_val);
                        let dest_reg = match i {
                            0 => RDI,
                            1 => RSI,
                            2 => RDX,
                            3 => RCX,
                            4 => R8,
                            5 => R9,
                            _ => { panic!("should not happen") },
                        };
                        let dest_val = Storage::from(TypedRegister::default(dest_reg));
                        moves.insert(dest_val, (x86_arg_ty, src_val));
                    }

                    while !moves.is_empty() {
                        let mut next_move = None;
                        let sources: HashSet<X86::Value> = moves.values()
                            .map(|(ty, src)| src).cloned()
                            .collect();

                        for (dest, (ty, src)) in &moves {
                            let dest_val = Value::from(dest.clone());
                            // check if register is safe to set,
                            // meaning no move uses it as a source
                            if &dest_val == src || !sources.contains(&dest_val) {
                                next_move = Some((ty.clone(), src.clone(), dest.clone()));
                                break;
                            }
                        }

                        // if no register can be set safely, offload one of
                        // them to RAX, it will break a cycle and make chosen
                        // register safe to set
                        if let None = next_move {
                            let dest = moves.keys().cloned().next().unwrap();
                            let (ty, mut src) = moves.get(&dest).unwrap().clone();

                            moves.remove(&dest);
                            new_instrs.push(Move {
                                ty: DEFAULT_TYPE.clone(),
                                src: src.with_type(&DEFAULT_TYPE),
                                dest: TypedRegister::default(RAX).into(),
                            });
                            moves.insert(dest, (ty, TypedRegister::default(RAX).into()));
                        } else {
                            let (ty, mut src, mut dest) = next_move.unwrap();
                            new_instrs.push(Move {
                                ty: ty.clone(),
                                src: src.with_type(&ty),
                                dest: dest.with_type(&ty),
                            });
                            moves.remove(&dest);
                        }
                    }

                    // function call
                    new_instrs.push(Call { name: name.clone() });

                    // remove pushed arguments
                    if args.len() > 6 {
                        new_instrs.push(Arithm {
                            ty: DEFAULT_TYPE.clone(),
                            op: ArithmOp::Add.into(),
                            src: ((args.len() as i32 - 6) * DEFAULT_WORD_SIZE).into(),
                            dest: TypedRegister::default(RSP).into(),
                        });
                    }

                    // restore caller-saved registers
                    for reg in allocated_registers.iter().rev() {
                        if reg.is_caller_saved() {
                            new_instrs.push(Pop {
                                ty: DEFAULT_TYPE.clone(),
                                dest: Storage::Register(TypedRegister::default(reg.clone())).into(),
                            });
                        }
                    }

                    // save function return
                    if let Some(reg) = dest_reg {
                        new_instrs.push(Move {
                            ty: ret_ty.clone().into(),
                            src: TypedRegister::new(ret_ty.clone().into(), RAX).into(),
                            dest: replace_register(reg.clone()).with_type(&ret_ty.clone().into()).into(),
                        });
                    }
                },

                LLVM::Instr::Phi { .. } => {
                    // handled during the jumps
                },

                LLVM::Instr::GetElementPtr { dest, src, args } => {
                    match src.1.clone() {
                        LLVM::Value::Static(s) => {
                            let static_ = self.statics_map.get(&s).unwrap().clone();
                            new_instrs.push(Lea {
                                ty: DEFAULT_TYPE.clone(),
                                src: static_.into(),
                                dest: TypedRegister::default(RAX).into(),
                            });
                            new_instrs.push(Move {
                                ty: DEFAULT_TYPE.clone(),
                                src: TypedRegister::default(RAX).into(),
                                dest: replace_register(dest.1.clone()).with_type(&DEFAULT_TYPE).into(),
                            });
                        },
                        LLVM::Value::Register(reg) => {
                            assert!(args.len() == 1);
                            let (idx_ty, idx_val) = args[0].clone();

                            let rax_reg = TypedRegister::new(src.0.clone().into(), RAX);
                            new_instrs.push(Move {
                                ty: src.0.clone().into(), // always ptr type
                                src: replace_register(reg.clone()).with_type(&src.0.clone().into()).into(),
                                dest: rax_reg.clone().into(),
                            });

                            new_instrs.push(Xor {
                                ty: DEFAULT_TYPE.clone(),
                                lhs: TypedRegister::default(RCX).into(),
                                rhs: TypedRegister::default(RCX).into(),
                            });

                            if let LLVM::Value::Const(c) = idx_val {
                                new_instrs.push(Move {
                                    ty: idx_ty.clone().into(),
                                    src: c.into(),
                                    dest: TypedRegister::new(idx_ty.clone().into(), RCX).into(),
                                });
                            } else if let LLVM::Value::Register(r) = idx_val {
                                new_instrs.push(Move {
                                    ty: idx_ty.clone().into(),
                                    src: replace_register(r.clone()).with_type(&idx_ty.clone().into()).into(),
                                    dest: TypedRegister::new(idx_ty.clone().into(), RCX).into(),
                                });
                            } else {
                                panic!("should not happen")
                            }

                            new_instrs.push(Lea {
                                ty: DEFAULT_TYPE.clone(),
                                src: X86::Storage::Memory(X86::Memory {
                                    base: rax_reg.clone().into(),
                                    offset_base: Some(
                                        Offset::Register(TypedRegister::default(RCX))
                                    ),
                                    offset_mul: Some(Offset::Const(dest.0.byte_size())),
                                }).into(),
                                dest: TypedRegister::default(RAX).into(),
                            });

                            new_instrs.push(Move {
                                ty: src.0.clone().into(),
                                src: TypedRegister::new(src.0.clone().into(), RAX).into(),
                                dest: replace_register(dest.1.clone()).with_type(&src.0.clone().into()).into(),
                            });
                        },
                        _ => { panic!("should not happen") }
                    }
                },

                LLVM::Instr::Arithm { dest, op, val_lhs, val_rhs } => {
                    use llvm::operators as LLVMOperator;
                    let x86_dest_ty: Type = dest.0.clone().into();

                    match op {
                        LLVMOperator::Operator::Arithm(LLVMOperator::ArithmOp::Div) |
                        LLVMOperator::Operator::Arithm(LLVMOperator::ArithmOp::Mod) => {
                            let rax_reg = TypedRegister::new(x86_dest_ty.clone(), RAX);
                            let rcx_reg = TypedRegister::new(x86_dest_ty.clone(), RCX);

                            new_instrs.push(Xor {
                                ty: DEFAULT_TYPE.clone(),
                                lhs: TypedRegister::default(RDX).into(),
                                rhs: TypedRegister::default(RDX).into(),
                            });
                            new_instrs.push(Move {
                                ty: x86_dest_ty.clone(),
                                src: cast_value_to_value(val_lhs).with_type(&x86_dest_ty),
                                dest: rax_reg.clone().into(),
                            });
                            new_instrs.push(Move {
                                ty: x86_dest_ty.clone(),
                                src: cast_value_to_value(val_rhs).with_type(&x86_dest_ty),
                                dest: rcx_reg.clone().into(),
                            });

                            new_instrs.push(Cdq);
                            new_instrs.push(Arithm {
                                ty: x86_dest_ty.clone(),
                                op: ArithmOp::Div.into(),
                                src: rcx_reg.clone().into(),
                                dest: X86::Storage::new_stack_memory(0).into(), //dummy
                            });

                            if let LLVMOperator::Operator::Arithm(LLVMOperator::ArithmOp::Div) = op {
                                new_instrs.push(Move {
                                    ty: x86_dest_ty.clone(),
                                    src: rax_reg.clone().into(),
                                    dest: replace_register(dest.1.clone()).with_type(&x86_dest_ty.clone()).into(),
                                });
                            } else {
                                new_instrs.push(Move {
                                    ty: x86_dest_ty.clone(),
                                    src: TypedRegister::new(x86_dest_ty.clone(), RDX).into(),
                                    dest: replace_register(dest.1.clone()).with_type(&x86_dest_ty.clone()).into(),
                                });
                            }
                        },
                        _ => {
                            cast_value_to_value_with_reg(&x86_dest_ty, val_lhs, RAX, &mut new_instrs); // lhs in rax
                            let rax_reg = TypedRegister::new(x86_dest_ty.clone(), RAX);
                            new_instrs.push(Arithm {
                                ty: x86_dest_ty.clone(),
                                op: op.clone().into(),
                                src: cast_value_to_value(&val_rhs).with_type(&x86_dest_ty),
                                dest: rax_reg.clone().into(),
                            });

                            new_instrs.push(Move {
                                ty: x86_dest_ty.clone(),
                                src: rax_reg.clone().into(),
                                dest: replace_register(dest.1.clone()).with_type(&x86_dest_ty).into(),
                            });
                        },
                    }
                },

                LLVM::Instr::Return { val, ty } => {
                    let src_val = cast_value_to_value(val).with_type(&ty.clone().into());

                    new_instrs.push(Move {
                        ty: ty.clone().into(),
                        src: src_val,
                        dest: TypedRegister::new(ty.clone().into(), RAX).into(),
                    });
                    new_instrs.push(Jump(X86::Jump::Direct { dest: self.fin_label.clone() }));
                },

                LLVM::Instr::ReturnVoid => {
                    new_instrs.push(Xor {
                        ty: DEFAULT_TYPE.clone(),
                        lhs: TypedRegister::default(RAX).into(),
                        rhs: TypedRegister::default(RAX).into(),
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
