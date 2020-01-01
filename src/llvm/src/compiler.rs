use crate::instructions as LLVM;
use crate::operators::{Operator, ArithmOp, RelOp};
use base::ast;
use base::symbol_table::{SymbolTable};
use crate::utils::{length_after_escape, instructions_to_blocks};

use std::panic;

#[derive(Debug, PartialEq, Clone)]
pub struct SymbolTableEntity {
    pub ty: LLVM::Type,
    pub reg: LLVM::Register,
}

impl SymbolTableEntity {
    pub fn new(ty: LLVM::Type, reg: LLVM::Register) -> Self {
        SymbolTableEntity {
            ty,
            reg,
        }
    }
}

struct Labeler {
    prefix: String,
    counter: usize
}

impl Labeler {
    fn new(prefix: String) -> Self {
        Labeler {
            prefix: prefix,
            counter: 0
        }
    }

    fn reset(&mut self) {
        self.counter = 0;
    }

    fn next(&mut self) -> String {
        let label = format!("{}{}", self.prefix, self.counter);
        self.counter += 1;
        label
    }
}

pub struct LLVMCompiler {
    symbol_table: SymbolTable<SymbolTableEntity>,
    _label_generator: Labeler,
    _register_generator: Labeler,
    _static_generator: Labeler,
    _instrs: Vec<LLVM::Instr>,
    _statics: Vec<(LLVM::Static, String)>,
    _current_block_label: LLVM::Label,
}

impl LLVMCompiler {
    fn new() -> Self {
        LLVMCompiler {
            symbol_table: SymbolTable::new(),
            _label_generator: Labeler::new("l_".to_string()),
            _register_generator: Labeler::new("v_".to_string()),
            _static_generator: Labeler::new("static_".to_string()),
            _instrs: vec![],
            _statics: vec![],
            _current_block_label: LLVM::Label { name: "".to_string() },
        }
    }

    fn next_label(&mut self) -> LLVM::Label {
        let name = self._label_generator.next();
        LLVM::Label { name }
    }

    fn next_register(&mut self) -> LLVM::Register {
        let name = self._register_generator.next();
        LLVM::Register { name }
    }

    fn next_static(&mut self) -> LLVM::Static {
        let name = self._static_generator.next();
        LLVM::Static { name }
    }

    fn clean_instrs(&mut self) {
        self._instrs = vec![];
    }

    fn get_instrs(&mut self) -> Vec<LLVM::Instr> {
        let instrs = self._instrs.to_vec();
        instrs
    }

    fn add_instr(&mut self, instr: LLVM::Instr) {
        if let LLVM::Instr::Label { val, preds: _ } = &instr {
            // when starting new block, make sure it won't be empty by
            // putting `unreachable` instruction
            self._instrs.push(instr.clone());
            self._instrs.push(LLVM::Instr::Unreachable);
            self._current_block_label = val.clone();
        } else {
            // remove `unreachable` instruction if block won't be empty
            if let Some(LLVM::Instr::Unreachable) = self._instrs.last() {
                self._instrs.pop();
            }
            self._instrs.push(instr.clone());
        }
    }

    fn add_static(&mut self, st: LLVM::Static, val: String) {
        self._statics.push((st, val));
    }

    fn get_statics(&mut self) -> Vec<(LLVM::Static, String)> {
        let statics = self._statics.to_vec();
        statics
    }

    fn get_globals(&mut self) -> Vec<LLVM::Function> {
        vec![
            LLVM::Function {
                ret_ty: LLVM::Type::Void,
                name: "printString".into(),
                args: vec![
                    LLVM::Type::new_ptr(LLVM::Type::Int8),
                ],
                body: vec![],
            },
            LLVM::Function {
                ret_ty: LLVM::Type::Void,
                name: "printInt".into(),
                args: vec![
                    LLVM::Type::Int32,
                ],
                body: vec![],
            },
            LLVM::Function {
                ret_ty: LLVM::Type::Void,
                name: "error".into(),
                args: vec![],
                body: vec![],
            },
            LLVM::Function {
                ret_ty: LLVM::Type::Int32,
                name: "readInt".into(),
                args: vec![],
                body: vec![],
            },
            LLVM::Function {
                ret_ty: LLVM::Type::new_ptr(LLVM::Type::Int8),
                name: "readString".into(),
                args: vec![],
                body: vec![],
            },
            LLVM::Function {
                ret_ty: LLVM::Type::Int1,
                name: "streq".into(),
                args: vec![
                    LLVM::Type::new_ptr(LLVM::Type::Int8),
                    LLVM::Type::new_ptr(LLVM::Type::Int8),
                ],
                body: vec![],
            },
            LLVM::Function {
                ret_ty: LLVM::Type::Int1,
                name: "strne".into(),
                args: vec![
                    LLVM::Type::new_ptr(LLVM::Type::Int8),
                    LLVM::Type::new_ptr(LLVM::Type::Int8),
                ],
                body: vec![],
            },
            LLVM::Function {
                ret_ty: LLVM::Type::new_ptr(LLVM::Type::Int8),
                name: "concat".into(),
                args: vec![
                    LLVM::Type::new_ptr(LLVM::Type::Int8),
                    LLVM::Type::new_ptr(LLVM::Type::Int8),
                ],
                body: vec![],
            },
        ]
    }

    pub fn run(ast_tree: &ast::Program) -> LLVM::Program {
        let mut compiler = LLVMCompiler::new();
        compiler.compile(ast_tree)
    }

    fn init_globals(&mut self) {
        for f in &self.get_globals() {
            self.symbol_table.insert(
                f.name.to_string(),
                SymbolTableEntity::new(
                    LLVM::Type::Fun { ret_ty: Box::new(f.ret_ty.clone()) },
                    LLVM::Register { name: "".to_string() }
                ),
            );
        }
    }

    fn compile(&mut self, ast_tree: &ast::Program) -> LLVM::Program {
        self.init_globals();
        self.compile_program(ast_tree)
    }

    fn compile_program(&mut self, program: &ast::Program) -> LLVM::Program {
        let mut functions = vec![];
        for fn_def in &program.defs {
            self.symbol_table.insert(
                fn_def.ident.clone(),
                SymbolTableEntity::new(
                    LLVM::Type::Fun { ret_ty: Box::new(LLVM::Type::from(&fn_def.ty)) },
                    LLVM::Register { name: "".to_string() }
                ),
            );
        }

        for fn_def in &program.defs {
            functions.push(self.compile_function(&fn_def));
        }
        LLVM::Program {
            declares: self.get_globals(),
            statics: self.get_statics(),
            functions: functions,
        }
    }

    fn compile_function(&mut self, fn_def: &ast::FnDef) -> LLVM::Function {
        self.symbol_table.begin_scope();
        self._label_generator.reset();
        self._register_generator.reset();
        self.clean_instrs();

        let fn_ret_ty = LLVM::Type::from(&fn_def.ty);
        for (i, a) in fn_def.args.iter().enumerate() {
            let arg_reg = LLVM::Register { name: format!("{}", i) };
            let new_reg = self.next_register();
            let arg_ty = LLVM::Type::from(&a.ty);
            self.symbol_table.insert(
                a.ident.clone(),
                SymbolTableEntity::new(
                    arg_ty.clone(),
                    new_reg.clone(),
                )
            );
            self.add_instr(LLVM::Instr::Alloc {
                dest: (arg_ty.clone(), new_reg.clone()),
            });
            self.add_instr(LLVM::Instr::Store {
                dest: (LLVM::Type::new_ptr(arg_ty.clone()), new_reg.clone().into()),
                src: (arg_ty.clone(), arg_reg.into()),
            });
        }

        let starting_label = self.next_label();
        self.add_instr(LLVM::Branch::Direct { label: starting_label.clone() }.into());
        self.add_instr(starting_label.clone().into());
        self.compile_block(&fn_def.block);

        // void function may not have an explicit return at the end
        if let LLVM::Type::Void = fn_ret_ty {
            if let Some(LLVM::Instr::ReturnVoid) = self._instrs.last() {
            } else {
                self.add_instr(LLVM::Instr::ReturnVoid);
            }
        }

        self.symbol_table.end_scope();


        LLVM::Function {
            ret_ty: fn_ret_ty,
            name: fn_def.ident.clone(),
            args: fn_def.args.iter().map(|a| LLVM::Type::from(&a.ty)).collect(),
            body: instructions_to_blocks(&self.get_instrs()),
        }
    }

    fn compile_block(&mut self, block: &ast::Block) {
        self.symbol_table.begin_scope();
        for stmt in &block.stmts {
            self.compile_statement(&stmt);
        }
        self.symbol_table.end_scope();
    }

    fn compile_statement(&mut self, stmt: &ast::Stmt) {
        use base::ast::StmtTypes::*;

        match &stmt.value {
            Empty => {},

            BStmt { block } => {
                self.compile_block(&block);
            },

            Decl { ty, ty_loc: _, items } => {
                for item in items {
                    let item_reg = self.next_register();
                    let item_ty = LLVM::Type::from(ty.clone());

                    // allocate variable it
                    self.add_instr(LLVM::Instr::Alloc {
                        dest: (item_ty.clone(), item_reg.clone())
                    });

                    // calculate and assign variable initial value if needed
                    let initial_value;
                    match &item.value {
                        Some(expr) => {
                            let (_ty, val) = self.compile_expression(&expr);
                            initial_value = val;
                        }
                        None => {
                            initial_value = LLVM::Type::default_value(&item_ty);
                        }
                    }

                    if initial_value != LLVM::Value::Const(LLVM::Const::Null) {
                        self.add_instr(LLVM::Instr::Store {
                            src: (item_ty.clone(), initial_value.clone()),
                            dest: (LLVM::Type::new_ptr(item_ty.clone()), item_reg.clone())
                        });
                    }

                    // add variable to the scope, after all calculations (important!)
                    self.symbol_table.insert(
                        item.ident.clone(),
                        SymbolTableEntity::new(
                            item_ty.clone(),
                            item_reg.clone()
                        )
                    );
                }
            },

            Ass { ident, ident_loc: _, expr } => {
                let var = self.symbol_table.get(&ident).unwrap();
                let (expr_ty, expr_val) = self.compile_expression(&expr);
                self.add_instr(LLVM::Instr::Store {
                    src: (expr_ty.clone(), expr_val.clone()),
                    dest: (LLVM::Type::new_ptr(var.ty.clone()), var.reg.clone())
                });
            },

            Incr { ident, ident_loc: _ } => {
                self.compile_incr_decr(&ident, ArithmOp::Add);
            }

            Decr { ident, ident_loc: _ } => {
                self.compile_incr_decr(&ident, ArithmOp::Sub);
            },

            Ret { value, ret_loc: _ } => {
                match &value {
                    Some(expr) => {
                        let (ret_ty, ret_val) = self.compile_expression(&expr);
                        self.add_instr(LLVM::Instr::Return {
                            ty: ret_ty.clone(),
                            val: ret_val.clone()
                        });
                    },
                    None => {
                        self.add_instr(LLVM::Instr::ReturnVoid);
                    },
                }
            },

            Cond { expr, block } => {
                let (cond_ty, cond_val) = self.compile_expression(&expr);
                let true_block_label = self.next_label();
                let continue_block_label = self.next_label();

                self.add_instr(LLVM::Branch::Conditional {
                    ty: cond_ty.clone(),
                    val: cond_val.clone(),
                    true_label: true_block_label.clone(),
                    false_label: continue_block_label.clone(),
                }.into());

                self.add_instr(true_block_label.into());
                self.compile_block(&block);
                self.add_instr(LLVM::Branch::Direct {
                    label: continue_block_label.clone(),
                }.into());

                self.add_instr(continue_block_label.into());
            },

            CondElse { expr, block_true, block_false } => {
                let (cond_ty, cond_val) = self.compile_expression(&expr);
                let true_block_label = self.next_label();
                let false_block_label = self.next_label();
                let continue_block_label = self.next_label();

                self.add_instr(LLVM::Branch::Conditional {
                    ty: cond_ty.clone(),
                    val: cond_val.clone(),
                    true_label: true_block_label.clone(),
                    false_label: false_block_label.clone(),
                }.into());

                self.add_instr(true_block_label.into());
                self.compile_block(&block_true);
                self.add_instr(LLVM::Branch::Direct {
                    label: continue_block_label.clone(),
                }.into());

                self.add_instr(false_block_label.into());
                self.compile_block(&block_false);
                self.add_instr(LLVM::Branch::Direct {
                    label: continue_block_label.clone(),
                }.into());

                self.add_instr(continue_block_label.into());
            },

            While { expr, block } => {
                let conditional_block_label = self.next_label();
                let loop_block_label = self.next_label();
                let continue_block_label = self.next_label();

                self.add_instr(LLVM::Branch::Direct {
                    label: conditional_block_label.clone(),
                }.into());

                self.add_instr(conditional_block_label.clone().into());
                let (cond_ty, cond_val) = self.compile_expression(&expr);
                self.add_instr(LLVM::Branch::Conditional {
                    ty: cond_ty.clone(),
                    val: cond_val.clone(),
                    true_label: loop_block_label.clone(),
                    false_label: continue_block_label.clone(),
                }.into());

                self.add_instr(loop_block_label.into());
                self.compile_block(&block);
                self.add_instr(LLVM::Branch::Direct {
                    label: conditional_block_label.clone(),
                }.into());

                self.add_instr(continue_block_label.into());
            },

            SExp { expr } => {
                self.compile_expression(&expr);
            },
        }
    }

    fn compile_incr_decr(&mut self, ident: &String, op: ArithmOp) {
        let var = self.symbol_table.get(&ident).unwrap();
        let load_reg = self.next_register();
        self.add_instr(LLVM::Instr::Load {
            dest: (var.ty.clone(), load_reg.clone()),
            src: (LLVM::Type::new_ptr(var.ty.clone()), var.reg.clone())
        });

        let incr_reg = self.next_register();
        self.add_instr(LLVM::Instr::Arithm {
            dest: (var.ty.clone(), incr_reg.clone()),
            op: op.into(),
            val_lhs: load_reg.into(),
            val_rhs: LLVM::Const::from(1).into()
        });

        self.add_instr(LLVM::Instr::Store {
            src: (var.ty.clone(), incr_reg.into()),
            dest: (LLVM::Type::new_ptr(var.ty.clone()), var.reg.clone())
        });
    }

    fn compile_expression(&mut self, expr: &ast::Expr) -> (LLVM::Type, LLVM::Value) {
        use ast::ExprTypes::*;
        match &expr.value {

            EOr { expr1, expr2 } => {
                let (ty1, val1) = self.compile_expression(&expr1);
                assert_eq!(ty1, LLVM::Type::Int1);

                let expr1_block_label = self._current_block_label.clone();
                let mut expr2_block_label = self.next_label();
                let continue_block_label = self.next_label();

                self.add_instr(LLVM::Branch::Conditional {
                    ty: LLVM::Type::Int1,
                    val: val1,
                    true_label: continue_block_label.clone(),
                    false_label: expr2_block_label.clone(),
                }.into());

                self.add_instr(expr2_block_label.clone().into());
                let (ty2, val2) = self.compile_expression(&expr2);
                assert_eq!(ty2, LLVM::Type::Int1);
                // we need to update label as expr2 might be calcualted using multiple blocks
                expr2_block_label = self._current_block_label.clone();
                self.add_instr(LLVM::Branch::Direct {
                    label: continue_block_label.clone(),
                }.into());

                self.add_instr(continue_block_label.into());
                let ret_reg = self.next_register();
                self.add_instr(LLVM::Instr::Phi {
                    dest: (LLVM::Type::Int1, ret_reg.clone()),
                    preds: vec![
                        (LLVM::Const::True.into(), expr1_block_label),
                        (val2.clone(), expr2_block_label.clone()),
                    ],
                });

                (LLVM::Type::Int1, ret_reg.clone().into())
            },

            EAnd { expr1, expr2 } => {
                let (ty1, val1) = self.compile_expression(&expr1);
                assert_eq!(ty1, LLVM::Type::Int1);

                let expr1_block_label = self._current_block_label.clone();
                let mut expr2_block_label = self.next_label();
                let continue_block_label = self.next_label();

                self.add_instr(LLVM::Branch::Conditional {
                    ty: LLVM::Type::Int1,
                    val: val1,
                    true_label: expr2_block_label.clone(),
                    false_label: continue_block_label.clone(),
                }.into());

                self.add_instr(expr2_block_label.clone().into());
                let (ty2, val2) = self.compile_expression(&expr2);
                assert_eq!(ty2, LLVM::Type::Int1);
                // we need to update label as expr2 might be calcualted using multiple blocks
                expr2_block_label = self._current_block_label.clone();
                self.add_instr(LLVM::Branch::Direct {
                    label: continue_block_label.clone(),
                }.into());

                self.add_instr(continue_block_label.into());
                let ret_reg = self.next_register();
                self.add_instr(LLVM::Instr::Phi {
                    dest: (LLVM::Type::Int1, ret_reg.clone()),
                    preds: vec![
                        (LLVM::Const::False.into(), expr1_block_label),
                        (val2.clone(), expr2_block_label.clone()),
                    ],
                });

                (LLVM::Type::Int1, ret_reg.clone().into())
            },

            ERel { op, op_loc: _, expr1, expr2 } => {
                let (ty1, val1) = self.compile_expression(&expr1);
                let (ty2, val2) = self.compile_expression(&expr2);
                assert_eq!(ty1, ty2);

                let ret_reg = self.next_register();

                match ty1.clone() {
                    LLVM::Type::Ptr(ty) => {
                        assert_eq!(*ty, LLVM::Type::Int8);
                        assert!(
                            *op == ast::Operator::RelOp(ast::RelOp::EQ)
                         || *op == ast::Operator::RelOp(ast::RelOp::NE),
                         "string supports only == and != relation operators");

                        let fn_name =  match *op {
                            ast::Operator::RelOp(ast::RelOp::EQ) => "streq",
                            ast::Operator::RelOp(ast::RelOp::NE) => "strne",
                            _ => { panic!("should not happen") },
                        };

                        self.add_instr(LLVM::Instr::Call {
                            dest_reg: Some(ret_reg.clone()),
                            ret_ty: LLVM::Type::Int1,
                            name: fn_name.to_string(),
                            args: vec![
                                (ty1.clone(), val1.clone()),
                                (ty2.clone(), val2.clone())
                            ],
                        });
                    },

                    _ => {
                        self.add_instr(LLVM::Instr::Compare {
                            dest_reg: ret_reg.clone(),
                            op: Operator::from(op.clone()),
                            ty: ty1.clone(),
                            val_lhs: val1,
                            val_rhs: val2,
                        });
                    },
                }
                (LLVM::Type::Int1, ret_reg.clone().into())
            },

            EAdd { op, op_loc: _, expr1, expr2 } |
            EMul { op, op_loc: _, expr1, expr2 } => {
                let (ty1, val1) = self.compile_expression(&expr1);
                let (ty2, val2) = self.compile_expression(&expr2);
                assert_eq!(ty1, ty2);

                let ret_reg = self.next_register();

                match ty1.clone() {
                    LLVM::Type::Ptr(ty) => {
                        assert_eq!(*ty, LLVM::Type::Int8);
                        assert!(
                            *op == ast::Operator::ArithmOp(ast::ArithmOp::Plus),
                         "string supports only + arithmetic operators");

                        let fn_name =  match *op {
                            ast::Operator::ArithmOp(ast::ArithmOp::Plus) => "concat",
                            _ => { panic!("should not happen") },
                        };

                        self.add_instr(LLVM::Instr::Call {
                            dest_reg: Some(ret_reg.clone()),
                            ret_ty: ty1.clone(),
                            name: fn_name.to_string(),
                            args: vec![
                                (ty1.clone(), val1.clone()),
                                (ty2.clone(), val2.clone())
                            ],
                        });
                    },

                    _ => {
                        self.add_instr(LLVM::Instr::Arithm {
                            dest: (ty1.clone(), ret_reg.clone()),
                            op: Operator::from(op.clone()),
                            val_lhs: val1,
                            val_rhs: val2,
                        });
                    }
                }
                (ty1.clone(), ret_reg.clone().into())
            },

            ENeg { expr } => {
                let (ty, val) = self.compile_expression(&expr);
                assert_eq!(ty, LLVM::Type::Int32);

                let ret_reg = self.next_register();

                self.add_instr(LLVM::Instr::Arithm {
                    dest: (LLVM::Type::Int32, ret_reg.clone()),
                    op: ArithmOp::Sub.into(),
                    val_lhs: LLVM::Const::from(0).into(),
                    val_rhs: val,
                });
                (LLVM::Type::Int32, ret_reg.clone().into())
            },

            ENot { expr } => {
                let (ty, val) = self.compile_expression(&expr);
                assert_eq!(ty, LLVM::Type::Int1);

                let ret_reg = self.next_register();
                self.add_instr(LLVM::Instr::Compare {
                    dest_reg: ret_reg.clone(),
                    op: RelOp::EQ.into(),
                    ty: ty.clone(),
                    val_lhs: val,
                    val_rhs: LLVM::Const::False.into(),
                });
                (LLVM::Type::Int1, ret_reg.clone().into())
            },

            EVar { ident, ident_loc: _ } => {
                let var = self.symbol_table.get(&ident).unwrap();
                let load_reg = self.next_register();
                self.add_instr(LLVM::Instr::Load {
                    dest: (var.ty.clone(), load_reg.clone()),
                    src: (LLVM::Type::new_ptr(var.ty.clone()), var.reg.clone()),
                });

                (var.ty.clone(), load_reg.clone().into())
            },

            ELitInt { value } => {
                (LLVM::Type::Int32, LLVM::Const::from(value.clone()).into())
            },

            ELitTrue => {
                (LLVM::Type::Int1, LLVM::Const::True.into())
            },

            ELitFalse => {
                (LLVM::Type::Int1, LLVM::Const::False.into())
            },

            EApp { ident, ident_loc: _, args, args_loc: _ } => {
                let var = self.symbol_table.get(&ident).unwrap();

                let args_expr = args.iter().map(|a| self.compile_expression(&a)).collect();
                let ret_reg = self.next_register();
                if let LLVM::Type::Fun { ret_ty } = var.ty {
                    if let LLVM::Type::Void = *ret_ty {
                        self.add_instr(LLVM::Instr::Call {
                            dest_reg: None,
                            ret_ty: *ret_ty.clone(),
                            name: ident.clone(),
                            args: args_expr,
                        });
                        (LLVM::Type::Void, LLVM::Const::Null.into())
                    } else {
                        self.add_instr(LLVM::Instr::Call {
                            dest_reg: Some(ret_reg.clone()),
                            ret_ty: *ret_ty.clone(),
                            name: ident.clone(),
                            args: args_expr,
                        });
                        (*ret_ty.clone(), ret_reg.clone().into())
                    }
                } else {
                    panic!("should not happen")
                }
            },

            EString { value } => {
                // TODO: properly handle strings
                let str_reg = self.next_static();
                self.add_static(str_reg.clone(), value.clone());

                let ret_reg = self.next_register();
                let arr = LLVM::Type::new_array(
                    LLVM::Type::Int8,
                    length_after_escape(value.clone())
                );
                self.add_instr(LLVM::Instr::GetElementPtr {
                    dest: (arr.clone(), ret_reg.clone()),
                    src: (LLVM::Type::new_ptr(arr.clone()), str_reg.into()),
                    idx1: (LLVM::Type::Int32, LLVM::Const::from(0).into()),
                    idx2: (LLVM::Type::Int32, LLVM::Const::from(0).into()),
                });

                (LLVM::Type::new_ptr(LLVM::Type::Int8), ret_reg.into())
            },
        }
    }
}
