use std::collections::{HashMap};
use crate::instructions as LLVM;
use crate::operators::{Operator, ArithmOp, RelOp};
use base::ast;
use base::types::{Labeler, Location};
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

pub struct LLVMCompiler {
    symbol_table: SymbolTable<SymbolTableEntity>,
    _label_generator: Labeler<LLVM::Label>,
    _register_generator: Labeler<LLVM::Register>,
    _static_generator: Labeler<LLVM::Static>,
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
            _current_block_label: LLVM::Label::new("".to_string(), 0), // dummy
        }
    }

    fn next_label(&mut self) -> LLVM::Label {
        self._label_generator.next()
    }

    fn next_register(&mut self) -> LLVM::Register {
        self._register_generator.next()
    }

    fn next_static(&mut self) -> LLVM::Static {
        self._static_generator.next()
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
            LLVM::Function {
                ret_ty: LLVM::Type::new_ptr(LLVM::Type::Int8),
                name: "calloc".into(),
                args: vec![
                    LLVM::Type::Int64,
                    LLVM::Type::Int64,
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
                    LLVM::Register::new("".to_string(), 0), // dummy
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
                    LLVM::Register::new("".to_string(), 0), // dummy
                ),
            );
        }

        for fn_def in &program.defs {
            functions.push(self.compile_function(&fn_def));
        }
        LLVM::Program {
            options: HashMap::new(),
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
            let arg_reg = LLVM::Register::new("".to_string(), i);
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
                    self._instrs.insert(0, LLVM::Instr::Alloc {
                        dest: (item_ty.clone(), item_reg.clone())
                    });
                    self._instrs.insert(1, LLVM::Instr::Store {
                        src: (item_ty.clone(), LLVM::Type::default_value(&item_ty)),
                        dest: (LLVM::Type::new_ptr(item_ty.clone()), item_reg.clone())
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

            Ass { lval, expr } => {
                let (lval_ty, lval_reg) = self.compile_lvalue(&lval);
                let (expr_ty, expr_val) = self.compile_expression(&expr);
                self.add_instr(LLVM::Instr::Store {
                    src: (expr_ty.clone(), expr_val.clone()),
                    dest: (lval_ty.clone(), lval_reg.clone())
                });
            },

            Incr { lval } => {
                self.compile_incr_decr(&lval, ArithmOp::Add);
            }

            Decr { lval } => {
                self.compile_incr_decr(&lval, ArithmOp::Sub);
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

            ForEach { ty, ident, expr, block, .. } => {
                let (arr_ty, arr_val) = self.compile_expression(&expr);
                let (_len_ty, arr_len_reg) = self.compile_array_length(
                    (&arr_ty.clone(), &arr_val.clone())
                );

                let previous_block_label = self._current_block_label.clone();
                let conditional_block_label = self.next_label();
                let loop_block_label = self.next_label();
                let continue_block_label = self.next_label();

                self.add_instr(LLVM::Branch::Direct {
                    label: conditional_block_label.clone(),
                }.into());
                self.add_instr(conditional_block_label.clone().into());

                let idx_reg = self.next_register();
                let idx_inc_reg = self.next_register();
                self.add_instr(LLVM::Instr::Phi {
                    dest: (LLVM::Type::Int32, idx_reg.clone()),
                    preds: vec![
                        (LLVM::Const::from(0).into(), previous_block_label.clone()),
                        (idx_inc_reg.clone().into(), loop_block_label.clone()),
                    ],
                });

                self.add_instr(LLVM::Instr::Arithm {
                    dest: (LLVM::Type::Int32, idx_inc_reg.clone()),
                    op: ArithmOp::Add.into(),
                    val_lhs: idx_reg.clone().into(),
                    val_rhs: LLVM::Const::from(1).into(),
                });

                let cmp_reg = self.next_register();
                self.add_instr(LLVM::Instr::Compare {
                    dest_reg: cmp_reg.clone(),
                    op: RelOp::LT.into(),
                    ty: LLVM::Type::Int32,
                    val_lhs: idx_reg.clone().into(),
                    val_rhs: arr_len_reg.clone().into(),
                });

                self.add_instr(LLVM::Branch::Conditional {
                    ty: LLVM::Type::Int1,
                    val: cmp_reg.clone().into(),
                    true_label: loop_block_label.clone(),
                    false_label: continue_block_label.clone(),
                }.into());

                self.add_instr(loop_block_label.into());
                self.symbol_table.begin_scope();
                {
                    // read iterator value
                    let (entry_ty, entry_reg) = self.compile_array_at(
                        (&arr_ty.clone(), &arr_val.clone()),
                        &idx_reg.clone().into()
                    );
                    self.symbol_table.insert(
                        ident.clone(),
                        SymbolTableEntity::new(
                            arr_ty.deref_ptr().unwrap().clone(),
                            entry_reg.clone()
                        )
                    );

                    // process ForEach body
                    self.compile_block(&block);
                }
                self.symbol_table.end_scope();

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

    fn compile_lvalue(&mut self, lval: &ast::LValue) -> (LLVM::Type, LLVM::Register) {
        use ast::LValueTypes::*;

        match &lval.value {
            Var { ident, ident_loc: _ } => {
                let var = self.symbol_table.get(&ident).unwrap();
                (LLVM::Type::new_ptr(var.ty.clone()), var.reg.clone())
            },

            ArrAt { arr_expr, idx_expr } => {
                let (_idx_ty, idx_val) = self.compile_expression(&idx_expr);
                let (arr_ty, arr_ptr) = self.compile_expression(&arr_expr);

                let (entry_ty, entry_ptr_reg) = self.compile_array_at(
                    (&arr_ty.clone(), &arr_ptr.clone()),
                    &idx_val,
                );

                (arr_ty.clone(), entry_ptr_reg)
            },
        }
    }

    fn compile_incr_decr(&mut self, lval: &ast::LValue, op: ArithmOp) {
        let (var_ty, var_reg) = self.compile_lvalue(lval);

        let load_ty = var_ty.deref_ptr().unwrap();
        let load_reg = self.next_register();
        self.add_instr(LLVM::Instr::Load {
            dest: (load_ty.clone(), load_reg.clone()),
            src: (var_ty.clone(), var_reg.clone())
        });

        let incr_reg = self.next_register();
        self.add_instr(LLVM::Instr::Arithm {
            dest: (load_ty.clone(), incr_reg.clone()),
            op: op.into(),
            val_lhs: load_reg.clone().into(),
            val_rhs: LLVM::Const::from(1).into()
        });

        self.add_instr(LLVM::Instr::Store {
            src: (load_ty.clone(), incr_reg.into()),
            dest: (var_ty.clone(), var_reg.clone())
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

            ELValue { lval } => {
                let (lval_ty, lval_reg) = self.compile_lvalue(&lval);
                let load_reg = self.next_register();
                self.add_instr(LLVM::Instr::Load {
                    dest: (lval_ty.deref_ptr().unwrap(), load_reg.clone()),
                    src: (lval_ty.clone(), lval_reg.clone()),
                });
                (lval_ty.deref_ptr().unwrap(), load_reg.clone().into())
            },


            ELitInt { value } => {
                (LLVM::Type::Int32, LLVM::Const::from(value.parse::<i32>().unwrap()).into())
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
                    args: vec![
                        (LLVM::Type::Int32, LLVM::Const::from(0).into()),
                        (LLVM::Type::Int32, LLVM::Const::from(0).into()),
                    ],
                });

                (LLVM::Type::new_ptr(LLVM::Type::Int8), ret_reg.into())
            },


            ENew { ty, ty_loc: _, len_expr } => {
                let arr_ty: LLVM::Type = ty.into();
                let entry_ty = arr_ty.deref_ptr().unwrap();
                let (_, len_val) = self.compile_expression(&len_expr);

                let total_len_i32 = self.next_register();
                self.add_instr(LLVM::Instr::Arithm {
                    dest: (LLVM::Type::Int32, total_len_i32.clone()),
                    op: ArithmOp::Add.into(),
                    val_lhs: len_val.clone().into(),
                    val_rhs: LLVM::Const::from(LLVM::ARRAY_LENGTH_TYPE.byte_size()).into(),
                });

                // calloc requires i64 arguments
                let total_len_i64 = self.next_register();
                self.add_instr(LLVM::Instr::Sext {
                    dest: (LLVM::Type::Int64, total_len_i64.clone()),
                    src: (LLVM::Type::Int32, total_len_i32.clone().into()),
                });

                // create array
                let arr_ptr_i8 = self.next_register();
                self.add_instr(LLVM::Instr::Call {
                    dest_reg: Some(arr_ptr_i8.clone()),
                    ret_ty: LLVM::Type::new_ptr(LLVM::Type::Int8),
                    name: "calloc".to_string(),
                    args: vec![
                        (LLVM::Type::Int64, total_len_i64.clone().into()),
                        (LLVM::Type::Int64, entry_ty.byte_size().into()),
                    ],
                });

                let arr_ptr_i32 = self.next_register();
                self.add_instr(LLVM::Instr::Bitcast {
                    dest: (LLVM::Type::new_ptr(LLVM::Type::Int32), arr_ptr_i32.clone()),
                    src: (LLVM::Type::new_ptr(LLVM::Type::Int8), arr_ptr_i8.clone().into()),
                });

                // save length of an array
                // length field is at the beginning on the array so
                // `getelementptr` is redundant
                self.add_instr(LLVM::Instr::Store {
                   dest: (LLVM::Type::new_ptr(LLVM::Type::Int32), arr_ptr_i32.clone()),
                   src: (LLVM::Type::Int32, len_val.into()),
                });

                let arr_ptr = self.next_register();
                self.add_instr(LLVM::Instr::Bitcast {
                    dest: (arr_ty.clone(), arr_ptr.clone()),
                    src: (LLVM::Type::new_ptr(LLVM::Type::Int8), arr_ptr_i8.clone().into()),
                });


                (arr_ty.clone(), arr_ptr.into())
            },

            EArrLen { arr_expr } => {
                let (arr_ty, arr_ptr) = self.compile_expression(&arr_expr);
                let (len_ty, len_reg) = self.compile_array_length((&arr_ty, &arr_ptr.into()));

                (len_ty, len_reg.into())
            },
        }
    }

    fn compile_array_at(&mut self, (arr_ty, arr_ptr): (&LLVM::Type, &LLVM::Value), idx: &LLVM::Value) -> (LLVM::Type, LLVM::Register) {
            let arr_ptr_i8 = self.next_register();
            self.add_instr(LLVM::Instr::Bitcast {
                dest: (LLVM::Type::new_ptr(LLVM::DEFAULT_ARRAY_TYPE.clone()), arr_ptr_i8.clone()),
                src: (arr_ty.clone(), arr_ptr.clone()),
            });

            // skip length field
            let data_ptr_i8 = self.next_register();
            self.add_instr(LLVM::Instr::GetElementPtr {
                dest: (LLVM::DEFAULT_ARRAY_TYPE.clone(), data_ptr_i8.clone()),
                src: (LLVM::Type::new_ptr(LLVM::DEFAULT_ARRAY_TYPE.clone()), arr_ptr_i8.clone().into()),
                args: vec![
                    (LLVM::Type::Int32, LLVM::ARRAY_LENGTH_TYPE.byte_size().into()),
                ],
            });

            // array data pointer of proper type
            let data_ptr = self.next_register();
            self.add_instr(LLVM::Instr::Bitcast {
                dest: (arr_ty.clone(), data_ptr.clone()),
                src: (LLVM::Type::new_ptr(LLVM::DEFAULT_ARRAY_TYPE.clone()), data_ptr_i8.clone().into()),
            });

            let entry_ptr_reg = self.next_register();
            self.add_instr(LLVM::Instr::GetElementPtr {
                dest: (arr_ty.deref_ptr().unwrap(), entry_ptr_reg.clone()),
                src: (arr_ty.clone(), data_ptr.clone().into()),
                args: vec![
                    (LLVM::Type::Int32, idx.clone()),
                ],
            });

            (arr_ty.clone(), entry_ptr_reg)
    }

    fn compile_array_length(&mut self, (arr_ty, arr_ptr): (&LLVM::Type, &LLVM::Value)) -> (LLVM::Type, LLVM::Register) {
            let arr_ptr_i32 = self.next_register();
            self.add_instr(LLVM::Instr::Bitcast {
                dest: (LLVM::Type::new_ptr(LLVM::Type::Int32), arr_ptr_i32.clone()),
                src: (arr_ty.clone(), arr_ptr.clone().into()),
            });

            // length field is at the beginning on the array so
            // `getelementptr` is redundant
            let arr_len_reg = self.next_register();
            self.add_instr(LLVM::Instr::Load {
                dest: (LLVM::Type::Int32, arr_len_reg.clone()),
                src: (LLVM::Type::new_ptr(LLVM::Type::Int32), arr_ptr_i32.clone().into()),
            });

            (LLVM::Type::Int32, arr_len_reg.clone())
    }
}
