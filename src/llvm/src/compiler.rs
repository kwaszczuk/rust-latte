use crate::instructions as LLVM;
use crate::operators::{Operator, ArithmOp, RelOp};
use base::ast;
use base::symbol_table::{SymbolTable};

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
    _instrs: Vec<LLVM::Instr>,
    _current_block_label: LLVM::Label,
}

impl LLVMCompiler {
    fn new() -> Self {
        LLVMCompiler {
            symbol_table: SymbolTable::new(),
            _label_generator: Labeler::new("l_".to_string()),
            _register_generator: Labeler::new("v_".to_string()),
            _instrs: vec![],
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

    fn clean_instrs(&mut self) {
        self._instrs = vec![];
    }

    fn get_instrs(&mut self) -> Vec<LLVM::Instr> {
        let instrs = self._instrs.to_vec();
        instrs
    }

    fn add_instr(&mut self, instr: LLVM::Instr) {
        self._instrs.push(instr.clone());
        match instr {
            LLVM::Instr::Label { val, preds: _ } => {
                self._current_block_label = val.clone();
            },
            _ => {}
        }
    }

    pub fn run(ast_tree: &ast::Program) -> LLVM::Program {
        let mut compiler = LLVMCompiler::new();
        compiler.compile(ast_tree)
    }

    fn init_globals(&mut self) {
        let global_fns = vec![
            ("printString", LLVM::Type::Void),
            ("printInt",    LLVM::Type::Void),
            ("error",       LLVM::Type::Void),
            ("readInt",     LLVM::Type::Int32),
            ("readString",  LLVM::Type::Ptr(Box::new(LLVM::Type::Int8))),
        ];

        for (fn_name, fn_ret_ty) in &global_fns {
            self.symbol_table.insert(
                fn_name.to_string(),
                SymbolTableEntity::new(
                    LLVM::Type::Fun { ret_ty: Box::new(fn_ret_ty.clone()) },
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
            functions: functions,
        }
    }

    fn compile_function(&mut self, fn_def: &ast::FnDef) -> LLVM::Function {
        self.symbol_table.begin_scope();
        self._label_generator.reset();
        self._register_generator.reset();
        self.clean_instrs();

        for (i, a) in fn_def.args.iter().enumerate() {
            let arg_reg = self.next_register();
            let arg_ty = LLVM::Type::from(&a.ty);
            self.symbol_table.insert(
                a.ident.clone(),
                SymbolTableEntity::new(
                    arg_ty.clone(),
                    arg_reg.clone(),
                    
                )
            );
            self.add_instr(LLVM::Instr::Alloc {
                reg_dest: arg_reg.clone(),
                ty: arg_ty.clone(),
            });
            self.add_instr(LLVM::Instr::Store {
                ty_src: arg_ty.clone(),
                val_src: (LLVM::Register { name: format!("{}", i) }).into(),
                ty_dest: LLVM::Type::Ptr(Box::new(arg_ty.clone())),
                reg_dest: arg_reg.clone()
            });
        }

        self.compile_block(&fn_def.block);
        self.symbol_table.end_scope();

        LLVM::Function {
            ret_ty: LLVM::Type::from(&fn_def.ty),
            name: fn_def.ident.clone(),
            args: fn_def.args.iter().map(|a| LLVM::Type::from(&a.ty)).collect(),
            body: self.get_instrs(),
        }
    }

    fn compile_block(&mut self, block: &ast::Block) {
        for stmt in &block.stmts {
            self.compile_statement(&stmt);
        }
    }

    fn compile_statement(&mut self, stmt: &ast::Stmt) {
        use base::ast::StmtTypes::*;

        match &stmt.value {
            Empty => {},

            BStmt { block } => {
                self.symbol_table.begin_scope();
                self.compile_block(&block);
                self.symbol_table.end_scope();
            },

            Decl { ty, ty_loc: _, items } => {
                for item in items {
                    // add variable to the scope
                    let item_reg = self.next_register();
                    let item_ty = LLVM::Type::from(ty.clone());
                    self.symbol_table.insert(
                        item.ident.clone(),
                        SymbolTableEntity::new(
                            item_ty.clone(),
                            item_reg.clone()
                        )
                    );
                    // allocate it
                    self.add_instr(LLVM::Instr::Alloc {
                        reg_dest: item_reg.clone(),
                        ty: item_ty.clone(),
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
                    self.add_instr(LLVM::Instr::Store {
                        ty_src: item_ty.clone(),
                        val_src: initial_value.clone(),
                        ty_dest: LLVM::Type::Ptr(Box::new(item_ty.clone())),
                        reg_dest: item_reg.clone()
                    });
                }
            },

            Ass { ident, ident_loc: _, expr } => {
                let var = self.symbol_table.get(&ident).unwrap();
                let (expr_ty, expr_val) = self.compile_expression(&expr);
                self.add_instr(LLVM::Instr::Store {
                    ty_src: expr_ty.clone(),
                    val_src: expr_val.clone(),
                    ty_dest: LLVM::Type::Ptr(Box::new(var.ty.clone())),
                    reg_dest: var.reg.clone()
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
                self.add_instr(false_block_label.into());
                self.compile_block(&block_false);
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
            ty_dest: var.ty.clone(),
            reg_dest: load_reg.clone(),
            ty_src: LLVM::Type::Ptr(Box::new(var.ty.clone())),
            reg_src: var.reg.clone()
        });

        let incr_reg = self.next_register();
        self.add_instr(LLVM::Instr::Arithm {
            reg_dest: incr_reg.clone(),
            op: op.into(),
            ty: var.ty.clone(),
            val_lhs: load_reg.into(),
            val_rhs: LLVM::Const::from(1).into()
        });

        self.add_instr(LLVM::Instr::Store {
            ty_src: var.ty.clone(),
            val_src: incr_reg.into(),
            ty_dest: LLVM::Type::Ptr(Box::new(var.ty.clone())),
            reg_dest: var.reg.clone()
        });
    }

    fn compile_expression(&mut self, expr: &ast::Expr) -> (LLVM::Type, LLVM::Value) {
        use ast::ExprTypes::*;
        match &expr.value {

            EOr { expr1, expr2 } => {
                let (_ty1, val1) = self.compile_expression(&expr1);
                let current_block_label = self._current_block_label.clone();
                let expr2_block_label = self.next_label();
                let continue_block_label = self.next_label();

                let cmp_reg = self.next_register();
                self.add_instr(LLVM::Instr::Compare {
                    reg_dest: cmp_reg.clone(),
                    op: RelOp::EQ.into(),
                    ty: LLVM::Type::Int1,
                    val_lhs: val1,
                    val_rhs: LLVM::Const::True.into(),
                });
                self.add_instr(LLVM::Branch::Conditional {
                    ty: LLVM::Type::Int1,
                    val: cmp_reg.clone().into(),
                    true_label: continue_block_label.clone(),
                    false_label: expr2_block_label.clone(),
                }.into());

                self.add_instr(expr2_block_label.clone().into());
                let (_ty2, val2) = self.compile_expression(&expr2);
                self.add_instr(LLVM::Branch::Direct {
                    label: continue_block_label.clone(),
                }.into());

                self.add_instr(continue_block_label.into());
                let ret_reg = self.next_register();
                self.add_instr(LLVM::Instr::Phi {
                    reg_dest: ret_reg.clone(),
                    ty: LLVM::Type::Int1,
                    val1: LLVM::Const::True.into(),
                    label1: current_block_label,
                    val2: val2.clone(),
                    label2: expr2_block_label.clone(),
                });

                (LLVM::Type::Int1, ret_reg.clone().into())
            },

            EAnd { expr1, expr2 } => {
                let (_ty1, val1) = self.compile_expression(&expr1);
                let current_block_label = self._current_block_label.clone();
                let expr2_block_label = self.next_label();
                let continue_block_label = self.next_label();

                let cmp_reg = self.next_register();
                self.add_instr(LLVM::Instr::Compare {
                    reg_dest: cmp_reg.clone(),
                    op: RelOp::EQ.into(),
                    ty: LLVM::Type::Int1,
                    val_lhs: val1,
                    val_rhs: LLVM::Const::False.into(),
                });
                self.add_instr(LLVM::Branch::Conditional {
                    ty: LLVM::Type::Int1,
                    val: cmp_reg.clone().into(),
                    true_label: continue_block_label.clone(),
                    false_label: expr2_block_label.clone(),
                }.into());

                self.add_instr(expr2_block_label.clone().into());
                let (_ty2, val2) = self.compile_expression(&expr2);
                self.add_instr(LLVM::Branch::Direct {
                    label: continue_block_label.clone(),
                }.into());

                self.add_instr(continue_block_label.into());
                let ret_reg = self.next_register();
                self.add_instr(LLVM::Instr::Phi {
                    reg_dest: ret_reg.clone(),
                    ty: LLVM::Type::Int1,
                    val1: LLVM::Const::False.into(),
                    label1: current_block_label,
                    val2: val2.clone(),
                    label2: expr2_block_label.clone(),
                });

                (LLVM::Type::Int1, ret_reg.clone().into())
            },

            ERel { op, op_loc: _, expr1, expr2 } => {
                let (ty1, val1) = self.compile_expression(&expr1);
                let (_ty2, val2) = self.compile_expression(&expr2);

                let ret_reg = self.next_register();
                self.add_instr(LLVM::Instr::Compare {
                    reg_dest: ret_reg.clone(),
                    op: Operator::from(op.clone()),
                    ty: ty1,
                    val_lhs: val1,
                    val_rhs: val2,
                });
                (LLVM::Type::Int1, ret_reg.clone().into())
            },

            EAdd { op, op_loc: _, expr1, expr2 } |
            EMul { op, op_loc: _, expr1, expr2 } => {
                let (ty1, val1) = self.compile_expression(&expr1);
                let (_ty2, val2) = self.compile_expression(&expr2);

                let ret_reg = self.next_register();
                self.add_instr(LLVM::Instr::Arithm {
                    reg_dest: ret_reg.clone(),
                    op: Operator::from(op.clone()),
                    ty: ty1,
                    val_lhs: val1,
                    val_rhs: val2,
                });
                (LLVM::Type::Int32, ret_reg.clone().into())
            },

            ENeg { expr } => {
                let (ty, val) = self.compile_expression(&expr);

                let ret_reg = self.next_register();
                self.add_instr(LLVM::Instr::Compare {
                    reg_dest: ret_reg.clone(),
                    op: ArithmOp::Sub.into(),
                    ty: ty.clone(),
                    val_lhs: LLVM::Const::from(0).into(),
                    val_rhs: val,
                });
                (ty.clone(), ret_reg.clone().into())
            },

            ENot { expr } => {
                let (ty, val) = self.compile_expression(&expr);

                let ret_reg = self.next_register();
                self.add_instr(LLVM::Instr::Compare {
                    reg_dest: ret_reg.clone(),
                    op: RelOp::EQ.into(),
                    ty: ty.clone(),
                    val_lhs: val,
                    val_rhs: LLVM::Const::False.into(),
                });
                (ty.clone(), ret_reg.clone().into())
            },

            EVar { ident, ident_loc: _ } => {
                let var = self.symbol_table.get(&ident).unwrap();
                let load_reg = self.next_register();
                self.add_instr(LLVM::Instr::Load {
                    ty_dest: var.ty.clone(),
                    reg_dest: load_reg.clone(),
                    ty_src: LLVM::Type::Ptr(Box::new(var.ty.clone())),
                    reg_src: var.reg.clone()
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
                            reg_dest: None,
                            ret_ty: *ret_ty.clone(),
                            name: ident.clone(),
                            args: args_expr,
                        });
                        (LLVM::Type::Void, LLVM::Const::Null.into())
                    } else {
                        self.add_instr(LLVM::Instr::Call {
                            reg_dest: Some(ret_reg.clone()),
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
                (LLVM::Type::Int32, LLVM::Const::from(1).into())
            },
        }
    }
}
