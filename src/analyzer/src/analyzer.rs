use std::convert::From;
use std::collections::{HashMap};
use std::vec::Vec;

use base::types::Location;
use base::ast;
use base::symbol_table::{SymbolTable};
use crate::errors::CompilerError::*;
use crate::errors::TypeError::*;
use crate::errors::SemanticError::*;
use crate::errors::{CompilerError, ErrorHandler, ErrorHandling};
use crate::types::*;

pub struct SemanticAnalyzer {
    symbol_table: SymbolTable<SymbolTableEntity>,
    error_handler: ErrorHandler,
    current_function: Option<(String, FunType)>,
}

impl ErrorHandling for SemanticAnalyzer {
    fn throw(&mut self, err: CompilerError) {
        self.error_handler.throw(err);
    }

    fn has_error(&self) -> bool {
        self.error_handler.has_error()
    }

    fn get_errors(&self) -> Vec<CompilerError> {
        self.error_handler.get_errors()
    }
}

macro_rules! throw_if_undeclared {
    ( $self:expr, $ident:expr, $ident_loc:expr ) => {
        {
            match $self.symbol_table.get(&$ident) {
                None => {
                    $self.throw(SemanticError(UndeclaredVariable {
                        ident: $ident.clone(),
                        loc: $ident_loc.clone(),
                    }));
                    true
                },
                _ => false,
            }
        }
    };
}

macro_rules! throw_if_unmatched_type {
    ( $self:expr, $exp_ty:expr, $ty:expr, $loc:expr ) => {
        {
            let mut thrown = false;
            if $exp_ty != $ty {
                $self.throw(TypeError(InvalidType {
                    exp_ty: $exp_ty.clone(),
                    ty: $ty.clone(),
                    loc: $loc.clone(),
                }));
                thrown = true;
            }
            thrown
        }
    };
}

macro_rules! analyse_expr_and_match_type {
    ( $self:expr, $expr:expr, $exp_ty:expr, $loc:expr ) => {
        {
            if let Ok(ty) = $self.analyse_expression($expr) {
                throw_if_unmatched_type![$self, $exp_ty, ty, $loc];
            }
        }
    };
}

impl SemanticAnalyzer {
    fn new() -> Self {
        SemanticAnalyzer {
            symbol_table: SymbolTable::new(),
            error_handler: ErrorHandler::new(),
            current_function: None,
        }
    }

    pub fn run(ast_tree: &ast::Program) -> Option<Vec<CompilerError>> {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyse(ast_tree);
        match analyzer.has_error() {
            false => None,
            true => Some(analyzer.error_handler.get_errors()),
        }
    }

    fn init_globals(&mut self) {
        use VarType::*;
        use SimpleType::*;

        let global_fns = vec![
            ("printString", FunType { ret: Void, args: vec![Str], }),
            ("printInt",    FunType { ret: Void, args: vec![Int], }),
            ("error",       FunType { ret: Void, args: vec![], }),
            ("readInt",     FunType { ret: Int,  args: vec![], }),
            ("readString",  FunType { ret: Str,  args: vec![], }),
        ];

        for (fn_name, fn_ty) in &global_fns {
            self.symbol_table.insert(fn_name.to_string(), SymbolTableEntity {
                ty: Fun(fn_ty.clone()),
                ty_loc: Location::new(0, 0),
                ident_loc: Location::new(0, 0),
            });
        }
    }

    fn analyse(&mut self, ast_tree: &ast::Program) {
        self.init_globals();
        self.analyse_program(ast_tree)
    }

    fn analyse_program(&mut self, program: &ast::Program) {
        self.symbol_table.begin_scope();

        let mut good_fns = vec![];

        for fn_def in &program.defs {
            // check if variable got defined previously in current scope
            if let Some(ent) = self.symbol_table.get_from_current_scope(&fn_def.ident) {
               self.throw(SemanticError(VariableRedefiniton {
                   ident: fn_def.ident.clone(),
                   loc: fn_def.ident_loc.clone(),
                   prev_loc: ent.ident_loc.clone()
               }));
               continue;
            }

            self.analyse_function_definition(fn_def);

            let fn_ty = FunType {
                ret: SimpleType::from(&fn_def.ty),
                args: fn_def.args.iter().map(|a| SimpleType::from(&a.ty)).collect()
            };

            // add all functions to the scope (enable recursion with any function)
            self.symbol_table.insert(fn_def.ident.clone(), SymbolTableEntity::new(
                VarType::Fun(fn_ty.clone()),
                fn_def.ty_loc.clone(),
                fn_def.ident_loc.clone()
            ));
            good_fns.push((fn_def.clone(), fn_ty));
        }

        // check if `main` function exist
        match self.symbol_table.get(&"main".to_string()) {
            None => {
                self.throw(SemanticError(MissingEntrypoint));
            },
            Some(ent) => {
                if let VarType::Fun(fn_ty) = ent.ty {
                    if fn_ty.ret != SimpleType::Int {
                        self.throw(TypeError(MainReturnType {
                            exp_ty: VarType::Simple(SimpleType::Int),
                            ty: VarType::Simple(fn_ty.ret.clone()),
                            loc: ent.ty_loc.clone(),
                        }));
                    }
                    if fn_ty.args.len() != 0 {
                        self.throw(TypeError(MainArgumentsCount {
                            exp_cnt: 0,
                            cnt: fn_ty.args.len(),
                            loc: ent.ident_loc.clone(),
                        }));
                    }
                }
            },
        }

        for (fn_def, fn_ty) in &good_fns {
            self.symbol_table.begin_scope();
            // initialize function parameters, before analyzing the function body
            for arg in &fn_def.args {
                self.symbol_table.insert(
                    arg.ident.clone(),
                    SymbolTableEntity::new(
                        VarType::from(&arg.ty),
                        arg.ty_loc.clone(),
                        arg.ident_loc.clone()
                    )
                );
            }
            self.current_function = Some((fn_def.ident.clone(), fn_ty.clone()));
            self.analyse_block(&fn_def.block);
            if fn_ty.ret != SimpleType::Void {
                self.analyse_function_returns(&fn_def.block, true);
            }
            self.current_function = None;
            self.symbol_table.end_scope();
        }

        self.symbol_table.end_scope();
    }

    fn analyse_function_definition(&mut self, fn_def: &ast::FnDef) {
        let mut params = HashMap::new();
        for arg in &fn_def.args {
            // void variables are not supported
            if arg.ty == ast::Type::Void {
                self.throw(TypeError(VoidVariable {
                    loc: arg.ident_loc.clone()
                }));
            }

            // if variable wasn't redefined previously, add it to the scope
            match params.get(&arg.ident) {
                None => {
                    params.insert(arg.ident.clone(), arg.ident_loc.clone());
                }
                Some(prev_loc) => {
                    self.throw(SemanticError(VariableRedefiniton {
                        ident: arg.ident.clone(),
                        loc: arg.ident_loc.clone(),
                        prev_loc: prev_loc.clone()
                    }));
                }
            }
        }
    }

    fn analyse_block(&mut self, block: &ast::Block) {
        for stmt in &block.stmts {
            self.analyse_statement(stmt);
        }
    }

    fn analyse_function_returns(&mut self, block: &ast::Block, should_throw: bool) -> bool {
        use base::ast::StmtTypes::*;
        let (fn_name, _) = self.current_function.clone().unwrap();

        for stmt in &block.stmts {
            match &stmt.value {
                Ret { ret_loc, value } => {
                    return true
                },
                Cond { expr, block } => {
                    if let Some(true) = self.expr_is_true(&expr) {
                        return self.analyse_function_returns(&block, true)
                    }
                },
                CondElse { expr, block_true, block_false } => {
                    if let Some(c) = self.expr_is_true(&expr) {
                        if c == true {
                            return self.analyse_function_returns(&block_true, true)
                        } else {
                            return self.analyse_function_returns(&block_false, true)
                        }
                    } else {
                        let ret1 = self.analyse_function_returns(&block_true, false);
                        let ret2 = self.analyse_function_returns(&block_false, false);
                        if ret1 && ret2 {
                            return true
                        }
                    }
                },
                While { expr, block } => {
                    // TODO: check if in the infinite loop there's at least one return
                    if let Some(true) = self.expr_is_true(&expr) {
                        return true
                    }
                },
                _ => {},
            }
        }

        if should_throw {
            self.throw(SemanticError(MissingReturn {
                ident: fn_name.clone(),
                loc: block.end_loc.clone(),
            }));
        }
        return false
    }

    fn expr_is_true(&mut self, expr: &ast::Expr) -> Option<bool> {
        use base::ast::ExprTypes::*;

        match &expr.value {
            ELitTrue => Some(true),
            ELitFalse => Some(false),
            ENot { expr } => None,
            _ => None,
        }
    }

    fn analyse_statement(&mut self, stmt: &ast::Stmt) {
        use base::ast::StmtTypes::*;

        match &stmt.value {
            Empty => {},

            BStmt { block } => {
                self.symbol_table.begin_scope();
                self.analyse_block(&block);
                self.symbol_table.end_scope();
            },

            Decl { ty, ty_loc, items } => {
                let is_void_type = ty == &ast::Type::Void;

                for item in items {
                    // void variables are not supported
                    if is_void_type {
                        self.throw(TypeError(VoidVariable {
                            loc: item.ident_loc.clone()
                        }));
                        continue;
                    }

                    // check if variable got defined previously in current scope
                    if let Some(ent) = self.symbol_table.get_from_current_scope(&item.ident) {
                       self.throw(SemanticError(VariableRedefiniton {
                           ident: item.ident.clone(),
                           loc: item.ident_loc.clone(),
                           prev_loc: ent.ident_loc.clone()
                       }));
                       continue;
                    }

                    // add variable to the scope
                    let item_ty = VarType::from(ty);
                    self.symbol_table.insert(
                        item.ident.clone(),
                        SymbolTableEntity::new(
                            item_ty.clone(),
                            ty_loc.clone(),
                            item.ident_loc.clone()
                        )
                    );

                    // if variable has assigned value, check it's value type
                    match &item.value {
                        Some(expr) => {
                            analyse_expr_and_match_type![self, &expr, item_ty, expr.all_loc];
                        }
                        None => {}
                    }
                }
            },

            Ass { ident, ident_loc, expr } => {
                if throw_if_undeclared!(self, ident, ident_loc) { return; }
                let var = self.symbol_table.get(&ident).unwrap();

                analyse_expr_and_match_type![self, &expr, var.ty, expr.all_loc];
            },

            Incr { ident, ident_loc } |
            Decr { ident, ident_loc } => {
                if throw_if_undeclared!(self, ident, ident_loc) { return; }

                let var = self.symbol_table.get(&ident).unwrap();
                let exp_ty = VarType::Simple(SimpleType::Int);

                throw_if_unmatched_type!(self, exp_ty, var.ty, ident_loc);
            },

            Ret { ret_loc, value } => {
                let mut ret_ty;
                match &value {
                    Some(expr) => {
                        if let Ok(ty) = self.analyse_expression(&expr) {
                            ret_ty = ty;
                            if ret_ty == VarType::Simple(SimpleType::Void) {
                                self.throw(TypeError(ReturnVoidType {
                                    loc: expr.all_loc.clone()
                                }));
                                return
                            }
                        } else {
                            return
                        }
                    },
                    None => {
                        ret_ty = VarType::Simple(SimpleType::Void);
                    },
                }

                let (fn_name, fn_ty) = self.current_function.clone().unwrap();
                if VarType::Simple(fn_ty.ret.clone()) != ret_ty {
                    self.throw(TypeError(InvalidReturnType {
                        ident: fn_name.clone(),
                        exp_ty: VarType::Simple(fn_ty.ret.clone()),
                        ty: ret_ty.clone(),
                        loc: ret_loc.clone(),
                    }));
                }

            },

            Cond { expr, block } => {
                let cond_exp_ty = VarType::Simple(SimpleType::Bool);
                analyse_expr_and_match_type![self, &expr, cond_exp_ty, expr.all_loc];

                self.symbol_table.begin_scope();
                self.analyse_block(&block);
                self.symbol_table.end_scope();
            },

            CondElse { expr, block_true, block_false } => {
                let cond_exp_ty = VarType::Simple(SimpleType::Bool);
                analyse_expr_and_match_type![self, &expr, cond_exp_ty, expr.all_loc];

                self.symbol_table.begin_scope();
                self.analyse_block(&block_true);
                self.symbol_table.end_scope();

                self.symbol_table.begin_scope();
                self.analyse_block(&block_false);
                self.symbol_table.end_scope();
            },

            While { expr, block } => {
                let while_exp_ty = VarType::Simple(SimpleType::Bool);
                analyse_expr_and_match_type![self, &expr, while_exp_ty, expr.all_loc];

                self.symbol_table.begin_scope();
                self.analyse_block(&block);
                self.symbol_table.end_scope();
            },

            SExp { expr } => {
                self.analyse_expression(&expr);
            },
        }
    }

    fn analyse_expression(&mut self, expr: &ast::Expr) -> Result<VarType, ()> {
        use ast::ExprTypes::*;
        use VarType::*;
        use SimpleType::*;

        match &expr.value {
            EOr { expr1, expr2 } => {
                let exp_ty = Simple(Bool);
                analyse_expr_and_match_type![self, expr1, exp_ty, expr1.all_loc];
                analyse_expr_and_match_type![self, expr2, exp_ty, expr2.all_loc];
                Ok(exp_ty)
            },

            EAnd { expr1, expr2 } => {
                let exp_ty = Simple(Bool);
                analyse_expr_and_match_type![self, expr1, exp_ty, expr1.all_loc];
                analyse_expr_and_match_type![self, expr2, exp_ty, expr2.all_loc];
                Ok(exp_ty)
            },

            ERel { op, op_loc, expr1, expr2 } => {
                let expr1_res = self.analyse_expression(&expr1);
                let expr2_res = self.analyse_expression(&expr2);
                if let Ok(expr1_ty) = expr1_res {
                    if expr1_ty.supports_operator(op) {
                        if let Ok(expr2_ty) = expr2_res {
                            throw_if_unmatched_type![self, expr1_ty, expr2_ty, expr2.all_loc];
                        }
                    } else {
                        self.throw(TypeError(OperatorUnsupported {
                            op: op.clone(),
                            loc: op_loc.clone(),
                            ty: expr1_ty.clone(),
                        }));
                    }
                }
                Ok(Simple(Bool))
            },

            EAdd { op, op_loc, expr1, expr2 } => {
                let expr1_res = self.analyse_expression(&expr1);
                let expr2_res = self.analyse_expression(&expr2);
                if let Ok(expr1_ty) = expr1_res {
                    if expr1_ty.supports_operator(op) {
                        if let Ok(expr2_ty) = expr2_res {
                            throw_if_unmatched_type![self, expr1_ty, expr2_ty, expr2.all_loc];
                            return Ok(expr1_ty)
                        }
                    } else {
                        self.throw(TypeError(OperatorUnsupported {
                            op: op.clone(),
                            loc: op_loc.clone(),
                            ty: expr1_ty.clone(),
                        }));
                    }
                }
                Err(())
            },

            EMul { op, op_loc, expr1, expr2 } => {
                let expr1_res = self.analyse_expression(&expr1);
                let expr2_res = self.analyse_expression(&expr2);
                if let Ok(expr1_ty) = expr1_res {
                    if expr1_ty.supports_operator(op) {
                        if let Ok(expr2_ty) = expr2_res {
                            throw_if_unmatched_type![self, expr1_ty, expr2_ty, expr2.all_loc];
                            return Ok(expr1_ty)
                        }
                    } else {
                        self.throw(TypeError(OperatorUnsupported {
                            op: op.clone(),
                            loc: op_loc.clone(),
                            ty: expr1_ty.clone(),
                        }));
                    }
                }
                Err(())
            },

            ENeg { expr } => {
                let exp_ty = Simple(Int);
                if let Ok(ty) = self.analyse_expression(&expr) {
                    if throw_if_unmatched_type![self, exp_ty, ty, expr.all_loc] {
                        return Ok(exp_ty)
                    }
                }
                Err(())
            },

            ENot { expr } => {
                let exp_ty = Simple(Bool);
                if let Ok(ty) = self.analyse_expression(&expr) {
                    if throw_if_unmatched_type![self, exp_ty, ty, expr.all_loc] {
                        return Ok(exp_ty)
                    }
                }
                Err(())
            },

            EVar { ident, ident_loc } => {
                if throw_if_undeclared!(self, ident, ident_loc) {
                    return Err(())
                }
                Ok(self.symbol_table.get(&ident).unwrap().ty)
            },

            ELitInt { value: _ } => Ok(Simple(Int)),

            ELitTrue |
            ELitFalse => Ok(Simple(Bool)),

            EApp { ident, ident_loc, args, args_loc: _ } => {
                if throw_if_undeclared!(self, ident, ident_loc) {
                    return Err(())
                }

                // check whether it's a function identifier
                match self.symbol_table.get(&ident).unwrap().ty {
                    Fun(fn_ty) => {
                        // check if function got called with correct number of arguments
                        if args.len() != fn_ty.args.len() {
                           self.throw(SemanticError(FunctionArgumentsCount {
                               exp_cnt: fn_ty.args.len(),
                               cnt: args.len(),
                               ident: ident.clone(),
                               loc: ident_loc.clone(),
                           }));
                           Err(())
                        } else {
                            // check arguments' types
                            for (expr, exp_ty) in args.iter().zip(fn_ty.args) {
                                analyse_expr_and_match_type![self, expr, Simple(exp_ty.clone()), expr.all_loc];
                            }
                            Ok(Simple(fn_ty.ret))
                        }
                    },
                    t => {
                        self.throw(TypeError(NotAFunction {
                            ty: t,
                            loc: ident_loc.clone(),
                        }));
                        Err(())
                    }
                }
            },

            EString { value: _ } => Ok(Simple(Str)),

            _ => Err(())
        }
    }
}
