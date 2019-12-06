use std::convert::From;
use std::collections::{HashMap};
use std::vec::Vec;

use base::ast;
use base::symbol_table::{SymbolTable};
use crate::errors::CompilerError::*;
use crate::errors::TypeError::*;
use crate::errors::SemanticError::*;
use crate::errors::{CompilerError, ErrorHandler, ErrorHandling};
use crate::types::*;

pub struct SemanticAnalyzer {
    symbol_table: SymbolTable<SymbolTableEntity>,
    error_handler: ErrorHandler
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
            match $self.analyse_expression($expr) {
                Ok(ty) => throw_if_unmatched_type![$self, $exp_ty, ty, $loc],
                Err(_) => false
            }
        }
    };
}

impl SemanticAnalyzer {
    fn new() -> Self {
        SemanticAnalyzer {
            symbol_table: SymbolTable::new(),
            error_handler: ErrorHandler::new()
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

    fn analyse(&mut self, ast_tree: &ast::Program) {
        self.analyse_program(ast_tree)
    }

    fn analyse_program(&mut self, program: &ast::Program) {
        self.symbol_table.begin_scope();

        for fn_def in &program.defs {
            self.analyse_function_definition(fn_def);

            // add all functions to the scope (enable recursion with any function)
            self.symbol_table.insert(fn_def.ident.clone(), SymbolTableEntity::new(
                VarType::Fun {
                    ret: SimpleType::from(&fn_def.ty),
                    args: fn_def.args.iter().map(|a| SimpleType::from(&a.ty)).collect()
                },
                fn_def.ty_loc.clone(),
                fn_def.ident_loc.clone()
            ));
        }

        // check if `main` function exist
        match self.symbol_table.get(&"main".to_string()) {
            None => {
                self.throw(SemanticError(MissingEntrypoint));
            }
            _ => {}
        }

        for fn_def in &program.defs {
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
            self.analyse_block(&fn_def.block);
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

    fn analyse_statement(&mut self, stmt: &ast::Stmt) {
        use base::ast::Stmt::*;
        match stmt {
            Empty => {},
            BStmt { block } => {
                self.symbol_table.begin_scope();
                self.analyse_block(block);
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
                    match self.symbol_table.get_from_current_scope(&item.ident) {
                        Some(ent) => {
                           self.throw(SemanticError(VariableRedefiniton {
                               ident: item.ident.clone(),
                               loc: item.ident_loc.clone(),
                               prev_loc: ent.ident_loc.clone()
                           }));
                           continue;
                        },
                        None => {}
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
                        Some(item_value) => {
                            analyse_expr_and_match_type![self, &item_value.expr, item_ty, item_value.expr_loc];
                        }
                        None => {}
                    }
                }
            },
            Ass { ident, ident_loc, expr, expr_loc } => {
                if throw_if_undeclared!(self, ident, ident_loc) { return; }
                let var = self.symbol_table.get(&ident).unwrap();

                analyse_expr_and_match_type![self, expr, var.ty, expr_loc];
            },
            Incr { ident, ident_loc, all_loc } |
            Decr { ident, ident_loc, all_loc } => {
                if throw_if_undeclared!(self, ident, ident_loc) { return; }

                let var = self.symbol_table.get(&ident).unwrap();
                let exp_ty = VarType::Simple(SimpleType::Int);

                throw_if_unmatched_type!(self, exp_ty, var.ty, ident_loc);
            },
            Ret { value, all_loc } => {
            }
            Cond { expr, expr_loc, stmt } => {
                let cond_exp_ty = VarType::Simple(SimpleType::Bool);
                analyse_expr_and_match_type![self, expr, cond_exp_ty, expr_loc];

                self.symbol_table.begin_scope();
                self.analyse_statement(stmt);
                self.symbol_table.end_scope();
            },
            CondElse { expr, expr_loc, stmt_true, stmt_false } => {
                let cond_exp_ty = VarType::Simple(SimpleType::Bool);
                analyse_expr_and_match_type![self, expr, cond_exp_ty, expr_loc];

                self.symbol_table.begin_scope();
                self.analyse_statement(stmt_true);
                self.symbol_table.end_scope();

                self.symbol_table.begin_scope();
                self.analyse_statement(stmt_false);
                self.symbol_table.end_scope();
            },
            While { expr, expr_loc, stmt } => {
                let while_exp_ty = VarType::Simple(SimpleType::Bool);
                analyse_expr_and_match_type![self, expr, while_exp_ty, expr_loc];

                self.symbol_table.begin_scope();
                self.analyse_statement(stmt);
                self.symbol_table.end_scope();
            },
            SExp { expr } => {
                self.analyse_expression(expr);
            }
            _ => {}
        }
    }

    fn analyse_expression(&mut self, expr: &ast::Expr) -> Result<VarType, ()> {
        use ast::Expr::*;
        use VarType::*;
        use SimpleType::*;
        match expr {
            ELitInt { value } => Ok(Simple(Int)),
            ELitTrue => Ok(Simple(Bool)),
            ELitFalse => Ok(Simple(Bool)),
            EString { value } => Ok(Simple(Str)),
            _ => Err(())
        }
    }
}
