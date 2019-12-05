use std::convert::From;
use std::collections::{HashMap};
use std::vec::Vec;

use base::ast;
use base::symbol_table::{SymbolTable};
use base::types::{Location};
use crate::errors::CompilerError::*;
use crate::errors::TypeError::*;
use crate::errors::SemanticError::*;
use crate::errors::{CompilerError, ErrorHandler, ErrorHandling};

enum SimpleType {
    Int,
    Str,
    Bool,
    Void
}

enum Type {
    Simple(SimpleType),
    Fun {
        ret: SimpleType,
        args: Vec<SimpleType>
    }
}

// impl From<ast::Type> for SimpleType {
//     fn from(v: ast::Type) -> Self {
//         match v {
//             ast::Type::Int => SimpleType::Int,
//             ast::Type::Str => SimpleType::Str,
//             ast::Type::Bool => SimpleType::Bool,
//             ast::Type::Void => SimpleType::Void,
//         }
//     }
// }

impl From<&ast::Type> for SimpleType {
    fn from(v: &ast::Type) -> Self {
        match v {
            ast::Type::Int => SimpleType::Int,
            ast::Type::Str => SimpleType::Str,
            ast::Type::Bool => SimpleType::Bool,
            ast::Type::Void => SimpleType::Void,
        }
    }
}

impl From<&ast::Type> for Type {
    fn from(v: &ast::Type) -> Self {
        match v {
            ast::Type::Int => Type::Simple(SimpleType::Int),
            ast::Type::Str => Type::Simple(SimpleType::Str),
            ast::Type::Bool => Type::Simple(SimpleType::Bool),
            ast::Type::Void => Type::Simple(SimpleType::Void),
        }
    }
}

pub struct SymbolTableEntity {
    ty: Type,
    ty_loc: Location,
    ident_loc: Location,
}

impl SymbolTableEntity {
    fn new(ty: Type, ty_loc: Location, ident_loc: Location) -> Self {
        SymbolTableEntity {
            ty,
            ty_loc,
            ident_loc,
        }
    }
}

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
        self.symbol_table.start_scope();

        for fn_def in &program.defs {
            self.analyse_function_definition(fn_def);

            // enable recursion with any function
            self.symbol_table.insert(fn_def.ident.clone(), SymbolTableEntity::new(
                Type::Fun {
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
            self.symbol_table.start_scope();
            // initialize function parameters
            for arg in &fn_def.args {
                self.symbol_table.insert(
                    arg.ident.clone(),
                    SymbolTableEntity::new(
                        Type::from(&arg.ty),
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
           if arg.ty == ast::Type::Void {
               self.throw(TypeError(FunctionVoidArgument {
                   loc: arg.ty_loc.clone()
               }));
           }
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
    }
}
