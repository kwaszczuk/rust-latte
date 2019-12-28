use base::ast;
use crate::errors::{CompilerError};

pub use crate::errors::{prettyprint_errors};

mod errors;
mod analyzer;
mod types;
mod evaluation;

pub fn run(program: &ast::Program) -> Option<Vec<CompilerError>> {
    match analyzer::SemanticAnalyzer::run(program) {
        Some(errors) => Some(errors),
        None => None
    }
}
