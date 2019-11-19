use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum CompileError {
    General {
        msg: String
    },
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use CompileError::*;
        match &self {
            General { msg } => write!(f, "{}", msg),
        }
    }
}
