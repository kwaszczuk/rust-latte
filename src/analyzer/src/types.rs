use std::fmt;

use base::ast;
use base::types::Location;

#[derive(Debug, PartialEq, Clone)]
pub enum SimpleType {
    Int,
    Str,
    Bool,
    Void
}

impl fmt::Display for SimpleType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SimpleType::*;
        match self {
            Int => write!(f, "int"),
            Str => write!(f, "string"),
            Bool => write!(f, "boolean"),
            Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum VarType {
    Simple(SimpleType),
    Fun {
        ret: SimpleType,
        args: Vec<SimpleType>
    }
}

impl fmt::Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use VarType::*;
        match self {
            Simple(t) => write!(f, "{}", t),
            Fun { ret, args } => {
                let args_vec_str: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                let args_str = args_vec_str.join(", ");
                write!(f, "({}) -> {}", args_str, ret)
            }
        }
    }
}

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

impl From<&ast::Type> for VarType {
    fn from(v: &ast::Type) -> Self {
        match v {
            ast::Type::Int => VarType::Simple(SimpleType::Int),
            ast::Type::Str => VarType::Simple(SimpleType::Str),
            ast::Type::Bool => VarType::Simple(SimpleType::Bool),
            ast::Type::Void => VarType::Simple(SimpleType::Void),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SymbolTableEntity {
    pub ty: VarType,
    pub ty_loc: Location,
    pub ident_loc: Location,
}

impl SymbolTableEntity {
    pub fn new(ty: VarType, ty_loc: Location, ident_loc: Location) -> Self {
        SymbolTableEntity {
            ty,
            ty_loc,
            ident_loc,
        }
    }
}
