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

#[derive(Debug, PartialEq, Clone)]
pub struct FunType {
    pub ret: SimpleType,
    pub args: Vec<SimpleType>
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
    Fun(FunType),
}

impl VarType {
    pub fn supports_operator(&self, op: &ast::Operator) -> bool {
        match op {
            ast::Operator::RelOp(rel_op) => match self {
                VarType::Simple(SimpleType::Int)
              | VarType::Simple(SimpleType::Bool)
              | VarType::Simple(SimpleType::Str) => true,

                VarType::Simple(SimpleType::Void)
              | VarType::Fun(_) => false,
            },

            ast::Operator::MulOp(mul_op) => match mul_op {
                ast::MulOp::Times => match self {
                    VarType::Simple(SimpleType::Int)
                  | VarType::Simple(SimpleType::Bool) => true,

                    VarType::Simple(SimpleType::Str)
                  | VarType::Simple(SimpleType::Void)
                  | VarType::Fun(_) => false,
                },

                ast::MulOp::Div => match self {
                    VarType::Simple(SimpleType::Int) => true,

                    VarType::Simple(SimpleType::Bool)
                  | VarType::Simple(SimpleType::Str)
                  | VarType::Simple(SimpleType::Void)
                  | VarType::Fun(_) => false,
                },

                ast::MulOp::Mod => match self {
                    VarType::Simple(SimpleType::Int) => true,

                    VarType::Simple(SimpleType::Bool)
                  | VarType::Simple(SimpleType::Str)
                  | VarType::Simple(SimpleType::Void)
                  | VarType::Fun(_) => false,
                },
            },

            ast::Operator::AddOp(add_op) => match add_op {
                ast::AddOp::Plus => match self {
                    VarType::Simple(SimpleType::Int)
                  | VarType::Simple(SimpleType::Bool)
                  | VarType::Simple(SimpleType::Str)
                  | VarType::Simple(SimpleType::Void) => true,

                    VarType::Fun(_) => false,
                },

                ast::AddOp::Minus => match self {
                    VarType::Simple(SimpleType::Int)
                  | VarType::Simple(SimpleType::Bool) => true,

                    VarType::Simple(SimpleType::Str)
                  | VarType::Simple(SimpleType::Void)
                  | VarType::Fun(_) => false,
                },
            },
        }
    }
}

impl fmt::Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use VarType::*;
        match self {
            Simple(t) => write!(f, "{}", t),
            Fun(FunType { ret, args }) => {
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
