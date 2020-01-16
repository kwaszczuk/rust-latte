use std::fmt;

use base::ast;
use base::types::Location;

#[derive(Debug, PartialEq, Clone)]
pub enum SimpleType {
    Int,
    Str,
    Bool,
    Void,
    Array(Box<SimpleType>),
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
            Array(t) => write!(f, "{}[]", t),
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
              | VarType::Simple(SimpleType::Bool) => true,

                VarType::Simple(SimpleType::Str) => match rel_op {
                    ast::RelOp::EQ
                  | ast::RelOp::NE => true,

                    _ => false
                },

                VarType::Simple(SimpleType::Void)
              | VarType::Simple(SimpleType::Array(_))
              | VarType::Fun(_) => false,
            },

            ast::Operator::ArithmOp(mul_op) => match mul_op {
                ast::ArithmOp::Times => match self {
                    VarType::Simple(SimpleType::Int) => true,

                    VarType::Simple(SimpleType::Bool)
                  | VarType::Simple(SimpleType::Str)
                  | VarType::Simple(SimpleType::Void)
                  | VarType::Simple(SimpleType::Array(_))
                  | VarType::Fun(_) => false,
                },

                ast::ArithmOp::Div => match self {
                    VarType::Simple(SimpleType::Int) => true,

                    VarType::Simple(SimpleType::Bool)
                  | VarType::Simple(SimpleType::Str)
                  | VarType::Simple(SimpleType::Void)
                  | VarType::Simple(SimpleType::Array(_))
                  | VarType::Fun(_) => false,
                },

                ast::ArithmOp::Mod => match self {
                    VarType::Simple(SimpleType::Int) => true,

                    VarType::Simple(SimpleType::Bool)
                  | VarType::Simple(SimpleType::Str)
                  | VarType::Simple(SimpleType::Void)
                  | VarType::Simple(SimpleType::Array(_))
                  | VarType::Fun(_) => false,
                },

                ast::ArithmOp::Plus => match self {
                    VarType::Simple(SimpleType::Int)
                  | VarType::Simple(SimpleType::Str) => true,

                    VarType::Simple(SimpleType::Void)
                  | VarType::Simple(SimpleType::Bool)
                  | VarType::Simple(SimpleType::Array(_))
                  | VarType::Fun(_) => false,
                },

                ast::ArithmOp::Minus => match self {
                    VarType::Simple(SimpleType::Int) => true,

                    VarType::Simple(SimpleType::Bool)
                  | VarType::Simple(SimpleType::Str)
                  | VarType::Simple(SimpleType::Void)
                  | VarType::Simple(SimpleType::Array(_))
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
            ast::Type::Array(t) => SimpleType::Array(Box::new(SimpleType::from(&*t.clone()))),
        }
    }
}

impl From<&ast::Type> for VarType {
    fn from(v: &ast::Type) -> Self {
        VarType::Simple(SimpleType::from(v))
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
