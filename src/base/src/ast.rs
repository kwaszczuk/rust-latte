use std::fmt;

use crate::types::{Location, Located};

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Str,
    Bool,
    Void,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub defs: Vec<FnDef>,
}

#[derive(Debug, PartialEq)]
pub struct FnDef {
    pub ty: Type,
    pub ty_loc: Location,
    pub ident: String,
    pub ident_loc: Location,
    pub args: Vec<Arg>,
    pub block: Block,
}

#[derive(Debug, PartialEq)]
pub struct Arg {
    pub ty: Type,
    pub ty_loc: Location,
    pub ident: String,
    pub ident_loc: Location,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct Item {
   pub ident: String,
   pub ident_loc: Location,
   pub value: Option<ExprValue>,
}

#[derive(Debug, PartialEq)]
pub struct ExprValue {
   pub expr: Expr,
   pub expr_loc: Location,
}


pub type Stmt = Located<StmtTypes>;
#[derive(Debug, PartialEq)]
pub enum StmtTypes {
    Empty,
    BStmt {
        block: Block,
    },
    Decl {
        ty: Type,
        ty_loc: Location,
        items: Vec<Item>,
    },
    Ass {
        ident: String,
        ident_loc: Location,
        expr: Expr,
        expr_loc: Location,
    },
    Incr {
        ident: String,
        ident_loc: Location,
    },
    Decr {
        ident: String,
        ident_loc: Location,
    },
    Ret {
        value: Option<ExprValue>,
    },
    Cond {
        expr: Expr,
        expr_loc: Location,
        stmt: Box<Stmt>,
    },
    CondElse {
        expr: Expr,
        expr_loc: Location,
        stmt_true: Box<Stmt>,
        stmt_false: Box<Stmt>,
    },
    While {
        expr: Expr,
        expr_loc: Location,
        stmt: Box<Stmt>,
    },
    SExp {
        expr: Expr,
    },
}

pub type Expr = Located<ExprTypes>;
#[derive(Debug, PartialEq)]
pub enum ExprTypes {
    EOr {
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    EAnd {
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    ERel {
        op: Operator,
        op_loc: Location,
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    EAdd {
        op: Operator,
        op_loc: Location,
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    EMul {
        op: Operator,
        op_loc: Location,
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    ENeg {
        expr: Box<Expr>,
        expr_loc: Location,
    },
    ENot {
        expr: Box<Expr>,
        expr_loc: Location,
    },
    EVar {
        ident: String,
        ident_loc: Location,
    },
    ELitInt {
        value: i32,
    },
    ELitTrue,
    ELitFalse,
    EApp {
        ident: String,
        ident_loc: Location,
        args: Vec<Expr>,
        args_loc: Location,
    },
    EString {
        value: String,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum AddOp {
    Plus,
    Minus,
}

impl fmt::Display for AddOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AddOp::*;
        match self {
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum MulOp {
    Times,
    Div,
    Mod,
}

impl fmt::Display for MulOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use MulOp::*;
        match self {
            Times => write!(f, "*"),
            Div => write!(f, "/"),
            Mod => write!(f, "%"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum RelOp {
    LTH,
    LE,
    GTH,
    GE,
    EQU,
    NE,
}

impl fmt::Display for RelOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use RelOp::*;
        match self {
            LTH => write!(f, "<="),
            LE => write!(f, "<"),
            GTH => write!(f, ">="),
            GE => write!(f, ">"),
            EQU => write!(f, "=="),
            NE => write!(f, "!="),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    RelOp(RelOp),
    AddOp(AddOp),
    MulOp(MulOp),
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Operator::*;
        match self {
            RelOp(op) => write!(f, "{}", op),
            AddOp(op) => write!(f, "{}", op),
            MulOp(op) => write!(f, "{}", op),
        }
    }
}
