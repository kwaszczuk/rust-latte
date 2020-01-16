use std::fmt;

use crate::types::{Location, Located};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Str,
    Bool,
    Void,
    Array(Box<Type>),
}

pub type LValue = Located<LValueTypes>;
pub type ArrAtIdx = Located<Box<Expr>>;

#[derive(Debug, PartialEq, Clone)]
pub enum LValueTypes {
    Var {
        ident: String,
        ident_loc: Location
    },
    ArrAt {
        arr_expr: Box<Expr>,
        idx_exprs: Vec<ArrAtIdx>,
    },

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

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub end_loc: Location,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Item {
   pub ident: String,
   pub ident_loc: Location,
   pub value: Option<Expr>,
}

pub type Stmt = Located<StmtTypes>;

#[derive(Debug, PartialEq, Clone)]
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
        lval: LValue,
        expr: Expr,
    },
    Incr {
        lval: LValue,
    },
    Decr {
        lval: LValue,
    },
    Ret {
        ret_loc: Location,
        value: Option<Expr>,
    },
    Cond {
        expr: Expr,
        block: Block,
    },
    CondElse {
        expr: Expr,
        block_true: Block,
        block_false: Block,
    },
    While {
        expr: Expr,
        block: Block,
    },
    ForEach {
        ty: Type,
        ty_loc: Location,
        ident: String,
        ident_loc: Location,
        expr: Expr,
        block: Block,
    },
    SExp {
        expr: Expr,
    },
}

pub type Expr = Located<ExprTypes>;

#[derive(Debug, PartialEq, Clone)]
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
    },
    ENot {
        expr: Box<Expr>,
    },
    ELValue {
        lval: LValue,
    },
    ELitInt {
        value: String,
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
    ENew {
        ty: Type,
        ty_loc: Location,
        len_expr: Box<Expr>,
    },
    EArrLen {
        arr_expr: Box<Expr>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum ArithmOp {
    Plus,
    Minus,
    Times,
    Div,
    Mod
}

impl fmt::Display for ArithmOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ArithmOp::*;
        match self {
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Times => write!(f, "*"),
            Div => write!(f, "/"),
            Mod => write!(f, "%"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum RelOp {
    LT,
    LE,
    GT,
    GE,
    EQ,
    NE,
}

impl fmt::Display for RelOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use RelOp::*;
        match self {
            LT => write!(f, "<="),
            LE => write!(f, "<"),
            GT => write!(f, ">="),
            GE => write!(f, ">"),
            EQ => write!(f, "=="),
            NE => write!(f, "!="),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    RelOp(RelOp),
    ArithmOp(ArithmOp),
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Operator::*;
        match self {
            RelOp(op) => write!(f, "{}", op),
            ArithmOp(op) => write!(f, "{}", op),
        }
    }
}
