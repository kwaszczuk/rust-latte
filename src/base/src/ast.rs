use std::fmt::{Debug, Error, Formatter};

pub type Location = usize;

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
    pub loc: Location,
    pub ty: Type,
    pub ident: String,
    pub args: Vec<Arg>,
    pub block: Block,
}

#[derive(Debug, PartialEq)]
pub struct Arg {
    pub loc: Location,
    pub ty: Type,
    pub ident: String,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub loc: Location,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq)]
pub enum Item {
    NoInit {
        loc: Location,
        ident: String,
    },
    Init {
        loc: Location,
        ident: String,
        expr: Expr,
    }
}


#[derive(Debug, PartialEq)]
pub enum Stmt {
    Empty,
    BStmt {
        loc: Location,
        block: Block,
    },
    Decl {
        loc: Location,
        ty: Type,
        items: Vec<Item>,
    },
    Ass {
        loc: Location,
        ident: String,
        expr: Expr,
    },
    Incr {
        loc: Location,
        ident: String,
    },
    Decr {
        loc: Location,
        ident: String,
    },
    Ret {
        loc: Location,
        expr: Expr,
    },
    VRet {
        loc: Location,
    },
    Cond {
        loc: Location,
        expr: Expr,
        stmt: Box<Stmt>,
    },
    CondElse {
        loc: Location,
        expr: Expr,
        stmt_true: Box<Stmt>,
        stmt_false: Box<Stmt>,
    },
    While {
        loc: Location,
        expr: Expr,
        stmt: Box<Stmt>,
    },
    SExp {
        loc: Location,
        expr: Expr,
    },
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    EOr {
        loc: Location,
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    EAnd {
        loc: Location,
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    ERel {
        loc: Location,
        op: RelOp,
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    EAdd {
        loc: Location,
        op: AddOp,
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    EMul {
        loc: Location,
        op: MulOp,
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    ENeg {
        loc: Location,
        expr: Box<Expr>,
    },
    ENot {
        loc: Location,
        expr: Box<Expr>,
    },
    EVar {
        loc: Location,
        ident: String,
    },
    ELitInt {
        loc: Location,
        value: i32,
    },
    ELitTrue {
        loc: Location,
    },
    ELitFalse {
        loc: Location,
    },
    EApp {
        loc: Location,
        ident: String,
        args: Vec<Expr>,
    },
    EString {
        loc: Location,
        value: String,
    },
}

#[derive(Debug, PartialEq)]
pub enum AddOp {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq)]
pub enum MulOp {
    Times,
    Div,
    Mod,
}

#[derive(Debug, PartialEq)]
pub enum RelOp {
    LTH,
    LE,
    GTH,
    GE,
    EQU,
    NE,
}
