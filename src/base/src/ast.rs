use std::fmt::{Debug, Error, Formatter};

use crate::types::Location;

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


#[derive(Debug, PartialEq)]
pub enum Stmt {
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
        all_loc: Location,
    },
    Decr {
        ident: String,
        ident_loc: Location,
        all_loc: Location,
    },
    Ret {
        value: Option<ExprValue>,
        all_loc: Location,
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

#[derive(Debug, PartialEq)]
pub enum Expr {
    EOr {
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    EAnd {
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    ERel {
        op: RelOp,
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    EAdd {
        op: AddOp,
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    EMul {
        op: MulOp,
        expr1: Box<Expr>,
        expr2: Box<Expr>,
    },
    ENeg {
        expr: Box<Expr>,
    },
    ENot {
        expr: Box<Expr>,
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
