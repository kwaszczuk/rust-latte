use std::fmt;
use base::ast;

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Arithm(ArithmOp),
    Rel(RelOp),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ArithmOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod
}

#[derive(Debug, PartialEq, Clone)]
pub enum RelOp {
    EQ,
    NE,
    GT,
    GE,
    LT,
    LE
}

impl From<ast::Operator> for Operator {
    fn from(op: ast::Operator) -> Self {
        match op {
            ast::Operator::RelOp(ro) => match ro {
                ast::RelOp::EQ => RelOp::EQ.into(),
                ast::RelOp::NE => RelOp::NE.into(),
                ast::RelOp::GT => RelOp::GT.into(),
                ast::RelOp::GE => RelOp::GE.into(),
                ast::RelOp::LT => RelOp::LT.into(),
                ast::RelOp::LE => RelOp::LE.into(),
            },
            ast::Operator::AddOp(ao) => match ao {
                ast::AddOp::Plus => ArithmOp::Add.into(),
                ast::AddOp::Minus => ArithmOp::Sub.into(),
            },
            ast::Operator::MulOp(mo) => match mo {
                ast::MulOp::Times => ArithmOp::Mul.into(),
                ast::MulOp::Div => ArithmOp::Div.into(),
                ast::MulOp::Mod => ArithmOp::Mod.into(),
            },
        }
    }
}

impl From<ArithmOp> for Operator {
    fn from(op: ArithmOp) -> Self {
        Operator::Arithm(op)
    }
}

impl From<RelOp> for Operator {
    fn from(op: RelOp) -> Self {
        Operator::Rel(op)
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Operator::*;
        match self {
            Arithm(a) => write!(f, "{}", a),
            Rel(r) => write!(f, "{}", r),
        }
    }
}

impl fmt::Display for ArithmOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ArithmOp::*;
        match self {
            Add => write!(f, "add"),
            Sub => write!(f, "sub"),
            Mul => write!(f, "mul"),
            Div => write!(f, "sdiv"),
            Mod => write!(f, "srem"),
        }
    }
}

impl fmt::Display for RelOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RelOp::*;
        match self {
            EQ => write!(f, "eq"),
            NE => write!(f, "ne"),
            GT => write!(f, "sgt"),
            GE => write!(f, "sge"),
            LT => write!(f, "slt"),
            LE => write!(f, "sle"),
        }
    }
}
