use std::fmt;
use llvm::operators as LLVM;

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

impl From<LLVM::Operator> for Operator {
    fn from(op: LLVM::Operator) -> Self {
        match op {
            LLVM::Operator::Rel(ro) => match ro {
                LLVM::RelOp::EQ => RelOp::EQ.into(),
                LLVM::RelOp::NE => RelOp::NE.into(),
                LLVM::RelOp::GT => RelOp::GT.into(),
                LLVM::RelOp::GE => RelOp::GE.into(),
                LLVM::RelOp::LT => RelOp::LT.into(),
                LLVM::RelOp::LE => RelOp::LE.into(),
            },
            LLVM::Operator::Arithm(ao) => match ao {
                LLVM::ArithmOp::Add => ArithmOp::Add.into(),
                LLVM::ArithmOp::Sub => ArithmOp::Sub.into(),
                LLVM::ArithmOp::Mul => ArithmOp::Mul.into(),
                LLVM::ArithmOp::Div => ArithmOp::Div.into(),
                LLVM::ArithmOp::Mod => { panic!("explicit modulo operator not supported") },
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
            Mul => write!(f, "imul"),
            Div => write!(f, "idiv"),
        }
    }
}

impl fmt::Display for RelOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RelOp::*;
        match self {
            EQ => write!(f, "e"),
            NE => write!(f, "ne"),
            GT => write!(f, "g"),
            GE => write!(f, "ge"),
            LT => write!(f, "l"),
            LE => write!(f, "le"),
        }
    }
}
