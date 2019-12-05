use std::fmt;

pub enum Operator {
    Arithm(ArithmOp),
}

pub enum ArithmOp {
    Add,
    Sub,
    Mul,
    Div
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Operator::*;
        match self {
            Arithm(a) => write!(f, "{}", a),
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
        }
    }
}
