use base::ast;

pub enum Value {
    Int(i32),
    Bool(bool),
    Str(String),
}

pub fn eval_bool_expr(expr: &ast::Expr) -> Option<bool> {
    let val = eval_expr(&expr);
    match val {
        Some(Value::Bool(v)) => Some(v),
        _ => None,
    }
}

fn eval_expr(expr: &ast::Expr) -> Option<Value> {
    use ast::ExprTypes::*;

    match &expr.value {
        EOr { expr1, expr2 } => {
            let val1 = eval_expr(&expr1);
            let val2 = eval_expr(&expr2);
            match (val1, val2) {
                (Some(Value::Bool(b1)), Some(Value::Bool(b2))) => {
                    Some(Value::Bool(b1 | b2))
                }
                _ => None
            }
        },

        EAnd { expr1, expr2 } => {
            let val1 = eval_expr(&expr1);
            let val2 = eval_expr(&expr2);
            match (val1, val2) {
                (Some(Value::Bool(b1)), Some(Value::Bool(b2))) => {
                    Some(Value::Bool(b1 & b2))
                }
                _ => None
            }
        },

        ERel { op, op_loc: _, expr1, expr2 } |
        EAdd { op, op_loc: _, expr1, expr2 } |
        EMul { op, op_loc: _, expr1, expr2 } => {
            let val1 = eval_expr(&expr1);
            let val2 = eval_expr(&expr2);
            match (val1, val2) {
                (Some(v1), Some(v2)) => {
                    eval_operator(&op, v1, v2)
                }
                _ => None,
            }
        },

        ENeg { expr } => {
            let val = eval_expr(&expr);
            match val{
                Some(Value::Int(v)) => Some(Value::Int(-v)),
                _ => None,
            }
        },

        ENot { expr } => {
            let val = eval_expr(&expr);
            match val {
                Some(Value::Bool(v)) => Some(Value::Bool(!v)),
                _ => None,
            }
        },

        ELValue { lval: _ } => None,

        ELitInt { value } => Some(Value::Int(value.parse::<i32>().unwrap())),

        ELitTrue => Some(Value::Bool(true)),

        ELitFalse => Some(Value::Bool(false)),

        EApp { ident: _, ident_loc: _, args: _, args_loc: _} => None,

        ENew { ty: _, ty_loc: _, len_expr: _ } => None,

        EArrLen { arr_expr: _ } => None,

        EString { value } => Some(Value::Str(value.clone())),
    }
}

fn eval_operator(op: &ast::Operator, val1: Value, val2: Value) -> Option<Value> {
    use ast::Operator::*;
    use ast::RelOp::*;
    use ast::ArithmOp::*;
    match (val1, val2) {
        (Value::Int(v1), Value::Int(v2)) => {
            match op {
               RelOp(LT) => Some(Value::Bool(v1 < v2)),
               RelOp(LE) => Some(Value::Bool(v1 <= v2)),
               RelOp(GT) => Some(Value::Bool(v1 > v2)),
               RelOp(GE) => Some(Value::Bool(v1 >= v2)),
               RelOp(EQ) => Some(Value::Bool(v1 == v2)),
               RelOp(NE) => Some(Value::Bool(v1 != v2)),
               ArithmOp(Plus) => Some(Value::Int(v1 + v2)),
               ArithmOp(Minus) => Some(Value::Int(v1 - v2)),
               ArithmOp(Times) => Some(Value::Int(v1 * v2)),
               ArithmOp(Div) => Some(Value::Int(v1 / v2)),
               ArithmOp(Mod) => Some(Value::Int(v1 % v2)),
            }
        }

        (Value::Bool(v1), Value::Bool(v2)) => {
            match op {
               RelOp(EQ) => Some(Value::Bool(v1 == v2)),
               RelOp(NE) => Some(Value::Bool(v1 != v2)),
               RelOp(LT) |
               RelOp(LE) |
               RelOp(GT) |
               RelOp(GE) |
               ArithmOp(Plus) |
               ArithmOp(Minus) |
               ArithmOp(Times) |
               ArithmOp(Div) |
               ArithmOp(Mod) => None,
            }
        }

        (Value::Str(v1), Value::Str(v2)) => {
            match op {
               RelOp(LT) => Some(Value::Bool(v1 < v2)),
               RelOp(LE) => Some(Value::Bool(v1 <= v2)),
               RelOp(GT) => Some(Value::Bool(v1 > v2)),
               RelOp(GE) => Some(Value::Bool(v1 >= v2)),
               RelOp(EQ) => Some(Value::Bool(v1 == v2)),
               RelOp(NE) => Some(Value::Bool(v1 != v2)),
               ArithmOp(Plus) => Some(Value::Str(format!("{}{}", v1, v2))),
               ArithmOp(Minus) |
               ArithmOp(Times) |
               ArithmOp(Div) |
               ArithmOp(Mod) => None,
            }
        }

        (_, _) => None,
    }
}
