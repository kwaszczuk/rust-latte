use std::fmt;
// use std::collections::{LinkedList};
use crate::operators::{Operator};
use base::ast;

#[derive(Debug, PartialEq, Clone)]
pub struct Register {
    pub name: String
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Label {
    pub name: String
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int32,
    Int8,
    Int1,
    Ptr(Box<Type>),
    Void,
    Fun {
        ret_ty: Box<Type>,
    }
}

impl Type {
    pub fn default_value(ty: &Type) -> Value {
        use Type::*;
        match ty {
            Int32 => Const::from(0).into(),
            Int8 => Const::from(0).into(),
            Int1 => Const::False.into(),
            Ptr(_) => Const::Null.into(),
            Void => {
                panic!("no default value for void type");
            },
            Fun { ret_ty: _ } => {
                panic!("no default value for fun type");
            },
        }
    }
}

impl From<ast::Type> for Type {
    fn from(t: ast::Type) -> Self {
        use ast::Type::*;
        match t {
            Int => Type::Int32,
            Bool => Type::Int1,
            Void => Type::Void,
            Str => Type::Ptr(Box::new(Type::Int8)),
        }
    }
}

impl From<&ast::Type> for Type {
    fn from(t: &ast::Type) -> Self {
        Type::from(t.clone())
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;
        match self {
            Int32 => write!(f, "i32"),
            Int8 => write!(f, "i8"),
            Int1 => write!(f, "i1"),
            Ptr(t) => write!(f, "{}*", t),
            Void => write!(f, "void"),
            Fun { ret_ty: _ } => {
                panic!("no display implementation for fun type")
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Const {
    Int {
        value: i32,
    },
    True,
    False,
    Null,
}

impl From<i32> for Const {
    fn from(v: i32) -> Self {
        Const::Int { value: v }
    }
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Const::*;
        match self {
            Int { value } => write!(f, "{}", value),
            True => write!(f, "true"),
            False => write!(f, "false"),
            Null => {
                panic!("no Display implementation for Null type")
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Const(Const),
    Register(Register),
}

impl From<Register> for Value {
    fn from(r: Register) -> Self {
        Value::Register(r.clone())
    }
}

impl From<Const> for Value {
    fn from(c: Const) -> Self {
        Value::Const(c.clone())
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            Const(c) => write!(f, "{}", c),
            Register(r) => write!(f, "{}", r),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Branch {
    Direct {
        label: Label,
    },
    Conditional {
        ty: Type,
        val: Value,
        true_label: Label,
        false_label: Label,
    }
}

impl fmt::Display for Branch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Branch::*;
        match self {
            Direct { label } => write!(f, "br label {}", label),
            Conditional { ty, val, true_label, false_label } => write!(
                f,
                "br {} {}, label {}, label {}",
                ty, val, true_label, false_label
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Instr {
    Label {
        val: Label,
        preds: Vec<Label>,
    },
    Load {
        ty_dest: Type,
        reg_dest: Register,
        ty_src: Type,
        reg_src: Register,
    },
    Store {
        ty_src: Type,
        val_src: Value,
        ty_dest: Type,
        reg_dest: Register,
    },
    Alloc {
        reg_dest: Register,
        ty: Type,
    },
    Compare {
        reg_dest: Register,
        op: Operator,
        ty: Type,
        val_lhs: Value,
        val_rhs: Value,
    },
    Call {
        reg_dest: Option<Register>,
        ret_ty: Type,
        name: String,
        args: Vec<(Type, Value)>,
    },
    Branch(Branch),
    Arithm {
        reg_dest: Register,
        op: Operator,
        ty: Type,
        val_lhs: Value,
        val_rhs: Value,
    },
    Phi {
        reg_dest: Register,
        ty: Type,
        val1: Value,
        label1: Label,
        val2: Value,
        label2: Label,
    },
    Return {
        ty: Type,
        val: Value,
    },
    ReturnVoid
}

impl From<Branch> for Instr {
    fn from(b: Branch) -> Self {
        Instr::Branch(b.clone())
    }
}

impl From<Label> for Instr {
    fn from(l: Label) -> Self {
        Instr::Label {
            val: l.clone(),
            preds: vec![]
        }
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Instr::*;
        match self {
            Label { val, preds: _ } => write!(f, "{}:", val.name),
            Load { ty_dest, reg_dest, ty_src, reg_src } => write!(
                f, "{} = load {}, {} {}",
                reg_dest, ty_dest, ty_src, reg_src
            ),
            Store { ty_src, val_src, ty_dest, reg_dest } => write!(
                f, "store {} {}, {} {}",
                ty_src, val_src, ty_dest, reg_dest
            ),
            Alloc { reg_dest, ty } => write!(
                f, "{} = alloca {}",
                reg_dest, ty
            ),
            Compare { reg_dest, op, ty, val_lhs, val_rhs } => write!(
                f, "{} = icmp {} {} {}, {}",
                reg_dest, op, ty, val_lhs, val_rhs
            ),
            Call { reg_dest, ret_ty, name, args } => {
                let args_vec: Vec<String> = args.iter()
                    .map(|(ty, val)| format!("{} {}", ty, val))
                    .collect();
                let args_str = args_vec.join(", ");
                match reg_dest {
                    Some(reg) => write!(f, "{} = call {} @{}({})", reg, ret_ty, name, args_str),
                    None      => write!(f, "call {} @{}({})", ret_ty, name, args_str),
                }
            },
            Branch(b) => write!(f, "{}", b),
            Arithm { reg_dest, op, ty, val_lhs, val_rhs } => write!(
                f, "{} = {} {} {}, {}",
                reg_dest, op, ty, val_lhs, val_rhs
            ),
            Phi { reg_dest, ty, val1, label1, val2, label2 } => write!(
                f, "{} = phi {} [ {}, {} ],  [ {}, {} ]",
                reg_dest, ty, val1, label1, val2, label2
            ),
            Return { ty, val } => write!(f, "ret {} {}", ty, val),
            ReturnVoid => write!(f, "ret void"),
        }
    }
}

pub struct Function {
    pub ret_ty: Type,
    pub name: String,
    pub args: Vec<Type>,
    pub body: Vec<Instr>
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args: Vec<String> = self.args.iter().map(|ty| format!("{}", ty)).collect();
        let args_str = args.join(", ");
        let fun_template_begin = vec![
            format!("define {} @{}({}) {{", self.ret_ty, self.name, args_str)
        ];
        let fun_template_end = vec![
            format!("}}")
        ];
        let body_strs: Vec<String> = self.body
            .iter()
            .map(|instr| {
                match instr {
                    Instr::Label { val: _, preds: _ } => "\n".to_owned() + &instr.to_string(),
                    _ => "\t".to_owned() + &instr.to_string(),
                }
            })
            .collect();

        let mut code = vec![];
        code.extend(fun_template_begin);
        code.extend(body_strs);
        code.extend(fun_template_end);
        write!(f, "{}", code.join("\n"))
    }
}


pub struct Program {
    pub functions: Vec<Function>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let code: Vec<String> = self.functions.iter().map(|f| format!("{}", f)).collect();
        let code_str = code.join("\n\n");
        write!(f, "{}", code_str)
    }
}
