use std::fmt;
use std::collections::{HashMap};
use crate::operators::{Operator};
use crate::utils::{escape_string, length_after_escape};
use base::ast;
use base::types;

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Register {
    pub prefix: String,
    pub counter: usize,
    pub name: String,
}

impl Register {
    pub fn new(prefix: String, counter: usize) -> Self {
        Register {
            prefix: prefix.clone(),
            counter,
            name: format!("{}{}", prefix.clone(), counter),
        }
    }
}

impl base::types::Labeled for Register {
    fn new(prefix: String, counter: usize) -> Self {
        Register::new(prefix, counter)
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.name)
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Label {
    pub prefix: String,
    pub counter: usize,
    pub name: String,
}

impl Label {
    pub fn new(prefix: String, counter: usize) -> Self {
        Label {
            prefix: prefix.clone(),
            counter,
            name: format!("{}{}", prefix.clone(), counter),
        }
    }

    pub fn is_entry(&self) -> bool {
        self.prefix.as_str() == ""
    }
}

impl base::types::Labeled for Label {
    fn new(prefix: String, counter: usize) -> Self {
        Label::new(prefix, counter)
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.name)
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Static {
    pub prefix: String,
    pub counter: usize,
    pub name: String,
}

impl Static {
    pub fn new(prefix: String, counter: usize) -> Self {
        Static {
            prefix: prefix.clone(),
            counter,
            name: format!("{}{}", prefix.clone(), counter),
        }
    }
}

impl base::types::Labeled for Static {
    fn new(prefix: String, counter: usize) -> Self {
        Static::new(prefix, counter)
    }
}

impl fmt::Display for Static {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int32,
    Int8,
    Int1,
    Ptr(Box<Type>),
    Array {
        ty: Box<Type>,
        len: usize,
    },
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
            Array { ty: _, len: _ } => {
                panic!("no default value for array type");
            },
        }
    }

    pub fn new_ptr(ty: Type) -> Type {
        Type::Ptr(Box::new(ty))
    }

    pub fn new_array(ty: Type, len: usize) -> Type {
        Type::Array { ty: Box::new(ty), len }
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
            Array { ty, len } => write!(f, "[ {} x {} ]", len, ty),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Const {
    Int(i32),
    True,
    False,
    Null,
}

impl From<bool> for Const {
    fn from(b: bool) -> Self {
        match b {
            false => Const::False,
            true => Const::True,
        }
    }
}

impl From<i32> for Const {
    fn from(v: i32) -> Self {
        Const::Int(v)
    }
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Const::*;
        match self {
            Int(value) => write!(f, "{}", value),
            True => write!(f, "true"),
            False => write!(f, "false"),
            Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Value {
    Const(Const),
    Register(Register),
    Static(Static),
}

impl From<Static> for Value {
    fn from(s: Static) -> Self {
        Value::Static(s)
    }
}

impl From<Register> for Value {
    fn from(r: Register) -> Self {
        Value::Register(r)
    }
}

impl From<Const> for Value {
    fn from(c: Const) -> Self {
        Value::Const(c)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            Const(c) => write!(f, "{}", c),
            Register(r) => write!(f, "{}", r),
            Static(s) => write!(f, "{}", s),
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
        dest: (Type, Register),
        src: (Type, Register),
    },
    Store {
        src: (Type, Value),
        dest: (Type, Register),
    },
    Alloc {
        dest: (Type, Register),
    },
    Compare {
        dest_reg: Register,
        op: Operator,
        ty: Type,
        val_lhs: Value,
        val_rhs: Value,
    },
    Call {
        dest_reg: Option<Register>,
        ret_ty: Type,
        name: String,
        args: Vec<(Type, Value)>,
    },
    Branch(Branch),
    Arithm {
        dest: (Type, Register),
        op: Operator,
        val_lhs: Value,
        val_rhs: Value,
    },
    Phi {
        dest: (Type, Register),
        preds: Vec<(Value, Label)>
    },
    GetElementPtr {
        dest: (Type, Register),
        src: (Type, Value),
        idx1: (Type, Value),
        idx2: (Type, Value),
    },
    Return {
        ty: Type,
        val: Value,
    },
    ReturnVoid,
    Unreachable
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
            Load { src, dest } => write!(
                f, "{} = load {}, {} {}",
                dest.1, dest.0, src.0, src.1
            ),
            Store { src, dest } => write!(
                f, "store {} {}, {} {}",
                src.0, src.1, dest.0, dest.1
            ),
            Alloc { dest } => write!(
                f, "{} = alloca {}",
                dest.1, dest.0
            ),
            Compare { dest_reg, op, ty, val_lhs, val_rhs } => write!(
                f, "{} = icmp {} {} {}, {}",
                dest_reg, op, ty, val_lhs, val_rhs
            ),
            Call { dest_reg, ret_ty, name, args } => {
                let args_vec: Vec<String> = args.iter()
                    .map(|(ty, val)| format!("{} {}", ty, val))
                    .collect();
                let args_str = args_vec.join(", ");
                match dest_reg {
                    Some(reg) => write!(f, "{} = call {} @{}({})", reg, ret_ty, name, args_str),
                    None      => write!(f, "call {} @{}({})", ret_ty, name, args_str),
                }
            },
            Branch(b) => write!(f, "{}", b),
            Arithm { dest, op, val_lhs, val_rhs } => write!(
                f, "{} = {} {} {}, {}",
                dest.1, op, dest.0, val_lhs, val_rhs
            ),
            Phi { dest, preds } => {
                let preds_str: Vec<String> = preds
                    .iter()
                    .map(|(val, lab)| format!("[ {}, {} ]", val, lab))
                    .collect();
                write!(
                    f, "{} = phi {} {}",
                    dest.1, dest.0, preds_str.join(", ")
                )
            },
            GetElementPtr { dest, src, idx1, idx2 } => {
                write!(
                    f, "{} = getelementptr {}, {} {}, {} {}, {} {}",
                    dest.1, dest.0, src.0, src.1, idx1.0, idx1.1, idx2.0, idx2.1
                )
            },
            Return { ty, val } => write!(f, "ret {} {}", ty, val),
            ReturnVoid => write!(f, "ret void"),
            Unreachable => write!(f, "unreachable"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub label: Label,
    pub instrs: Vec<Instr>,
}

impl Block {
    pub fn is_entry(&self) -> bool {
        self.label.is_entry()
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let instrs_strs: Vec<String> = self.instrs
            .iter()
            .map(|i| "\t".to_owned() + &i.to_string())
            .collect();
        match self.label.is_entry() {
            true => write!(f, "{}", instrs_strs.join("\n")),
            false => write!(f, "{}:\n{}", self.label.name, instrs_strs.join("\n")),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub ret_ty: Type,
    pub name: String,
    pub args: Vec<Type>,
    pub body: Vec<Block>
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
        let body_strs: Vec<String> = self.body.iter().map(|b| b.to_string()).collect();

        let mut code = vec![];
        code.extend(fun_template_begin);
        code.extend(body_strs);
        code.extend(fun_template_end);
        write!(f, "{}", code.join("\n"))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub options: HashMap<String, bool>,
    pub declares: Vec<Function>,
    pub statics: Vec<(Static, String)>,
    pub functions: Vec<Function>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut options: Vec<String> = self.options.iter().map(|(k, v)| {
            format!(";     {}: {}", k, v)
        }).collect();
        options.sort();
        options.insert(0, "; compilation options:".to_string());

        let declares: Vec<String> = self.declares.iter().map(|f| {
            let args: Vec<String> = f.args.iter().map(|a| a.to_string()).collect();
            let args_str = args.join(", ");
            format!(
                "declare {} @{}({})",
                f.ret_ty, f.name, args_str
            )
        }).collect();

        let statics: Vec<String> = self.statics.iter().map(|(static_, str_)| {
            let escaped_str = escape_string(str_.clone());
            let ty = Type::new_array(Type::Int8, length_after_escape(str_.clone()));
            format!(
                "{} = constant {} c\"{}\", align 1",
                static_, ty, escaped_str
            )
        }).collect();

        let functions: Vec<String> = self.functions.iter().map(|f| format!("{}", f)).collect();
        write!(
            f, "{}\n\n{}\n\n{}\n\n{}",
            options.join("\n"),
            declares.join("\n"),
            statics.join("\n"),
            functions.join("\n\n"),
        )
    }
}
