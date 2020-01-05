use std::fmt;
use std::collections::{HashMap};
use crate::operators::{Operator, ArithmOp};
use llvm::utils::{unescape_string};
use llvm::instructions as LLVM;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Byte,
    Word,
    Double,
    Quad,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;
        match self {
            Byte => write!(f, "b"),
            Word => write!(f, "w"),
            Double => write!(f, "l"),
            Quad => write!(f, "q"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Register {
    RAX,
    RBX,
    RCX,
    RDX,
    RBP,
    RSP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Register::*;
        match self {
            RAX => write!(f, "%rax"),
            RBX => write!(f, "%rbx"),
            RCX => write!(f, "%rcx"),
            RDX => write!(f, "%rdx"),
            RBP => write!(f, "%rbp"),
            RSP => write!(f, "%rsp"),
            RSI => write!(f, "%rsi"),
            RDI => write!(f, "%rdi"),
            R8  => write!(f, "%r8"),
            R9  => write!(f, "%r9"),
            R10 => write!(f, "%r10"),
            R11 => write!(f, "%r11"),
            R12 => write!(f, "%r12"),
            R13 => write!(f, "%r13"),
            R14 => write!(f, "%r14"),
            R15 => write!(f, "%r15"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Memory {
    pub offset: i32,
}

impl fmt::Display for Memory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.offset, Register::RBP)
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Storage {
    Register(Register),
    Memory(Memory),
}

impl From<Register> for Storage {
    fn from(r: Register) -> Self {
        Storage::Register(r)
    }
}

impl From<Memory> for Storage {
    fn from(m: Memory) -> Self {
        Storage::Memory(m)
    }
}

impl fmt::Display for Storage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Storage::Register(r) => write!(f, "{}", r),
            Storage::Memory(m) => write!(f, "{}", m),
        }
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
        write!(f, ".L{}", self.name)
    }
}

impl From<LLVM::Label> for Label {
    fn from(s: LLVM::Label) -> Self {
        Label {
            prefix: s.prefix,
            counter: s.counter,
            name: s.name,
        }
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

impl From<LLVM::Static> for Static {
    fn from(s: LLVM::Static) -> Self {
        Static {
            prefix: s.prefix,
            counter: s.counter,
            name: s.name,
        }
    }
}

impl fmt::Display for Static {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ".{}", self.name)
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Value {
    Const(i32),
    Storage(Storage),
    Static(Static),
}

impl From<Static> for Value {
    fn from(s: Static) -> Self {
        Value::Static(s)
    }
}

impl From<Register> for Value {
    fn from(r: Register) -> Self {
        Value::Storage(r.into())
    }
}

impl From<Storage> for Value {
    fn from(s: Storage) -> Self {
        Value::Storage(s)
    }
}

impl From<i32> for Value {
    fn from(c: i32) -> Self {
        Value::Const(c)
    }
}

impl From<LLVM::Const> for Value {
    fn from(c: LLVM::Const) -> Self {
        match c {
            LLVM::Const::Int(v) => Value::Const(v),
            LLVM::Const::False => Value::Const(0),
            LLVM::Const::True => Value::Const(1),
            LLVM::Const::Null => Value::Const(0),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            Const(c) => write!(f, "${}", c),
            Storage(s) => write!(f, "{}", s),
            Static(s) => write!(f, "{}", s),
        }
    }
}

pub struct TypedValue {
    ty: Type,
    val: Value,
}

impl TypedValue {
    pub fn new(ty: &Type, val: &Value) -> Self {
        TypedValue {
            ty: ty.clone(),
            val: val.clone(),
        }
    }
}

impl fmt::Display for TypedValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Register::*;
        if let Value::Storage(Storage::Register(r)) = self.val.clone() {
            match self.ty {
                Type::Byte => match r {
                    RAX => write!(f, "%al"),
                    RBX => write!(f, "%bl"),
                    RCX => write!(f, "%cl"),
                    RDX => write!(f, "%dl"),
                    RBP => write!(f, "%bpl"),
                    RSP => write!(f, "%spl"),
                    RSI => write!(f, "%sil"),
                    RDI => write!(f, "%dil"),
                    R8  => write!(f, "%r8b"),
                    R9  => write!(f, "%r9b"),
                    R10 => write!(f, "%r10b"),
                    R11 => write!(f, "%r11b"),
                    R12 => write!(f, "%r12b"),
                    R13 => write!(f, "%r13b"),
                    R14 => write!(f, "%r14b"),
                    R15 => write!(f, "%r15b"),
                },

                Type::Word => match r {
                    RAX => write!(f, "%ax"),
                    RBX => write!(f, "%bx"),
                    RCX => write!(f, "%cx"),
                    RDX => write!(f, "%dx"),
                    RBP => write!(f, "%bp"),
                    RSP => write!(f, "%sp"),
                    RSI => write!(f, "%si"),
                    RDI => write!(f, "%di"),
                    R8  => write!(f, "%r8w"),
                    R9  => write!(f, "%r9w"),
                    R10 => write!(f, "%r10w"),
                    R11 => write!(f, "%r11w"),
                    R12 => write!(f, "%r12w"),
                    R13 => write!(f, "%r13w"),
                    R14 => write!(f, "%r14w"),
                    R15 => write!(f, "%r15w"),
                },

                Type::Double => match r {
                    RAX => write!(f, "%eax"),
                    RBX => write!(f, "%ebx"),
                    RCX => write!(f, "%ecx"),
                    RDX => write!(f, "%edx"),
                    RBP => write!(f, "%ebp"),
                    RSP => write!(f, "%esp"),
                    RSI => write!(f, "%esi"),
                    RDI => write!(f, "%edi"),
                    R8  => write!(f, "%r8d"),
                    R9  => write!(f, "%r9d"),
                    R10 => write!(f, "%r10d"),
                    R11 => write!(f, "%r11d"),
                    R12 => write!(f, "%r12d"),
                    R13 => write!(f, "%r13d"),
                    R14 => write!(f, "%r14d"),
                    R15 => write!(f, "%r15d"),
                },

                Type::Quad => match r {
                    RAX => write!(f, "%rax"),
                    RBX => write!(f, "%rbx"),
                    RCX => write!(f, "%rcx"),
                    RDX => write!(f, "%rdx"),
                    RBP => write!(f, "%rbp"),
                    RSP => write!(f, "%rsp"),
                    RSI => write!(f, "%rsi"),
                    RDI => write!(f, "%rdi"),
                    R8  => write!(f, "%r8"),
                    R9  => write!(f, "%r9"),
                    R10 => write!(f, "%r10"),
                    R11 => write!(f, "%r11"),
                    R12 => write!(f, "%r12"),
                    R13 => write!(f, "%r13"),
                    R14 => write!(f, "%r14"),
                    R15 => write!(f, "%r15"),
                },
            }
        } else {
            write!(f, "{}", self.val)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Jump {
    Direct {
        dest: Label,
    },
    Conditional {
        op: Operator,
        dest: Label,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Instr {
    Xor {
        ty: Type,
        lhs: Value,
        rhs: Value,
    },
    Move {
        src: Value,
        dest: Storage,
        ty: Type,
    },
    Lea {
        src: Value,
        dest: Storage,
        ty: Type,
    },
    Pop {
        dest: Storage,
        ty: Type,
    },
    Push {
        src: Value,
        ty: Type,
    },
    Compare {
        lhs: Value,
        rhs: Value,
        ty: Type,
    },
    Set {
        op: Operator,
        dest: Storage,
    },
    Call {
        name: String,
    },
    Jump(Jump),
    Arithm {
        op: Operator,
        src: Value,
        dest: Storage,
        ty: Type,
    },
    Return,
    NoOp,
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Xor { ty, lhs, rhs } => write!(f, "xor{} {}, {}", ty, lhs, rhs),
            Instr::Move { ty, dest, src } => write!(f, "mov{} {}, {}", ty, src, dest),
            Instr::Lea { ty, dest, src } => write!(f, "lea{} {}, {}", ty, src, dest),
            Instr::Pop { ty, dest } => write!(f, "pop{} {}", ty, dest),
            Instr::Push { ty, src } => write!(f, "push{} {}", ty, src),
            Instr::Compare { ty, lhs, rhs } => write!(
                f, "cmp{} {}, {}",
                ty, TypedValue::new(ty, lhs), TypedValue::new(ty, rhs)
            ),
            Instr::Set { op, dest } => {
                match op {
                    Operator::Rel(o) => write!(f, "set{} {}", o, dest),
                    _ => { panic!("set only supports relation operator") }
                }
            },
            Instr::Call { name } => write!(f, "call {}", name),
            Instr::Jump(j) => {
                match j {
                    Jump::Direct { dest } => write!(f, "jmp {}", dest),
                    Jump::Conditional { op, dest } => write!(f, "j{} {}", op, dest),
                }
            },
            Instr::Arithm { ty, op, src, dest } => {
                match op {
                    Operator::Arithm(ArithmOp::Div) => write!(f, "{}{} {}", op, ty, src),
                    _ => write!(f, "{}{} {}, {}", op, ty, src, dest),
                }
            }
            Instr::Return => write!(f, "ret"),
            Instr::NoOp => write!(f, "nop"),
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
            false => write!(f, "{}:\n{}", self.label, instrs_strs.join("\n")),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: String,
    pub body: Vec<Block>
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fun_template_begin = vec![
            format!("{}:", self.name)
        ];
        let body_strs: Vec<String> = self.body.iter().map(|b| b.to_string()).collect();

        let mut code = vec![];
        code.extend(fun_template_begin);
        code.extend(body_strs);
        write!(f, "{}", code.join("\n"))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub options: HashMap<String, bool>,
    pub statics: Vec<(Static, String)>,
    pub functions: Vec<Function>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut options: Vec<String> = self.options.iter().map(|(k, v)| {
            format!("#     {}: {}", k, v)
        }).collect();
        options.sort();
        options.insert(0, "# compilation options:".to_string());

        let init = vec![
            format!("\t.text"),
            format!("\t.global main"),
            format!("\t.type main, @function"),
        ];

        let statics: Vec<String> = self.statics.iter().map(|(name, value)| {
            let escaped_str = unescape_string(value.clone());
            format!("{}:\n\t.string \"{}\"", name, value)
        }).collect();

        let functions: Vec<String> = self.functions.iter().map(|f| format!("{}", f)).collect();
        write!(
            f, "{}\n\n{}\n\n{}\n\n{}",
            options.join("\n"),
            init.join("\n"),
            statics.join("\n"),
            functions.join("\n\n"),
        )
    }
}
