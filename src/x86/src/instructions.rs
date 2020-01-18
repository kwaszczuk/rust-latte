use std::fmt;
use std::collections::{HashMap};
use crate::operators::{Operator, ArithmOp};
use llvm::utils::{unescape_string};
use llvm::instructions as LLVM;

pub static DEFAULT_WORD_SIZE: i32 = 8;
pub static DEFAULT_TYPE: Type = Type::Quad;

pub static DEFAULT_ARGS_OFFSET: i32 = 16;

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Type {
    Byte,
    Word,
    Double,
    Quad,
}

impl From<LLVM::Type> for Type {
    fn from(ty: LLVM::Type) -> Self {
        match ty.byte_size() {
            1 => Type::Byte,
            2 => Type::Word,
            4 => Type::Double,
            8 => Type::Quad,
            _ => { panic!("should not happen") },
        }
    }
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
pub struct TypedRegister {
    pub ty: Type,
    pub reg: Register,
}

impl TypedRegister {
    pub fn new(ty: Type, reg: Register) -> Self {
        TypedRegister { ty, reg }
    }

    pub fn default(reg: Register) -> Self {
        TypedRegister::new(DEFAULT_TYPE.clone(), reg)
    }
}

impl fmt::Display for TypedRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Register::*;
        match self.ty {
            Type::Byte => match self.reg {
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

            Type::Word => match self.reg {
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

            Type::Double => match self.reg {
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

            Type::Quad => match self.reg {
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
pub enum Storage {
    Register(TypedRegister),
    Memory(Memory),
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Offset {
    Register(TypedRegister),
    Const(i32),
}

impl fmt::Display for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Offset::Register(r) => write!(f, "{}", r),
            Offset::Const(c) => write!(f, "{}", c),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Memory {
    pub base: TypedRegister,
    pub offset_base: Option<Offset>,
    pub offset_mul: Option<Offset>,
}

impl From<TypedRegister> for Memory {
    fn from(r: TypedRegister) -> Self {
        Memory {
            base: r,
            offset_base: None,
            offset_mul: None,
        }
    }
}

impl Storage {
    pub fn new_stack_memory(offset: i32) -> Self {
        Storage::Memory(Memory {
            base: TypedRegister::default(Register::RBP),
            offset_base: Some(Offset::Const(offset)),
            offset_mul: None,
        })
    }
}

impl From<TypedRegister> for Storage {
    fn from(r: TypedRegister) -> Self {
        Storage::Register(r)
    }
}

impl From<Memory> for Storage {
    fn from(m: Memory) -> Self {
        Storage::Memory(m)
    }
}

impl From<Value> for Storage {
    fn from(v: Value) -> Self {
        match v {
            Value::Storage(s) => s,
            _ => { panic!("should not happen") },
        }
    }
}

impl fmt::Display for Storage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Storage::Register(r) => write!(f, "{}", r),
            Storage::Memory(m) => match &m.offset_base {
                Some(offset_base) => match &m.offset_mul {
                    Some(offset_mul) => write!(f, "({}, {}, {})", m.base, offset_base, offset_mul),
                    None => match offset_base {
                        Offset::Const(c) => write!(f, "{}({})", c, m.base),
                        Offset::Register(r) => write!(f, "({}, {},)", m.base, r),
                    }
                },
                _ => write!(f, "({})", m.base),
            },
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

impl From<TypedRegister> for Value {
    fn from(r: TypedRegister) -> Self {
        Value::Storage(Storage::Register(r))
    }
}

impl From<Storage> for Value {
    fn from(s: Storage) -> Self {
        Value::Storage(s)
    }
}

impl From<Memory> for Value {
    fn from(m: Memory) -> Self {
        Storage::from(m).into()
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
            LLVM::Const::Int(v) => v.into(),
            LLVM::Const::False => 0.into(),
            LLVM::Const::True => 1.into(),
            LLVM::Const::Null => 0.into(),
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
        dest: TypedRegister,
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
    Cdq,
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
            Instr::Compare { ty, lhs, rhs } => write!(f, "cmp{} {}, {}", ty, lhs, rhs),
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
            Instr::Cdq => write!(f, "cdq"),
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
