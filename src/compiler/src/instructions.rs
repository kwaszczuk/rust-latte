use std::fmt;
use std::collections::{LinkedList};
use crate::operators::{Operator};

#[derive(Clone)]
pub struct Register {
    pub name: String
}

#[derive(Clone, Copy)]
pub enum Type {
    Integer,
}

#[derive(Clone)]
pub enum Value {
    Constant {
        value: i32
    },
    Register(Register),
}

pub enum ProgramSection {
    FunDef {
        ty: Type,
        name: String,
        body: InstrList,
    },
    Static,
}

pub enum Instr {
    Arithm {
        reg: Register,
        op: Operator,
        ty: Type,
        val1: Value,
        val2: Value,
    },
    Print {
        ty: Type,
        val: Value,
    },
    Return {
        ty: Type,
        val: Value,
    }
}

pub type InstrList = LinkedList<Instr>;
pub type ProgramSectionList = LinkedList<ProgramSection>;

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;
        match self {
            Integer => write!(f, "i32")
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.name)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            Constant { value } => write!(f, "{}", value),
            Register(register) => write!(f, "{}", register),
        }
    }
}


impl fmt::Display for ProgramSection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ProgramSection::*;
        match self {
            FunDef { name, ty, body } => {
                let mut fun_template_begin = vec![
                    format!("define {} @{}() {{", ty, name)
                ];
                let mut fun_template_end = vec![
                    format!("}}")
                ];
                let mut body_strs: Vec<String> = body
                    .iter()
                    .map(|instr| "\t".to_owned() + &instr.to_string())
                    .collect();

                let mut code = vec![];
                code.extend(fun_template_begin);
                code.extend(body_strs);
                code.extend(fun_template_end);
                write!(f, "{}", code.join("\n"))
            },
            Static => {
                let statics = vec![
                    "declare i32 @printf(i8*, ...)",
                    "@print_fstring = internal constant [4 x i8] c\"%d\\0A\\00\""
                ];
                write!(f, "{}", statics.join("\n"))
            }
        }
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Instr::*;
        match self {
            Arithm { reg, op, ty, val1, val2  } => write!(f,
                "{} = {} {} {}, {}",
                reg,
                op,
                ty,
                val1,
                val2),
            Print { ty, val } => write!(f,
                "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @print_fstring, i32 0, i32 0), {} {} )",
                ty,
                val),
            Return { ty, val } => write!(f, "ret {} {}", ty, val),
        }
    }
}
