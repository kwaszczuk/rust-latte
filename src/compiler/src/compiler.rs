use crate::instructions::{Instr, Value, Type, Register, ProgramSection, InstrList, ProgramSectionList};
use crate::operators::{Operator, ArithmOp};
use base::error::{CompileError};
use base::ast;

use std::collections::{HashMap, LinkedList};
use std::panic;

struct Labeler {
    prefix: String,
    counter: usize
}

impl Labeler {
    fn new(prefix: String) -> Self {
        Labeler {
            prefix: prefix,
            counter: 1
        }
    }

    fn next(&mut self) -> Register {
        let label = format!("{}{}", self.prefix, self.counter);
        self.counter += 1;
        Register {
            name: label
        }
    }
}

pub struct LLVMCompiler {
    // symbol_table: SymbolTable,
    symbol_table: HashMap<String, Value>,
    labeler: Labeler,
    source_file: String,
    optimize: u8
}

impl LLVMCompiler {
    fn new(optimize: u8, source_file: &str) -> Self {
        LLVMCompiler {
            symbol_table: HashMap::new(),
            labeler: Labeler::new("v".to_string()),
            source_file: source_file.to_string(),
            optimize
        }
    }

    pub fn run(ast_tree: &ast::Program, optimize: u8, source_file: &str) -> Result<String, CompileError> {
        let mut compiler = LLVMCompiler::new(optimize, source_file);
        compiler.compile(ast_tree)
    }

    fn compile(&mut self, ast_tree: &ast::Program) -> Result<String, CompileError> {
        let mut ret = self.compile_program(ast_tree)?;
        ret.push_front(ProgramSection::Static);
        let ret_strs: Vec<String> = ret.iter().map(|ps| ps.to_string()).collect();
        Ok(ret_strs.join("\n"))
    }

    fn compile_program(&mut self, program: &ast::Program) -> Result<ProgramSectionList, CompileError> {
        let mut main_instrs = self.compile_statements(&program.statements)?;
        main_instrs.push_back(
            Instr::Return {
                ty: Type::Integer,
                val: Value::Constant { value: 0 },
            }
        );
        let mut ret = LinkedList::new();
        ret.push_back(ProgramSection::FunDef {
            name: "main".to_string(),
            ty: Type::Integer,
            body: main_instrs,
        });
        Ok(ret)
    }

    fn compile_statements(&mut self, statements: &[ast::Stmt]) -> Result<InstrList, CompileError> {
        let mut instrs = LinkedList::new();
        for statement in statements {
            let mut stmt_instrs = self.compile_statement(statement)?;
            instrs.append(&mut stmt_instrs);
        }
        Ok(instrs)
    }

    fn compile_statement(&mut self, statement: &ast::Stmt) -> Result<InstrList, CompileError> {
        use base::ast::Stmt::*;
        match &statement {
            SAss { ident, exp, loc } => {
                self.compile_assignment(ident, exp)
            }
            SExp { exp, loc } => {
                let (val, ty, mut instrs) = self.compile_expression(exp)?;
                instrs.push_back(Instr::Print { val, ty });
                Ok(instrs)
            }
            _ => Err(CompileError::General { msg: "Unknown statement".to_string() }),
        }
    }

    fn compile_assignment(&mut self, ident: &ast::Ident, exp: &ast::Exp) -> Result<InstrList, CompileError> {
        let (val, ty, mut instrs) = self.compile_expression(exp)?;
        self.symbol_table.insert(ident.value.clone(), val);
        //      Dummy operation for every assignment
        // let reg = self.labeler.next();
        // instrs.push_back(Instr::Arithm {
        //     reg: reg.clone(),
        //     op: Operator::Arithm(ArithmOp::Add),
        //     ty: ty,
        //     val1: Value::Constant { value: 0 },
        //     val2: val.clone(),
        // });
        // self.symbol_table.insert(ident.value.clone(), Value::Register(reg.clone()));
        Ok(instrs)
    }

    fn compile_expression(&mut self, expression: &ast::Exp) -> Result<(Value, Type, InstrList), CompileError> {
        use base::ast::Exp::*;
        use crate::operators::ArithmOp::*;
        match &expression {
            EAdd { loc, exp1, exp2 } => self.compile_arithmetic_expr(Operator::Arithm(Add), exp1, exp2),
            ESub { loc, exp1, exp2 } => self.compile_arithmetic_expr(Operator::Arithm(Sub), exp1, exp2),
            EMul { loc, exp1, exp2 } => self.compile_arithmetic_expr(Operator::Arithm(Mul), exp1, exp2),
            EDiv { loc, exp1, exp2 } => self.compile_arithmetic_expr(Operator::Arithm(Div), exp1, exp2),
            ELit { loc, value } => Ok((Value::Constant { value: value.value }, Type::Integer, InstrList::new())),
            EVar { loc, ident } => {
                let var_opt = self.symbol_table.get(&ident.value);
                match var_opt {
                    Some(val) => Ok((val.clone(), Type::Integer, InstrList::new())),
                    None => Err(CompileError::General { msg: "Unknown ident".to_string() } ),
                }
            },
            _ => Err(CompileError::General { msg: "Unknown expression".to_string() }),
        }
    }

    fn compile_arithmetic_expr(&mut self, op: Operator, exp1: &ast::Exp, exp2: &ast::Exp) -> Result<(Value, Type, InstrList), CompileError> {
        let (exp1_val, exp1_ty, mut exp1_instrs) = self.compile_expression(exp1)?;
        let (exp2_val, exp2_ty, mut exp2_instrs) = self.compile_expression(exp2)?;
        let mut ret_instrs = LinkedList::new();
        ret_instrs.append(&mut exp1_instrs);
        ret_instrs.append(&mut exp2_instrs);

        let reg = self.labeler.next();
        ret_instrs.push_back(Instr::Arithm {
            reg: reg.clone(),
            op,
            ty: exp1_ty,
            val1: exp1_val,
            val2: exp2_val,
        });

        Ok((Value::Register(reg.clone()), exp1_ty.clone(), ret_instrs))
    }
}
