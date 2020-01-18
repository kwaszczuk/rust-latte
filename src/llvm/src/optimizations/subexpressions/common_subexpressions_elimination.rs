use std::collections::{HashMap};
use std::collections::hash_map::Entry;

use crate::mem2reg::dominance::{DominatorTree};
use crate::instructions as LLVM;
use crate::optimizations::base;
use crate::utils::{blocks_to_instructions, instructions_to_blocks};

pub struct Optimizer {
    expressions: HashMap<String, LLVM::Register>,
    new_values: HashMap<LLVM::Register, LLVM::Register>,
    blocks_map: HashMap<LLVM::Label, LLVM::Block>,
    dom_tree: DominatorTree
}

impl Optimizer {
    pub fn new() -> Self {
        Optimizer {
            expressions: HashMap::new(),
            new_values: HashMap::new(),
            blocks_map: HashMap::new(),
            dom_tree: DominatorTree { edges: HashMap::new() }
        }
    }

    pub fn optimize_program(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        let new_functions = prog.functions.iter().map(|f| {
            let new_f = self.optimize_function(f);
            new_f
        }).collect();

        LLVM::Program {
            options: prog.options.clone(),
            declares: prog.declares.clone(),
            statics: prog.statics.clone(),
            functions: new_functions,
        }
    }

    pub fn optimize_function(&mut self, fun: &LLVM::Function) -> LLVM::Function {
        self.expressions.clear();
        self.new_values.clear();
        self.blocks_map = fun.body.iter()
            .map(|b| (b.label.clone(), b.clone()))
            .collect();
        self.dom_tree = DominatorTree::from(&fun.body);

        let entry_block = self.blocks_map.get(&self.dom_tree.get_root().unwrap()).unwrap().clone();
        self.analyse_block(&entry_block);

        let instrs = blocks_to_instructions(&fun.body);
        let new_instrs = self.optimize_instructions(&instrs);
        let new_body = instructions_to_blocks(&new_instrs);

        LLVM::Function {
            ret_ty: fun.ret_ty.clone(),
            name: fun.name.clone(),
            args: fun.args.iter().cloned().collect(),
            body: new_body,
        }
    }

    fn analyse_block(&mut self, block: &LLVM::Block) {
        use LLVM::Instr::*;

        let mut added = vec![];
        let mut reg;
        let mut instr_str = String::from("");
        for i in &block.instrs {
            match i {
                Branch(_) |
                Label { .. } |
                Call { .. } |
                Return { .. } |
                ReturnVoid |
                Unreachable => {
                    continue;
                },

                Compare { dest_reg, .. } => {
                    reg = dest_reg.clone();
                    instr_str = format!("{}", i)
                        .split("=")
                        .collect::<Vec<&str>>()[1]
                        .trim()
                        .to_string();
                },

                Store { dest, src } => {
                    reg = dest.1.clone();
                    let new_i = Store {
                        src: src.clone(),
                        dest: (dest.0.clone(), LLVM::Register::new("".to_string(), 0)),
                    };
                    instr_str = format!("{}", new_i).trim().to_string();
                },

                Load { dest, .. } |
                Alloc { dest } |
                Arithm { dest, .. } |
                Sext { dest, .. } |
                Phi { dest, .. } |
                Bitcast { dest, .. } |
                GetElementPtr { dest, .. } => {
                    reg = dest.1.clone();
                    instr_str = format!("{}", i)
                        .split("=")
                        .collect::<Vec<&str>>()[1]
                        .trim()
                        .to_string();
                }
            }

            match self.expressions.get(&instr_str) {
                Some(new_reg) => {
                    // println!("found! {:?} {:?}", reg.clone(), new_reg.clone());
                    self.new_values.insert(reg.clone(), new_reg.clone());
                },
                None => {
                    self.expressions.insert(instr_str.clone(), reg.clone());
                    added.push(instr_str.clone());
                },
            }
        }

        for lab in self.dom_tree.edges.get(&block.label).unwrap().clone() {
            let child_block = self.blocks_map.get(&lab).unwrap().clone();
            self.analyse_block(&child_block);
        }

        for v in &added {
            self.expressions.remove(v);
        }
    }


    fn optimize_instructions(&mut self, instrs: &Vec<LLVM::Instr>) -> Vec<LLVM::Instr> {
        use LLVM::Instr::*;

        let mut new_instrs = vec![];
        for i in instrs {
            let new_instr: LLVM::Instr;
            match i.clone() {
                Alloc { .. } |
                ReturnVoid |
                Unreachable |
                Branch(LLVM::Branch::Direct { .. }) |
                Label { .. } |
                Sext { .. } => {
                    new_instr = i.clone();
                },

                Load { src, dest } => {
                    let new_val = self.optimize_value(&src.1.clone().into());
                    if let LLVM::Value::Register(new_src) = new_val {
                        new_instr = Load {
                            src: (src.0, new_src),
                            dest,
                        };
                    } else {
                        new_instr = i.clone();
                    }
                },

                Bitcast { src, dest } => {
                    let new_val = self.optimize_value(&src.1);
                    new_instr = Bitcast {
                        src: (src.0, new_val),
                        dest,
                    };
                },

                Store { src, dest } => {
                    let new_val = self.optimize_value(&src.1);
                    new_instr = Store {
                        src: (src.0, new_val),
                        dest,
                    };
                },

                Compare { dest_reg, op, ty, val_lhs, val_rhs } => {
                    let new_val_lhs = self.optimize_value(&val_lhs);
                    let new_val_rhs = self.optimize_value(&val_rhs);
                    new_instr = Compare {
                        dest_reg,
                        op,
                        ty,
                        val_lhs: new_val_lhs.clone(),
                        val_rhs: new_val_rhs.clone(),
                    };
                },

                Call { dest_reg, ret_ty, name, args } => {
                    let new_args = args.iter().cloned().map(|(ty, val)| {
                        let new_val = self.optimize_value(&val);
                        (ty, new_val)
                    }).collect();
                    new_instr = Call {
                        dest_reg,
                        ret_ty,
                        name,
                        args: new_args,
                    };
                },

                Branch(LLVM::Branch::Conditional { ty, val, true_label, false_label }) => {
                    let new_val = self.optimize_value(&val);
                    new_instr = Branch(LLVM::Branch::Conditional {
                        ty,
                        val: new_val,
                        true_label,
                        false_label,
                    });
                },

                Arithm { dest, op, val_lhs, val_rhs } => {
                    let new_val_lhs = self.optimize_value(&val_lhs);
                    let new_val_rhs = self.optimize_value(&val_rhs);

                    new_instr = Arithm {
                        dest,
                        op,
                        val_lhs: new_val_lhs.clone(),
                        val_rhs: new_val_rhs.clone(),
                    };
                },

                Phi { dest, preds } => {
                    let new_preds = preds.iter().cloned().map(|(val, lab)| {
                        let new_val = self.optimize_value(&val);
                        (new_val, lab)
                    }).collect();
                    new_instr = Phi {
                        dest,
                        preds: new_preds,
                    };
                },

                GetElementPtr { dest, src, args } => {
                    let new_val = self.optimize_value(&src.1);
                    new_instr = GetElementPtr {
                        dest,
                        src: (src.0, new_val),
                        args: args.iter()
                            .map(|idx| (idx.0.clone(), self.optimize_value(&idx.1)))
                            .collect(),
                    };
                },

                Return { ty, val } => {
                    let new_val = self.optimize_value(&val);
                    new_instr = Return {
                        ty,
                        val: new_val,
                    };
                },
            }
            new_instrs.push(new_instr);
        }

        new_instrs
    }

    fn optimize_value(&mut self, value: &LLVM::Value) -> LLVM::Value {
        if let LLVM::Value::Register(r) = value.clone() {
            if let Some(new_reg) = self.new_values.get(&r) {
                return new_reg.clone().into();
            }
        }
        value.clone()
    }
}

impl base::Optimizer for Optimizer {
    fn run(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        self.optimize_program(&prog)
    }
}
