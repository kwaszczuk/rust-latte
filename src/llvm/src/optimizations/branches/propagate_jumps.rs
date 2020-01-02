/* This optimization finds a blocks where only `br` instruction is invoked
 * and replaces and jumps to such blocks with a jump to the final one */

use std::collections::{HashMap};

use crate::instructions as LLVM;
use crate::optimizations::base;
use crate::utils::{blocks_to_instructions, instructions_to_blocks};

pub struct Optimizer {
    jump_only_blocks: HashMap<LLVM::Label, (LLVM::Label, bool)>,
}

impl Optimizer {
    pub fn new() -> Self {
        Optimizer {
            jump_only_blocks: HashMap::new(),
        }
    }

    pub fn optimize_program(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        let new_functions = prog.functions.iter().map(|f| {
            let new_f = self.optimize_function(f);
            new_f
        }).collect();

        LLVM::Program {
            declares: prog.declares.clone(),
            statics: prog.statics.clone(),
            functions: new_functions,
        }
    }

    pub fn optimize_function(&mut self, fun: &LLVM::Function) -> LLVM::Function {
        self.jump_only_blocks.clear();
        self.find_jump_only_blocks(&fun.body);

        let new_body = self.replace_branches(&fun.body);

        LLVM::Function {
            ret_ty: fun.ret_ty.clone(),
            name: fun.name.clone(),
            args: fun.args.iter().cloned().collect(),
            body: new_body,
        }
    }

    fn find_jump_only_blocks(&mut self, blocks: &Vec<LLVM::Block>) {
        use LLVM::Instr::*;

        for b in blocks {
            if !b.label.is_entry() {
                if b.instrs.len() == 1 {
                    if let Branch(LLVM::Branch::Direct { label }) = &b.instrs[0] {
                        self.jump_only_blocks.insert(b.label.clone(), (label.clone(), true));
                    }
                }
            }
        }

        // ignore last jump only blocks on every path, as removing them
        // may cause some phi conditions to be outdated
        for b in blocks {
            if let Some((lab, _)) = self.jump_only_blocks.get(&b.label) {
                if let Branch(LLVM::Branch::Direct { label }) = &b.instrs[0] {
                    if let None = self.jump_only_blocks.get(&label) {
                        self.jump_only_blocks.insert(b.label.clone(), (lab.clone(), false));
                    }
                }
            }
        }

        let mut updates = vec![];
        for (from, (to, remove)) in &self.jump_only_blocks {
            let mut new_to = to.clone();
            let mut prev_to = from.clone();
            while let Some((jump_to, _)) = self.jump_only_blocks.get(&new_to) {
                prev_to = new_to.clone();
                new_to = jump_to.clone();
            }
            updates.push((from.clone(), (prev_to.clone(), remove.clone())));
        }
        for (k, v) in updates {
            self.jump_only_blocks.insert(k, v);
        }
    }

    fn replace_branches(&mut self, blocks: &Vec<LLVM::Block>) -> Vec<LLVM::Block> {
        use LLVM::Instr::*;

        let mut new_blocks = vec![];
        for b in blocks {
            let mut ignore = false;
            let mut new_instrs = vec![];
            for i in &b.instrs {
                match i {
                    Branch(LLVM::Branch::Direct { label }) => {
                        if let Some((_, remove)) = self.jump_only_blocks.get(&b.label) {
                            ignore = remove.clone();
                        }
                        let mut new_label = label.clone();
                        if let Some((jump_to, _)) = self.jump_only_blocks.get(&label) {
                            new_label = jump_to.clone();
                        }
                        new_instrs.push(Branch(LLVM::Branch::Direct {
                            label: new_label,
                        }));
                    },
                    Branch(LLVM::Branch::Conditional { ty, val, true_label, false_label }) => {
                        let mut new_true_label = true_label.clone();
                        let mut new_false_label = false_label.clone();
                        if let Some((jump_to, _)) = self.jump_only_blocks.get(&true_label) {
                            new_true_label = jump_to.clone();
                        }
                        if let Some((jump_to, _)) = self.jump_only_blocks.get(&false_label) {
                            new_false_label = jump_to.clone();
                        }
                        new_instrs.push(Branch(LLVM::Branch::Conditional {
                            ty: ty.clone(),
                            val: val.clone(),
                            true_label: new_true_label,
                            false_label: new_false_label,
                        }));
                    },
                    _ => {
                        new_instrs.push(i.clone());
                    }
                }
            }

            if !ignore {
                new_blocks.push(LLVM::Block {
                    label: b.label.clone(),
                    instrs: new_instrs,
                });
            }
        }
        new_blocks
    }

}

impl base::Optimizer for Optimizer {
    fn run(&mut self, prog: &LLVM::Program) -> LLVM::Program {
        self.optimize_program(prog)
    }
}
