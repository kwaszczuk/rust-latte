use std::collections::{HashMap};

use crate::instructions as LLVM;
use crate::optimizations::base;
use crate::control_flow_graph::{generate_graph, CFGNode, ControlFlowGraph};

fn is_node_backward_reducible(node: &CFGNode, cfg: &ControlFlowGraph) -> bool {
    if node.prevs.len() == 1 {
        // If some of the next block or node itself contains PHI instruction, then
        // it's condition will be messed up after join, so we can't merge it just yet.
        // Maybe we will be able to do it on some other run if it'll appear to be
        // a trivial phi.
        if let Some(LLVM::Instr::Phi { dest: _, preds: _ }) = node.block.instrs.first() {
            return false;
        }

        return node.prevs.iter().any(|p| {
            if let Some(next_node) = cfg.nodes.get(&p) {
                if let Some(LLVM::Instr::Phi { dest: _, preds: _ }) = next_node.block.instrs.first() {
                    return false;
                }
            }
            true
        });
    }
    false
}

fn is_node_forward_reducible(node: &CFGNode, _cfg: &ControlFlowGraph) -> bool {
    node.nexts.len() == 1
}

pub struct Optimizer {
    join_with: HashMap<LLVM::Label, LLVM::Label>,
}

impl Optimizer {
    pub fn new() -> Self {
        Optimizer {
            join_with: HashMap::new(),
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
        let cfg = generate_graph(&fun.body);
        self.join_with.clear();
        self.find_reducibles(&cfg);

        let new_body = self.join_blocks(&fun.body);

        LLVM::Function {
            ret_ty: fun.ret_ty.clone(),
            name: fun.name.clone(),
            args: fun.args.iter().cloned().collect(),
            body: new_body.clone(),
        }
    }

    fn find_reducibles(&mut self, cfg: &ControlFlowGraph) {
        for (_, node) in &cfg.nodes {
            if is_node_forward_reducible(&node, cfg) && !node.block.label.is_entry() {
                let next_node = cfg.nodes.get(&node.nexts[0]).unwrap();
                if is_node_backward_reducible(&next_node, cfg) {
                    self.join_with.insert(node.block.label.clone(), next_node.block.label.clone());
                }
            }
        }
    }

    fn join_blocks(&mut self, blocks: &Vec<LLVM::Block>) -> Vec<LLVM::Block> {
        let mut blocks_map: HashMap<LLVM::Label, LLVM::Block> = blocks
            .iter()
            .cloned()
            .map(|b| (b.label.clone(), b.clone()))
            .collect();

        for label1 in self.join_with.keys() {
            if let Some(block1) = blocks_map.get(&label1) {
                let mut new_instrs = block1.instrs.clone();
                let mut next_label = label1.clone();

                while let Some(label2) = self.join_with.get(&next_label) {
                    next_label = label2.clone();
                    if let Some(block2) = blocks_map.get(&next_label) {
                        if let Some(LLVM::Instr::Branch(_)) = new_instrs.last() {
                            new_instrs.pop();
                        }
                        new_instrs.extend(block2.instrs.clone());
                        blocks_map.remove(&next_label);
                    }
                }

                let new_block = LLVM::Block {
                    label: label1.clone(),
                    instrs: new_instrs,
                };
                blocks_map.insert(label1.clone(), new_block);
            }
        }

        let mut new_blocks = vec![];
        for b in blocks {
            if let Some(b) = blocks_map.get(&b.label) {
                new_blocks.push(b.clone());
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
