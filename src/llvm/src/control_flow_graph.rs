use std::collections::{HashMap};
use crate::instructions as LLVM;

#[derive(Debug, PartialEq, Clone)]
pub struct CFGNode {
    pub block: LLVM::Block,
    pub prevs: Vec<LLVM::Label>,
    pub nexts: Vec<LLVM::Label>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ControlFlowGraph {
    pub nodes: HashMap<LLVM::Label, CFGNode>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        ControlFlowGraph {
            nodes: HashMap::new(),
        }
    }
}

fn branches_to(block: &LLVM::Block) -> Vec<LLVM::Label> {
    use LLVM::Instr::*;

    let mut ret = vec![];
    for i in &block.instrs {
        match i {
            Branch(LLVM::Branch::Direct { label }) => {
                ret.push(label.clone());
                break;
            },
            Branch(LLVM::Branch::Conditional { ty: _, val: _, true_label, false_label }) => {
                ret.push(true_label.clone());
                ret.push(false_label.clone());
                break;
            },
            _ => {},
        }
    }
    ret
}


pub fn generate_graph(blocks: &Vec<LLVM::Block>) -> ControlFlowGraph {
    let mut cfg = ControlFlowGraph::new();
    let mut nodes_prevs: HashMap<LLVM::Label, Vec<LLVM::Label>> = HashMap::new();
    let mut nodes_nexts: HashMap<LLVM::Label, Vec<LLVM::Label>> = HashMap::new();

    for b in blocks {
        let nexts = branches_to(&b);
        for l in &nexts {
            if let Some(cur_prevs) = nodes_prevs.get(l) {
                let mut prevs = cur_prevs.clone();
                prevs.push(b.label.clone());
                nodes_prevs.insert(l.clone(), prevs);
            } else {
                nodes_prevs.insert(l.clone(), vec![b.label.clone()]);
            }
        }
        nodes_nexts.insert(b.label.clone(), nexts);
    }

    for b in blocks {
        let prevs = nodes_prevs.get(&b.label).map_or_else(|| vec![], |v| v.clone());
        let nexts = nodes_nexts.get(&b.label).map_or_else(|| vec![], |v| v.clone());
        cfg.nodes.insert(b.label.clone(), CFGNode {
            block: b.clone(),
            prevs,
            nexts,
        });
    }

    cfg
}