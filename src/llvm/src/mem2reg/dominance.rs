use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;

use crate::instructions as LLVM;
use crate::control_flow_graph::{ControlFlowGraph};

pub struct DominatorTree {
    pub edges: HashMap<LLVM::Label, HashSet<LLVM::Label>>
}

impl DominatorTree {
    pub fn new() -> Self {
        DominatorTree {
            edges: HashMap::new(),
        }
    }

    pub fn dominator_of(&self, l: &LLVM::Label) -> Option<LLVM::Label> {
        for (parent, childs) in &self.edges {
            if childs.contains(l) {
                return Some(parent.clone())
            }
        }
        None
    }

    pub fn get_root(&self) -> Option<LLVM::Label> {
        for node in self.edges.keys() {
            if self.dominator_of(&node) == None {
                return Some(node.clone())
            }
        }
        None
    }
}

impl From<&Vec<LLVM::Block>> for DominatorTree {
    fn from(blocks: &Vec<LLVM::Block>) -> Self {
        DominatorTree::from(&ControlFlowGraph::from(blocks))
    }
}

impl From<&ControlFlowGraph> for DominatorTree {
    fn from(cfg: &ControlFlowGraph) -> Self {
        get_dominator_tree(cfg)
    }
}

/* Algorithm for finding a dominator tree
 * of given control flow graph based on:
 * https://en.wikipedia.org/wiki/Dominator_(graph_theory)
 */
fn get_dominator_tree(cfg: &ControlFlowGraph) -> DominatorTree {
    // calculate dominators of every node
    let mut dominators: HashMap<LLVM::Label, HashSet<LLVM::Label>> = HashMap::new();
    for (l, n) in &cfg.nodes {
        let mut s = HashSet::new();
        if n.block.is_entry() {
            s.insert(l.clone());
        } else {
            for k in cfg.nodes.keys() {
                s.insert(k.clone());
            }
        }
        dominators.insert(l.clone(), s);
    }

    let mut changed = true;
    while changed {
        changed = false;
        for (l, n) in &cfg.nodes {
            if !n.block.is_entry() {
                let act_dom = dominators.get(&l).unwrap().clone();
                let pred_dominators: Vec<HashSet<LLVM::Label>> = n.prevs
                    .iter()
                    .map(|p| dominators.get(&p).unwrap().clone())
                    .collect();
                let mut new_dom: HashSet<LLVM::Label>;
                if pred_dominators.len() == 0 {
                    new_dom = HashSet::new();
                } else {
                    new_dom = pred_dominators[0].clone();
                    for s in pred_dominators {
                        new_dom = new_dom.intersection(&s).cloned().collect();
                    }
                }
                new_dom.insert(l.clone());
                if act_dom != new_dom {
                    dominators.insert(l.clone(), new_dom);
                    changed = true;
                }
            }
        }
    }

    // calculate dominator tree
    let mut dominator_tree = DominatorTree {
        edges: cfg.nodes.clone().into_iter()
            .map(|(l, _)| (l.clone(), HashSet::new()))
            .collect(),
    };
    for (l, n) in &cfg.nodes {
        if !n.block.is_entry() {
            // immediate dominator must dominate a node
            let doms = dominators.get(&l).unwrap();
            for d1 in doms {
                if d1 == l {
                    continue
                }
                let mut good = true;
                for d2 in doms {
                    if d1 != d2 && d2 != l {
                        // and it can't dominate any other dominator
                        if dominators.get(&d2).unwrap().contains(&d1) {
                            good = false;
                            break;
                        }
                    }
                }
                // found immediate dominator
                if good {
                    match dominator_tree.edges.entry(d1.clone()) {
                        Entry::Occupied(mut s) => {
                            s.get_mut().insert(l.clone());
                        },
                        _ => { panic!("should not happen") },
                    }
                    break;
                }
            }
        }
    }

    // every node always has some immediate dominator
    for (l, n) in &cfg.nodes {
        if !n.block.is_entry() {
            if let None = dominator_tree.dominator_of(&l) {
                assert!(false, format!("idom not found for {:?}", l));
            }
        }
    }

    dominator_tree
}

pub struct DominanceFrontiers {
    pub frontiers: HashMap<LLVM::Label, Vec<LLVM::Label>>
}

impl DominanceFrontiers {
    pub fn new() -> Self {
        DominanceFrontiers {
            frontiers: HashMap::new(),
        }
    }
}

impl From<&Vec<LLVM::Block>> for DominanceFrontiers {
    fn from(blocks: &Vec<LLVM::Block>) -> Self {
        DominanceFrontiers::from(&ControlFlowGraph::from(blocks))
    }
}

impl From<&ControlFlowGraph> for DominanceFrontiers {
    fn from(cfg: &ControlFlowGraph) -> Self {
        let dom_tree = DominatorTree::from(cfg);
        DominanceFrontiers::from((&dom_tree, cfg))
    }
}

impl From<(&DominatorTree, &ControlFlowGraph)> for DominanceFrontiers {
    fn from(t: (&DominatorTree, &ControlFlowGraph)) -> Self {
        get_dominance_frontiers(t)
    }
}

/* Algorithm for finding a dominance frontier
 * of given control flow graph based on:
 * https://en.wikipedia.org/wiki/Static_single_assignment_form#Computing_minimal_SSA_using_dominance_frontiers
 */
fn get_dominance_frontiers((dom, cfg): (&DominatorTree, &ControlFlowGraph)) -> DominanceFrontiers {
    let mut dominance_frontiers = DominanceFrontiers::new();
    for (l, _) in &cfg.nodes {
        dominance_frontiers.frontiers.insert(l.clone(), vec![]);
    }

    for (l, n) in &cfg.nodes {
        if n.prevs.len() >= 2 {
            for pred_label in &n.prevs {
                let mut runner = pred_label.clone();
                // runner is not immediate dominator of l
                while runner != dom.dominator_of(l).unwrap() {
                    match dominance_frontiers.frontiers.entry(runner.clone()) {
                        Entry::Occupied(mut e) => {
                            e.get_mut().push(l.clone());
                        },
                        _ => { panic!("should not happen") },
                    }
                    runner = dom.dominator_of(&runner).unwrap();
                }
            }
        }
    }

    dominance_frontiers
}
