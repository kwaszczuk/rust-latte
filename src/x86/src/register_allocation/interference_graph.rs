use llvm::instructions as LLVM;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use crate::register_allocation::liveness::{liveness_analysis};
use std::hash::{Hash};

use std::panic;

#[derive(Debug, PartialEq, Clone)]
pub struct InterferenceGraph<T>
    where T: PartialEq + Clone + Eq + Hash
{
    pub nodes: HashSet<T>,
    pub edges: HashMap<T, HashSet<T>>
}

use std::fmt;
impl fmt::Display for InterferenceGraph<LLVM::Register> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ret = "".to_string();
        for (from, tos) in &self.edges {
            let mut arr = "".to_string();
            for to in tos {
                arr = format!("{}, {}", arr, to.counter)
            }
            ret = format!("{}{} -> {}\n", ret, from.counter, arr);
        }
        write!(f, "{}", ret)
    }
}

impl From<&LLVM::Function> for InterferenceGraph<LLVM::Register> {
    fn from(fun: &LLVM::Function) -> Self {
        let mut graph = InterferenceGraph { nodes: HashSet::new(), edges: HashMap::new() };
        let analysis = liveness_analysis(fun);
        let all_registers = used_registers(fun);
        for reg in all_registers {
            graph.nodes.insert(reg.clone());
            graph.edges.insert(reg.clone(), HashSet::new());
        }

        for (_id, (in_set, _out_set)) in &analysis {
            for x in in_set {
                graph.nodes.insert(x.clone());
                for y in in_set {
                    if x == y {
                        continue;
                    }
                    match graph.edges.entry(x.clone()) {
                        Entry::Occupied(mut s) => {
                            s.get_mut().insert(y.clone());
                        },
                        _ => { panic!("should not happen") },
                    }

                    match graph.edges.entry(y.clone()) {
                        Entry::Occupied(mut s) => {
                            s.get_mut().insert(x.clone());
                        },
                        _ => { panic!("should not happen") },
                    }
                }
            }
        }

        graph
    }
}

fn used_registers(fun: &LLVM::Function) -> Vec<LLVM::Register> {
    let mut ret: Vec<LLVM::Register> = vec![];
    use LLVM::Instr::*;
    for b in &fun.body {
        for i in &b.instrs {
            match i {
                Alloc { dest } |
                Load { dest, .. } |
                Store { dest, .. } |
                Arithm { dest, .. } |
                Phi { dest, .. } |
                Sext { dest, .. } |
                Bitcast { dest, .. } |
                GetElementPtr { dest, .. } => {
                    ret.push(dest.1.clone());
                }

                Compare { dest_reg, .. } => {
                    ret.push(dest_reg.clone());
                },

                Call { dest_reg, .. } => {
                    if let Some(reg) = dest_reg {
                        ret.push(reg.clone());
                    }
                },

                ReturnVoid |
                Return { .. } |
                Unreachable |
                Branch(_) |
                Label { .. } => {},
            };
        }
    }
    for i in 0..fun.args.len() {
        ret.push(LLVM::Register::new("".to_string(), i));
    }
    ret
}
