use std::cmp;
use std::collections::{HashSet, HashMap};
use std::collections::hash_map::Entry;
use base::types::{Labeler};
use crate::instructions as LLVM;
use crate::control_flow_graph::{ControlFlowGraph};
use crate::utils::{blocks_to_instructions, instructions_to_blocks};
use crate::mem2reg::dominance::{DominatorTree, DominanceFrontiers};

fn find_variables(blocks: &Vec<LLVM::Block>) -> Vec<(LLVM::Type, LLVM::Register)> {
    let instrs = blocks_to_instructions(blocks);
    let mut vars = vec![];
    for i in &instrs {
        match i {
            LLVM::Instr::Alloc { dest } => {
                vars.push(dest.clone());
            },
            _ => {}
        }
    }
    vars
}

fn redefines_variable(block: &LLVM::Block, var: &LLVM::Register) -> bool {
    for i in &block.instrs {
        match i {
            LLVM::Instr::Store { src: _, dest } => {
                if &dest.1 == var {
                    return true;
                }
            },
            _ => {},
        }
    }
    false
}

fn get_max_counter(blocks: &Vec<LLVM::Block>) -> usize {
    use LLVM::Instr::*;

    let mut res = 0;
    let instrs = blocks_to_instructions(blocks);
    for i in &instrs {
        match i {
            Alloc { dest } |
            Load { src: _, dest } |
            Store { src: _, dest } |
            Arithm { dest, op: _, val_lhs: _, val_rhs: _ } |
            Phi { dest, preds: _ } |
            Sext { dest, .. } |
            Bitcast { dest, .. } |
            GetElementPtr { dest, src: _, args: _ } => {
                res = cmp::max(res, dest.1.counter.clone());
            }

            Compare { dest_reg, op: _, ty: _, val_lhs: _, val_rhs: _ } => {
                res = cmp::max(res, dest_reg.counter.clone());
            },

            Call { dest_reg, ret_ty: _, name: _, args: _ } => {
                if let Some(reg) = dest_reg {
                    res = cmp::max(res, reg.counter.clone());
                }
            },

            ReturnVoid |
            Return { ty: _, val: _ } |
            Unreachable |
            Branch(_) |
            Label { val: _, preds: _ } => {},
        }
    }

    res + 1
}

struct PhiNode {
    reg: LLVM::Register,
    var: (LLVM::Type, LLVM::Register),
    preds: Vec<(LLVM::Value, LLVM::Label)>,
}

impl PhiNode {
    pub fn add_pred(&mut self, val: LLVM::Value, lab: LLVM::Label) {
        self.preds.push((val, lab))
    }
}

pub struct SSATransformer {
    blocks: Vec<LLVM::Block>,
    variables_versions: HashMap<LLVM::Register, Vec<LLVM::Value>>,
    labeler: Labeler<LLVM::Register>,
    variables: Vec<(LLVM::Type, LLVM::Register)>,
    dominator_tree: DominatorTree,
    dominance_frontiers: DominanceFrontiers,
    cf_graph: ControlFlowGraph,
    new_phis: HashMap<LLVM::Label, Vec<PhiNode>>,
    replacements: HashMap<LLVM::Register, LLVM::Value>,
}

impl SSATransformer {
    fn new(blocks: &Vec<LLVM::Block>) -> Self {
        let cfg = ControlFlowGraph::from(blocks);
        let dominator_tree = DominatorTree::from(&cfg);
        let dominance_frontiers = DominanceFrontiers::from((&dominator_tree, &cfg));

        let mut labeler = Labeler::new("v_".to_string());
        labeler.set(get_max_counter(blocks));

        let variables = find_variables(blocks);
        let variables_versions = variables.clone()
            .into_iter()
            .map(|(_, v)| (v, vec![]))
            .collect();

        let new_phis = blocks.into_iter().map(|b| (b.label.clone(), vec![])).collect();

        SSATransformer {
            blocks: blocks.clone(),
            variables_versions,
            labeler,
            variables,
            dominator_tree,
            dominance_frontiers,
            cf_graph: cfg,
            new_phis,
            replacements: HashMap::new(),
        }
    }

    pub fn to_ssa(fun: &LLVM::Function) -> LLVM::Function {
        LLVM::Function {
            ret_ty: fun.ret_ty.clone(),
            name: fun.name.clone(),
            args: fun.args.clone(),
            body: SSATransformer::new(&fun.body).transform(),
        }
    }

    fn transform(&mut self) -> Vec<LLVM::Block> {
        self.find_new_phis();

        let root_label = self.dominator_tree.get_root().unwrap();
        let root_block = self.cf_graph.nodes.get(&root_label).unwrap().block.clone();
        self.rename_variables(&root_block);

        let mut new_blocks = self.insert_phis(&self.blocks);
        new_blocks = self.replace_values(&new_blocks);

        new_blocks
    }

    // inserts a new phis based on dominance frontiers,
    // base on Algorithm 3.1 from: http://ssabook.gforge.inria.fr/latest/book.pdf
    fn find_new_phis(&mut self) {
        for var in &self.variables {
            let redefining_blocks: HashSet<LLVM::Label> = self.cf_graph.nodes.iter()
                .filter(|(_, n)| redefines_variable(&n.block, &var.1))
                .map(|(l, _)| l.clone())
                .collect();

            let mut blocks_done: HashSet<LLVM::Label> = HashSet::new();
            let mut blocks_to_process: Vec<LLVM::Label> = redefining_blocks.iter().cloned().collect();

            while !blocks_to_process.is_empty() {
                let b: LLVM::Label = blocks_to_process.pop().unwrap();
                for f in self.dominance_frontiers.frontiers.get(&b).unwrap() {
                    if !blocks_done.contains(f) {
                        blocks_done.insert(f.clone());
                        match self.new_phis.entry(f.clone()) {
                            Entry::Occupied(mut phi_nodes) => {
                                phi_nodes.get_mut().push(
                                    PhiNode {
                                        reg: self.labeler.next(),
                                        var: var.clone(),
                                        preds: vec![],
                                    }
                                );
                            },
                            _ => { panic!("should not happen") },
                        }
                        if !redefining_blocks.contains(f) {
                            blocks_to_process.push(f.clone());
                        }
                    }
                }
            }
        }
    }

    // replace the `alloc` versions of variables with the new,
    // scope awared, ones, based on algorithm from Figure 12
    // of: https://piazza.com/class_profile/get_resource/hy7enxf648g7me/i2o85s01fnq2x2
    fn rename_variables(&mut self, b: &LLVM::Block) {
        let mut new_vars_versions: Vec<LLVM::Register> = vec![];

        // first fill the current versions of the variables with block's phis
        for phi_node in self.new_phis.get(&b.label).unwrap() {
            match self.variables_versions.entry(phi_node.var.1.clone()) {
                Entry::Occupied(mut versions) => {
                    versions.get_mut().push(phi_node.reg.clone().into());
                },
                _ => {},
            }
            new_vars_versions.push(phi_node.var.1.clone());
        }

        use LLVM::Instr::*;
        for i in &b.instrs {
            match i {
                // load instructions registers should be replaced with the
                // most recent version of the variable they load
                Load { src, dest } => {
                    if self.variables_versions.contains_key(&src.1) {
                        let new_var = self.variables_versions
                            .get(&src.1).unwrap()
                            .last().unwrap();
                        self.replacements.insert(dest.1.clone(), new_var.clone());
                    }
                },

                Store { src, dest } => {
                    if self.variables_versions.contains_key(&dest.1) {
                        let mut new_ver = src.1.clone();
                        if let LLVM::Value::Register(r) = &src.1 {
                            if let Some(v) = self.replacements.get(&r) {
                                new_ver = v.clone()
                            }
                        }
                        match self.variables_versions.entry(dest.1.clone()) {
                            Entry::Occupied(mut versions) => {
                                versions.get_mut().push(new_ver);
                            },
                            _ => {},
                        }
                        new_vars_versions.push(dest.1.clone());
                    }
                }

                _ => {},
            }
        }

        // updates phis of succesors
        for succ in &self.cf_graph.nodes.get(&b.label).unwrap().nexts {
            for phi_node in self.new_phis.get_mut(&succ).unwrap() {
                let cur_var_val = self.variables_versions
                    .get(&phi_node.var.1).unwrap()
                    .last().unwrap();
                phi_node.add_pred(cur_var_val.clone(), b.label.clone());
            }
        }

        let mut child_blocks = vec![];
        for child in self.dominator_tree.edges.get(&b.label).unwrap().iter() {
            let child_block = self.cf_graph.nodes.get(&child).unwrap().block.clone();
            child_blocks.push(child_block);
        }
        for cb in &child_blocks {
            self.rename_variables(&cb);
        }

        // remove new copies of the variables created in the current block scope
        for var in &new_vars_versions {
            match self.variables_versions.entry(var.clone()) {
                Entry::Occupied(mut v) => {
                    v.get_mut().pop();
                },
                _ => { panic!("should not happen") },
            }
        }
    }

    fn insert_phis(&self, blocks: &Vec<LLVM::Block>) -> Vec<LLVM::Block> {
        let mut new_blocks = vec![];
        for b in blocks {
            let mut new_instrs = vec![];

            for phi_node in self.new_phis.get(&b.label).unwrap() {
                new_instrs.push(LLVM::Instr::Phi {
                    dest: (phi_node.var.0.clone(), phi_node.reg.clone()),
                    preds: phi_node.preds.clone(),
                });
            }

            new_instrs.append(&mut b.instrs.clone());
            new_blocks.push(LLVM::Block {
                label: b.label.clone(),
                instrs: new_instrs,
            });
        }
        new_blocks
    }

    fn replace_values(&self, blocks: &Vec<LLVM::Block>) -> Vec<LLVM::Block> {
        use LLVM::Instr::*;

        let replace_value = |v| {
            if let LLVM::Value::Register(r) = &v {
                if let Some(new_v) = self.replacements.get(&r) {
                    return new_v.clone();
                }
            }
            v
        };

        let instrs = blocks_to_instructions(blocks);
        let mut new_instrs = vec![];
        for i in instrs {
            match i.clone() {
                // memory instructions are no longer needed
                Alloc { dest: _ } => {},

                Load { src, dest: _ } => {
                    if !self.variables_versions.contains_key(&src.1) {
                        new_instrs.push(i.clone());
                    }
                },

                Store { src, dest } => {
                    if !self.variables_versions.contains_key(&dest.1) {
                        new_instrs.push(Store {
                            src: (src.0, replace_value(src.1)),
                            dest,
                        });
                    }
                }

                ReturnVoid |
                Unreachable |
                Branch(LLVM::Branch::Direct { label: _ }) |
                Label { val: _, preds: _ } |
                Sext { .. } => {
                    new_instrs.push(i.clone());
                },

                Bitcast { src, dest } => {
                    new_instrs.push(Bitcast {
                        src: (src.0, replace_value(src.1)),
                        dest,
                    });
                }

                Compare { dest_reg, op, ty, val_lhs, val_rhs } => {
                    let new_val_lhs = replace_value(val_lhs);
                    let new_val_rhs = replace_value(val_rhs);
                    new_instrs.push(Compare {
                        dest_reg,
                        op,
                        ty,
                        val_lhs: new_val_lhs.clone(),
                        val_rhs: new_val_rhs.clone(),
                    });
                },

                Call { dest_reg, ret_ty, name, args } => {
                    let new_args = args.iter().cloned().map(|(ty, val)| {
                        let new_val = replace_value(val);
                        (ty, new_val)
                    }).collect();
                    new_instrs.push(Call {
                        dest_reg,
                        ret_ty,
                        name,
                        args: new_args,
                    });
                },

                Branch(LLVM::Branch::Conditional { ty, val, true_label, false_label }) => {
                    let new_val = replace_value(val);
                    new_instrs.push(Branch(LLVM::Branch::Conditional {
                        ty,
                        val: new_val,
                        true_label,
                        false_label,
                    }));
                },

                Arithm { dest, op, val_lhs, val_rhs } => {
                    let new_val_lhs = replace_value(val_lhs);
                    let new_val_rhs = replace_value(val_rhs);

                    new_instrs.push(Arithm {
                        dest,
                        op,
                        val_lhs: new_val_lhs.clone(),
                        val_rhs: new_val_rhs.clone(),
                    });
                },

                Phi { dest, preds } => {
                    let new_preds = preds.iter().cloned().map(|(val, lab)| {
                        let new_val = replace_value(val);
                        (new_val, lab)
                    }).collect();
                    new_instrs.push(Phi {
                        dest,
                        preds: new_preds,
                    });
                },

                GetElementPtr { dest, src, args } => {
                    new_instrs.push(GetElementPtr {
                        dest,
                        src,
                        args: args.iter()
                            .map(|idx| (idx.0.clone(), replace_value(idx.1.clone())))
                            .collect(),
                    });
                },

                Return { ty, val } => {
                    let new_val = replace_value(val);
                    new_instrs.push(Return {
                        ty,
                        val: new_val,
                    });
                },
            }
        }

        instructions_to_blocks(&new_instrs)
    }
}
