use llvm::instructions as LLVM;
use llvm::utils::{blocks_to_instructions, instructions_to_blocks};
use std::collections::{HashMap, HashSet};

fn uses_of(instr: &LLVM::Instr) -> HashSet<LLVM::Register> {
    let mut uses = HashSet::new();
    let mut uses_candidates = vec![];

    use LLVM::Instr::*;
    match instr {
        Alloc { .. } |
        Label { .. } |
        ReturnVoid |
        Unreachable |
        Branch(LLVM::Branch::Direct { .. }) |
        Sext { .. } |
        Bitcast { .. } => {},

        Load { src, .. } => {
            uses_candidates.push(src.1.clone().into());
        },

        Store { src, dest } => {
            uses_candidates.push(src.1.clone());
            uses_candidates.push(dest.1.clone().into());
        },

        Phi { preds, .. } => {
            for p in preds {
                uses_candidates.push(p.0.clone());
            }
        },

        GetElementPtr { src, args, .. } => {
            uses_candidates.push(src.1.clone());
            for a in args {
                uses_candidates.push(a.1.clone());
            }
        },

        Arithm { val_lhs, val_rhs, .. } |
        Compare { val_lhs, val_rhs, .. } => {
            uses_candidates.push(val_lhs.clone());
            uses_candidates.push(val_rhs.clone());
        },

        Call { args, .. } => {
            for a in args {
                uses_candidates.push(a.1.clone());
            }
        },


        Branch(LLVM::Branch::Conditional { val, .. }) |
        Return { val, .. } => {
            uses_candidates.push(val.clone());
        },
    };

    for can in &uses_candidates {
        if let LLVM::Value::Register(r) = can {
            uses.insert(r.clone());
        }
    }

    uses
}

fn definitions_of(instr: &LLVM::Instr) -> HashSet<LLVM::Register> {
    let mut defs = HashSet::new();

    use LLVM::Instr::*;
    match &instr {
        Alloc { dest } |
        Load { dest, .. } |
        Store { dest, .. } |
        Arithm { dest, .. } |
        Phi { dest, .. } |
        GetElementPtr { dest, .. } => {
            defs.insert(dest.1.clone());
        }

        Compare { dest_reg, .. } => {
            defs.insert(dest_reg.clone());
        },

        Call { dest_reg, .. } => {
            if let Some(reg) = dest_reg {
                defs.insert(reg.clone());
            }
        },

        ReturnVoid |
        Return { .. } |
        Unreachable |
        // Sext and Bitcast are treated in a way
        // that no stack variable is used for them
        Sext { .. } |
        Bitcast { .. } |
        Branch(_) |
        Label { .. } => {},
    };

    defs
}

struct InstructionsGraph {
    nodes: Vec<IGNode>,
}
type IGraph = InstructionsGraph;

impl From<&LLVM::Function> for IGraph {
    fn from(fun: &LLVM::Function) -> Self {
        let mut nodes = vec![];
        let mut block_first_id: HashMap<LLVM::Label, usize> = HashMap::new();

        let mut cnt = 0;
        for b in &fun.body {
            block_first_id.insert(b.label.clone(), cnt.clone());
            cnt += b.instrs.len();
        }

        let mut id_counter = 0;
        for b in &fun.body {
            for i in &b.instrs {
                let mut nexts = vec![];
                match i {
                    LLVM::Instr::Branch(LLVM::Branch::Direct { label }) => {
                        nexts.push(block_first_id.get(&label).unwrap().clone());
                    },

                    LLVM::Instr::Branch(LLVM::Branch::Conditional { true_label, false_label, .. }) => {
                        nexts.push(block_first_id.get(&true_label).unwrap().clone());
                        nexts.push(block_first_id.get(&false_label).unwrap().clone());
                    },

                    LLVM::Instr::ReturnVoid |
                    LLVM::Instr::Return { .. } => {},

                    _ => {
                        nexts.push(id_counter + 1);
                    }
                }

                nodes.push(IGNode {
                    id: id_counter.clone(),
                    instr: i.clone(),
                    nexts,
                });
                id_counter += 1;
            }
        }

        IGraph { nodes }
    }
}

struct InstructionsGraphNode {
    pub id: usize,
    pub instr: LLVM::Instr,
    pub nexts: Vec<usize>,
}
type IGNode = InstructionsGraphNode;

fn fix_function(fun: &LLVM::Function) -> LLVM::Function {
    use LLVM::Instr::*;

    let instrs = blocks_to_instructions(&fun.body);
    let mut aliases = HashMap::new();
    for i in &instrs {
        match i {
            Sext { src, dest } |
            Bitcast { src, dest } => {
                aliases.insert(LLVM::Value::from(dest.1.clone()), src.1.clone());
            },
            _ => {},
        }
    }
    for (dest, mut src) in aliases.clone() {
        let mut check = true;
        while check {
            check = false;
            if aliases.contains_key(&src) {
                src = aliases.get(&src).unwrap().clone();
                check = true;
            }
        }
        aliases.insert(dest.clone(), src.clone());
    }
    let fix_value = |val| {
        match aliases.get(&val) {
            Some(new_val) => new_val.clone(),
            _ => val.clone(),
        }
    };

    let mut new_instrs: Vec<LLVM::Instr> = vec![];
    for i in &instrs {
        let new_instr: LLVM::Instr;
        match i.clone() {
            Alloc { .. } |
            ReturnVoid |
            Unreachable |
            Branch(LLVM::Branch::Direct { .. }) |
            Label { .. }  => {
                new_instr = i.clone();
            },

            Sext { .. } |
            Bitcast { .. } => {
                continue;
            },

            Load { src, dest } => {
                let new_val = fix_value(src.1.into());
                if let LLVM::Value::Register(new_val) = new_val {
                    new_instr = Load {
                        src: (src.0, new_val),
                        dest,
                    };
                } else {
                    panic!("should not happen");
                }
            },

            Store { src, dest } => {
                let new_src = fix_value(src.1);
                let new_dest = fix_value(dest.1.into());
                if let LLVM::Value::Register(new_dest) = new_dest {
                    new_instr = Store {
                        src: (src.0, new_src),
                        dest: (dest.0, new_dest),
                    };
                } else {
                    panic!("should not happen");
                }
            },

            Compare { dest_reg, op, ty, val_lhs, val_rhs } => {
                let new_val_lhs = fix_value(val_lhs);
                let new_val_rhs = fix_value(val_rhs);

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
                    let new_val = fix_value(val);
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
                let new_val = fix_value(val.clone());
                new_instr = Branch(LLVM::Branch::Conditional {
                    ty,
                    val: new_val,
                    true_label,
                    false_label,
                });
            },

            Arithm { dest, op, val_lhs, val_rhs } => {
                let new_val_lhs = fix_value(val_lhs);
                let new_val_rhs = fix_value(val_rhs);

                new_instr = Arithm {
                    dest,
                    op,
                    val_lhs: new_val_lhs.clone(),
                    val_rhs: new_val_rhs.clone(),
                };
            },

            Phi { dest, preds } => {
                let new_preds: Vec<(LLVM::Value, LLVM::Label)> = preds.iter().cloned().map(|(val, lab)| {
                    let new_val = fix_value(val);
                    (new_val, lab)
                }).collect();

                new_instr = Phi {
                    dest,
                    preds: new_preds,
                };
            },

            GetElementPtr { dest, src, args } => {
                let new_src = fix_value(src.1);
                new_instr = GetElementPtr {
                    dest,
                    src: (src.0, new_src),
                    args: args.iter()
                        .map(|idx| (idx.0.clone(), fix_value(idx.1.clone())))
                        .collect(),
                };
            },

            Return { ty, val } => {
                let new_val = fix_value(val);
                new_instr = Return {
                    ty,
                    val: new_val,
                };
            },
        }
        new_instrs.push(new_instr);
    }
    let new_body = instructions_to_blocks(&new_instrs);

    LLVM::Function {
        ret_ty: fun.ret_ty.clone(),
        name: fun.name.clone(),
        args: fun.args.clone(),
        body: new_body.clone(),
    }
}

pub fn liveness_analysis(fun: &LLVM::Function) -> HashMap<usize, (HashSet<LLVM::Register>, HashSet<LLVM::Register>)> {
    let new_fun = fix_function(fun);
    let mut live_sets = HashMap::new();
    let instr_cfg_graph = IGraph::from(&new_fun);

    let mut defs = HashMap::new();
    let mut uses = HashMap::new();
    for node in &instr_cfg_graph.nodes {
        defs.insert(node.id.clone(), definitions_of(&node.instr));
        uses.insert(node.id.clone(), uses_of(&node.instr));
        live_sets.insert(node.id.clone(), (HashSet::new(), HashSet::new()));
    }

    let mut changed = true;
    while changed {
        changed = false;
        for node in &instr_cfg_graph.nodes {
            let (in_set, out_set) = live_sets.get(&node.id).unwrap();
            let use_set = uses.get(&node.id).unwrap();
            let def_set = defs.get(&node.id).unwrap();

            let set_diff = out_set.difference(&def_set).cloned().collect();
            let new_in_set = use_set.union(&set_diff).cloned().collect();

            let mut new_out_set = HashSet::new();
            for succ_id in &node.nexts {
                let (succ_in_set, _) = live_sets.get(&succ_id).unwrap();
                new_out_set = new_out_set.union(&succ_in_set).cloned().collect();
            }

            if in_set != &new_in_set || out_set != &new_out_set {
                changed = true;
            }

            live_sets.insert(node.id.clone(), (new_in_set, new_out_set));
        }
    }

    live_sets
}
