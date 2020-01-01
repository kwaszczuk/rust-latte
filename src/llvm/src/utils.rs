use std::collections::HashMap;
use crate::instructions as LLVM;

fn get_mappings<'a>() -> HashMap<&'a str, &'a str> {
    [
        ("\\\\", "\\5C"),
        ("\\b", "\\08"),
        ("\\f", "\\0C"),
        ("\\n", "\\0A"),
        ("\\t", "\\09"),
        ("\\\"", "\\22"),
    ].iter().cloned().collect()
}

pub fn escape_string(mut s: String) -> String {
    for (k, v) in get_mappings() {
        s = s.replace(k, v);
    }
    s.push_str("\\00");
    s
}

pub fn unescape_string(mut s: String) -> String {
    for (k, v) in get_mappings() {
        s = s.replace(v, k);
    }
    s = s.replace("\\00", "");
    s
}

pub fn length_after_escape(s: String) -> usize {
    let escaped_str = escape_string(s);
    let escapes: Vec<_> = escaped_str.matches("\\").collect();
    escaped_str.len() - escapes.len() * 2
}

pub fn instructions_to_blocks(instrs: &Vec<LLVM::Instr>) -> Vec<LLVM::Block> {
    let mut blocks = vec![];
    let mut current_block = LLVM::Block {
        label: LLVM::Label { name: "".to_string() },
        instrs: vec![],
    };

    for i in instrs {
        if let LLVM::Instr::Label { val, preds: _ } = i {
            blocks.push(current_block);
            current_block = LLVM::Block {
                label: val.clone(),
                instrs: vec![],
            };
        } else {
            current_block.instrs.push(i.clone());
        }
    }
    blocks.push(current_block);

    blocks
}

pub fn blocks_to_instructions(blocks: &Vec<LLVM::Block>) -> Vec<LLVM::Instr> {
    let mut instrs = vec![];
    for b in blocks {
        if !b.label.is_entry() {
            instrs.push(LLVM::Instr::Label {
                val: b.label.clone(),
                preds: vec![],
            });
        }
        instrs.extend(b.instrs.iter().cloned());
    }
    instrs
}
