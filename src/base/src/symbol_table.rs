use std::collections::{HashMap};
use std::vec::Vec;
use std::panic;

pub struct SymbolTable<T> {
    scopes: Vec<HashMap<String, T>>
}

impl<T> SymbolTable<T> {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()]
        }
    }

    pub fn get(&mut self, name: &String) -> Option<&T> {
        for st in self.scopes.iter().rev() {
            match st.get(name) {
                None => {}
                ret => { return ret; },
            }
        }
        None
    }

    pub fn get_from_current_scope(&mut self, name: &String) -> Option<&T> {
        self.get_current_scope().get(name)
    }

    pub fn contains(&self, name: &String) -> bool {
        for st in self.scopes.iter().rev() {
            match st.contains_key(name) {
                false => {}
                _ => { return true; },
            }
        }
        false
    }

    pub fn contains_in_current_scope(&mut self, name: &String) -> bool {
        self.get_current_scope().contains_key(name)
    }

    fn get_current_scope(&mut self) -> &mut HashMap<String, T> {
        self.scopes.last_mut().unwrap()
    }

    pub fn insert(&mut self, name: String, ent: T) {
        self.get_current_scope().insert(name, ent);
    }

    pub fn start_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        if self.scopes.len() == 1 {
            panic!("cannot remove last symbol table");
        } else {
            self.scopes.pop();
        }
    }
}
