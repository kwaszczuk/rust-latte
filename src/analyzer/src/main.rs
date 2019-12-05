use std::fs;
use std::env;
use std::fmt;
use std::process;
use std::path::Path;
use compiler;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    compiler::run(filename);
}
