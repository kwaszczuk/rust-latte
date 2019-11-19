use std::env;
use std::fs;
use std::panic;
use std::fmt;
use std::process;

pub extern crate compiler;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Error: expecting 1 arguments got {}", args.len() - 1);
        process::exit(1);
    }
    let filename = &args[1];

    compiler::run(filename)
}
