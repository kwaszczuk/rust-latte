use std::env;
use std::fs;
use std::panic;
use std::fmt;
use std::process;

// pub extern crate compiler;
pub extern crate analyzer;
pub extern crate parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Error: expecting 1 arguments got {}", args.len() - 1);
        process::exit(1);
    }
    let filename = &args[1];

    let ast_tree = parser::parse_file(filename)
        .unwrap_or_else(|e| {
            eprintln!("Parsing error:\n{:?}", e);
            process::exit(1)
        });

    match analyzer::run(&ast_tree) {
        Some(errors)=> {
            eprintln!("ERROR");
            analyzer::prettyprint_errors(errors, filename);
            process::exit(1)
        }
        None => {}
    }
    eprintln!("OK");
}
