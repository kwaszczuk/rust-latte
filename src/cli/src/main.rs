use std::env;
use std::fs;
use std::panic;
use std::fmt;
use std::process;
use std::path::Path;

// pub extern crate compiler;
pub extern crate analyzer;
pub extern crate parser;
pub extern crate llvm;

fn get_output_name(filename: &str) -> String {
    let filename_wo_ext = Path::new(filename)
                               .file_stem()
                               .unwrap()
                               .to_str()
                               .unwrap();
    let path_wo_ext = Path::new(filename)
                           .parent()
                           .map_or(
                               Path::new("."),
                               |x|
                               if x.to_str().unwrap() == "" {
                                   Path::new(".")
                               } else {
                                   x
                               }
                            );

    let filename_ll = format!("{}.ll", path_wo_ext.join(filename_wo_ext).to_str().unwrap());
    filename_ll
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Error: expecting 1 arguments got {}", args.len() - 1);
        process::exit(1);
    }
    let filename = &args[1];

    let ast_tree = parser::parse_file(filename)
        .unwrap_or_else(|e| {
            eprintln!("Parsing error:\n{}", e);
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

    let code = llvm::compile(&ast_tree);
    let output_filename = get_output_name(filename);
    fs::write(output_filename.as_str(), format!("{}", code));

    eprintln!("OK");
}
