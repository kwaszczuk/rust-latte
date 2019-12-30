use std::env;
use std::fs;
use std::panic;
use std::fmt;
use std::process;
use std::path::Path;
use std::io::{self, Write};

// pub extern crate compiler;
pub extern crate analyzer;
pub extern crate parser;
pub extern crate llvm;

fn get_output_name(filename: &str, ext: &str) -> String {
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

    let filename_ret = format!("{}.{}", path_wo_ext.join(filename_wo_ext).to_str().unwrap(), ext);
    filename_ret
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
            eprintln!("ERROR");
            eprintln!("Parsing error: {}", e);
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
    let output_filename_ll = get_output_name(filename, "ll");
    fs::write(output_filename_ll.as_str(), format!("{}", code));

    let output_filename_bc = get_output_name(filename, "bc");
    let as_out = process::Command::new("llvm-as")
                     .arg("-o")
                     .arg(&output_filename_bc)
                     .arg(&output_filename_ll)
                     .output()
                     .expect("unable to write .bc file");
    if !as_out.status.success() {
        eprintln!("ERROR");
        io::stderr().write_all(&as_out.stderr).unwrap();
        process::exit(1)
    }

    let link_out = process::Command::new("llvm-link")
                     .arg("-o")
                     .arg(&output_filename_bc)
                     .arg(&output_filename_bc)
                     .arg("lib/runtime.bc")
                     .output()
                     .expect("unable to link .bc file and runtime.bc");
    if !link_out.status.success() {
        eprintln!("ERROR");
        io::stderr().write_all(&link_out.stderr).unwrap();
        process::exit(1)
    }

    eprintln!("OK");
}
