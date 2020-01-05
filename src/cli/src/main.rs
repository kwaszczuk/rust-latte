#[cfg(all(feature="emit-llvm", feature="emit-x86_64"))]
compile_error!("features `emit-llvm` and `emit-x86_64` are mutually exclusive");

use std::env;
use std::fs;
use std::process;
use std::path::Path;
use std::io::{self, Write};

pub extern crate analyzer;
pub extern crate parser;
pub extern crate llvm;

#[cfg(feature="emit-x86_64")]
pub extern crate x86;

fn get_output_name(filename: &str, ext: &str) -> String {
    let filename_wo_ext = Path::new(filename)
       .file_stem().unwrap()
       .to_str().unwrap();
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

    if ext == "" {
        format!("{}", path_wo_ext.join(filename_wo_ext).to_str().unwrap())
    } else {
        format!("{}.{}", path_wo_ext.join(filename_wo_ext).to_str().unwrap(), ext)
    }
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

    #[cfg(not(feature="no-analysis"))] {
        match analyzer::run(&ast_tree) {
            Some(errors)=> {
                eprintln!("ERROR");
                analyzer::prettyprint_errors(errors, filename);
                process::exit(1)
            }
            None => {}
        }
    }

    #[cfg(all(not(feature="no-codegen"), not(feature="no-analysis")))] {
        let llvm_code = llvm::compile(&ast_tree);
        #[cfg(feature="emit-llvm")] {
            save_llvm(&llvm_code, &filename);
        }
        #[cfg(feature="emit-x86_64")] {
            let x86_code = x86::compile(&llvm_code);
            save_x86(&x86_code, &filename);
        }
    }

    eprintln!("OK");
}

#[cfg(feature="emit-llvm")]
fn save_llvm(code: &llvm::instructions::Program, filename: &String) {
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
}

#[cfg(feature="emit-x86_64")]
fn save_x86(code: &x86::instructions::Program, filename: &String) {
    let output_filename_s = get_output_name(filename, "s");
    fs::write(output_filename_s.as_str(), format!("{}", code));

    let output_filename = get_output_name(filename, "");
    let link_out = process::Command::new("clang")
                     .arg("-o")
                     .arg(&output_filename)
                     .arg(&output_filename_s)
                     .arg("lib/runtime.o")
                     .output()
                     .expect("unable to link .s file and runtime.o");
    if !link_out.status.success() {
        eprintln!("ERROR");
        io::stderr().write_all(&link_out.stderr).unwrap();
        process::exit(1)
    }
}
