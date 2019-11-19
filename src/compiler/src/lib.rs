use std::fmt;
use std::fs;
use std::process;
use std::path::Path;
extern crate parser;
// mod instructions;
// mod operators;
// mod compiler;

pub fn compile(optimize: u8, source_file: &str) -> Result<String, String> {
    match parser::parse_file(source_file) {
        Ok(ast_tree) => {
            println!("{:?}", ast_tree);
            Ok(format!("Good"))
            // let ret = compiler::X86Compiler::run(&ast_tree, optimize, source_file);
            // match ret {
            //     Ok(code) => Ok(code),
            //     Err(e) => Err(format!("Compilation error:\n{:?}", e)),
            // }
        },
        Err(e) => Err(format!("Parsing error:\n{:?}", e)),
    }
}

pub fn run(filename: &str) {
    let code = compile(0, filename)
        .unwrap_or_else(|e| {
            eprintln!("Compilation failed!\n{:?}", e);
            process::exit(1)
        });

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

    // let filename_ll = format!("{}.ll", path_wo_ext.join(filename_wo_ext).to_str().unwrap());
    // fs::write(&filename_ll, &code)
    //     .expect("unable to write .ll file");

    // let filename_bc = format!("{}.bc", path_wo_ext.join(filename_wo_ext).to_str().unwrap());
    // process::Command::new("llvm-as")
    //                  .arg("-o")
    //                  .arg(&filename_bc)
    //                  .arg(&filename_ll)
    //                  .output()
    //                  .expect("unable to write .bc file");
}
