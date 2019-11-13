pub mod grammar;

use std::fs;
use base::ast;

pub fn parse_file(filename: &str) -> Result<ast::Program, String> {
    let contents = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");
    match grammar::ProgramParser::new().parse(contents.as_ref()) {
        Err(err) => Err(format!("{:?}", err)),
        Ok(tree) => Ok(tree)
    }
}
