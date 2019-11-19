pub mod grammar;

use std::fs;
use std::fmt;
use base::ast;

// returns an input string without following comments:
// - single-line comments - // ...
// - multi-line comments - /* ... */
fn filter_comments(s: &str) -> String {
    let len = s.len();
    let mut ret = String::new();
    let mut i = 0;
    let chars: Vec<char> = s.chars().collect();
    while i < len {
        // handle string values
        if chars[i] == '"' {
            ret.push(chars[i]);
            while i < len {
                i += 1;
                ret.push(chars[i]);
                if chars[i] == '"' && chars[i - 1] != '\\' {
                    i += 1;
                    break;
                }
            }
        } else if chars[i] == '/' && i + 1 < len && (chars[i + 1] == '/' || chars[i + 1] == '*') {
            // single-line comments => //
            if chars[i + 1] == '/' {
                let mut j = i;
                while j < len {
                    if chars[j] == '\n' {
                        break;
                    }
                    j += 1;
                }
                i = j + 1;
            // multi-line comments => /* */
            } else if chars[i + 1] == '*' {
                let mut j = i + 1;
                let mut found = false;
                while j < len {
                    if chars[j] == '*' && j + 1 < len && chars[j + 1] == '/' {
                        i = j + 2;
                        found = true;
                        break;
                    }
                    j += 1;
                }
                if !found {
                    ret.push(chars[i]);
                    i += 1;

                }
            }
        } else {
            ret.push(chars[i]);
            i += 1;
        }
    }
    ret
}

pub fn parse_file(filename: &str) -> Result<ast::Program, String> {
    let contents = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");
    let wo_comments = filter_comments(contents.as_ref());
    println!("{}", wo_comments);
    match grammar::ProgramParser::new().parse(wo_comments.as_ref()) {
        Err(err) => Err(format!("{:?}", err)),
        Ok(tree) => Ok(tree)
    }
}
