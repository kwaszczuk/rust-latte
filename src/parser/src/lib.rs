pub mod grammar;

use std::fs;
use std::fmt;
use base::ast;

// returns an input string without following comments:
// - single-line comments - // ...
// - multi-line comments - /* ... */
pub fn filter_comments(s: &str) -> String {
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

pub fn get_content_wo_comments(filename: &str) -> String {
    let contents = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");
    filter_comments(contents.as_ref())
}

pub fn parse_file(filename: &str) -> Result<ast::Program, String> {
    let contents = get_content_wo_comments(filename);
    match grammar::ProgramParser::new().parse(contents.as_ref()) {
        Err(err) => Err(format!("{:?}", err)),
        Ok(tree) => Ok(tree)
    }
}
