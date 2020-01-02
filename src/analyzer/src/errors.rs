use std::vec::Vec;
use crate::types::*;

use base::ast::{Operator};
use base::types::{Location};

use codespan::{Files, FileId};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{emit, Styles, Chars, DisplayStyle};
use parser::{get_content_wo_comments};

#[derive(Clone, Debug, PartialEq)]
pub enum CompilerError {
    TypeError(TypeError),
    SemanticError(SemanticError),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeError {
    VoidVariable {
        loc: Location
    },
    InvalidType {
        exp_ty: VarType,
        ty: VarType,
        loc: Location,
    },
    NotAFunction {
        ty: VarType,
        loc: Location,
    },
    OperatorUnsupported {
        op: Operator,
        loc: Location,
        ty: VarType,
    },
    MainReturnType {
        exp_ty: VarType,
        ty: VarType,
        loc: Location,
    },
    MainArgumentsCount {
        exp_cnt: usize,
        cnt: usize,
        loc: Location,
    },
    InvalidReturnType {
        ident: String,
        exp_ty: VarType,
        ty: VarType,
        loc: Location,
    },
    ReturnVoidType {
        loc: Location,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum SemanticError {
    MissingEntrypoint,
    VariableRedefiniton {
        ident: String,
        loc: Location,
        prev_loc: Location
    },
    UndeclaredVariable {
        ident: String,
        loc: Location,
    },
    FunctionArgumentsCount {
        ident: String,
        exp_cnt: usize,
        cnt: usize,
        loc: Location,
    },
    MissingReturn {
        ident: String,
        loc: Location,
    },
    IntegerOverflow {
        value: String,
        loc: Location,
    },
}

macro_rules! bold_it {
    ( $v:expr ) => {
        {
            Style::new().bold().paint($v.to_string())
        }
    };
}

use ansi_term::Style;

impl TypeError {
    fn to_diagnostic(&self, f_id: FileId) -> Diagnostic {
        use TypeError::*;
        match self {
            VoidVariable { loc } =>
                Diagnostic::new_error(
                    "variable declared with type `void`",
                    Label::new(f_id, loc, ""),
                ),
            InvalidType { exp_ty, ty, loc } =>
                Diagnostic::new_error(
                    "invalid type",
                    Label::new(f_id, loc, ""),
                ).with_notes(vec![
                    format!("expected type `{}`", bold_it![exp_ty]),
                    format!("   found type `{}`", bold_it![ty]),
                ]),
            NotAFunction { ty, loc } =>
                Diagnostic::new_error(
                    "call expression requires function",
                    Label::new(f_id, loc, format!("type `{}` found here",
                                                  ty)),
                ),
            OperatorUnsupported { op, loc, ty } =>
                Diagnostic::new_error(
                    format!("operation `{}` cannot be applied to type `{}`",
                            op, ty),
                    Label::new(f_id, loc, format!("")),
                ),
            MainReturnType { exp_ty, ty, loc } =>
                Diagnostic::new_error(
                    format!("`main` function requires return type `{}` but `{}` was found",
                            exp_ty, ty),
                    Label::new(f_id, loc, format!("")),
                ),
            MainArgumentsCount { exp_cnt, cnt, loc } =>
                Diagnostic::new_error(
                    format!("`main` function requires {} arguments but {} were defined",
                            exp_cnt, cnt),
                    Label::new(f_id, loc, format!("function defined here")),
                ),
            InvalidReturnType { ident, exp_ty, ty, loc } =>
                Diagnostic::new_error(
                    format!("function `{}` requires return type `{}` but `{}` was found",
                            ident, exp_ty, ty),
                    Label::new(f_id, loc, format!("return defined here")),
                ),
            ReturnVoidType { loc } =>
                Diagnostic::new_error(
                    format!("cannot return a `void` value"),
                    Label::new(f_id, loc, format!("has type `{}`", bold_it!["void"])),
                ),
        }
    }
}

impl SemanticError {
    fn to_diagnostic(&self, f_id: FileId) -> Diagnostic {
        use SemanticError::*;
        match self {
            MissingEntrypoint =>
                Diagnostic::new_error(
                    "program entrypoint `main` not found",
                    Label::new(f_id, 0..0, ""),
                ),
            VariableRedefiniton { ident, loc, prev_loc } =>
                Diagnostic::new_error(
                    format!("redefinition of `{}`", ident),
                    Label::new(f_id, loc, "declared here")
                ).with_secondary_labels(vec![Label::new(
                    f_id,
                    prev_loc,
                    "previously declared here",
                )]),
            UndeclaredVariable { ident, loc } =>
                Diagnostic::new_error(
                    format!("undeclared variable `{}`", ident),
                    Label::new(f_id, loc, ""),
                ),
            FunctionArgumentsCount { ident, exp_cnt, cnt, loc } =>
                Diagnostic::new_error(
                    format!("function `{}` takes {} arguments but {} were provided",
                            ident, exp_cnt, cnt),
                    Label::new(f_id, loc, format!("expected {} arguments", exp_cnt)),
                ),
            MissingReturn { ident, loc } =>
                Diagnostic::new_error(
                    format!("missing return in the function `{}` body", ident),
                    Label::new(f_id, loc, format!("return expected after this statement")),
                ),
            IntegerOverflow { value, loc } =>
                Diagnostic::new_error(
                    format!("integer overflow"),
                    Label::new(f_id, loc, format!("`int` value range is from -2147483648 to 2147483647")),
                ),
        }
    }
}

impl CompilerError {
    fn to_diagnostic(&self, f_id: FileId) -> Diagnostic {
        use CompilerError::*;
        match self {
            TypeError(e) => e.to_diagnostic(f_id),
            SemanticError(e) => e.to_diagnostic(f_id),
        }
    }
}

pub struct ErrorHandler {
    errors: Vec<CompilerError>
}

impl ErrorHandler {
    pub fn new() -> Self {
        ErrorHandler {
            errors: vec![]
        }
    }

    pub fn throw(&mut self, err: CompilerError) {
        self.errors.push(err);
    }

    pub fn has_error(&self) -> bool {
        self.errors.len() != 0
    }

    pub fn get_errors(&self) -> Vec<CompilerError> {
        self.errors.clone()
    }

    pub fn print_errors(&self) {
        for e in &self.errors {
            println!("{:?}", e);
        }
    }
}

pub trait ErrorHandling {
    fn throw(&mut self, err: CompilerError);

    fn has_error(&self) -> bool;

    fn get_errors(&self) -> Vec<CompilerError>;
}

pub fn prettyprint_errors(errors: Vec<CompilerError>, filename: &str) {
    let content = get_content_wo_comments(filename);

    let mut files = Files::new();

    let file_id = files.add(
        filename,
        content
    );

    let diagnostics: Vec<Diagnostic> = errors.iter().map(|e| e.to_diagnostic( file_id)).collect();

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config {
        display_style: DisplayStyle::Rich,
        tab_width: 4,
        styles: Styles::default(),
        chars: Chars {
            source_border_top_left: '-',
            source_border_top: '-',
            source_border_left: '|',
            source_border_left_break: ' ',

            note_bullet: '=',

            primary_caret: '^',
            secondary_caret: '^',

            multiline_primary_caret: '^',
            multiline_secondary_caret: '\'',
            multiline_top_left: '╭',
            multiline_top: '─',
            multiline_bottom_left: '╰',
            multiline_bottom: '─',
            multiline_left: '│',
        },
    };

    for diagnostic in &diagnostics {
        emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
    }
}
