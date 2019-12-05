// Source: https://gist.github.com/brendanzab/4c5e5e1836ecc3a46afd05ed046c695c
// The MIT License (MIT)

// Copyright (c) 2017 Brendan Zabarauskas

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

use std::str::CharIndices;

use source::BytePos;
use unicode_xid::UnicodeXID;

fn is_symbol(ch: char) -> bool {
    match ch {
        '&' | '!' | ',' | '.' | '=' | '/' | '%' | '>' | '<' | '-' | '|' | '+' | ';' | '*' => true,
        _ => false,
    }
}

fn is_ident_start(ch: char) -> bool {
    UnicodeXID::is_xid_start(ch) || ch == '_'
}

fn is_ident_continue(ch: char) -> bool {
    UnicodeXID::is_xid_continue(ch) || ch == '_'
}

fn is_dec_digit(ch: char) -> bool {
    ch.is_digit(10)
}

/// An error that occurred while lexing the source file
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    /// The location where the lexer error occured
    pub location: BytePos,
    /// The error code
    pub code: ErrorCode,
}

fn error<T>(location: BytePos, code: ErrorCode) -> Result<T, Error> {
    Err(Error { location, code })
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorCode {
    /// An unexpected character was encountered
    UnexpectedCharacter,
    /// Expected a binary literal
    ExpectedBinLiteral,
    /// Expected a hexidecimal literal
    ExpectedHexLiteral,
}

/// A token in the source file, to be emitted by the `Lexer`
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token<'input> {
    // Data
    Ident(&'input str),
    DecLiteral(i64),

    // Keywords
    If,
    Else,
    Where,

    // Symbols
    Ampersand,    // &&
    Pipe,         // ||
    Bang,         // !
    BangEqual,    // !=
    Colon,        // :
    Comma,        // ,
    Dot,          // .
    Equal,        // =
    EqualEqual,   // ==
    ForwardSlash, // /
    Percent,      // %
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=
    Minus,        // -
    Plus,         // +
    Semi,         // ;
    Star,         // *

    // Delimiters
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]
}

/// An iterator over a source string that yeilds `Token`s for subsequent use by
/// the parser
pub struct Lexer<'input> {
    src: &'input str,
    chars: CharIndices<'input>,
    lookahead: Option<(usize, char)>,
}

impl<'input> Lexer<'input> {
    /// Create a new lexer from the source string
    pub fn new(src: &'input str) -> Self {
        let mut chars = src.char_indices();

        Lexer {
            src,
            lookahead: chars.next(),
            chars,
        }
    }

    /// Return the next character in the source string
    fn lookahead(&self) -> Option<(BytePos, char)> {
        self.lookahead.map(|(index, ch)| (BytePos(index), ch))
    }

    /// Bump the current position in the source string by one character,
    /// returning the current character and byte position.
    fn bump(&mut self) -> Option<(BytePos, char)> {
        let current = self.lookahead();
        self.lookahead = self.chars.next();
        current
    }

    /// Return a slice of the source string
    fn slice(&self, start: BytePos, end: BytePos) -> &'input str {
        &self.src[start.0..end.0]
    }

    /// Test a predicate againt the next character in the source
    fn test_lookahead<F>(&self, mut pred: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.lookahead.map_or(false, |(_, ch)| pred(ch))
    }

    /// Consume characters while the predicate matches for the current
    /// character, then return the consumed slice and the end byte
    /// position.
    fn take_while<F>(&mut self, start: BytePos, mut keep_going: F) -> (BytePos, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(start, |ch| !keep_going(ch))
    }

    /// Consume characters until the predicate matches for the next character
    /// in the lookahead, then return the consumed slice and the end byte
    /// position.
    fn take_until<F>(&mut self, start: BytePos, mut terminate: F) -> (BytePos, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        while let Some((end, ch)) = self.lookahead() {
            if terminate(ch) {
                return (end, self.slice(start, end));
            } else {
                self.bump();
            }
        }

        let eof = BytePos(self.src.len());
        (eof, self.slice(start, eof))
    }

    /// Consume an identifier token
    fn ident(&mut self, start: BytePos) -> (BytePos, Token<'input>, BytePos) {
        let (end, ident) = self.take_while(start, is_ident_continue);

        let token = match ident {
            "if" => Token::If,
            "else" => Token::Else,
            "where" => Token::Where,
            ident => Token::Ident(ident),
        };

        (start, token, end)
    }

    /// Consume a decimal literal token
    fn dec_literal(&mut self, start: BytePos) -> (BytePos, Token<'input>, BytePos) {
        let (end, src) = self.take_while(start, is_dec_digit);
        let int = i64::from_str_radix(src, 10).unwrap();
        (start, Token::DecLiteral(int), end)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(BytePos, Token<'input>, BytePos), Error>;

    fn next(&mut self) -> Option<Result<(BytePos, Token<'input>, BytePos), Error>> {
        while let Some((start, ch)) = self.bump() {
            let end = start.map(|x| x + 1);

            return Some(match ch {
                ch if is_symbol(ch) => {
                    let (end, symbol) = self.take_while(start, is_symbol);

                    match symbol {
                        "&&" => Ok((start, Token::Ampersand, end)),
                        "||" => Ok((start, Token::Pipe, end)),
                        "!" => Ok((start, Token::Bang, end)),
                        "!=" => Ok((start, Token::BangEqual, end)),
                        "," => Ok((start, Token::Comma, end)),
                        "." => Ok((start, Token::Dot, end)),
                        "=" => Ok((start, Token::Equal, end)),
                        "==" => Ok((start, Token::EqualEqual, end)),
                        "/" => Ok((start, Token::ForwardSlash, end)),
                        "%" => Ok((start, Token::Percent, end)),
                        ">" => Ok((start, Token::Greater, end)),
                        ">=" => Ok((start, Token::GreaterEqual, end)),
                        "<" => Ok((start, Token::Less, end)),
                        "<=" => Ok((start, Token::LessEqual, end)),
                        "-" => Ok((start, Token::Minus, end)),
                        "+" => Ok((start, Token::Plus, end)),
                        ";" => Ok((start, Token::Semi, end)),
                        "*" => Ok((start, Token::Star, end)),
                        symbol if symbol.starts_with("//") => {
                            // Line comments
                            self.take_until(start, |ch| ch == '\n');
                            continue;
                        },
                        symbol if symbol.starts_with("/*") => {
                            // Line comments
                            self.take_until(start, |ch| ch == "*/");
                            continue;
                        },
                        _ => error(start, ErrorCode::UnexpectedCharacter),
                    }
                }
                '(' => Ok((start, Token::LParen, end)),
                ')' => Ok((start, Token::RParen, end)),
                '{' => Ok((start, Token::LBrace, end)),
                '}' => Ok((start, Token::RBrace, end)),
                '[' => Ok((start, Token::LBracket, end)),
                ']' => Ok((start, Token::RBracket, end)),
                ch if is_dec_digit(ch) => Ok(self.dec_literal(start)),
                ch if is_ident_start(ch) => Ok(self.ident(start)),
                ch if ch.is_whitespace() => continue,
                _ => error(start, ErrorCode::UnexpectedCharacter),
            });
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A handy macro to give us a nice syntax for declaring test cases
    ///
    /// This was inspired by the tests in the LALRPOP lexer
    macro_rules! test {
        ($src:expr, $($span:expr => $token:expr,)*) => {{
            let lexed_tokens: Vec<_> = Lexer::new($src).collect();
            let expected_tokens = vec![$({
                let start = BytePos($span.find("~").unwrap());
                let end = BytePos($span.rfind("~").unwrap() + 1);
                Ok((start, $token, end))
            }),*];

            assert_eq!(lexed_tokens, expected_tokens);
        }};
    }

    #[test]
    fn data() {
        test! {
            "  u8  1234 ",
            "  ~~       " => Token::Ident("u8"),
            "      ~~~~ " => Token::DecLiteral(1234),
        };
    }

    #[test]
    fn keywords() {
        test! {
            "  struct  union  where ",
            "  ~~~~~~               " => Token::Struct,
            "          ~~~~~        " => Token::Union,
            "                 ~~~~~ " => Token::Where,
        };
    }

    #[test]
    fn symbols() {
        test! {
            " & ! != : , . = == => / > >= < <= - | + ; * ",
            " ~                                          " => Token::Ampersand,
            "   ~                                        " => Token::Bang,
            "     ~~                                     " => Token::BangEqual,
            "        ~                                   " => Token::Colon,
            "          ~                                 " => Token::Comma,
            "            ~                               " => Token::Dot,
            "              ~                             " => Token::Equal,
            "                ~~                          " => Token::EqualEqual,
            "                   ~~                       " => Token::EqualGreater,
            "                      ~                     " => Token::ForwardSlash,
            "                        ~                   " => Token::Greater,
            "                          ~~                " => Token::GreaterEqual,
            "                             ~              " => Token::Less,
            "                               ~~           " => Token::LessEqual,
            "                                  ~         " => Token::Minus,
            "                                    ~       " => Token::Pipe,
            "                                      ~     " => Token::Plus,
            "                                        ~   " => Token::Semi,
            "                                          ~ " => Token::Star,
        }
    }

    #[test]
    fn delimiters() {
        test! {
            " ( ) { } [ ] ",
            " ~           " => Token::LParen,
            "   ~         " => Token::RParen,
            "     ~       " => Token::LBrace,
            "       ~     " => Token::RBrace,
            "         ~   " => Token::LBracket,
            "           ~ " => Token::RBracket,
        }
    }

    #[test]
    fn array_ty() {
        test! {
            "[u8; 34]",
            "~       " => Token::LBracket,
            " ~~     " => Token::Ident("u8"),
            "   ~    " => Token::Semi,
            "     ~~ " => Token::DecLiteral(34),
            "       ~" => Token::RBracket,
        };
    }

    #[test]
    fn struct_ty() {
        test! {
            "{ foo : u16 }",
            "~            " => Token::LBrace,
            "  ~~~        " => Token::Ident("foo"),
            "      ~      " => Token::Colon,
            "        ~~~  " => Token::Ident("u16"),
            "            ~" => Token::RBrace,
        };
    }
}
