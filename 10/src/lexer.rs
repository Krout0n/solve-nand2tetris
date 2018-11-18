#[derive(Clone, Debug, PartialEq)]
pub enum KeyWordKind {
    Class,
    Constructor,
    Function,
    Method,
    Field,
    Static,
    Var,
    Int,
    Char,
    Boolean,
    Void,
    True,
    False,
    Null,
    This,
    Let,
    Do,
    If,
    Else,
    While,
    Return,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolKind {
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Dot,
    Comma,
    Semicolon,
    Add,
    Minus,
    Star,
    Slash,
    Ampersand,
    VerticalBar,
    LT,
    GT,
    Assign,
    Tilde,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    KeyWord(KeyWordKind),
    Symbol(SymbolKind),
    IntegerConstant(u16),
    StringConstant(String),
    Ident(String),
}

use self::KeyWordKind::*;
use self::SymbolKind::*;
use self::Token::*;

pub struct Lexer {
    index: usize,
    src: String,
    result: Vec<Token>,
}

impl Lexer {
    fn new(src: String) -> Self {
        Lexer {
            index: 0,
            src,
            result: Vec::new(),
        }
    }

    fn lex(&mut self) -> Token {
        Symbol(LT)
    }

    fn skip_whitespace(&mut self) {
        while let Some('\t') | Some(' ') | Some('\n') | Some('\r') = self.ch {
            self.read_char();
        }
    }

    fn read_char(&mut self) {
        self.ch = self.src.chars().nth(self.position);
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn backtrack(&mut self) {
        self.read_position -= 1;
        self.position = self.read_position - 1;
    }

    fn peek_char(&self) -> Option<char> {
        self.src.chars().nth(self.position)
    }
}

#[cfg(test)]
mod tests {
    use self::Token::*;
    use super::{Lexer, Token};

    fn s(st: &'static str) -> String {
        st.to_string()
    }

    #[test]
    fn test_helper() {
        let input = s("hogefuga~");
        assert_eq!(input, "hogefuga~".to_string())
    }
    #[test]
    fn test_int_const() {
        let input = s("12345");
        let mut l = Lexer::new(input);
        assert_eq!(l.lex(), IntegerConstant(12345));
    }
}
