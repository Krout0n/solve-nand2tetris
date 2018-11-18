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
    EOF,
}

use self::KeyWordKind::*;
use self::SymbolKind::*;
use self::Token::*;

pub struct Lexer {
    index: usize,
    src: String,
    ch: Option<char>,
    result: Vec<Token>,
}

impl Lexer {
    fn new(src: String) -> Self {
        Lexer {
            index: 0,
            src,
            ch: None,
            result: Vec::new(),
        }
    }

    fn lex(&mut self) -> Token {
        // Symbol(LT)
        self.read_char();
        if let None = self.ch {
            return EOF;
        };

        let ch = self.ch.unwrap();
        match ch {
            '0'..='9' => {
                let mut literal = String::new();
                while let Some('0' ..= '9') = self.ch {
                    literal.push_str(&self.ch.unwrap().to_string());
                    self.read_char();
                }
                IntegerConstant(literal.parse::<u16>().unwrap())
            }
            _ => panic!("err"),
        }
    }
    fn skip_whitespace(&mut self) {
        while let Some('\t') | Some(' ') | Some('\n') | Some('\r') = self.ch {
            self.read_char();
        }
    }

    fn read_char(&mut self) {
        self.ch = self.src.chars().nth(self.index);
        self.index += 1;
    }

    fn store(&mut self, t: Token) {
        self.result.push(t);
    }
}

#[cfg(test)]
mod tests {
    use self::Token::*;
    use super::{Lexer, Token};

    fn s(st: &'static str) -> String {
        st.to_string()
    }

    fn l(s: String) -> Lexer {
        Lexer::new(s)
    }

    #[test]
    fn test_helper() {
        let input = s("hogefuga~");
        assert_eq!(&input, &"hogefuga~".to_string());
        let lex = l(input);
        assert_eq!(lex.src, s("hogefuga~"));
    }
    #[test]
    fn test_int_const() {
        let input = s("1");
        let mut le = l(input);
        assert_eq!(le.lex(), IntegerConstant(1));
        let input = s("12345");
        let mut le = l(input);
        assert_eq!(le.lex(), IntegerConstant(12345));
    }
}
