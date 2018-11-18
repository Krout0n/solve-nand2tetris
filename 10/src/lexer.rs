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
        self.read_char();
        if let None = self.ch {
            return EOF;
        };

        let ch = self.ch.unwrap();
        match ch {
            '0'..='9' => {
                let mut literal = String::new();
                while let Some('0'..='9') = self.ch {
                    literal.push_str(&self.ch.unwrap().to_string());
                    self.read_char();
                }
                IntegerConstant(literal.parse::<u16>().unwrap())
            }
            'a'..='z' | 'A'..='Z' => {
                let mut literal = String::new();
                while let Some('0'..='9') | Some('a'..='z') | Some('A'..='Z') = self.ch {
                    literal.push_str(&self.ch.unwrap().to_string());
                    self.read_char();
                }
                Lexer::lookup(literal)
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

    fn lookup(literal: String) -> Token {
        match &*literal {
            "class" => KeyWord(Class),
            "constructor" => KeyWord(Constructor),
            "function" => KeyWord(Function),
            "method" => KeyWord(Method),
            "field" => KeyWord(Field),
            "static" => KeyWord(Static),
            "var" => KeyWord(Var),
            "int" => KeyWord(Int),
            "char" => KeyWord(Char),
            "boolean" => KeyWord(Boolean),
            "void" => KeyWord(Void),
            "true" => KeyWord(True),
            "false" => KeyWord(False),
            "null" => KeyWord(Null),
            "this" => KeyWord(This),
            "let" => KeyWord(Let),
            "do" => KeyWord(Do),
            "if" => KeyWord(If),
            "else" => KeyWord(Else),
            "while" => KeyWord(While),
            "return" => KeyWord(Return),
            _ => Ident(literal),
        }
    }
}

#[cfg(test)]
mod tests {
    use self::KeyWordKind::*;
    use self::Token::*;
    use super::{KeyWordKind, Lexer, Token};

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
    fn tokenize_int_const() {
        fn test(input: &'static str) {
            let st = s(input);
            let mut le = l(st);
            assert_eq!(le.lex(), IntegerConstant(input.parse::<u16>().unwrap()));
        }

        test("1");
        test("1345");
    }

    #[test]
    fn tokenize_ident() {
        fn test(input: &'static str) {
            let st = s(input);
            let mut le = l(st);
            assert_eq!(le.lex(), Ident(s(input)));
        }

        test("hogefuga");
        test("kuruton");
        test("a1b3j0");
    }

    #[test]
    fn tokenize_keyword() {
        fn test(input: &'static str) {
            let st = s(input);
            let mut le = l(st.clone());
            assert_eq!(le.lex(), Lexer::lookup(st));
        }

        test("class");
        test("constructor");
        test("function");
        test("method");
        test("field");
        test("static");
        test("var");
        test("int");
        test("char");
        test("boolean");
        test("void");
        test("true");
        test("false");
        test("null");
        test("this");
        test("let");
        test("do");
        test("if");
        test("else");
        test("while");
        test("return");
    }
}
