#![allow(dead_code)]

use ast::{Expr, Statements};
use self::Expr::*;
use token::*;
use token::Token::*;

pub struct Parser {
    index: usize,
    tokens: Vec<Token>,
    result: Statements,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            index: 0,
            tokens,
            result: vec![],
        }
    }

    fn term(&mut self) -> Expr {
        let t = self.get();
        match t {
            IntegerConstant(i) => Integer(i),
            Symbol('~') => Expr::unary('~', self.term()),
            Symbol('-') => Expr::unary('-', self.term()),
            _ => panic!("term error! {:?}", t),
        }
    }

    fn expr(&mut self) -> Expr {
        let mut left = self.term();
        loop {
            if let Symbol(op) = self.peek() {
                match op {
                    '+' | '-' | '*' | '/' | '<' | '>' | '&' | '|' => {
                        self.get();
                        left = Expr::BinOP(op, Box::new(left), Box::new(self.term()));
                    }
                    _ => break,
                };
            } else {
                break;
            }
        }
        left
    }

    fn peek(&self) -> Token {
        if let Some(t) = self.tokens.get(self.index) {
            t.clone()
        } else {
            Token::EOF
        }
    }

    fn get(&mut self) -> Token {
        let t = self.peek();
        self.index += 1;
        t.clone()
    }
}

mod tests {

    use lexer::Lexer;
    use token::*;

    use ast::{Expr, Statement};
    use self::Expr::*;
    use super::Parser;

    fn tokenize(s: &'static str) -> Vec<Token> {
        let mut l = Lexer::new(s.to_string());
        l.lex_all();
        l.result
    }

    #[test]
    fn expr() {
        fn test(t: Vec<Token>, right: Expr) {
            let mut p = Parser::new(t);
            assert_eq!(p.expr(), right);
        }

        test(
            tokenize("1+1"),
            Expr::binop('+', Integer(1), Integer(1))
        );

        test(
            tokenize("-1"),
            Expr::unary('-', Integer(1))
        );

        test(
            tokenize("~1"),
            Expr::unary('~', Integer(1))
        );

        test(
            tokenize("-1+1"),
            Expr::binop('+', Expr::unary('-', Integer(1)), Integer(1))
        );
    }
}
