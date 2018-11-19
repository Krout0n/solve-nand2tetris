use self::Token::*;
use lexer::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum KeyWordConstant {
    True,
    False,
    Null,
    This,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    Int(u16),
    Identifier(String),
    BinOP(char, Box<AST>, Box<AST>),
    UnaryOP(char, Box<AST>),
    KeyWord(KeyWordConstant),
}

use self::AST::*;

impl AST {
    fn unaryop(op: char, left: AST) -> Self {
        AST::UnaryOP(op, Box::new(left))
    }

    fn binop(op: char, left: AST, right: AST) -> Self {
        AST::BinOP(op, Box::new(left), Box::new(right))
    }
}

pub struct Parser {
    index: usize,
    tokens: Vec<Token>,
    result: Vec<AST>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser {
            index: 0,
            tokens,
            result: vec![],
        }
    }

    fn term(&mut self) -> AST {
        let t = self.get();
        match t {
            IntegerConstant(i) => Int(i),
            Ident(s) => Identifier(s),
            Symbol('-') => AST::unaryop('-', self.term()),
            Symbol('~') => AST::unaryop('~', self.term()),
            _ => panic!("term error! {:?}", t),
        }
    }

    fn expression(&mut self) -> AST {
        let mut left = self.term();
        loop {
            if let Symbol(c) = self.peek() {
                self.get();
                match c {
                    '+' | '-' | '*' | '/' | '<' | '>' | '&' | '|' => {
                        left = AST::binop(c, left, self.term());
                    }
                    _ => panic!("unexpected binop! {:?}", c),
                }
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
            EOF
        }
    }

    fn get(&mut self) -> Token {
        let t = self.peek();
        self.index += 1;
        t.clone()
    }
}

#[cfg(test)]
mod tests {
    use self::Token::*;
    use self::AST::*;
    use super::{Parser, Token, AST};

    fn ident(x: &'static str) -> Token {
        Ident(x.to_string())
    }

    #[test]
    fn integer() {
        fn test(t: Token, ast: AST) {
            let mut p = Parser::new(vec![t]);
            assert_eq!(p.term(), ast);
        }
        test(IntegerConstant(10), Int(10));
    }

    #[test]
    fn expression() {
        fn test(v: Vec<Token>, ast: AST) {
            let mut p = Parser::new(v);
            assert_eq!(p.expression(), ast);
        }
        test(
            vec![IntegerConstant(1), Symbol('+'), IntegerConstant(2)],
            AST::binop('+', Int(1), Int(2)),
        );
        test(
            vec![
                IntegerConstant(1),
                Symbol('+'),
                IntegerConstant(2),
                Symbol('*'),
                IntegerConstant(3),
            ],
            AST::binop('*', AST::binop('+', Int(1), Int(2)), Int(3)),
        );
        test(
            vec![ident("x"), Symbol('+'), IntegerConstant(2)],
            AST::binop('+', Identifier("x".to_string()), Int(2)),
        );
    }

    #[test]
    fn unary() {
        fn test(v: Vec<Token>, ast: AST) {
            let mut p = Parser::new(v);
            assert_eq!(p.term(), ast);
        }

        test(
            vec![Symbol('-'), IntegerConstant(1)],
            AST::unaryop('-', Int(1)),
        );

        test(
            vec![Symbol('~'), ident("x")],
            AST::unaryop('~', Identifier("x".to_string())),
        )
    }
}
