use self::Token::*;
use lexer::{KeyWordKind, Token};

#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    Integer(u16),
    LetStmt(String, Box<AST>),
    Identifier(String),
    BinOP(char, Box<AST>, Box<AST>),
    UnaryOP(char, Box<AST>),
    Bool(bool),
}

use self::AST::*;

impl AST {
    fn let_stmt(name: String, expr: AST) -> Self {
        LetStmt(name, Box::new(expr))
    }

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
            IntegerConstant(i) => Integer(i),
            Ident(s) => Identifier(s),
            Symbol('-') => AST::unaryop('-', self.term()),
            Symbol('~') => AST::unaryop('~', self.term()),
            _ => panic!("term error! {:?}", t),
        }
    }

    fn expression(&mut self) -> AST {
        let mut left = self.term();
        loop {
            if let Symbol(op) = self.peek() {
                match op {
                    '+' | '-' | '*' | '/' | '<' | '>' | '&' | '|' => {
                        self.get();
                        left = AST::binop(op, left, self.term());
                    }
                    _ => break,
                };
            } else {
                break;
            }
        }
        left
    }

    fn let_stmt(&mut self) -> AST {
        self.get();
        if let Ident(name) = self.get() {
            assert_eq!(self.get(), Symbol('='));
            let stmt = AST::let_stmt(name, self.expression());
            assert_eq!(self.get(), Symbol(';'));
            stmt
        } else {
            panic!("expected ident token!");
        }
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
    use self::KeyWordKind::*;
    use self::Token::*;
    use self::AST::*;
    use super::{KeyWordKind, Parser, Token, AST};

    fn ident(x: &'static str) -> Token {
        Ident(x.to_string())
    }

    #[test]
    fn integer() {
        fn test(t: Token, ast: AST) {
            let mut p = Parser::new(vec![t]);
            assert_eq!(p.term(), ast);
        }
        test(IntegerConstant(10), Integer(10));
    }

    #[test]
    fn expression() {
        fn test(v: Vec<Token>, ast: AST) {
            let mut p = Parser::new(v);
            assert_eq!(p.expression(), ast);
        }
        test(
            vec![IntegerConstant(1), Symbol('+'), IntegerConstant(2)],
            AST::binop('+', Integer(1), Integer(2)),
        );
        test(
            vec![
                IntegerConstant(1),
                Symbol('+'),
                IntegerConstant(2),
                Symbol('*'),
                IntegerConstant(3),
            ],
            AST::binop('*', AST::binop('+', Integer(1), Integer(2)), Integer(3)),
        );
        test(
            vec![ident("x"), Symbol('+'), IntegerConstant(2)],
            AST::binop('+', Identifier("x".to_string()), Integer(2)),
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
            AST::unaryop('-', Integer(1)),
        );

        test(
            vec![Symbol('~'), ident("x")],
            AST::unaryop('~', Identifier("x".to_string())),
        )
    }

    #[test]
    fn let_stmt() {
        fn test(v: Vec<Token>, ast: AST) {
            let mut p = Parser::new(v);
            assert_eq!(p.let_stmt(), ast);
        }

        test(
            vec![
                KeyWord(Let),
                ident("x"),
                Symbol('='),
                IntegerConstant(1),
                Symbol(';'),
            ],
            AST::let_stmt("x".to_string(), Integer(1)),
        );
        test(
            vec![
                KeyWord(Let),
                ident("x"),
                Symbol('='),
                IntegerConstant(1),
                Symbol('-'),
                IntegerConstant(10),
                Symbol(';'),
            ],
            AST::let_stmt("x".to_string(), AST::binop('-', Integer(1), Integer(10))),
        );
    }
}
