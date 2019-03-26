#![allow(dead_code)]

use self::Expr::*;
use ast::{Expr, KeyConstant, Stmt, Stmts};
use token::Token::*;
use token::*;

pub struct Parser {
    index: usize,
    tokens: Vec<Token>,
    result: Stmts,
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
            Token::StringConstant(s) => Expr::StringConstant(s),
            Ident(ref s) if Symbol('[') == self.peek() => {
                self.get();
                let expr = self.expr();
                self.expect(Symbol(']'));
                Expr::array_acc(s.clone(), expr)
            }
            Ident(s) => Identifier(s),
            Symbol('~') => Expr::unary('~', self.term()),
            Symbol('-') => Expr::unary('-', self.term()),
            Symbol('(') => {
                let expr = self.expr();
                self.expect(Symbol(')'));
                expr
            }
            KeyWord(kind) => Expr::keyword(kind),
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

    fn while_stmt(&mut self) -> Stmt {
        self.get();
        self.expect(Symbol('('));
        let cond = self.expr();
        self.expect(Symbol(')'));
        self.expect(Symbol('{'));
        let stmts = self.stmts();
        self.expect(Symbol('}'));
        Stmt::while_stmt(cond, stmts)
    }

    fn if_stmt(&mut self) -> Stmt {
        self.get();
        self.expect(Symbol('('));
        let cond = self.expr();
        self.expect(Symbol(')'));
        self.expect(Symbol('{'));
        let stmts = self.stmts();
        self.expect(Symbol('}'));
        if let KeyWord(KeyWordKind::Else) = self.peek() {
            self.get();
            self.expect(Symbol('{'));
            let els = self.stmts();
            self.expect(Symbol('}'));
            Stmt::if_stmt(cond, stmts, Some(els))
        } else {
            Stmt::if_stmt(cond, stmts, None)
        }
    }

    fn let_stmt(&mut self) -> Stmt {
        self.get();
        if let Ident(name) = self.get() {
            let acc = if let Symbol('[') = self.peek() {
                self.get();
                let acc = self.expr();
                self.expect(Symbol(']'));
                Some(acc)
            } else {
                None
            };
            self.expect(Symbol('='));
            let expr = self.expr();
            self.expect(Symbol(';'));
            Stmt::let_stmt(name, acc, expr)
        } else {
            panic!("Unexpected token!");
        }
    }

    fn return_stmt(&mut self) -> Stmt {
        self.get();
        if let Symbol(';') = self.peek() {
            self.get();
            Stmt::Return(None)
        } else {
            let expr = self.expr();
            self.expect(Symbol(';'));
            Stmt::Return(Some(expr))
        }
    }

    fn stmt(&mut self) -> Stmt {
        match self.peek() {
            KeyWord(KeyWordKind::Return) => self.return_stmt(),
            KeyWord(KeyWordKind::Let) => self.let_stmt(),
            KeyWord(KeyWordKind::If) => self.if_stmt(),
            KeyWord(KeyWordKind::While) => self.while_stmt(),
            _ => unimplemented!(),
        }
    }

    fn stmts(&mut self) -> Stmts {
        let mut v = vec![];
        while let KeyWord(_) = self.peek() {
            v.push(self.stmt())
        }
        v
    }

    fn expect(&mut self, t: Token) {
        assert_eq!(self.get(), t);
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

    use self::Expr::*;
    use self::KeyConstant::*;
    use super::Parser;
    use ast::{Expr, KeyConstant, Stmt};

    const I1: Expr = Integer(1);

    fn tokenize(s: &'static str) -> Vec<Token> {
        let mut l = Lexer::new(s.to_string());
        l.lex_all();
        l.result
    }

    #[test]
    fn term() {
        fn test(t: Vec<Token>, right: Expr) {
            let mut p = Parser::new(t);
            assert_eq!(p.term(), right);
        }

        test(tokenize("-1"), Expr::unary('-', Integer(1)));

        test(tokenize("~true"), Expr::unary('~', Expr::Keyword(True)));

        test(tokenize("true"), Expr::Keyword(True));

        test(tokenize("false"), Expr::Keyword(False));

        test(tokenize("this"), Expr::Keyword(This));

        test(tokenize("null"), Expr::Keyword(Null));

        test(tokenize("x"), Expr::Identifier("x".to_string()));

        test(
            tokenize("\"abcdef\""),
            Expr::StringConstant("abcdef".to_string()),
        );

        test(
            tokenize("x[1+1]"),
            Expr::array_acc("x".to_string(), Expr::binop('+', I1, I1)),
        );
    }

    #[test]
    fn expr() {
        fn test(t: Vec<Token>, right: Expr) {
            let mut p = Parser::new(t);
            assert_eq!(p.expr(), right);
        }

        test(tokenize("1+1"), Expr::binop('+', I1, I1));

        test(
            tokenize("-1+1"),
            Expr::binop('+', Expr::unary('-', Integer(1)), Integer(1)),
        );

        test(
            tokenize("1+(2+3)"),
            Expr::binop('+', I1, Expr::binop('+', Integer(2), Integer(3))),
        );
    }

    #[test]
    fn stmt() {
        fn test(t: Vec<Token>, right: Stmt) {
            let mut p = Parser::new(t);
            assert_eq!(p.stmt(), right);
        }

        test(tokenize("return 1;"), Stmt::Return(Some(Integer(1))));

        test(tokenize("return;"), Stmt::Return(None));

        test(
            tokenize("let x = 1;"),
            Stmt::let_stmt("x".to_string(), None, Integer(1)),
        );

        test(
            tokenize("let x = 1+1;"),
            Stmt::let_stmt("x".to_string(), None, Expr::binop('+', I1, I1)),
        );

        test(
            tokenize("let x[1] = 2;"),
            Stmt::let_stmt("x".to_string(), Some(I1), Integer(2)),
        );

        test(
            tokenize("if (true) { return; }"),
            Stmt::if_stmt(Expr::Keyword(True), vec![Stmt::Return(None)], None),
        );

        test(
            tokenize("if (true) { return; } else { return 2;}"),
            Stmt::if_stmt(
                Expr::Keyword(True),
                vec![Stmt::Return(None)],
                Some(vec![Stmt::Return(Some(Integer(2)))]),
            ),
        );

        test(
            tokenize("while (true) { return; }"),
            Stmt::while_stmt(Expr::Keyword(True), vec![Stmt::Return(None)]),
        );
    }
}
