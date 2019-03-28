#![allow(dead_code)]

use self::Expr::*;
use ast::{ClassVarDec, Expr, KeyConstant, StaticOrField, Stmt, Stmts, Subroutine, VarType};
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

    fn expr_list(&mut self) -> Vec<Expr> {
        if let Symbol(')') = self.peek() {
            self.get();
            vec![]
        } else {
            let mut v = vec![self.expr()];
            while let Symbol(',') = self.peek() {
                self.get();
                v.push(self.expr());
            }
            self.expect(Symbol(')'));
            v
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
            Ident(ref s) if Symbol('(') == self.peek() => {
                self.get();
                Expr::SubroutineCall(s.clone(), self.expr_list())
            }
            Ident(ref obj_name) if Symbol('.') == self.peek() => {
                self.get();
                if let Ident(method_name) = self.get() {
                    self.expect(Symbol('('));
                    Expr::ObjectSubroutineCall(obj_name.clone(), method_name, self.expr_list())
                } else {
                    panic!(
                        "Tried to parse around {} as method call but can't",
                        obj_name
                    );
                }
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

    fn do_stmt(&mut self) -> Stmt {
        self.get();
        let stmt = Stmt::Do(self.expr());
        self.expect(Symbol(';'));
        stmt
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
            KeyWord(KeyWordKind::Do) => self.do_stmt(),
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

    fn get_var_type(&mut self) -> VarType {
        match self.get() {
            KeyWord(KeyWordKind::Int) => VarType::Int,
            KeyWord(KeyWordKind::Char) => VarType::Char,
            KeyWord(KeyWordKind::Boolean) => VarType::Boolean,
            Ident(typ) => VarType::Defined(typ),
            _ => panic!("Unexpected VarType"),
        }
    }

    fn var_dec(&mut self) -> Subroutine {
        self.get();
        let var_type = self.get_var_type();
        let vars = {
            let mut v = vec![];
            if let Ident(name) = self.get() {
                v.push(name);
                while let Symbol(',') = self.peek() {
                    self.get();
                    if let Ident(name) = self.get() {
                        v.push(name);
                    } else {
                        panic!("trailing comma!");
                    }
                }
                v
            } else {
                panic!("Expected ident!");
            }
        };
        self.expect(Symbol(';'));
        Subroutine::VarDec(var_type, vars)
    }

    fn subroutine_body(&mut self) -> Vec<Subroutine> {
        self.expect(Symbol('{'));
        let mut subroutines = vec![];
        loop {
            if let Symbol('}') = self.peek() {
                self.get();
                break;
            } else {
                let body = if let KeyWord(KeyWordKind::Var) = self.peek() {
                    self.var_dec()
                } else {
                    Subroutine::Stmts(self.stmts())
                };
                subroutines.push(body);
            }
        }
        subroutines
    }

    fn class_var_dec(&mut self) -> ClassVarDec {
        let static_or_field = match self.get() {
            KeyWord(KeyWordKind::Static) => StaticOrField::Static,
            KeyWord(KeyWordKind::Field) => StaticOrField::Field,
            _ => panic!("expected static or field"),
        };
        let var_type = self.get_var_type();
        let var_names = {
            if let Ident(var_name) = self.get() {
                let mut v = vec![var_name];
                while let Symbol(',') = self.peek() {
                    self.get();
                    if let Ident(var_name) = self.get() {
                        v.push(var_name);
                    } else {
                        panic!("expected other var name!");
                    }
                }
                v
            } else {
                panic!("expected var name!");
            }
        };
        self.expect(Symbol(';'));
        ClassVarDec::new(static_or_field, var_type, var_names)
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
    use self::StaticOrField::*;
    use super::Parser;
    use ast::{ClassVarDec, Expr, KeyConstant, StaticOrField, Stmt, Stmts, Subroutine, VarType};

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

        test(
            tokenize("x()"),
            Expr::SubroutineCall("x".to_string(), vec![]),
        );

        test(
            tokenize("x(1)"),
            Expr::SubroutineCall("x".to_string(), vec![I1]),
        );

        test(
            tokenize("x(1 + 1, 2)"),
            Expr::SubroutineCall(
                "x".to_string(),
                vec![Expr::binop('+', I1, I1), Expr::Integer(2)],
            ),
        );

        test(
            tokenize("point.calc()"),
            Expr::ObjectSubroutineCall("point".to_string(), "calc".to_string(), vec![]),
        );

        test(
            tokenize("point.calc(1+1, 2)"),
            Expr::ObjectSubroutineCall(
                "point".to_string(),
                "calc".to_string(),
                vec![Expr::binop('+', I1, I1), Expr::Integer(2)],
            ),
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

        test(
            tokenize("do x();"),
            Stmt::Do(Expr::SubroutineCall("x".to_string(), vec![])),
        );

        test(
            tokenize("do point.calc(1+1, 2);"),
            Stmt::Do(Expr::ObjectSubroutineCall(
                "point".to_string(),
                "calc".to_string(),
                vec![Expr::binop('+', I1, I1), Expr::Integer(2)],
            )),
        );
    }

    #[test]
    fn stmts() {
        fn test(t: Vec<Token>, right: Stmts) {
            let mut p = Parser::new(t);
            assert_eq!(p.stmts(), right);
        }

        test(
            tokenize("if (true) { let x = 1; while (true) { return; }  }"),
            vec![Stmt::if_stmt(
                Expr::Keyword(True),
                vec![
                    Stmt::let_stmt("x".to_string(), None, Integer(1)),
                    Stmt::while_stmt(Expr::Keyword(True), vec![Stmt::Return(None)]),
                ],
                None,
            )],
        );
    }

    #[test]
    fn get_var_type() {
        fn test(t: Vec<Token>, right: VarType) {
            let mut p = Parser::new(t);
            assert_eq!(p.get_var_type(), right);
        }

        test(tokenize("int"), VarType::Int);

        test(tokenize("char"), VarType::Char);

        test(tokenize("boolean"), VarType::Boolean);

        test(tokenize("hoge"), VarType::Defined("hoge".to_string()));
    }

    #[test]
    fn class_var_dec() {
        fn test(t: Vec<Token>, right: ClassVarDec) {
            let mut p = Parser::new(t);
            assert_eq!(p.class_var_dec(), right);
        }

        test(
            tokenize("static int s;"),
            ClassVarDec::new(Static, VarType::Int, vec!["s".to_string()]),
        );

        test(
            tokenize("field int s, t;"),
            ClassVarDec::new(Field, VarType::Int, vec!["s".to_string(), "t".to_string()]),
        );

        test(
            tokenize("field typ s, t;"),
            ClassVarDec::new(
                Field,
                VarType::Defined("typ".to_string()),
                vec!["s".to_string(), "t".to_string()],
            ),
        );
    }

    #[test]
    fn subroutine_body() {
        use self::Expr::*;
        use self::Stmt::*;
        use self::Subroutine::*;
        fn test(t: Vec<Token>, right: Vec<Subroutine>) {
            let mut p = Parser::new(t);
            assert_eq!(p.subroutine_body(), right);
        }

        test(tokenize("{ var SquareGame game; let game = SquareGame.new(); do game.run(); do game.dispose(); return;}",), vec![
            VarDec(VarType::Defined("SquareGame".to_string()), vec!["game".to_string()]),
            Stmts(vec![
                Stmt::let_stmt("game".to_string(), None, ObjectSubroutineCall("SquareGame".to_string(), "new".to_string(), vec![])),
                Do(ObjectSubroutineCall("game".to_string(), "run".to_string(), vec![])),
                Do(ObjectSubroutineCall("game".to_string(), "dispose".to_string(), vec![])),
                Return(None)
            ])
        ]);
    }
}
