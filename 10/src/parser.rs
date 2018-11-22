use self::Token::*;
use lexer::{KeyWordKind, Token};

#[derive(Clone, Debug, PartialEq)]
pub enum VarType {
    Int,
    Char,
    Boolean,
    UsrDef(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    Integer(u16),
    Identifier(String),
    SubroutineCall(String, Vec<AST>),
    SubScriptAcc(String, Box<AST>),
    MethodCall(String, String, Vec<AST>),
    VarDec(VarType, Vec<String>),
    SubroutineBody(Vec<AST>, Box<AST>),
    LetStmt(String, Box<AST>),
    IfStmt(Box<AST>, Box<AST>, Option<Box<AST>>),
    WhileStmt(Box<AST>, Box<AST>),
    DoStmt(Box<AST>),
    ReturnStmt(Option<Box<AST>>),
    Compound(Vec<AST>),
    BinOP(char, Box<AST>, Box<AST>),
    UnaryOP(char, Box<AST>),
    Bool(bool),
    Null,
    This,
}

use self::AST::*;

impl AST {
    fn subroutine_body(v: Vec<AST>, stmts: AST) -> Self {
        SubroutineBody(v, Box::new(stmts))
    }

    fn subscript_acc(s: String, expr: AST) -> Self {
        SubScriptAcc(s, Box::new(expr))
    }

    fn let_stmt(name: String, expr: AST) -> Self {
        LetStmt(name, Box::new(expr))
    }

    fn if_stmt(cond: AST, stmts: AST, else_stmt: Option<AST>) -> Self {
        let cond = Box::new(cond);
        let stmts = Box::new(stmts);
        if let Some(else_stmt) = else_stmt {
            IfStmt(cond, stmts, Some(Box::new(else_stmt)))
        } else {
            IfStmt(cond, stmts, None)
        }
    }

    fn while_stmt(cond: AST, stmt: AST) -> Self {
        WhileStmt(Box::new(cond), Box::new(stmt))
    }

    fn do_stmt(call: AST) -> Self {
        DoStmt(Box::new(call))
    }

    fn return_stmt(expr: Option<AST>) -> Self {
        if let Some(expr) = expr {
            AST::ReturnStmt(Some(Box::new(expr)))
        } else {
            AST::ReturnStmt(None)
        }
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
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            index: 0,
            tokens,
            result: vec![],
        }
    }

    fn expression_list(&mut self) -> Vec<AST> {
        self.expect(Symbol('('));
        let mut expr_list = vec![];
        loop {
            if let Symbol(')') = self.peek() {
                break;
            }
            expr_list.push(self.expression());
            if let Symbol(')') = self.peek() {
                break;
            }
            self.expect(Symbol(','));
        }
        self.expect(Symbol(')'));
        expr_list
    }

    fn term(&mut self) -> AST {
        let t = self.get();
        match t {
            IntegerConstant(i) => Integer(i),
            Ident(s) => match self.peek() {
                Symbol('(') => AST::SubroutineCall(s, self.expression_list()),
                Symbol('.') => {
                    self.get();
                    if let Ident(t) = self.get() {
                        AST::MethodCall(s, t, self.expression_list())
                    } else {
                        panic!("expected ident!");
                    }
                }
                Symbol('[') => {
                    self.get();
                    let expr = self.expression();
                    self.expect(Symbol(']'));
                    AST::subscript_acc(s, expr)
                }
                _ => Identifier(s),
            },
            Symbol('-') => AST::unaryop('-', self.term()),
            Symbol('~') => AST::unaryop('~', self.term()),
            Symbol('(') => {
                let expr = self.expression();
                self.expect(Symbol(')'));
                expr
            }
            KeyWord(KeyWordKind::True) => Bool(true),
            KeyWord(KeyWordKind::False) => Bool(false),
            KeyWord(KeyWordKind::Null) => Null,
            KeyWord(KeyWordKind::This) => This,
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
        let t = self.get();
        if let Ident(name) = t {
            self.expect(Symbol('='));
            let stmt = AST::let_stmt(name, self.expression());
            self.expect(Symbol(';'));
            stmt
        } else {
            panic!("expected ident token! {:#?}", t);
        }
    }

    fn if_stmt(&mut self) -> AST {
        self.expect(Symbol('('));
        let cond = self.expression();
        self.expect(Symbol(')'));
        self.expect(Symbol('{'));
        let stmts = self.statements();
        self.expect(Symbol('}'));
        if let KeyWord(KeyWordKind::Else) = self.peek() {
            self.get();
            self.expect(Symbol('{'));
            let else_stmts = self.statements();
            self.expect(Symbol('}'));
            AST::if_stmt(cond, stmts, Some(else_stmts))
        } else {
            AST::if_stmt(cond, stmts, None)
        }
    }

    fn while_stmt(&mut self) -> AST {
        self.expect(Symbol('('));
        let cond = self.expression();
        self.expect(Symbol(')'));
        self.expect(Symbol('{'));
        let stmts = self.statements();
        self.expect(Symbol('}'));
        AST::while_stmt(cond, stmts)
    }

    fn do_stmt(&mut self) -> AST {
        let term = self.term();
        self.expect(Symbol(';'));
        AST::do_stmt(term)
    }

    fn return_stmt(&mut self) -> AST {
        if let Symbol(';') = self.peek() {
            self.get();
            AST::return_stmt(None)
        } else {
            let expr = self.expression();
            self.expect(Symbol(';'));
            AST::return_stmt(Some(expr))
        }
    }

    fn statement(&mut self) -> AST {
        let t = self.get();
        if let KeyWord(word) = t {
            match word {
                KeyWordKind::Let => self.let_stmt(),
                KeyWordKind::If => self.if_stmt(),
                KeyWordKind::While => self.while_stmt(),
                KeyWordKind::Do => self.do_stmt(),
                KeyWordKind::Return => self.return_stmt(),
                unexpected => panic!("undefined statement! {:?}", unexpected),
            }
        } else {
            panic!("expected stmt! {:?}", t);
        }
    }

    pub fn statements(&mut self) -> AST {
        let mut stmts = vec![];
        loop {
            if self.peek() == Symbol('}') {
                break;
            } else {
                stmts.push(self.statement());
            }
        }
        AST::Compound(stmts)
    }

    fn var_dec(&mut self) -> AST {
        self.expect(KeyWord(KeyWordKind::Var));
        let typ = match self.get() {
            KeyWord(KeyWordKind::Int) => VarType::Int,
            KeyWord(KeyWordKind::Char) => VarType::Char,
            KeyWord(KeyWordKind::Boolean) => VarType::Boolean,
            err_token => panic!("vardecl: expected type keyword!! {:?}", err_token),
        };
        let mut vars = vec![];
        if let Ident(s) = self.get() {
            vars.push(s);
        } else {
            panic!("vardecl: expected ident!!");
        }
        while let Symbol(',') = self.peek() {
            self.get();
            if let Ident(s) = self.get() {
                vars.push(s);
            } else {
                panic!("vardecl: expected ident!!");
            }
        }
        self.expect(Symbol(';'));
        AST::VarDec(typ, vars)
    }

    fn subroutine_body(&mut self) -> AST {
        self.expect(Symbol('{'));
        let mut vars = vec![];
        loop {
            if let KeyWord(KeyWordKind::Var) = self.peek() {
                vars.push(self.var_dec());
            } else {
                break;
            }
        }
        let stmts = self.statements();
        self.expect(Symbol('}'));
        AST::subroutine_body(vars, stmts)
    }

    fn expect(&mut self, t: Token) {
        assert_eq!(self.get(), t);
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
    use super::{KeyWordKind, Parser, Token, VarType, AST};
    use lexer::Lexer;

    fn ident(x: &'static str) -> Token {
        Ident(x.to_string())
    }

    fn tokenize(input: &'static str) -> Vec<Token> {
        let mut l = Lexer::new(input.to_string());
        l.lex_all();
        l.result
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
        test(
            tokenize("1+(2+3)"),
            AST::binop('+', Integer(1), AST::binop('+', Integer(2), Integer(3))),
        );
        test(
            tokenize("1+2+3"),
            AST::binop('+', AST::binop('+', Integer(1), Integer(2)), Integer(3)),
        );
        test(
            tokenize("x[y[hoge()]]"),
            AST::subscript_acc(
                "x".to_string(),
                AST::subscript_acc(
                    "y".to_string(),
                    AST::SubroutineCall("hoge".to_string(), vec![]),
                ),
            ),
        )
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
            assert_eq!(p.statement(), ast);
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

    #[test]
    fn while_stmt() {
        fn test(v: Vec<Token>, ast: AST) {
            let mut p = Parser::new(v);
            assert_eq!(p.statement(), ast);
        }
        test(
            tokenize("while (1) { let x = 1;} "),
            AST::while_stmt(
                Integer(1),
                AST::Compound(vec![AST::let_stmt("x".to_string(), Integer(1))]),
            ),
        );
    }

    #[test]
    fn if_stmt() {
        fn test(v: Vec<Token>, ast: AST) {
            let mut p = Parser::new(v);
            assert_eq!(p.statement(), ast);
        }
        test(
            tokenize("if (1 < 2) { let x = 1;} "),
            AST::if_stmt(
                AST::binop('<', Integer(1), Integer(2)),
                AST::Compound(vec![AST::let_stmt("x".to_string(), Integer(1))]),
                None,
            ),
        );

        test(
            tokenize("if (1 < 2) { let x = 1; } else { let y= 2 ;}"),
            AST::if_stmt(
                AST::binop('<', Integer(1), Integer(2)),
                AST::Compound(vec![AST::let_stmt("x".to_string(), Integer(1))]),
                Some(AST::Compound(vec![AST::let_stmt(
                    "y".to_string(),
                    Integer(2),
                )])),
            ),
        );

        test(
            tokenize("if (1 < 2) { let x = 1; } else { if (2 < 3) { let y= 2 ;}}"),
            AST::if_stmt(
                AST::binop('<', Integer(1), Integer(2)),
                AST::Compound(vec![AST::let_stmt("x".to_string(), Integer(1))]),
                Some(AST::Compound(vec![AST::if_stmt(
                    AST::binop('<', Integer(2), Integer(3)),
                    AST::Compound(vec![AST::let_stmt("y".to_string(), Integer(2))]),
                    None,
                )])),
            ),
        );
    }

    #[test]
    fn keyword_constant() {
        fn test(t: Token, ast: AST) {
            let mut p = Parser::new(vec![t]);
            assert_eq!(p.term(), ast);
        }
        test(KeyWord(True), Bool(true));
        test(KeyWord(False), Bool(false));
        test(KeyWord(KeyWordKind::Null), AST::Null);
        test(KeyWord(KeyWordKind::This), AST::This);
    }

    #[test]
    fn statements() {
        fn test(v: Vec<Token>, ast: AST) {
            let mut p = Parser::new(v);
            assert_eq!(p.statement(), ast);
        }
        test(
            tokenize("if (1 < 2) { let x = 1; let y = 2;} "),
            AST::if_stmt(
                AST::binop('<', Integer(1), Integer(2)),
                AST::Compound(vec![
                    AST::let_stmt("x".to_string(), Integer(1)),
                    AST::let_stmt("y".to_string(), Integer(2)),
                ]),
                None,
            ),
        );
    }

    #[test]
    fn return_stmt() {
        fn test(v: Vec<Token>, ast: AST) {
            let mut p = Parser::new(v);
            assert_eq!(p.statement(), ast);
        }
        test(tokenize("return 1;"), AST::return_stmt(Some(Integer(1))));
        test(
            tokenize("return 1 + 2;"),
            AST::return_stmt(Some(AST::binop('+', Integer(1), Integer(2)))),
        );
        test(tokenize("return ;"), AST::return_stmt(None))
    }

    #[test]
    fn do_stmt() {
        fn test(input: &'static str, ast: AST) {
            let mut p = Parser::new(tokenize(input));
            assert_eq!(p.statement(), ast);
        }
        test(
            "do b.q();",
            AST::do_stmt(AST::MethodCall("b".to_string(), "q".to_string(), vec![])),
        );
        test(
            "do g(5,7);",
            AST::do_stmt(AST::SubroutineCall(
                "g".to_string(),
                vec![Integer(5), Integer(7)],
            )),
        );
    }

    #[test]
    fn var_dec() {
        fn test(v: Vec<Token>, ast: AST) {
            let mut p = Parser::new(v);
            assert_eq!(p.var_dec(), ast);
        }
        test(
            tokenize("var int i;"),
            AST::VarDec(VarType::Int, vec!["i".to_string()]),
        );
        test(
            tokenize("var char c;"),
            AST::VarDec(VarType::Char, vec!["c".to_string()]),
        );
        test(
            tokenize("var boolean b;"),
            AST::VarDec(VarType::Boolean, vec!["b".to_string()]),
        );
        test(
            tokenize("var int x,y;"),
            AST::VarDec(VarType::Int, vec!["x".to_string(), "y".to_string()]),
        )
    }

    #[test]
    fn subroutine_call() {
        fn test(v: &'static str, ast: AST) {
            let mut p = Parser::new(tokenize(v));
            assert_eq!(p.term(), ast);
        }

        fn subroutine_call(s: &'static str, expr_list: Vec<AST>) -> AST {
            AST::SubroutineCall(s.to_string(), expr_list)
        }

        fn method_call(s: &'static str, t: &'static str, v: Vec<AST>) -> AST {
            AST::MethodCall(s.to_string(), t.to_string(), v)
        }

        test("add()", subroutine_call("add", vec![]));
        test(
            "add(1, 2+3)",
            subroutine_call(
                "add",
                vec![AST::Integer(1), AST::binop('+', Integer(2), Integer(3))],
            ),
        );
        test("hoge.to_string()", method_call("hoge", "to_string", vec![]));
        test(
            "hoge.add(true, 2+3)",
            method_call(
                "hoge",
                "add",
                vec![AST::Bool(true), AST::binop('+', Integer(2), Integer(3))],
            ),
        );
    }

    #[test]
    fn subroutine_body() {
        fn test(input: &'static str, ast: AST) {
            let mut p = Parser::new(tokenize(input));
            assert_eq!(p.subroutine_body(), ast);
        }
        
        let x = "x".to_string();
        let y = "y".to_string();

        test(
            "{ var int x, y; }",
            AST::subroutine_body(
                vec![AST::VarDec(VarType::Int, vec![x.clone(), y.clone()])],
                AST::Compound(vec![]),
            ),
        );

        test(
            "{var int x, y; if (x < y) { return 1; } else { return 2;} } ",
            AST::subroutine_body(
                vec![AST::VarDec(VarType::Int, vec![x.clone(), y.clone()])],
                AST::Compound(vec![AST::if_stmt(
                    AST::binop('<', AST::Identifier(x.clone()), AST::Identifier(y.clone())),
                    AST::Compound(vec![AST::return_stmt(Some(AST::Integer(1)))]),
                    Some(AST::Compound(vec![AST::return_stmt(Some(AST::Integer(2)))])),
                )]),
            ),
        );

        test(
            "{var int x, y; if (x < y) { return 1; } else { return 2;} do calc(); } ",
            AST::subroutine_body(
                vec![AST::VarDec(VarType::Int, vec![x.clone(), y.clone()])],
                AST::Compound(vec![
                    AST::if_stmt(
                        AST::binop('<', AST::Identifier(x.clone()), AST::Identifier(y.clone())),
                        AST::Compound(vec![AST::return_stmt(Some(AST::Integer(1)))]),
                        Some(AST::Compound(vec![AST::return_stmt(Some(AST::Integer(2)))])),
                    ),
                    AST::do_stmt(AST::SubroutineCall("calc".to_string(), vec![])),
                ]),
            ),
        );

        test(
            "{ if (x < y) { return 1; } else { return 2;} } ",
            AST::subroutine_body(
                vec![],
                AST::Compound(vec![AST::if_stmt(
                    AST::binop('<', AST::Identifier(x.clone()), AST::Identifier(y.clone())),
                    AST::Compound(vec![AST::return_stmt(Some(AST::Integer(1)))]),
                    Some(AST::Compound(vec![AST::return_stmt(Some(AST::Integer(2)))])),
                )]),
            ),
        );
    }
}
