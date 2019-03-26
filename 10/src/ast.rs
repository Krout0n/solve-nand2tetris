use token::KeyWordKind;

pub type Stmts = Vec<Stmt>;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let {
        var_name: String,
        acc: Option<Expr>,
        expr: Expr,
    },
    If {
        cond: Expr,
        stmts: Stmts,
        els: Option<Stmts>,
    },
    While {
        cond: Expr,
        stmts: Stmts,
    },
    Do(Expr),
    Return(Option<Expr>),
}

impl Stmt {
    pub fn let_stmt(var_name: String, acc: Option<Expr>, expr: Expr) -> Self {
        Stmt::Let {
            var_name,
            acc,
            expr,
        }
    }

    pub fn if_stmt(cond: Expr, stmts: Stmts, els: Option<Stmts>) -> Self {
        Stmt::If { cond, stmts, els }
    }
}

#[derive(Debug, PartialEq)]
pub enum KeyConstant {
    True,
    False,
    Null,
    This,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    BinOP(char, Box<Expr>, Box<Expr>),
    Unary(char, Box<Expr>),
    Integer(u16),
    Keyword(KeyConstant),
    Identifier(String),
}

impl Expr {
    pub fn binop(op: char, lhs: Self, rhs: Self) -> Self {
        Expr::BinOP(op, Box::new(lhs), Box::new(rhs))
    }

    pub fn unary(op: char, expr: Expr) -> Self {
        Expr::Unary(op, Box::new(expr))
    }

    pub fn keyword(kind: KeyWordKind) -> Self {
        Expr::Keyword(match kind {
            KeyWordKind::True => KeyConstant::True,
            KeyWordKind::False => KeyConstant::False,
            KeyWordKind::Null => KeyConstant::Null,
            KeyWordKind::This => KeyConstant::This,
            _ => panic!("Unexpected keyword! {:?}", kind),
        })
    }
}
