use token::KeyWordKind;

pub type Statements = Vec<Statement>;

pub enum Statement {
    Let {
        var_name: String,
        expr: Expr,
    },
    If {
        cond: Expr,
        stmts: Statements,
        els: Option<Statements>,
    },
    While {
        cond: Expr,
        stmts: Statements,
    },
    Do(Expr),
    Return(Expr),
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
