use self::Token::*;
use token::KeyWordKind;
use token::Token;

type Identifier = String;
type ClassName = Identifier;
type VarName = Identifier;
type SubroutineName = Identifier;

#[derive(Debug, PartialEq)]
pub struct Class {
    name: ClassName,
    class_var_decs: Vec<ClassVarDec>,
    subroutine_decs: Vec<SubroutineDec>,
}

impl Class {
    pub fn new(name: ClassName, class_var_decs: Vec<ClassVarDec>, subroutine_decs: Vec<SubroutineDec>) -> Self {
        Class {
            name,
            class_var_decs,
            subroutine_decs,
        }
    }
}


#[derive(Debug, PartialEq)]
pub enum StaticOrField {
    Static,
    Field,
}

#[derive(Debug, PartialEq)]
pub enum VarType {
    Int,
    Char,
    Boolean,
    Void,
    Defined(String),
}

#[derive(Debug, PartialEq)]
pub struct ClassVarDec {
    static_or_field: StaticOrField,
    var_type: VarType,
    var_name: Vec<VarName>,
}

impl ClassVarDec {
    pub fn new(static_or_field: StaticOrField, var_type: VarType, var_name: Vec<VarName>) -> Self {
        ClassVarDec {
            static_or_field,
            var_type,
            var_name,
        }
    }
}

// TODO Renameしましょう
#[derive(Debug, PartialEq)]
pub enum SubroutineKind {
    Constructor,
    Function,
    Method,
}

impl SubroutineKind {
    pub fn by_token(t: Token) -> Self {
        match t {
            KeyWord(KeyWordKind::Constructor) => SubroutineKind::Constructor,
            KeyWord(KeyWordKind::Function) => SubroutineKind::Function,
            KeyWord(KeyWordKind::Method) => SubroutineKind::Method,
            _ => panic!(
                "Expected one of 'constructor' | 'function' | 'method', but got {:?}'",
                t
            ),
        }
    }
}

pub type Parameter = (VarType, VarName);

#[derive(Debug, PartialEq)]
pub enum Subroutine {
    VarDec(VarType, Vec<VarName>),
    Stmts(Stmts),
}

#[derive(Debug, PartialEq)]
pub struct SubroutineDec {
    kind: SubroutineKind,
    ret_type: VarType,
    subroutine_name: SubroutineName,
    parameter_list: Vec<Parameter>,
    subroutine_body: Vec<Subroutine>,
}

impl SubroutineDec {
    pub fn new(
        kind: SubroutineKind,
        ret_type: VarType,
        subroutine_name: SubroutineName,
        parameter_list: Vec<Parameter>,
        subroutine_body: Vec<Subroutine>,
    ) -> Self {
        SubroutineDec {
            kind,
            ret_type,
            subroutine_name,
            parameter_list,
            subroutine_body,
        }
    }
}

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

    pub fn while_stmt(cond: Expr, stmts: Stmts) -> Self {
        Stmt::While { cond, stmts }
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
    Identifier(VarName),
    StringConstant(String),
    ArrayAcc(VarName, Box<Expr>),
    SubroutineCall(VarName, Vec<Expr>),
    ObjectSubroutineCall(SubroutineName, VarName, Vec<Expr>),
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

    pub fn array_acc(name: VarName, expr: Expr) -> Self {
        Expr::ArrayAcc(name, Box::new(expr))
    }
}
