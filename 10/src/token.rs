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
pub enum Token {
    KeyWord(KeyWordKind),
    Symbol(char),
    IntegerConstant(u16),
    StringConstant(String),
    Ident(String),
    EOF,
}
