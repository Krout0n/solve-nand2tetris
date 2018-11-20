use self::Token::*;
use lexer::{KeyWordKind, Token};

fn print_format(kind: &'static str, value: String) {
    println!("<{}> {} </{}>", kind, value, kind);
}

pub fn output_tokens(v: Vec<Token>) {
    println!("<tokens>");
    for token in v {
        match token {
            IntegerConstant(i) => print_format("integerConstant", i.to_string()),
            Symbol(c) => print_format("symbol", c.to_string()),
            Ident(s) => print_format("identifier", s),
            StringConstant(s) => print_format("stringConstant", s),
            KeyWord(word) => print_format(
                "keyword",
                match word {
                    KeyWordKind::Class => "class",
                    KeyWordKind::Constructor => "constructor",
                    KeyWordKind::Function => "function",
                    KeyWordKind::Method => "method",
                    KeyWordKind::Field => "field",
                    KeyWordKind::Static => "static",
                    KeyWordKind::Var => "var",
                    KeyWordKind::Int => "int",
                    KeyWordKind::Char => "char",
                    KeyWordKind::Boolean => "boolean",
                    KeyWordKind::Void => "void",
                    KeyWordKind::True => "true",
                    KeyWordKind::False => "false",
                    KeyWordKind::Null => "null",
                    KeyWordKind::This => "this",
                    KeyWordKind::Let => "let",
                    KeyWordKind::Do => "do",
                    KeyWordKind::If => "if",
                    KeyWordKind::Else => "else",
                    KeyWordKind::While => "while",
                    KeyWordKind::Return => "return",
                }.to_string(),
            ),
            EOF => break,
        };
    }
    println!("</tokens>");
}

#[cfg(test)]
mod tests {
    use super::output_tokens;
    use lexer::{Lexer, Token};

    fn tokenize(s: &'static str) -> Vec<Token> {
        let mut l = Lexer::new(s.to_string());
        l.lex_all();
        l.result
    }

    #[test]
    fn hoge() {
        output_tokens(tokenize("if (1<2) { return a; }"));
        output_tokens(tokenize(
            r#"class Main {
    static boolean test;   
                           
    function void main() {
      var SquareGame game;
      let game = SquareGame.new();
      do game.run();
      do game.dispose();
      return;
    }

    function void test() {  
        var int i, j;       
        var String s;
        var Array a;
        if (false) {
            let s = "string constant";
            let s = null;
            let a[1] = a[2];
        }
        else {              
            let i = i * (-j);
            let j = j / (-2);
            let i = i | j;
        }
        return;
    }
}"#,
        ));
    }
}
