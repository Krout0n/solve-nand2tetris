use token::*;
use self::KeyWordKind::*;
use self::Token::*;

pub struct Lexer {
    index: usize,
    src: String,
    ch: Option<char>,
    pub result: Vec<Token>,
}

impl Lexer {
    pub fn new(src: String) -> Self {
        Lexer {
            index: 0,
            src,
            ch: None,
            result: Vec::new(),
        }
    }

    fn lex(&mut self) -> Token {
        self.skip_whitespace();
        self.read_char();
        if let None = self.ch {
            return EOF;
        };

        let ch = self.ch.unwrap();

        // ignore comment out
        if '/' == ch {
            self.read_char();
            match self.ch {
                Some('/') => {
                    loop {
                        match self.ch {
                            Some('\n') | None => break,
                            _ => self.read_char(),
                        };
                    }
                    return self.lex();
                }
                Some('*') => loop {
                    self.read_char();
                    if let Some('*') = self.ch {
                        self.read_char();
                        if let Some('/') = self.ch {
                            return self.lex();
                        }
                    } else if let None = self.ch {
                        panic!("unmatched comment!");
                    }
                },
                _ => (),
            };
            self.backtrack();
        }

        match ch {
            '0'..='9' => {
                let mut literal = String::new();
                while let Some('0'..='9') = self.ch {
                    literal.push_str(&self.ch.unwrap().to_string());
                    self.read_char();
                }
                self.backtrack();
                IntegerConstant(literal.parse::<u16>().unwrap())
            }
            'a'..='z' | 'A'..='Z' => {
                let mut literal = String::new();
                while let Some('0'..='9') | Some('a'..='z') | Some('A'..='Z') | Some('_') = self.ch
                {
                    literal.push_str(&self.ch.unwrap().to_string());
                    self.read_char();
                }
                self.backtrack();
                Lexer::lookup(literal)
            }
            '{' | '}' | '(' | ')' | '[' | ']' | '.' | ',' | ';' | '+' | '-' | '*' | '/' | '&'
            | '|' | '<' | '>' | '=' | '~' => Symbol(ch),
            '"' => {
                self.read_char();
                let mut literal = String::new();
                while Some('"') != self.ch {
                    literal.push_str(&self.ch.unwrap().to_string());
                    self.read_char();
                }
                StringConstant(literal)
            }
            _ => panic!("unexpected char!: '{:?}'", ch),
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some('\t') | Some(' ') | Some('\n') | Some('\r') = self.peek() {
            self.read_char();
        }
    }

    fn peek(&self) -> Option<char> {
        self.src.chars().nth(self.index)
    }

    fn read_char(&mut self) {
        self.ch = self.src.chars().nth(self.index);
        self.index += 1;
    }

    fn store(&mut self, t: Token) {
        self.result.push(t);
    }

    fn lookup(literal: String) -> Token {
        match &*literal {
            "class" => KeyWord(Class),
            "constructor" => KeyWord(Constructor),
            "function" => KeyWord(Function),
            "method" => KeyWord(Method),
            "field" => KeyWord(Field),
            "static" => KeyWord(Static),
            "var" => KeyWord(Var),
            "int" => KeyWord(Int),
            "char" => KeyWord(Char),
            "boolean" => KeyWord(Boolean),
            "void" => KeyWord(Void),
            "true" => KeyWord(True),
            "false" => KeyWord(False),
            "null" => KeyWord(Null),
            "this" => KeyWord(This),
            "let" => KeyWord(Let),
            "do" => KeyWord(Do),
            "if" => KeyWord(If),
            "else" => KeyWord(Else),
            "while" => KeyWord(While),
            "return" => KeyWord(Return),
            _ => Ident(literal),
        }
    }

    pub fn lex_all(&mut self) {
        loop {
            let t = self.lex();
            if let EOF = t {
                break;
            } else {
                self.store(t);
            }
        }
    }

    fn backtrack(&mut self) {
        self.index -= 1;
    }
}

#[cfg(test)]
mod tests {
    use self::KeyWordKind::*;
    use self::Token::*;
    use super::{KeyWordKind, Lexer, Token};

    fn s(st: &'static str) -> String {
        st.to_string()
    }

    fn l(s: String) -> Lexer {
        Lexer::new(s)
    }

    #[test]
    fn test_helper() {
        let input = s("hogefuga~");
        assert_eq!(&input, &"hogefuga~".to_string());
        let lex = l(input);
        assert_eq!(lex.src, s("hogefuga~"));
    }
    #[test]
    fn tokenize_int_const() {
        fn test(input: &'static str) {
            let st = s(input);
            let mut le = l(st);
            assert_eq!(le.lex(), IntegerConstant(input.parse::<u16>().unwrap()));
        }

        test("1");
        test("1345");
    }

    #[test]
    fn tokenize_ident() {
        fn test(input: &'static str) {
            let st = s(input);
            let mut le = l(st);
            assert_eq!(le.lex(), Ident(s(input)));
        }

        test("hogefuga");
        test("kuruton");
        test("a1b3j0");
    }

    #[test]
    fn tokenize_keyword() {
        fn test(input: &'static str) {
            let st = s(input);
            let mut le = l(st.clone());
            assert_eq!(le.lex(), Lexer::lookup(st));
        }

        test("class");
        test("constructor");
        test("function");
        test("method");
        test("field");
        test("static");
        test("var");
        test("int");
        test("char");
        test("boolean");
        test("void");
        test("true");
        test("false");
        test("null");
        test("this");
        test("let");
        test("do");
        test("if");
        test("else");
        test("while");
        test("return");
    }

    #[test]
    fn tokenize_eof() {
        let mut le = l(s(""));
        assert_eq!(le.lex(), EOF);
    }

    #[test]
    fn tokenize_symbol() {
        fn test(input: char) {
            let st = input.to_string();
            let mut le = l(st.clone());
            assert_eq!(le.lex(), Symbol(input));
        }

        test('{');
        test('}');
        test('(');
        test(')');
        test('[');
        test(']');
        test('.');
        test(',');
        test(';');
        test('+');
        test('-');
        test('*');
        test('/');
        test('&');
        test('|');
        test('<');
        test('>');
        test('=');
        test('~');
    }

    #[test]
    fn tokenize_stmt() {
        fn test(input: &'static str, right: Vec<Token>) {
            let mut le = l(s(input));
            le.lex_all();
            assert_eq!(le.result, right);
        }
        test(
            "if ( 1 < 2) return true;",
            vec![
                KeyWord(If),
                Symbol('('),
                IntegerConstant(1),
                Symbol('<'),
                IntegerConstant(2),
                Symbol(')'),
                KeyWord(Return),
                KeyWord(True),
                Symbol(';'),
            ],
        );
        test("123;", vec![IntegerConstant(123), Symbol(';')]);
    }

    #[test]
    fn string_constant() {
        fn test(input: &'static str) {
            let st = format!("\"{}\"", input);
            let mut le = l(st.clone());
            assert_eq!(le.lex(), StringConstant(input.to_string()));
        }
        test("hello");
        test("hello, world");
    }

    #[test]
    fn comment() {
        fn test(input: &'static str, right: Vec<Token>) {
            let mut le = l(s(input));
            le.lex_all();
            assert_eq!(le.result, right);
        }

        test("// kuruton is god", vec![]);
        test(
            r#" // kuruton is god
        if ( 1 < 2) return true;"#,
            vec![
                KeyWord(If),
                Symbol('('),
                IntegerConstant(1),
                Symbol('<'),
                IntegerConstant(2),
                Symbol(')'),
                KeyWord(Return),
                KeyWord(True),
                Symbol(';'),
            ],
        );
        test(
            "static boolean test;    // Added for testing -- there is no static keyword",
            vec![
                KeyWord(Static),
                KeyWord(Boolean),
                Ident("test".to_string()),
                Symbol(';'),
            ],
        );

        test(
            "/** Initializes a new Square Dance game and starts running it. */ class Main {}",
            vec![
                KeyWord(Class),
                Ident("Main".to_string()),
                Symbol('{'),
                Symbol('}'),
            ],
        );

        test(
            r#"/** Initializes a new
            Square Dance game and starts running it. */class Main {}"#,
            vec![
                KeyWord(Class),
                Ident("Main".to_string()),
                Symbol('{'),
                Symbol('}'),
            ],
        );
    }
}
