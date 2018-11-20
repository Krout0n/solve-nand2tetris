extern crate jack_rs;

use jack_rs::codegen::output_tokens;
use jack_rs::lexer::Lexer;
use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let mut file = File::open(&args[1])?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    let mut l = Lexer::new(input);
    l.lex_all();
    output_tokens(l.result);
    Ok(())
}
