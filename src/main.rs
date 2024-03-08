#![allow(dead_code)]
pub mod ast;
pub mod env;
pub mod eval;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod repl;
pub mod token;
pub mod utils;

fn main() {
    repl::start();
}
