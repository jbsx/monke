#![allow(dead_code)]
pub mod ast;
pub mod lexer;
pub mod parser;
pub mod repl;
pub mod token;
pub mod utils;

fn main() {
    repl::start();
}
