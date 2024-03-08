use crate::ast;
use crate::env::Environment;
use crate::eval::eval;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{self, Write};

pub fn start() {
    let mut env = Environment::new_env();

    loop {
        print!("> ");
        let _ = io::stdout().flush();

        let mut buf = String::new();

        match io::stdin().read_line(&mut buf) {
            Ok(_) => {
                let mut l = Lexer::new(buf.as_bytes().into());
                let mut p = Parser::new(&mut l);
                let program = p.parse_program();
                match program {
                    Ok(prog) => {
                        for stmt in prog.statements.iter() {
                            match eval(ast::Node::Statement(stmt.clone()), &mut env) {
                                Ok(val) => {
                                    println!("{val}");
                                }
                                Err(e) => {
                                    println!("Eval error: {}", e);
                                }
                            }
                        }
                    }
                    Err(e) => {
                        println!("Parse error: {}", e);
                    }
                }
            }
            Err(e) => {
                print!("Error: {}", e);
            }
        }
    }
}
