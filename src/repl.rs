use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{self, Write};

pub fn start() {
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
                        println!("{}", prog);
                    }
                    Err(err) => {
                        println!("Error: {}", err);
                    }
                }
            }
            Err(err) => {
                print!("Error: {}", err);
            }
        }
    }
}
