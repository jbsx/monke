use crate::lexer::Lexer;
use crate::token::TokenType;
use std::io;

pub fn start() {
    let mut buf = String::new();
    loop {
        match io::stdin().read_line(&mut buf) {
            Ok(_) => {
                let mut l = Lexer::new(buf.as_bytes().into());
                let mut tok = l.next_token().unwrap();
                while tok.token_type != TokenType::EOF {
                    println!("{:?}", tok);
                    tok = l.next_token().unwrap();
                }
            }
            Err(err) => {
                print!("Error: {}", err);
            }
        }
    }
}
