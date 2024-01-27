pub mod lexer;
pub mod token;
pub mod utils;

use lexer::Lexer;

fn main() {
    let mut l = Lexer::new(
        "
         let five = 5;
         let ten = 10;
         let add = fn(x, y) {
             x + y;
         };
         let result = add(five, ten);
        "
        .as_bytes()
        .into(),
    );

    let mut tok: token::Token = l.next_token().unwrap();

    while tok.token_type != token::TokenType::EOF {
        print!("{:?} ", &tok);
        tok = l.next_token().unwrap();
    }
}
