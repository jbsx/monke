pub mod lexer;
pub mod token;

use token::TokenType;

fn main() {
    let mut l = lexer::Lexer::new("=+(){},;".to_string().as_bytes().into());
    let expected = [
        TokenType::ASSIGN,
        TokenType::PLUS,
        TokenType::LPAREN,
        TokenType::RPAREN,
        TokenType::LBRACE,
        TokenType::RBRACE,
        TokenType::COMMA,
        TokenType::SEMICOLON,
    ];

    for _ in expected {
        let tok = l.next_token();
        print!("{:?}", tok);
    }
}
