use crate::token::{Token, TokenType};

pub struct Lexer {
    input: Box<[u8]>,
    position: u32,
    read_position: u32,
    ch: u8,
}

impl Lexer {
    pub fn new(input: Box<[u8]>) -> Self {
        return Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
    }

    pub fn next_token(&mut self) -> Token {
        self.read_char();
        match char::from(self.ch) {
            '=' => return Token::new(TokenType::ASSIGN, self.ch),
            '+' => return Token::new(TokenType::PLUS, self.ch),
            '(' => return Token::new(TokenType::LPAREN, self.ch),
            ')' => return Token::new(TokenType::RPAREN, self.ch),
            '{' => return Token::new(TokenType::LBRACE, self.ch),
            '}' => return Token::new(TokenType::RBRACE, self.ch),
            ',' => return Token::new(TokenType::COMMA, self.ch),
            ';' => return Token::new(TokenType::SEMICOLON, self.ch),
            _ => return Token::new(TokenType::EOF, self.ch),
        }
    }

    fn read_char(&mut self) {
        if self.read_position as usize >= self.input.len() {
            self.ch = 0
        } else {
            self.ch = self.input[self.read_position as usize]
        }

        self.position = self.read_position;
        self.read_position += 1;
    }
}

#[test]
fn test_next_token() {
    let mut l = Lexer::new("=+(){},;".to_string().as_bytes().into());
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

    for ex in expected {
        let tok = l.next_token();
        assert_eq!(ex, tok.token_type);
    }
}
