use crate::token::{Token, TokenType};
use crate::utils::{is_digit, is_letter};
use anyhow::Result;

pub struct Lexer {
    input: Box<[u8]>,
    position: u32,
    read_position: u32,
    ch: u8,
}

impl Lexer {
    pub fn new(input: Box<[u8]>) -> Self {
        let mut res = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        res.read_char();
        return res;
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        let tok;
        match self.ch {
            b'=' => {
                if self.peek_char() == '=' {
                    tok = Token::new(TokenType::EQ, "==".to_string());
                    self.read_char();
                } else {
                    tok = Token::new(TokenType::ASSIGN, String::from_utf8([self.ch].into())?)
                }
            }
            b'+' => tok = Token::new(TokenType::PLUS, String::from_utf8([self.ch].into())?),
            b'-' => tok = Token::new(TokenType::MINUS, String::from_utf8([self.ch].into())?),
            b'!' => {
                if self.peek_char() == '=' {
                    tok = Token::new(TokenType::NOT_EQ, "!=".to_string());
                    self.read_char();
                } else {
                    tok = Token::new(TokenType::BANG, String::from_utf8([self.ch].into())?)
                }
            }
            b'*' => tok = Token::new(TokenType::ASTERISK, String::from_utf8([self.ch].into())?),
            b'/' => tok = Token::new(TokenType::SLASH, String::from_utf8([self.ch].into())?),

            b'<' => tok = Token::new(TokenType::LT, String::from_utf8([self.ch].into())?),
            b'>' => tok = Token::new(TokenType::GT, String::from_utf8([self.ch].into())?),

            b'(' => tok = Token::new(TokenType::LPAREN, String::from_utf8([self.ch].into())?),
            b')' => tok = Token::new(TokenType::RPAREN, String::from_utf8([self.ch].into())?),
            b'{' => tok = Token::new(TokenType::LBRACE, String::from_utf8([self.ch].into())?),
            b'}' => tok = Token::new(TokenType::RBRACE, String::from_utf8([self.ch].into())?),

            b',' => tok = Token::new(TokenType::COMMA, String::from_utf8([self.ch].into())?),
            b';' => tok = Token::new(TokenType::SEMICOLON, String::from_utf8([self.ch].into())?),
            b'\0' => tok = Token::new(TokenType::EOF, String::from_utf8([self.ch].into())?),
            _ => {
                if is_letter(self.ch) {
                    tok = Token::keyword_from_literal(&self.read_identifier());
                } else if is_digit(self.ch) {
                    tok = Token::new(TokenType::INT, self.read_number());
                } else {
                    print!("{}", self.ch);
                    tok = Token::new(TokenType::ILLEGAL, self.ch.to_string());
                }
            }
        }

        self.read_char();
        return Ok(tok);
    }

    fn read_char(&mut self) {
        if self.read_position as usize >= self.input.len() {
            self.ch = b'\0';
        } else {
            self.ch = self.input[self.read_position as usize]
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> char {
        return char::from(self.input[self.read_position as usize]);
    }

    fn read_identifier(&mut self) -> String {
        let start = self.position as usize;

        while (self.read_position as usize) < self.input.len()
            && is_letter(self.input[self.read_position as usize])
        {
            self.read_char();
        }

        return String::from_utf8(self.input[start..self.read_position as usize].to_vec()).unwrap();
    }

    fn read_number(&mut self) -> String {
        let start = self.position as usize;

        while (self.read_position as usize) < self.input.len()
            && is_digit(self.input[self.read_position as usize])
        {
            self.read_char();
        }

        return String::from_utf8(self.input[start..self.read_position as usize].to_vec()).unwrap();
    }

    fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\r' || self.ch == b'\n' {
            self.read_char()
        }
    }
}
