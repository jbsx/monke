use core::fmt;
use std::fmt::Display;

use phf::phf_map;

#[allow(non_camel_case_types)]
#[derive(Debug, Hash, Clone, PartialEq)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT,
    INT,

    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,

    EQ,
    NOT_EQ,

    // Delimiters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "fn" => TokenType::FUNCTION,
    "let" => TokenType::LET,
    "true" => TokenType::TRUE,
    "false" => TokenType::FALSE,
    "if" => TokenType::IF,
    "else" => TokenType::ELSE,
    "return" => TokenType::RETURN,
};

impl TokenType {
    pub fn as_str(&self) -> &'static str {
        match self {
            TokenType::ILLEGAL => "ILLEGAL",
            TokenType::EOF => "EOF",

            TokenType::IDENT => "IDENT",
            TokenType::INT => "INT",

            TokenType::ASSIGN => "=",
            TokenType::PLUS => "+",
            TokenType::MINUS => "-",
            TokenType::BANG => "!",
            TokenType::ASTERISK => "*",
            TokenType::SLASH => "/",

            TokenType::LT => "<",
            TokenType::GT => ">",

            TokenType::EQ => "==",
            TokenType::NOT_EQ => "!=",

            TokenType::COMMA => ",",
            TokenType::SEMICOLON => ";",

            TokenType::LPAREN => "(",
            TokenType::RPAREN => ")",
            TokenType::LBRACE => "{",
            TokenType::RBRACE => "}",

            TokenType::FUNCTION => "FUNCTION",
            TokenType::LET => "LET",
            TokenType::TRUE => "TRUE",
            TokenType::FALSE => "FALSE",
            TokenType::IF => "IF",
            TokenType::ELSE => "ELSE",
            TokenType::RETURN => "RETURN",
        }
    }
}

#[derive(Hash, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: u8,
}

impl Token {
    pub fn new(token_type: TokenType, literal: u8) -> Self {
        Token {
            token_type,
            literal,
        }
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_type.as_str())
    }
}
