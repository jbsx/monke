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

pub static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "fn" => TokenType::FUNCTION,
    "let" => TokenType::LET,
    "true" => TokenType::TRUE,
    "false" => TokenType::FALSE,
    "if" => TokenType::IF,
    "else" => TokenType::ELSE,
    "return" => TokenType::RETURN,
};

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match &self {
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

            TokenType::FUNCTION => "FUNCTION", // TODO replace with how it would look like in actual
            TokenType::LET => "LET",
            TokenType::TRUE => "TRUE",
            TokenType::FALSE => "FALSE",
            TokenType::IF => "IF",
            TokenType::ELSE => "ELSE",
            TokenType::RETURN => "RETURN",
        };
        write!(f, "{}", s)
    }
}

#[derive(Hash, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Token {
            token_type,
            literal,
        }
    }

    pub fn keyword_from_literal(input: &str) -> Self {
        match KEYWORDS.get(input) {
            Some(val) => {
                return Token {
                    token_type: val.clone(),
                    literal: input.to_string(),
                }
            }
            None => {
                return Token {
                    token_type: TokenType::IDENT,
                    literal: input.to_string(),
                };
            }
        }
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "TokenType: {:?}, Literal: {}",
            self.token_type, self.literal
        )
    }
}
