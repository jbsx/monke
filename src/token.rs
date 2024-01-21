mod Token {

    enum TokenType {
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

    impl TokenType {
        fn as_str(&self) -> &'static str {
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
                TokenType::LBRACE => "(",
                TokenType::RBRACE => ")",

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

    pub struct Token {
        TokenType: string,
        Literal: string,
    }
}
