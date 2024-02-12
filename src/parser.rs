use anyhow::{anyhow, Result};

use crate::ast;
use crate::lexer;
use crate::token::{Token, TokenType};

pub struct Parser<'l> {
    l: &'l mut lexer::Lexer,
    errors: Vec<String>,
    curr_token: Token,
    peek_token: Token,
}

impl Parser<'_> {
    pub fn new(l: &mut lexer::Lexer) -> Parser {
        let mut p = Parser {
            l,
            errors: Vec::new(),
            curr_token: Token::new(TokenType::EOF, '\0'.to_string()),
            peek_token: Token::new(TokenType::EOF, '\0'.to_string()),
        };

        // Read two tokens, so curToken and peekToken are both set
        p.next_token();
        p.next_token();

        return p;
    }

    pub fn parse_program(&mut self) -> Result<ast::Program> {
        let mut program = ast::Program {
            statements: Vec::new(),
        };

        while self.curr_token.token_type != TokenType::EOF {
            let stmt = self.parse_statement();
            match stmt {
                Ok(val) => program.statements.push(val),
                Err(e) => self.errors.push(e.to_string()),
            }
            self.next_token();
        }

        return Ok(program);
    }

    fn next_token(&mut self) {
        let curr = core::mem::replace(&mut self.peek_token, self.l.next_token().unwrap());
        self.curr_token = curr;
    }

    fn expect_peek(&mut self, ex_type: TokenType) -> bool {
        if self.peek_token_is(&ex_type) {
            self.peek_error(&ex_type);
            self.next_token();
            return true;
        } else {
            return false;
        }
    }

    fn peek_token_is(&self, ex_type: &TokenType) -> bool {
        return self.peek_token.token_type == *ex_type;
    }

    fn peek_error(&mut self, t: &TokenType) {
        self.errors.push(format!(
            "Expected {:?}, found {:?}",
            t, self.curr_token.token_type
        ));
    }

    //fn errors(&self) -> &Vec<String>{}

    fn parse_statement(&mut self) -> Result<Box<dyn ast::Statement>> {
        match self.curr_token.token_type {
            TokenType::LET => return self.parse_let_statement(),
            TokenType::RETURN => return self.parse_return_statement(),
            _ => {
                return Err(anyhow!(
                    "Token Type not supported yet: {:?}",
                    self.curr_token.token_type
                ))
            }
        }
    }

    fn parse_let_statement(&mut self) -> Result<Box<dyn ast::Statement>> {
        let token = self.curr_token.clone();

        if !self.expect_peek(TokenType::IDENT) {
            return Err(anyhow!(
                "Expected identifier after 'let' keyword. Found: {}",
                self.peek_token.literal
            ));
        }

        let ident = ast::Identifier::new(self.curr_token.clone(), self.curr_token.literal.clone());

        if !self.expect_peek(TokenType::ASSIGN) {
            return Err(anyhow!(
                "Expected Assign (=) in let statement. Found: {}",
                self.peek_token.literal
            ));
        }

        //TODO: Skipping expression until the semicolon
        while self.curr_token.token_type != TokenType::SEMICOLON {
            self.next_token();
        }

        return Ok(Box::new(ast::LetStatement::new(
            token,
            ident,
            //Placeholder expression
            Box::new(ast::Identifier::new(
                self.curr_token.clone(),
                self.curr_token.literal.clone(),
            )),
        )));
    }

    fn parse_return_statement(&mut self) -> Result<Box<dyn ast::Statement>> {
        let temp_expression = ast::Identifier::new(
            Token::new(TokenType::IDENT, "temp_expression".to_string()),
            "temp_expression".to_string(),
        );

        let res = Box::new(ast::ReturnStatement::new(
            self.curr_token.clone(),
            Box::new(temp_expression),
        ));

        while self.curr_token.token_type != TokenType::SEMICOLON {
            self.next_token();
        }

        return Ok(res);
    }
}

#[test]
fn test_let_statements() {
    let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ";

    let mut l = lexer::Lexer::new(input.as_bytes().into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program().unwrap();

    for err in &p.errors {
        println!("Error: {}", err);
    }

    //assert_eq!(p.errors.len(), 0);
    assert_eq!(program.statements.len(), 3);

    let expected: Vec<&'static str> = vec!["let", "let", "let"];

    fn test_let_statement(stmt: &Box<dyn ast::Statement>) {
        assert_eq!(stmt.token_literal().unwrap(), "let");
        println!("{:?}", stmt);
    }

    for (idx, stmt) in program.statements.iter().enumerate() {
        let lit = stmt.token_literal().unwrap();
        assert_eq!(expected[idx], lit);
        test_let_statement(stmt);
    }
}

#[test]
fn test_return_statement() {
    let input = "
        return 5;
        return 10;
        return 993322;
    ";

    let mut l = lexer::Lexer::new(input.as_bytes().into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program().unwrap();

    for err in &p.errors {
        println!("Error: {}", err);
    }

    assert_eq!(p.errors.len(), 0);
    assert_eq!(program.statements.len(), 3);

    let expected: Vec<&'static str> = vec!["return", "return", "return"];

    for (idx, stmt) in program.statements.iter().enumerate() {
        assert_eq!(stmt.token_literal().unwrap(), expected[idx]);
    }
}
