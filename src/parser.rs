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

#[derive(PartialOrd, PartialEq, Debug)]
enum Presedence {
    LOWEST,
    EQUALS,      // == or !=
    LESSGREATER, // > or <
    SUM,         // + or -
    PRODUCT,     // * or /
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
    INDEX,       // array[index]
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
            let stmt = self.parse_statement()?;
            program.statements.push(stmt);
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
            self.next_token();
            return true;
        } else {
            self.peek_error(&ex_type);
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

    fn parse_statement(&mut self) -> Result<ast::Statement> {
        match self.curr_token.token_type {
            TokenType::LET => return self.parse_let_statement(),
            TokenType::RETURN => return self.parse_return_statement(),
            _ => return self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement> {
        let tok = self.curr_token.clone();

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

        self.next_token();

        let exp = self.parse_expression(Presedence::LOWEST)?;

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        return Ok(ast::Statement::LetStatement(ast::LetStatement::new(
            tok, ident, exp,
        )));
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement> {
        let tok = self.curr_token.clone();

        self.next_token();

        let exp = self.parse_expression(Presedence::LOWEST)?;

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        return Ok(ast::Statement::ReturnStatement(ast::ReturnStatement::new(
            tok, exp,
        )));
    }

    fn parse_expression_statement(&mut self) -> Result<ast::Statement> {
        let expr = self.parse_expression(Presedence::LOWEST)?;

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        return Ok(ast::Statement::ExpressionStatement(
            ast::ExpressionStatement::new(expr),
        ));
    }

    fn parse_expression(&mut self, pre: Presedence) -> Result<ast::Expression> {
        let mut res;
        match &self.curr_token.token_type {
            TokenType::IDENT => res = self.parse_identifier(),
            TokenType::INT => res = self.parse_integer_literal(),
            TokenType::BANG => res = self.parse_prefix_expression(),
            TokenType::MINUS => res = self.parse_prefix_expression(),
            _ => {
                return Err(anyhow!(
                    "No prefix found for token: {:?}",
                    self.curr_token.clone()
                ))
            }
        }

        while !self.peek_token_is(&TokenType::SEMICOLON) && pre < self.peek_precedence() {
            match &self.peek_token.token_type {
                TokenType::ASTERISK
                | TokenType::SLASH
                | TokenType::PLUS
                | TokenType::MINUS
                | TokenType::LT
                | TokenType::GT
                | TokenType::EQ
                | TokenType::NOT_EQ => {
                    self.next_token();
                    res = self.parse_infix_expression(res?);
                }
                _ => {
                    println!("infix operator not found");
                    return res;
                }
            }
        }

        return res;
    }

    fn parse_identifier(&mut self) -> Result<ast::Expression> {
        return Ok(ast::Expression::Identifier(ast::Identifier::new(
            self.curr_token.clone(),
            self.curr_token.literal.clone(),
        )));
    }

    fn parse_integer_literal(&mut self) -> Result<ast::Expression> {
        return Ok(ast::Expression::IntLiteral(ast::IntLiteral::new(
            self.curr_token.clone(),
            self.curr_token.literal.parse()?,
        )));
    }

    fn parse_prefix_expression(&mut self) -> Result<ast::Expression> {
        let op;
        match &self.curr_token.token_type {
            TokenType::MINUS => op = TokenType::MINUS,
            TokenType::BANG => op = TokenType::BANG,
            _ => return Err(anyhow!("No prefix found")),
        }

        self.next_token();

        let exp = self.parse_expression(Presedence::PREFIX)?;

        return Ok(ast::Expression::PrefixExpression(
            ast::PrefixExpression::new(op, exp),
        ));
    }

    fn get_precedence(tok_type: &TokenType) -> Presedence {
        match tok_type {
            TokenType::ASTERISK => return Presedence::PRODUCT,
            TokenType::SLASH => return Presedence::PRODUCT,
            TokenType::PLUS => return Presedence::SUM,
            TokenType::MINUS => return Presedence::SUM,
            TokenType::LT => return Presedence::LESSGREATER,
            TokenType::GT => return Presedence::LESSGREATER,
            TokenType::EQ => return Presedence::EQUALS,
            TokenType::NOT_EQ => return Presedence::EQUALS,
            _ => return Presedence::LOWEST,
        }
    }

    fn peek_precedence(&self) -> Presedence {
        return Self::get_precedence(&self.peek_token.token_type);
    }

    fn curr_precedence(&self) -> Presedence {
        return Self::get_precedence(&self.curr_token.token_type);
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Result<ast::Expression> {
        let prec = self.curr_precedence();
        let op = self.curr_token.token_type.clone();

        self.next_token();

        let right = self.parse_expression(prec)?;

        return Ok(ast::Expression::InfixExpression(ast::InfixExpression::new(
            left, op, right,
        )));
    }
}

#[test]
fn test_let_statements() {
    let expected = vec![
        ast::Statement::LetStatement(ast::LetStatement::new(
            Token::new(TokenType::LET, "let".to_string()),
            ast::Identifier::new(
                Token::new(TokenType::IDENT, "x".to_string()),
                "x".to_string(),
            ),
            ast::Expression::IntLiteral(ast::IntLiteral::new(
                Token::new(TokenType::INT, "5".to_string()),
                5,
            )),
        )),
        ast::Statement::LetStatement(ast::LetStatement::new(
            Token::new(TokenType::LET, "let".to_string()),
            ast::Identifier::new(
                Token::new(TokenType::IDENT, "y".to_string()),
                "y".to_string(),
            ),
            ast::Expression::IntLiteral(ast::IntLiteral::new(
                Token::new(TokenType::INT, "10".to_string()),
                10,
            )),
        )),
        ast::Statement::LetStatement(ast::LetStatement::new(
            Token::new(TokenType::LET, "let".to_string()),
            ast::Identifier::new(
                Token::new(TokenType::IDENT, "foobar".to_string()),
                "foobar".to_string(),
            ),
            ast::Expression::IntLiteral(ast::IntLiteral::new(
                Token::new(TokenType::INT, "838383".to_string()),
                838383,
            )),
        )),
    ];

    let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ";

    let mut l = lexer::Lexer::new(input.as_bytes().into());
    let mut p = Parser::new(&mut l);

    let program = p.parse_program().unwrap();

    assert_eq!(p.errors.len(), 0);
    assert_eq!(program.statements.len(), 3);

    for err in p.errors.iter() {
        println!("{}", err);
    }

    for (idx, stmt) in program.statements.iter().enumerate() {
        assert_eq!(stmt, &expected[idx]);
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

    assert_eq!(p.errors.len(), 0);
    assert_eq!(program.statements.len(), 3);

    let expected = vec![
        ast::Statement::ReturnStatement(ast::ReturnStatement::new(
            Token::new(TokenType::RETURN, "return".to_string()),
            ast::Expression::IntLiteral(ast::IntLiteral::new(
                Token::new(TokenType::INT, "5".to_string()),
                5,
            )),
        )),
        ast::Statement::ReturnStatement(ast::ReturnStatement::new(
            Token::new(TokenType::RETURN, "return".to_string()),
            ast::Expression::IntLiteral(ast::IntLiteral::new(
                Token::new(TokenType::INT, "10".to_string()),
                10,
            )),
        )),
        ast::Statement::ReturnStatement(ast::ReturnStatement::new(
            Token::new(TokenType::RETURN, "return".to_string()),
            ast::Expression::IntLiteral(ast::IntLiteral::new(
                Token::new(TokenType::INT, "993322".to_string()),
                993322,
            )),
        )),
    ];

    for (idx, stmt) in program.statements.iter().enumerate() {
        assert_eq!(stmt, &expected[idx]);
    }
}

#[test]
fn test_int_expression_statement() {
    let input = "5;";

    let mut l = lexer::Lexer::new(input.as_bytes().into());
    let mut p = Parser::new(&mut l);
    let program = p.parse_program().unwrap();

    assert_eq!(p.errors.len(), 0);
    assert_eq!(program.statements.len(), 1);
    assert_eq!(
        program.statements[0],
        ast::Statement::ExpressionStatement(ast::ExpressionStatement::new(
            ast::Expression::IntLiteral(ast::IntLiteral::new(
                Token::new(TokenType::INT, "5".to_string()),
                5
            ))
        ))
    )
}

#[test]
fn test_prefix_expression() {
    let expected = vec![
        ast::Statement::ExpressionStatement(ast::ExpressionStatement::new(
            ast::Expression::PrefixExpression(ast::PrefixExpression::new(
                TokenType::BANG,
                ast::Expression::IntLiteral(ast::IntLiteral::new(
                    Token::new(TokenType::INT, "5".to_string()),
                    5,
                )),
            )),
        )),
        ast::Statement::ExpressionStatement(ast::ExpressionStatement::new(
            ast::Expression::PrefixExpression(ast::PrefixExpression::new(
                TokenType::MINUS,
                ast::Expression::IntLiteral(ast::IntLiteral::new(
                    Token::new(TokenType::INT, "15".to_string()),
                    15,
                )),
            )),
        )),
    ];

    let tests = vec!["!5", "-15"];

    for (idx, t) in tests.iter().enumerate() {
        let mut lex = lexer::Lexer::new(t.as_bytes().into());
        let mut p = Parser::new(&mut lex);
        let program = p.parse_program().unwrap();

        assert_eq!(p.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0], expected[idx]);
    }
}

#[test]
fn test_infix_expression() {
    let tests = vec![
        "5 + 5;", "5 - 5;", "5 * 5;", "5 / 5;", "5 == 5;", "5 != 5;", "5 < 5;", "5 > 5;",
    ];

    for t in tests.iter() {
        let mut lex = lexer::Lexer::new(t.as_bytes().into());
        let mut p = Parser::new(&mut lex);
        let program = p.parse_program().unwrap();

        assert_eq!(p.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);
        if let ast::Statement::ExpressionStatement(exp_stmt) = &program.statements[0] {
            if let ast::Expression::InfixExpression(inf_exp) = &exp_stmt.expression {
                assert_eq!(
                    format!("{} {} {};", inf_exp.left, inf_exp.operator, inf_exp.right),
                    t.to_string(),
                );
            } else {
                panic!("Not an Infix Expression");
            }
        } else {
            panic!("Not an Expression Statement");
        }
    }
}
