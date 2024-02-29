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
enum Precedence {
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
            self.errors.push(format!(
                "Expected {:?}, found {:?}",
                ex_type, self.peek_token.token_type
            ));
            return false;
        }
    }

    fn peek_token_is(&self, ex_type: &TokenType) -> bool {
        return &self.peek_token.token_type == ex_type;
    }

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

        let exp = self.parse_expression(Precedence::LOWEST)?;

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

        let exp = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        return Ok(ast::Statement::ReturnStatement(ast::ReturnStatement::new(
            tok, exp,
        )));
    }

    fn parse_expression_statement(&mut self) -> Result<ast::Statement> {
        let expr = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        return Ok(ast::Statement::ExpressionStatement(
            ast::ExpressionStatement::new(expr),
        ));
    }

    fn parse_block_statement(&mut self) -> Result<ast::Statement> {
        let mut res = ast::BlockStatement::new(std::vec::Vec::new());
        self.next_token();

        while self.curr_token.token_type != TokenType::RBRACE
            && self.curr_token.token_type != TokenType::EOF
        {
            let stmt = self.parse_statement()?;
            res.push(stmt);
            self.next_token();
        }

        return Ok(ast::Statement::BlockStatement(res));
    }

    fn parse_expression(&mut self, pre: Precedence) -> Result<ast::Expression> {
        let mut res;

        //Prefix Parse
        match &self.curr_token.token_type {
            TokenType::IDENT => res = self.parse_identifier(),
            TokenType::INT => res = self.parse_integer_literal(),
            TokenType::BANG => res = self.parse_prefix_expression(),
            TokenType::MINUS => res = self.parse_prefix_expression(),
            TokenType::TRUE | TokenType::FALSE => res = self.parse_boolean(),
            TokenType::LPAREN => res = self.parse_grouped_expression(),
            TokenType::IF => res = self.parse_if_expression(),
            TokenType::FUNCTION => res = self.parse_fn_literal(),
            _ => {
                return Err(anyhow!(
                    "No prefix found for token: {:?}",
                    self.curr_token.clone()
                ))
            }
        }

        //Infix parse
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
                TokenType::LPAREN => {
                    self.next_token();
                    res = self.parse_call_expression(res?);
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

        let exp = self.parse_expression(Precedence::PREFIX)?;

        return Ok(ast::Expression::PrefixExpression(
            ast::PrefixExpression::new(op, exp),
        ));
    }

    fn get_precedence(tok_type: &TokenType) -> Precedence {
        match tok_type {
            TokenType::LPAREN => return Precedence::CALL,
            TokenType::ASTERISK => return Precedence::PRODUCT,
            TokenType::SLASH => return Precedence::PRODUCT,
            TokenType::PLUS => return Precedence::SUM,
            TokenType::MINUS => return Precedence::SUM,
            TokenType::LT => return Precedence::LESSGREATER,
            TokenType::GT => return Precedence::LESSGREATER,
            TokenType::EQ => return Precedence::EQUALS,
            TokenType::NOT_EQ => return Precedence::EQUALS,
            _ => return Precedence::LOWEST,
        }
    }

    fn peek_precedence(&self) -> Precedence {
        return Self::get_precedence(&self.peek_token.token_type);
    }

    fn curr_precedence(&self) -> Precedence {
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

    fn parse_boolean(&self) -> Result<ast::Expression> {
        match &self.curr_token.token_type {
            TokenType::TRUE => return Ok(ast::Expression::Boolean(ast::Boolean::new(true))),
            TokenType::FALSE => return Ok(ast::Expression::Boolean(ast::Boolean::new(false))),
            _ => Err(anyhow!(
                "Could not parse boolean: Curr token is not of type boolean."
            )),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<ast::Expression> {
        self.next_token();

        let res = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(TokenType::RPAREN) {
            return Err(anyhow!(
                "Expected RPAREN ')'. Instead found '{}'",
                self.peek_token.token_type
            ));
        }

        return res;
    }

    fn parse_if_expression(&mut self) -> Result<ast::Expression> {
        self.expect_peek(TokenType::LPAREN);

        self.next_token();

        let condition = Box::new(self.parse_expression(Precedence::LOWEST)?);

        self.expect_peek(TokenType::RPAREN);
        self.expect_peek(TokenType::LBRACE);

        let consequence;

        //Could not find a better way to unwrap enum
        if let ast::Statement::BlockStatement(val) = self.parse_block_statement()? {
            consequence = val;
        } else {
            panic!();
        };

        if self.peek_token_is(&TokenType::ELSE) {
            self.next_token();
            self.expect_peek(TokenType::LBRACE);
            if let ast::Statement::BlockStatement(alternative) = self.parse_block_statement()? {
                return Ok(ast::Expression::IfExpression(ast::IfExpression {
                    condition,
                    consequence,
                    alternative: Some(alternative),
                }));
            } else {
                panic!()
            }
        } else {
            return Ok(ast::Expression::IfExpression(ast::IfExpression {
                condition,
                consequence,
                alternative: None,
            }));
        }
    }

    fn parse_fn_literal(&mut self) -> Result<ast::Expression> {
        self.expect_peek(TokenType::LPAREN);
        self.next_token();

        let parameters = self.parse_fn_params()?;

        self.expect_peek(TokenType::LBRACE);

        let body;

        if let ast::Statement::BlockStatement(val) = self.parse_block_statement()? {
            body = val;
        } else {
            panic!("Expected BlockStatement");
        }

        return Ok(ast::Expression::FnLiteral(ast::FnLiteral {
            parameters,
            body,
        }));
    }

    fn parse_fn_params(&mut self) -> Result<Vec<ast::Identifier>> {
        let mut parameters: Vec<ast::Identifier> = Vec::new();

        while self.curr_token.token_type == TokenType::IDENT {
            parameters.push(ast::Identifier::new(
                self.curr_token.clone(),
                self.curr_token.literal.clone(),
            ));

            if self.peek_token_is(&TokenType::COMMA) {
                self.next_token();
            }

            self.next_token();
        }

        if self.curr_token.token_type != TokenType::RPAREN {
            return Err(anyhow!(
                "Expected RPAREN, found {}",
                self.curr_token.token_type
            ));
        }

        return Ok(parameters);
    }

    fn parse_call_expression(&mut self, func: ast::Expression) -> Result<ast::Expression> {
        self.next_token();
        let args = self.parse_call_args()?;

        return Ok(ast::Expression::CallExpression(ast::CallExpression {
            function: Box::new(func),
            arguments: args,
        }));
    }

    fn parse_call_args(&mut self) -> Result<Vec<ast::Expression>> {
        let mut args: Vec<ast::Expression> = Vec::new();

        if self.peek_token_is(&TokenType::RPAREN) {
            self.next_token();
            return Ok(args);
        }

        args.push(self.parse_expression(Precedence::LOWEST)?);

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::LOWEST)?);
        }

        self.expect_peek(TokenType::RPAREN);

        return Ok(args);
    }
}

mod test {
    use crate::ast;
    use crate::lexer;
    use crate::parser::{Parser, Precedence};
    use crate::token::{Token, TokenType};

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

    #[test]
    fn test_infix_expression_p2() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for (t, expected) in tests.iter() {
            let mut lex = lexer::Lexer::new(t.as_bytes().into());
            let mut p = Parser::new(&mut lex);
            let program = p.parse_program().unwrap();

            assert_eq!(p.errors.len(), 0);
            assert_eq!(program.to_string(), expected.to_string());
        }
    }

    #[test]
    fn test_boolean() {
        let mut lex = lexer::Lexer::new("let x = false".as_bytes().into());
        let mut p = Parser::new(&mut lex);
        let program = p.parse_program().unwrap();

        assert_eq!(&p.errors.len(), &0);
        assert_eq!(&program.to_string(), &"let x = false;".to_string());
    }

    #[test]
    fn test_operator_precedence() {
        let expected = vec![
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        for (t, exp) in expected.iter() {
            let mut lex = lexer::Lexer::new(t.as_bytes().into());
            let mut p = Parser::new(&mut lex);
            let program = p.parse_program().unwrap();

            assert_eq!(&p.errors.len(), &0);
            assert_eq!(&program.to_string(), &exp.to_string());
        }
    }

    #[test]
    fn test_if_statement() {
        let input = "if (x < y) { x }";
        let expected = ast::Statement::ExpressionStatement(ast::ExpressionStatement::new(
            ast::Expression::IfExpression(ast::IfExpression {
                condition: Box::new(ast::Expression::InfixExpression(ast::InfixExpression::new(
                    ast::Expression::Identifier(ast::Identifier::new(
                        Token::new(TokenType::IDENT, "x".to_string()),
                        "x".to_string(),
                    )),
                    TokenType::LT,
                    ast::Expression::Identifier(ast::Identifier::new(
                        Token::new(TokenType::IDENT, "y".to_string()),
                        "y".to_string(),
                    )),
                ))),
                consequence: ast::BlockStatement::new(vec![ast::Statement::ExpressionStatement(
                    ast::ExpressionStatement::new(ast::Expression::Identifier(
                        ast::Identifier::new(
                            Token::new(TokenType::IDENT, "x".to_string()),
                            "x".to_string(),
                        ),
                    )),
                )]),
                alternative: None,
            }),
        ));

        let mut lex = lexer::Lexer::new(input.as_bytes().into());
        let mut p = Parser::new(&mut lex);
        let program = p.parse_program().unwrap();

        assert_eq!(p.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0], expected);
    }

    #[test]
    fn test_if_else_statement() {
        let input = "if (x < y) { x } else { y }";
        let expected = ast::Statement::ExpressionStatement(ast::ExpressionStatement::new(
            ast::Expression::IfExpression(ast::IfExpression {
                condition: Box::new(ast::Expression::InfixExpression(ast::InfixExpression::new(
                    ast::Expression::Identifier(ast::Identifier::new(
                        Token::new(TokenType::IDENT, "x".to_string()),
                        "x".to_string(),
                    )),
                    TokenType::LT,
                    ast::Expression::Identifier(ast::Identifier::new(
                        Token::new(TokenType::IDENT, "y".to_string()),
                        "y".to_string(),
                    )),
                ))),
                consequence: ast::BlockStatement::new(vec![ast::Statement::ExpressionStatement(
                    ast::ExpressionStatement::new(ast::Expression::Identifier(
                        ast::Identifier::new(
                            Token::new(TokenType::IDENT, "x".to_string()),
                            "x".to_string(),
                        ),
                    )),
                )]),
                alternative: Some(ast::BlockStatement::new(vec![
                    ast::Statement::ExpressionStatement(ast::ExpressionStatement::new(
                        ast::Expression::Identifier(ast::Identifier::new(
                            Token::new(TokenType::IDENT, "y".to_string()),
                            "y".to_string(),
                        )),
                    )),
                ])),
            }),
        ));

        let mut lex = lexer::Lexer::new(input.as_bytes().into());
        let mut p = Parser::new(&mut lex);
        let program = p.parse_program().unwrap();

        assert_eq!(p.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0], expected);
    }

    #[test]
    fn test_fn_literal() {
        let input = "fn(x, y) { x + y; }";
        let expected = ast::Statement::ExpressionStatement(ast::ExpressionStatement::new(
            ast::Expression::FnLiteral(ast::FnLiteral {
                parameters: vec![
                    ast::Identifier::new(
                        Token::new(TokenType::IDENT, "x".to_string()),
                        "x".to_string(),
                    ),
                    ast::Identifier::new(
                        Token::new(TokenType::IDENT, "y".to_string()),
                        "y".to_string(),
                    ),
                ],
                body: ast::BlockStatement::new(vec![ast::Statement::ExpressionStatement(
                    ast::ExpressionStatement::new(ast::Expression::InfixExpression(
                        ast::InfixExpression::new(
                            ast::Expression::Identifier(ast::Identifier::new(
                                Token::new(TokenType::IDENT, "x".to_string()),
                                "x".to_string(),
                            )),
                            TokenType::PLUS,
                            ast::Expression::Identifier(ast::Identifier::new(
                                Token::new(TokenType::IDENT, "y".to_string()),
                                "y".to_string(),
                            )),
                        ),
                    )),
                )]),
            }),
        ));

        let mut lex = lexer::Lexer::new(input.as_bytes().into());
        let mut p = Parser::new(&mut lex);
        let program = p.parse_program().unwrap();

        assert_eq!(p.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0], expected);
    }

    #[test]
    fn test_fn_call() {
        let input = "add(1, 2 * 3, 4 + 5)";
        let expected = ast::Statement::ExpressionStatement(ast::ExpressionStatement::new(
            ast::Expression::CallExpression(ast::CallExpression {
                function: Box::new(ast::Expression::Identifier(ast::Identifier::new(
                    Token::new(TokenType::IDENT, "add".to_string()),
                    "add".to_string(),
                ))),
                arguments: vec![
                    ast::Expression::IntLiteral(ast::IntLiteral::new(
                        Token::new(TokenType::INT, "1".to_string()),
                        1,
                    )),
                    ast::Expression::InfixExpression(ast::InfixExpression::new(
                        ast::Expression::IntLiteral(ast::IntLiteral::new(
                            Token::new(TokenType::INT, "2".to_string()),
                            2,
                        )),
                        TokenType::ASTERISK,
                        ast::Expression::IntLiteral(ast::IntLiteral::new(
                            Token::new(TokenType::INT, "3".to_string()),
                            3,
                        )),
                    )),
                    ast::Expression::InfixExpression(ast::InfixExpression::new(
                        ast::Expression::IntLiteral(ast::IntLiteral::new(
                            Token::new(TokenType::INT, "4".to_string()),
                            4,
                        )),
                        TokenType::PLUS,
                        ast::Expression::IntLiteral(ast::IntLiteral::new(
                            Token::new(TokenType::INT, "5".to_string()),
                            5,
                        )),
                    )),
                ],
            }),
        ));

        let mut lex = lexer::Lexer::new(input.as_bytes().into());
        let mut p = Parser::new(&mut lex);
        let program = p.parse_program().unwrap();

        for err in p.errors.iter() {
            println!("{}", err);
        }

        assert_eq!(p.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0], expected);
    }
}
