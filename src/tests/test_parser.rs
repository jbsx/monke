#[cfg(test)]
mod test_parser {
    use crate::ast;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
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

        let mut l = Lexer::new(input.as_bytes().into());
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

        let mut l = Lexer::new(input.as_bytes().into());
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

        let mut l = Lexer::new(input.as_bytes().into());
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
            let mut lex = Lexer::new(t.as_bytes().into());
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
            let mut lex = Lexer::new(t.as_bytes().into());
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
            let mut lex = Lexer::new(t.as_bytes().into());
            let mut p = Parser::new(&mut lex);
            let program = p.parse_program().unwrap();

            assert_eq!(p.errors.len(), 0);
            assert_eq!(program.to_string(), expected.to_string());
        }
    }

    #[test]
    fn test_boolean() {
        let mut lex = Lexer::new("let x = false".as_bytes().into());
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
            let mut lex = Lexer::new(t.as_bytes().into());
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

        let mut lex = Lexer::new(input.as_bytes().into());
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

        let mut lex = Lexer::new(input.as_bytes().into());
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

        let mut lex = Lexer::new(input.as_bytes().into());
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

        let mut lex = Lexer::new(input.as_bytes().into());
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
