#[cfg(test)]
mod test_eval {
    use crate::ast;
    use crate::env::Environment;
    use crate::eval::{eval, eval_stmts};
    use crate::lexer;
    use crate::object;
    use crate::object::Object;
    use crate::parser;
    use crate::token::TokenType;

    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn test_int() {
        let mut l = lexer::Lexer::new("2".as_bytes().into());
        let mut p = parser::Parser::new(&mut l);
        let e = Rc::new(RefCell::new(Environment::new()));
        let prog = p.parse_program().unwrap();

        let t = eval(ast::Node::Statement(prog.statements[0].clone()), e.clone());

        assert_eq!(prog.statements.len(), 1);
        assert_eq!(t.unwrap(), Object::INT(2));
    }

    #[test]
    fn test_bang() {
        let input = "
        !true;
        !false;
        !5;
    ";

        let expected = vec![Object::BOOL(false), Object::BOOL(true), Object::BOOL(false)];

        let mut l = lexer::Lexer::new(input.as_bytes().into());
        let mut p = parser::Parser::new(&mut l);
        let e = Rc::new(RefCell::new(Environment::new()));
        let prog = p.parse_program().unwrap();

        assert_eq!(prog.statements.len(), 3);

        for (idx, stmt) in prog.statements.iter().enumerate() {
            let t = eval(ast::Node::Statement(stmt.clone()), e.clone());
            assert_eq!(t.unwrap(), expected[idx]);
        }
    }

    #[test]
    fn test_minus() {
        let input = "
        -5;
    ";

        let expected = vec![Object::INT(-5)];

        let mut l = lexer::Lexer::new(input.as_bytes().into());
        let mut p = parser::Parser::new(&mut l);
        let e = Rc::new(RefCell::new(Environment::new()));
        let prog = p.parse_program().unwrap();

        assert_eq!(prog.statements.len(), 1);

        for (idx, stmt) in prog.statements.iter().enumerate() {
            let t = eval(ast::Node::Statement(stmt.clone()), e.clone());
            assert_eq!(t.unwrap(), expected[idx]);
        }
    }

    #[test]
    fn test_eval_int_exp() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (i, t) in tests.iter() {
            let mut l = lexer::Lexer::new(i.as_bytes().into());
            let mut p = parser::Parser::new(&mut l);
            let e = Rc::new(RefCell::new(Environment::new()));
            let prog = p.parse_program().unwrap();
            assert_eq!(prog.statements.len(), 1);
            assert_eq!(
                eval(ast::Node::Statement(prog.statements[0].clone()), e.clone()).unwrap(),
                Object::INT(*t)
            );
        }
    }

    #[test]
    fn test_eval_bool_exp() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (i, t) in tests.iter() {
            let mut l = lexer::Lexer::new(i.as_bytes().into());
            let mut p = parser::Parser::new(&mut l);
            let e = Rc::new(RefCell::new(Environment::new()));
            let prog = p.parse_program().unwrap();

            assert_eq!(prog.statements.len(), 1);
            assert_eq!(
                eval(ast::Node::Statement(prog.statements[0].clone()), e.clone()).unwrap(),
                Object::BOOL(*t)
            );
        }
    }

    #[test]
    fn test_if_stmt() {
        let tests = vec![
            ("if (true) { 10 }", Object::INT(10)),
            ("if (false) { 10 }", Object::NULL()),
            ("if (1) { 10 }", Object::INT(10)),
            ("if (1 < 2) { 10 }", Object::INT(10)),
            ("if (1 > 2) { 10 }", Object::NULL()),
            ("if (1 > 2) { 10 } else { 20 }", Object::INT(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::INT(10)),
        ];

        for (i, t) in tests.iter() {
            let mut l = lexer::Lexer::new(i.as_bytes().into());
            let mut p = parser::Parser::new(&mut l);
            let e = Rc::new(RefCell::new(Environment::new()));
            let prog = p.parse_program().unwrap();
            assert_eq!(prog.statements.len(), 1);
            assert_eq!(
                eval(ast::Node::Statement(prog.statements[0].clone()), e.clone()).unwrap(),
                *t
            );
        }
    }

    #[test]
    fn test_return_stmt() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
        ];

        for (i, t) in tests.iter() {
            let mut l = lexer::Lexer::new(i.as_bytes().into());
            let mut p = parser::Parser::new(&mut l);
            let e = Rc::new(RefCell::new(Environment::new()));
            let prog = p.parse_program().unwrap();
            assert_eq!(
                eval_stmts(prog.statements, e.clone()).unwrap(),
                Object::INT(*t)
            );
        }
    }

    #[test]
    fn test_return_stmt_p2() {
        let tests = vec![(
            "if (10 > 1) {
            if (10 > 1) {
                return 10;
            }
            return 1;
        }",
            10,
        )];

        for (i, t) in tests.iter() {
            let mut l = lexer::Lexer::new(i.as_bytes().into());
            let mut p = parser::Parser::new(&mut l);
            let e = Rc::new(RefCell::new(Environment::new()));
            let prog = p.parse_program().unwrap();

            assert_eq!(
                eval_stmts(prog.statements, e.clone()).unwrap(),
                Object::INT(*t)
            );
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "Type mismatch: INT(5) + BOOL(true)"),
            ("5 + true; 5;", "Type mismatch: INT(5) + BOOL(true)"),
            ("-true", "Unknown operator: -BOOL(true)"),
            (
                "true + false;",
                "Unknown operator: BOOL(true) + BOOL(false)",
            ),
            (
                "5; true + false; 5",
                "Unknown operator: BOOL(true) + BOOL(false)",
            ),
            (
                "if (10 > 1) { true + false; }",
                "Unknown operator: BOOL(true) + BOOL(false)",
            ),
            (
                "if (10 > 1) {
                if (10 > 1) {
                    return true + false;
                }
                return 1;
            }",
                "Unknown operator: BOOL(true) + BOOL(false)",
            ),
            ("foobar", "Identifier not found: foobar"),
        ];

        for (i, t) in tests.iter() {
            let mut l = lexer::Lexer::new(i.as_bytes().into());
            let mut p = parser::Parser::new(&mut l);
            let e = Rc::new(RefCell::new(Environment::new()));
            let prog = p.parse_program().unwrap();

            let e = eval_stmts(prog.statements, e.clone());
            match e {
                Ok(..) => {
                    panic!("expected error but compiled successfully.")
                }
                Err(err) => {
                    assert_eq!(err.to_string(), *t);
                }
            }
        }
    }

    #[test]
    fn test_let_stmts() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (i, t) in tests.iter() {
            let mut l = lexer::Lexer::new(i.as_bytes().into());
            let mut p = parser::Parser::new(&mut l);
            let env = Rc::new(RefCell::new(Environment::new()));
            let prog = p.parse_program().unwrap();

            let e = eval_stmts(prog.statements, env.clone()).unwrap();
            assert_eq!(e, Object::INT(*t));
        }
    }

    #[test]
    fn test_fn() {
        let input = "fn(x) { x + 2; };";
        let mut l = lexer::Lexer::new(input.as_bytes().into());
        let mut p = parser::Parser::new(&mut l);
        let env = Rc::new(RefCell::new(Environment::new()));
        let prog = p.parse_program().unwrap();

        assert_eq!(prog.statements.len(), 1);

        let t = eval(
            ast::Node::Statement(prog.statements[0].clone()),
            env.clone(),
        )
        .unwrap();

        assert_eq!(
            t,
            Object::FUNCTION(object::Function {
                params: vec![ast::Identifier::new(
                    crate::token::Token {
                        token_type: TokenType::IDENT,
                        literal: "x".to_string()
                    },
                    "x".to_string()
                )],
                body: ast::BlockStatement::new(vec![ast::Statement::ExpressionStatement(
                    ast::ExpressionStatement::new(ast::Expression::InfixExpression(
                        ast::InfixExpression::new(
                            ast::Expression::Identifier(ast::Identifier::new(
                                crate::token::Token {
                                    token_type: TokenType::IDENT,
                                    literal: "x".to_string()
                                },
                                "x".to_string()
                            )),
                            TokenType::PLUS,
                            ast::Expression::IntLiteral(ast::IntLiteral::new(
                                crate::token::Token {
                                    token_type: TokenType::INT,
                                    literal: "2".to_string()
                                },
                                2
                            ))
                        )
                    ))
                )]),
                env: env.clone()
            })
        );
    }

    #[test]
    fn test_fn_call() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (i, t) in tests.iter() {
            let mut l = lexer::Lexer::new(i.as_bytes().into());
            let mut p = parser::Parser::new(&mut l);
            let env = Rc::new(RefCell::new(Environment::new()));
            let prog = p.parse_program().unwrap();

            let e = eval_stmts(prog.statements, env.clone()).unwrap();
            assert_eq!(e, Object::INT(*t));
        }
    }
}
