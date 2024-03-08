use crate::ast;
use crate::env::Environment;
use crate::lexer;
use crate::object::Object;
use crate::parser;
use crate::token::TokenType;
use anyhow::anyhow;
use anyhow::Result;
use std::mem::discriminant;

pub fn eval(node: ast::Node, env: &mut Environment) -> Result<Object> {
    match node {
        ast::Node::Statement(stmt) => match stmt {
            ast::Statement::ExpressionStatement(val) => {
                return eval(ast::Node::Expression(val.expression), env);
            }
            ast::Statement::BlockStatement(val) => {
                let mut res = Object::NULL();
                for stmt in val.statements.iter() {
                    res = eval(ast::Node::Statement(stmt.clone()), env)?; //TODO: optimise .clone
                    if let Object::RETURN(..) = res {
                        return Ok(res); //Return early when 'return' encountered
                    }
                }
                return Ok(res);
            }
            ast::Statement::ReturnStatement(val) => {
                let res = eval(ast::Node::Expression(val.return_value), env)?;
                return Ok(Object::RETURN(Box::new(res)));
            }
            ast::Statement::LetStatement(val) => {
                let obj = eval(ast::Node::Expression(val.value), env)?;
                env.set(val.name.value, obj);
                return Ok(Object::NULL());
            }
        },
        ast::Node::Expression(exp) => match exp {
            ast::Expression::IntLiteral(val) => {
                return Ok(Object::INT(val.value));
            }
            ast::Expression::Boolean(val) => {
                return Ok(Object::BOOL(val.value));
            }
            ast::Expression::PrefixExpression(val) => {
                let right = eval(ast::Node::Expression(*val.right), env)?;
                match val.operator {
                    crate::token::TokenType::BANG => {
                        return Ok(eval_bang_operator_exp(right));
                    }
                    crate::token::TokenType::MINUS => {
                        return eval_minus_operator_exp(right);
                    }
                    _ => {
                        todo!()
                    }
                }
            }
            ast::Expression::InfixExpression(val) => {
                let left = eval(ast::Node::Expression(*val.left), env)?;
                let right = eval(ast::Node::Expression(*val.right), env)?;
                return eval_infix_exp(left, val.operator, right);
            }
            ast::Expression::IfExpression(val) => {
                let con = eval(ast::Node::Expression(*val.condition), env)?;
                if is_truthy(con) {
                    return eval(
                        ast::Node::Statement(ast::Statement::BlockStatement(val.consequence)),
                        env,
                    );
                } else {
                    match val.alternative {
                        Some(alt) => {
                            return eval(
                                ast::Node::Statement(ast::Statement::BlockStatement(alt)),
                                env,
                            );
                        }
                        _ => {
                            return Ok(Object::NULL());
                        }
                    }
                }
            }
            ast::Expression::Identifier(val) => {
                let name = val.value;
                let v = env.get(&name);

                match v {
                    Some(res) => {
                        return Ok((*res).clone());
                    }
                    None => {
                        return Err(anyhow!("Identifier not found: {}", name));
                    }
                }
            }
            ast::Expression::FnLiteral(_val) => {
                todo!()
            }
            ast::Expression::CallExpression(_val) => {
                todo!()
            }
        },
    }
}

fn eval_stmts(stmts: Vec<ast::Statement>, env: &mut Environment) -> Result<Object> {
    let mut res = Object::NULL();
    for stmt in stmts.iter() {
        res = eval(ast::Node::Statement(stmt.clone()), env)?; //TODO: optimise .clone

        if let Object::RETURN(val) = res {
            return Ok(*val); //Return early when 'return' encountered
        }
    }
    return Ok(res);
}

fn eval_bang_operator_exp(obj: Object) -> Object {
    match obj {
        Object::BOOL(val) => {
            return Object::BOOL(!val);
        }
        Object::NULL() => {
            return Object::BOOL(true);
        }
        _ => return Object::BOOL(false),
    }
}

fn eval_minus_operator_exp(obj: Object) -> Result<Object> {
    match obj {
        Object::INT(val) => {
            return Ok(Object::INT(-val));
        }
        _ => return Err(anyhow!("Unknown operator: -{:?}", obj)),
    }
}

fn eval_infix_exp(left: Object, op: TokenType, right: Object) -> Result<Object> {
    let res;
    match (&left, &right) {
        (Object::INT(l), Object::INT(r)) => res = eval_int_infix_exp(*l, op, *r),
        _ => match op {
            TokenType::EQ => res = Object::BOOL(left == right),
            TokenType::NOT_EQ => res = Object::BOOL(left != right),
            _ => {
                if discriminant(&left) == discriminant(&right) {
                    return Err(anyhow!("Unknown operator: {:?} {} {:?}", left, op, right));
                }
                return Err(anyhow!("Type mismatch: {:?} {} {:?}", left, op, right));
            }
        },
    }

    return Ok(res);
}

fn eval_int_infix_exp(left: i32, op: TokenType, right: i32) -> Object {
    match op {
        TokenType::PLUS => return Object::INT(left + right),
        TokenType::MINUS => return Object::INT(left - right),
        TokenType::ASTERISK => return Object::INT(left * right),
        TokenType::SLASH => return Object::INT(left / right),

        TokenType::LT => return Object::BOOL(left < right),
        TokenType::GT => return Object::BOOL(left > right),
        TokenType::EQ => return Object::BOOL(left == right),
        TokenType::NOT_EQ => return Object::BOOL(left != right),

        _ => return Object::NULL(),
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::NULL() => return false,
        Object::BOOL(val) => return val,
        _ => return true,
    }
}

#[test]
fn test_int() {
    let mut l = lexer::Lexer::new("2".as_bytes().into());
    let mut p = parser::Parser::new(&mut l);
    let prog = p.parse_program().unwrap();
    let mut e = Environment::new_env();

    let t = eval(ast::Node::Statement(prog.statements[0].clone()), &mut e);

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
    let prog = p.parse_program().unwrap();
    let mut e = Environment::new_env();
    assert_eq!(prog.statements.len(), 3);

    for (idx, stmt) in prog.statements.iter().enumerate() {
        let t = eval(ast::Node::Statement(stmt.clone()), &mut e);
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
    let prog = p.parse_program().unwrap();
    let mut e = Environment::new_env();
    assert_eq!(prog.statements.len(), 1);

    for (idx, stmt) in prog.statements.iter().enumerate() {
        let t = eval(ast::Node::Statement(stmt.clone()), &mut e);
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
        let mut e = Environment::new_env();
        let prog = p.parse_program().unwrap();
        assert_eq!(prog.statements.len(), 1);
        assert_eq!(
            eval(ast::Node::Statement(prog.statements[0].clone()), &mut e).unwrap(),
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
        let prog = p.parse_program().unwrap();
        let mut e = Environment::new_env();

        assert_eq!(prog.statements.len(), 1);
        assert_eq!(
            eval(ast::Node::Statement(prog.statements[0].clone()), &mut e).unwrap(),
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
        let prog = p.parse_program().unwrap();
        let mut e = Environment::new_env();
        assert_eq!(prog.statements.len(), 1);
        assert_eq!(
            eval(ast::Node::Statement(prog.statements[0].clone()), &mut e).unwrap(),
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
        let prog = p.parse_program().unwrap();
        let mut e = Environment::new_env();
        assert_eq!(
            eval_stmts(prog.statements, &mut e).unwrap(),
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
        let prog = p.parse_program().unwrap();
        let mut e = Environment::new_env();
        assert_eq!(
            eval_stmts(prog.statements, &mut e).unwrap(),
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
        let prog = p.parse_program().unwrap();
        let mut e = Environment::new_env();

        let e = eval_stmts(prog.statements, &mut e);
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
        let prog = p.parse_program().unwrap();
        let mut env = Environment::new_env();

        let e = eval_stmts(prog.statements, &mut env).unwrap();
        assert_eq!(e, Object::INT(*t));
    }
}
