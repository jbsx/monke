use crate::ast;
use crate::lexer;
use crate::object::Object;
use crate::parser;
use crate::token::TokenType;
use anyhow::Result;

pub fn eval(node: ast::Node) -> Result<Object> {
    match node {
        ast::Node::Statement(stmt) => match stmt {
            ast::Statement::ExpressionStatement(val) => {
                return eval(ast::Node::Expression(val.expression));
            }
            ast::Statement::BlockStatement(val) => {
                let mut res = Object::NULL();
                for stmt in val.statements.iter() {
                    res = eval(ast::Node::Statement(stmt.clone()))?; //TODO: optimise clone
                }
                return Ok(res);
            }
            _ => {
                todo!()
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
                let right = eval(ast::Node::Expression(*val.right))?;
                match val.operator {
                    crate::token::TokenType::BANG => {
                        return Ok(eval_bang_operator_exp(right));
                    }
                    crate::token::TokenType::MINUS => {
                        return Ok(eval_minus_operator_exp(right));
                    }
                    _ => {
                        todo!()
                    }
                }
            }
            ast::Expression::InfixExpression(val) => {
                let left = eval(ast::Node::Expression(*val.left))?;
                let right = eval(ast::Node::Expression(*val.right))?;
                return Ok(eval_infix_exp(left, val.operator, right));
            }
            ast::Expression::IfExpression(val) => {
                let con = eval(ast::Node::Expression(*val.condition))?;
                if is_truthy(con) {
                    return eval(ast::Node::Statement(ast::Statement::BlockStatement(
                        val.consequence,
                    )));
                } else {
                    match val.alternative {
                        Some(alt) => {
                            return eval(ast::Node::Statement(ast::Statement::BlockStatement(alt)));
                        }
                        _ => {
                            return Ok(Object::NULL());
                        }
                    }
                }
            }
            _ => {
                todo!()
            }
        },
    }
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

fn eval_minus_operator_exp(obj: Object) -> Object {
    match obj {
        Object::INT(val) => {
            return Object::INT(-val);
        }
        _ => return Object::NULL(),
    }
}

fn eval_infix_exp(left: Object, op: TokenType, right: Object) -> Object {
    match (&left, &right) {
        (Object::INT(l), Object::INT(r)) => return eval_int_infix_exp(*l, op, *r),
        _ => match op {
            TokenType::EQ => return Object::BOOL(left == right),
            TokenType::NOT_EQ => return Object::BOOL(left != right),
            _ => {
                return Object::NULL();
            }
        },
    }
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

    let t = eval(ast::Node::Statement(prog.statements[0].clone()));

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
    assert_eq!(prog.statements.len(), 3);

    for (idx, stmt) in prog.statements.iter().enumerate() {
        let t = eval(ast::Node::Statement(stmt.clone()));
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
    assert_eq!(prog.statements.len(), 1);

    for (idx, stmt) in prog.statements.iter().enumerate() {
        let t = eval(ast::Node::Statement(stmt.clone()));
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
        let prog = p.parse_program().unwrap();
        assert_eq!(prog.statements.len(), 1);
        assert_eq!(
            eval(ast::Node::Statement(prog.statements[0].clone())).unwrap(),
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
        assert_eq!(prog.statements.len(), 1);
        assert_eq!(
            eval(ast::Node::Statement(prog.statements[0].clone())).unwrap(),
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
        assert_eq!(prog.statements.len(), 1);
        assert_eq!(
            eval(ast::Node::Statement(prog.statements[0].clone())).unwrap(),
            *t
        );
    }
}
