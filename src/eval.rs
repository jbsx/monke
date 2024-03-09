use crate::ast;
use crate::env::Environment;
use crate::object::{Function, Object};
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
                    TokenType::BANG => {
                        return Ok(eval_bang_operator_exp(right));
                    }
                    TokenType::MINUS => {
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
            ast::Expression::FnLiteral(val) => {
                return Ok(Object::FUNCTION(Function {
                    params: val.parameters,
                    body: val.body,
                    env: Environment::new_env(), //TODO
                }));
            }
            ast::Expression::CallExpression(_val) => {
                todo!()
            }
        },
    }
}

pub fn eval_stmts(stmts: Vec<ast::Statement>, env: &mut Environment) -> Result<Object> {
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
