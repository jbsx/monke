use crate::ast;
use crate::lexer;
use crate::object;
use crate::parser;
use anyhow::Result;

pub fn eval(node: ast::Node) -> Result<object::Object> {
    match node {
        ast::Node::Statement(stmt) => match stmt {
            ast::Statement::ExpressionStatement(val) => {
                return eval(ast::Node::Expression(val.expression));
            }
            _ => {
                todo!()
            }
        },
        ast::Node::Expression(exp) => match exp {
            ast::Expression::IntLiteral(val) => {
                return Ok(object::Object::INT(val.value));
            }
            ast::Expression::Boolean(val) => {
                return Ok(object::Object::BOOL(val.value));
            }
            _ => {
                todo!()
            }
        },
    }
}

#[test]
fn test_int() {
    let mut l = lexer::Lexer::new("2".as_bytes().into());
    let mut p = parser::Parser::new(&mut l);
    let prog = p.parse_program().unwrap();

    let t = eval(ast::Node::Statement(prog.statements[0].clone()));

    assert_eq!(prog.statements.len(), 1);
    assert_eq!(t.unwrap(), object::Object::INT(2));
}
