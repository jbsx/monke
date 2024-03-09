use std::fmt::Display;

use crate::ast;
use crate::env::Environment;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    INT(i32),
    BOOL(bool),
    NULL(),
    RETURN(Box<Object>),
    FUNCTION(Function),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Object::INT(val) => return write!(f, "{}", val),
            Object::BOOL(val) => return write!(f, "{}", val),
            Object::NULL() => return write!(f, "NULL"),
            Object::RETURN(val) => return write!(f, "{}", *val),
            Object::FUNCTION(val) => return write!(f, "{}", val),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub params: Vec<ast::Identifier>,
    pub body: ast::BlockStatement,
    pub env: Environment,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = String::new();

        res.push_str("fn (");

        for p in self.params.iter() {
            res.push_str(&p.to_string());
            res.push_str(", ");
        }

        if self.params.len() != 0 {
            res.pop();
            res.pop();
        }

        res.push_str(") {\n{}");
        res.push_str(&format!("{{{}}}", &self.body.to_string()));

        return write!(f, "{}", res);
    }
}
