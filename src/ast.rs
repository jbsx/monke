use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> Option<&String>;
}

pub trait Statement: Node {}
pub trait Expression: Node {}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> Option<&String> {
        if self.statements.len() > 0 {
            return self.statements[0].token_literal();
        } else {
            return None;
        }
    }
}

#[derive(Debug)]
pub struct Identifier {
    token: Token,
    value: String,
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        return Identifier { token, value };
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> Option<&String> {
        return Some(&self.token.literal);
    }
}

impl Expression for Identifier {}

#[derive(Debug)]
pub struct LetStatement {
    token: Token,
    name: Identifier,
    value: Box<dyn Expression>,
}

impl LetStatement {
    pub fn new(token: Token, name: Identifier, value: Box<dyn Expression>) -> Self {
        return LetStatement { token, name, value };
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> Option<&String> {
        return Some(&self.token.literal);
    }
}
impl Statement for LetStatement {}

impl std::fmt::Debug for dyn Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_literal().unwrap())
    }
}

impl std::fmt::Debug for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in self.statements.iter() {
            write!(f, "\n{:?}", stmt.token_literal());
        }
        return Ok(());
    }
}
