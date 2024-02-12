use crate::token::Token;
use std::fmt::Debug;

pub trait Node {
    fn token_literal(&self) -> Option<&String>;
}

pub trait Statement: Node + Debug {}
pub trait Expression: Node + Debug {}

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

impl Debug for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = String::new();
        for stmt in &self.statements {
            res.push_str(format!("{:?}\n", stmt).as_str());
        }

        write!(f, "{}", res)
    }
}

// ---------------------------IDENTIFIER---------------------------

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

impl Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.value)
    }
}

//---------------------------LET---------------------------

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

impl Debug for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = String::new();

        res.push_str(&self.token.literal);
        res.push_str(" = ");
        res.push_str(&format!("{:?};", &self.value));

        write!(f, "{}", res)
    }
}

//---------------------------RETURN---------------------------

pub struct ReturnStatement {
    token: Token,
    return_value: Box<dyn Expression>,
}

impl ReturnStatement {
    pub fn new(token: Token, return_value: Box<dyn Expression>) -> Self {
        return ReturnStatement {
            token,
            return_value,
        };
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> Option<&String> {
        return Some(&self.token.literal);
    }
}

impl Statement for ReturnStatement {}

impl Debug for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = String::new();

        res.push_str(&self.token.literal);
        res.push_str(&format!(" {:?};", &self.return_value));

        write!(f, "{}", res)
    }
}

//---------------------------Expression Statement---------------------------

pub struct ExpressionStatement {
    token: Token,
    expression: Box<dyn Expression>,
}

impl ExpressionStatement {
    pub fn new(token: Token, expression: Box<dyn Expression>) -> Self {
        return ExpressionStatement { token, expression };
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> Option<&String> {
        return Some(&self.token.literal);
    }
}

impl Statement for ExpressionStatement {}

impl Debug for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?};", &self.expression)
    }
}
