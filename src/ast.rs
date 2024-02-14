use crate::token::{Token, TokenType};
use std::fmt::Display;

pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

pub enum Expression {
    Identifier(Identifier),
    IntLiteral(IntLiteral),
    //PrefixExpression(PrefixExpression),
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.statements.len() == 0 {
            write!(f, "")
        } else {
            write!(f, "{}", self.statements[0])
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            &Self::LetStatement(stmt) => {
                write!(f, "{}", stmt)
            }
            &Self::ReturnStatement(stmt) => {
                write!(f, "{}", stmt)
            }
            &Self::ExpressionStatement(stmt) => {
                write!(f, "{}", stmt)
            }
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            &Self::Identifier(val) => {
                write!(f, "{}", val)
            }
            &Self::IntLiteral(val) => {
                write!(f, "{}", val)
            }
        }
    }
}

// ---------------------------Identifier---------------------------

pub struct Identifier {
    token: Token,
    value: String,
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        return Identifier { token, value };
    }
    pub fn token_literal(&self) -> Option<&String> {
        return Some(&self.token.literal);
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.value)
    }
}

//---------------------------Let Statement---------------------------

pub struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression,
}

impl LetStatement {
    pub fn new(token: Token, name: Identifier, value: Expression) -> Self {
        return LetStatement { token, name, value };
    }

    pub fn token_literal(&self) -> &String {
        return &self.token.literal;
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = String::new();

        res.push_str(&self.token.literal);
        res.push_str(" = ");
        res.push_str(&format!("{};", &self.value));

        write!(f, "{}", res)
    }
}

//---------------------------Return Statement---------------------------

pub struct ReturnStatement {
    token: Token,
    return_value: Expression,
}

impl ReturnStatement {
    pub fn new(token: Token, return_value: Expression) -> Self {
        return ReturnStatement {
            token,
            return_value,
        };
    }

    pub fn token_literal(&self) -> Option<&String> {
        return Some(&self.token.literal);
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = String::new();

        res.push_str(&self.token.literal);
        res.push_str(&format!(" {};", &self.return_value));

        write!(f, "{}", res)
    }
}

//---------------------------Expression Statement---------------------------

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl ExpressionStatement {
    pub fn new(token: Token, expression: Expression) -> Self {
        return ExpressionStatement { token, expression };
    }
    pub fn token_literal(&self) -> Option<&String> {
        return Some(&self.token.literal);
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{};", &self.expression)
    }
}

//--------------------------------INT--------------------------------

pub struct IntLiteral {
    token: Token,
    value: u64,
}

impl IntLiteral {
    pub fn new(token: Token, value: u64) -> Self {
        return IntLiteral { token, value };
    }
    pub fn token_literal(&self) -> Option<&String> {
        return Some(&self.token.literal);
    }
}

impl Display for IntLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self.token.literal)
    }
}

//--------------------------------PREFIX EXPRESSION--------------------------------

pub struct PrefixExpression {
    token: Token,
    operator: TokenType,
    right: Expression,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: TokenType, right: Expression) -> Self {
        return PrefixExpression {
            token,
            operator,
            right,
        };
    }
    pub fn token_literal(&self) -> Option<&String> {
        return Some(&self.token.literal);
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", &self.operator, &self.right)
    }
}
