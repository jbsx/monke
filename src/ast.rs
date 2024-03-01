use crate::token::{Token, TokenType};
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntLiteral(IntLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Boolean(Boolean),
    IfExpression(IfExpression),
    FnLiteral(FnLiteral),
    CallExpression(CallExpression),
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = String::new();
        for stmt in &self.statements {
            res.push_str(&stmt.to_string());
        }
        return write!(f, "{}", res);
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            &Self::LetStatement(stmt) => return write!(f, "{}", stmt),
            &Self::ReturnStatement(stmt) => return write!(f, "{}", stmt),
            &Self::ExpressionStatement(stmt) => return write!(f, "{}", stmt),
            &Self::BlockStatement(stmt) => return write!(f, "{}", stmt),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            &Self::Identifier(val) => return write!(f, "{}", val),
            &Self::IntLiteral(val) => return write!(f, "{}", val),
            &Self::PrefixExpression(val) => return write!(f, "{}", val),
            &Self::InfixExpression(val) => return write!(f, "{}", val),
            &Self::Boolean(val) => return write!(f, "{}", val),
            &Self::IfExpression(val) => return write!(f, "{}", val),
            &Self::FnLiteral(val) => return write!(f, "{}", val),
            &Self::CallExpression(val) => return write!(f, "{}", val),
        }
    }
}

// ---------------------------Identifier---------------------------

#[derive(Debug, PartialEq)]
pub struct Identifier {
    token: Token,
    value: String,
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        return Identifier { token, value };
    }
    pub fn token_literal(&self) -> &String {
        return &self.token.literal;
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{}", &self.value);
    }
}

//---------------------------Let Statement---------------------------

#[derive(Debug, PartialEq)]
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
        res.push_str(" ");
        res.push_str(&self.name.to_string());
        res.push_str(" = ");
        res.push_str(&format!("{};", &self.value));

        return write!(f, "{}", res);
    }
}

//---------------------------Return Statement---------------------------

#[derive(Debug, PartialEq)]
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

    pub fn token_literal(&self) -> &String {
        return &self.token.literal;
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = String::new();

        res.push_str(&self.token.literal);
        res.push_str(&format!(" {};", &self.return_value));

        return write!(f, "{}", res);
    }
}

//---------------------------Expression Statement---------------------------

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

impl ExpressionStatement {
    pub fn new(expression: Expression) -> Self {
        return ExpressionStatement { expression };
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{}", &self.expression);
    }
}

//---------------------------Block Statement---------------------------

#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(stmts: Vec<Statement>) -> Self {
        return BlockStatement { statements: stmts };
    }

    pub fn push(&mut self, stmt: Statement) {
        self.statements.push(stmt);
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = String::new();
        res.push_str("\n");
        for stmt in self.statements.iter() {
            res.push_str("  ");
            res.push_str(&stmt.to_string());
            res.push_str("\n");
        }
        return write!(f, "{}", res);
    }
}

//--------------------------------INT--------------------------------

#[derive(Debug, PartialEq)]
pub struct IntLiteral {
    token: Token,
    value: u64,
}

impl IntLiteral {
    pub fn new(token: Token, value: u64) -> Self {
        return IntLiteral { token, value };
    }
    pub fn token_literal(&self) -> &String {
        return &self.token.literal;
    }
}

impl Display for IntLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{}", &self.token.literal);
    }
}

//--------------------------------PREFIX EXPRESSION--------------------------------

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    operator: TokenType,
    right: Box<Expression>,
}

impl PrefixExpression {
    pub fn new(operator: TokenType, right: Expression) -> Self {
        return PrefixExpression {
            operator,
            right: Box::new(right),
        };
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "({}{})", &self.operator, &self.right);
    }
}

//--------------------------------INFIX EXPRESSION--------------------------------

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub operator: TokenType,
    pub right: Box<Expression>,
}

impl InfixExpression {
    pub fn new(left: Expression, operator: TokenType, right: Expression) -> Self {
        return InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        };
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "({} {} {})", &self.left, &self.operator, &self.right);
    }
}

//--------------------------------BOOLEAN--------------------------------

#[derive(Debug, PartialEq)]
pub struct Boolean {
    pub val: bool,
}

impl Boolean {
    pub fn new(val: bool) -> Self {
        return Boolean { val };
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{}", &self.val);
    }
}

//--------------------------------If Expression--------------------------------

#[derive(Debug, PartialEq)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = format!("if({}){{\n{}\n}}", &*self.condition, &self.consequence);

        if self.alternative.is_some() {
            res.push_str(&format!(
                "else{{\n{}\n}}",
                &self.alternative.as_ref().unwrap()
            ));
        }

        return write!(f, "{}", res);
    }
}

//--------------------------------Function Literal--------------------------------

#[derive(Debug, PartialEq)]
pub struct FnLiteral {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl FnLiteral {
    pub fn new() -> Self {
        Self {
            parameters: Vec::new(),
            body: BlockStatement::new(Vec::new()),
        }
    }
}

impl Display for FnLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = format!("fn(");

        for p in self.parameters.iter() {
            res.push_str(&p.to_string());
            res.push_str(",");
        }

        res.pop();

        res.push_str(&format!(") {{{}}}", &self.body.to_string()));

        return write!(f, "{}", res);
    }
}

//--------------------------------Function Call--------------------------------

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = String::new();

        res.push_str(&self.function.to_string());

        res.push_str("(");

        for arg in self.arguments.iter() {
            res.push_str(&arg.to_string());
            res.push_str(",");
        }

        res.pop();
        res.push_str(")");

        return write!(f, "{}", res);
    }
}
