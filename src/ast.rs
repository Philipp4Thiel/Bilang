use from_pest::{ConversionError, FromPest, Void};
use pest::iterators::Pairs;
use pest_ast::FromPest;
use super::parser::Rule;

pub fn get_ast(parsed_file: &mut Pairs<Rule>) -> Result<Program, ConversionError<Void>> {
    Program::from_pest(parsed_file)
}

fn span_to_str(span: pest::Span) -> &str {
    span.as_str()
}

fn span_to_i32(span: pest::Span) -> i32 {
    span.as_str().parse().unwrap()
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(Rule::program))]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(Rule::statement))]
pub enum Statement {
    Declaration(Box<Identifier>, Box<Type>, Box<Expression>),
    Print(Box<Expression>),
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(Rule::IDENTIFIER))]
pub struct Identifier {
    #[pest_ast(outer(with(span_to_str), with(str::parse), with(Result::unwrap)))]
    pub name: String,
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(Rule::NUMBER))]
pub struct IntLiteral {
    #[pest_ast(outer(with(span_to_i32)))]
    pub value: i32,
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(Rule::type0))]
pub enum Type {
    Int,
    Func(Box<Type>, Box<Type>),
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(Rule::expression))]
pub enum Expression {
    Application(Box<Expression>, Vec<Expression>),
    BinaryOp(Box<BinaryExpression>),
    UnaryOp(Box<UnaryExpression>),
    Identifier(Box<Identifier>),
    Lambda(Box<Identifier>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    IntLiteral(IntLiteral),
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(Rule::binary))]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(Rule::operator))]
pub enum BinaryOperator {
    // TODO, match the operator to the enum
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(Rule::unary))]
pub enum UnaryExpression {
    Neg(Box<Expression>),
    Not(Box<Expression>),
}