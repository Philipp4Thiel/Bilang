use from_pest::{ConversionError, Void};
use pest::iterators::Pair;

use super::parser::Rule;

pub fn get_program_ast(pair: Pair<Rule>) -> Result<Program, ConversionError<Void>> {
    assert_eq!(pair.as_rule(), Rule::program);

    let statements: Vec<Statement> = pair.into_inner().map(get_statement_ast).collect::<Result<Vec<Statement>, ConversionError<Void>>>()?;

    Ok(Program {
        statements,
    })
}

pub fn get_statement_ast(pair: Pair<Rule>) -> Result<Statement, ConversionError<Void>> {
    assert_eq!(pair.as_rule(), Rule::statement);
    let inner_pair = pair.into_inner().next().unwrap();

    match inner_pair.as_rule() {
        Rule::declaration => Ok(Statement::Declaration(Box::new(get_declaration_ast(inner_pair)?))),
        Rule::printStatement => Ok(Statement::Print(Box::new(get_print_statement_ast(inner_pair)?))),
        x => panic!("Unexpected rule: {:?}", x),
    }
}

pub fn get_print_statement_ast(pair: Pair<Rule>) -> Result<PrintStatement, ConversionError<Void>> {
    assert_eq!(pair.as_rule(), Rule::printStatement);
    let inner_pair = pair.into_inner().next().unwrap();

    let expression = get_expression_ast(inner_pair)?;

    Ok(PrintStatement {
        expression,
    })
}

pub fn get_declaration_ast(pair: Pair<Rule>) -> Result<Declaration, ConversionError<Void>> {
    assert_eq!(pair.as_rule(), Rule::declaration);
    let mut inner_pairs = pair.into_inner();

    let identifier = get_identifier_ast(inner_pairs.next().unwrap())?;
    let type0 = get_type0_ast(inner_pairs.next().unwrap())?;
    let expression = get_expression_ast(inner_pairs.next().unwrap())?;

    Ok(Declaration {
        identifier,
        type0,
        expression,
    })
}

pub fn get_identifier_ast(pair: Pair<Rule>) -> Result<Identifier, ConversionError<Void>> {
    assert_eq!(pair.as_rule(), Rule::IDENTIFIER);
    Ok(Identifier {
        name: span_to_str(pair.as_span()).to_string(),
    })
}

pub fn get_type0_ast(pair: Pair<Rule>) -> Result<Type, ConversionError<Void>> {
    assert!(pair.as_rule() == Rule::type0 || pair.as_rule() == Rule::type1);

    match pair.as_rule() {
        Rule::type1 => {
            Ok(Type::Int)
        }
        Rule::type0 => {
            let mut inner_pairs = pair.into_inner();
            match inner_pairs.len() {
                1 => {
                    Ok(Type::Int)
                }
                2 => {
                    let type0 = get_type0_ast(inner_pairs.next().unwrap())?;
                    let type1 = get_type0_ast(inner_pairs.next().unwrap())?;
                    Ok(Type::Func(Box::new(type0), Box::new(type1)))
                }
                x => panic!("Unexpected number of inner pairs: {}", x),
            }
        }
        x => panic!("Unexpected rule: {:?}", x),
    }
}

pub fn get_expression_ast(pair: Pair<Rule>) -> Result<Expression, ConversionError<Void>> {
    assert_eq!(pair.as_rule(), Rule::expression);
    let inner_pair = pair.into_inner().next().unwrap();

    match inner_pair.as_rule() {
        Rule::literal => Ok(Expression::IntLiteral(IntLiteral {
            value: span_to_i32(inner_pair.as_span()),
        })),
        Rule::binary => Ok(Expression::BinaryOp(
            Box::new(get_binary_expression_ast(inner_pair)?)
        )),
        Rule::lambda => Ok(Expression::Lambda(
            Box::new(get_lambda_expression_ast(inner_pair)?)
        )),
        Rule::IDENTIFIER => Ok(Expression::Identifier(
            Box::new(get_identifier_ast(inner_pair)?)
        )),
        Rule::application => Ok(Expression::Application(
            Box::new(get_application_ast(inner_pair)?)
        )),
        x => todo!("unexpected rule: {:?}", x),
    }
}

pub fn get_application_ast(pair: Pair<Rule>) -> Result<Application, ConversionError<Void>> {
    assert_eq!(pair.as_rule(), Rule::application);
    let mut inner_pairs = pair.into_inner();

    let function = get_expression_ast(inner_pairs.next().unwrap())?;
    let arguments = inner_pairs.map(|pair| Box::new(get_expression_ast(pair).unwrap())).collect();

    Ok(Application {
        function: Box::new(function),
        arguments,
    })
}

pub fn get_lambda_expression_ast(pair: Pair<Rule>) -> Result<LambdaExpression, ConversionError<Void>> {
    assert_eq!(pair.as_rule(), Rule::lambda);
    let mut inner_pairs = pair.into_inner();

    let mut params = vec![];
    let mut body = None;

    for inner_pair in inner_pairs {
        match inner_pair.as_rule() {
            Rule::IDENTIFIER => {
                params.push(get_identifier_ast(inner_pair)?);
            }
            Rule::expression => {
                body = Some(get_expression_ast(inner_pair)?);
            }
            x => panic!("Unexpected rule: {:?}", x),
        }
    }

    Ok(LambdaExpression {
        parameter: params,
        body: Box::new(body.unwrap()),
    })
}

pub fn get_binary_expression_ast(pair: Pair<Rule>) -> Result<BinaryExpression, ConversionError<Void>> {
    assert_eq!(pair.as_rule(), Rule::binary);
    let mut inner_pairs = pair.into_inner();

    let left = get_expression_ast(inner_pairs.next().unwrap())?;
    let operator = get_binary_operator(inner_pairs.next().unwrap())?;
    let right = get_expression_ast(inner_pairs.next().unwrap())?;

    Ok(BinaryExpression {
        left: Box::new(left),
        operator,
        right: Box::new(right),
    })
}

pub fn get_binary_operator(pair: Pair<Rule>) -> Result<BinaryOperator, ConversionError<Void>> {
    assert_eq!(pair.as_rule(), Rule::operator);

    match span_to_str(pair.as_span()) {
        "+" => Ok(BinaryOperator::Add),
        "-" => Ok(BinaryOperator::Sub),
        "*" => Ok(BinaryOperator::Mul),
        "/" => Ok(BinaryOperator::Div),
        "==" => Ok(BinaryOperator::Eq),
        "!=" => Ok(BinaryOperator::Neq),
        "<" => Ok(BinaryOperator::Lt),
        ">" => Ok(BinaryOperator::Gt),
        "<=" => Ok(BinaryOperator::Le),
        ">=" => Ok(BinaryOperator::Ge),
        "&&" => Ok(BinaryOperator::And),
        "||" => Ok(BinaryOperator::Or),
        x => panic!("Unexpected operator: {}", x),
    }
}

fn span_to_str(span: pest::Span) -> &str {
    span.as_str()
}

fn span_to_i32(span: pest::Span) -> i32 {
    span.as_str().parse().unwrap()
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Declaration(Box<Declaration>),
    Print(Box<PrintStatement>),
}

#[derive(Debug)]
pub struct PrintStatement {
    pub expression: Expression,
}

#[derive(Debug)]
pub struct Declaration {
    pub identifier: Identifier,
    pub type0: Type,
    pub expression: Expression,
}

#[derive(Debug)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug)]
pub struct IntLiteral {
    pub value: i32,
}

#[derive(Debug)]
pub enum Type {
    Int,
    Func(Box<Type>, Box<Type>),
}

#[derive(Debug)]
pub enum Expression {
    Application(Box<Application>),
    BinaryOp(Box<BinaryExpression>),
    UnaryOp(Box<UnaryExpression>),
    Identifier(Box<Identifier>),
    Lambda(Box<LambdaExpression>),
    Conditional(Box<ConditionalExpression>),
    IntLiteral(IntLiteral),
}

#[derive(Debug)]
pub struct ConditionalExpression {
    pub condition: Box<Expression>,
    pub then_branch: Box<Expression>,
    pub else_branch: Box<Expression>,
}

#[derive(Debug)]
pub struct Application {
    pub function: Box<Expression>,
    pub arguments: Vec<Box<Expression>>,
}

#[derive(Debug)]
pub struct LambdaExpression {
    pub parameter: Vec<Identifier>,
    pub body: Box<Expression>,
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub enum BinaryOperator {
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

#[derive(Debug)]
pub enum UnaryExpression {
    Neg(Box<Expression>),
    Not(Box<Expression>),
}