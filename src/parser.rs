use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "bilang.pest"]
struct BilangParser;

#[allow(unused_imports)]
use super::ast;

pub fn parse_file(file_path: &str) {
    let source = std::fs::read_to_string(file_path).unwrap();
    let mut parsed_file = BilangParser::parse(Rule::program, &source)
        .expect("unsuccessful parse");

    let ast = ast::get_program_ast(parsed_file.next().unwrap()).expect("parsing error");


    /* TODO:
        - actually evaluate the AST
        - because i already know closures will be a mess:
            - ignore captured variables of closures until later -.-
        future plans:
        - check if types that are given are sound
            - first only check single variables and simple expressions
            - check higher order functions
        - infer types
        - add simple test suite
     */
}