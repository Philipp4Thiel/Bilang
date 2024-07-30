// TODO remove this as soon as we have a semi working version

#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_mut)]

mod parser;
mod ast;

fn main() {
    // read file path from argument
    let file_path = std::env::args().nth(1).unwrap();
    let ast = parser::parse_file(&file_path);
}