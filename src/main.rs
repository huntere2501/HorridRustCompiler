mod lexer_scanner;
use crate::lexer_scanner::scanner::{Lexer, TokenType};
mod syntax_parser;
mod error_handler;
mod symbol_table_mng;

use std::fs;
use crate::syntax_parser::Ast;
use crate::syntax_parser::parser::Parser;


fn main() {
    // Just for testing, the Parse will call upon the Lexer in actual runs
    let file_path: &str = "C:/Users/hunte/RustroverProjects/HorridRustCompiler/src/lexer_scanner/rust_test.rs";
    let input = &fs::read_to_string(file_path).unwrap();
    let mut lexer  = Lexer::new(input);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token();
        if token.kind == TokenType::EOF{
            tokens.push(token);
            break;
        }
        else{
            tokens.push(token);
        }
    }
    println!("{:?}", tokens);
    // let mut ast: Ast = Ast::new();
    // let mut parser: Parser = Parser::from_tokens(input);
    // while let Some(stmt) = parser.next_statement(){
    //     ast.add_statement(stmt);
    // }
    // ast.visualize();
}
