mod lexer_scanner;
use crate::lexer_scanner::scanner::Lexer;
mod syntax_parser;
use crate::syntax_parser::Ast;
use crate::syntax_parser::parser::Parser;

fn main() {
    // Just for testing, the Parse will call upon the Lexer in actual runs.
    let input  = "7";
    let mut lexer  = Lexer::new(input);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next_token(){
        tokens.push(token);
    }
    let mut ast: Ast = Ast::new();
    let mut parser: Parser = Parser::from_tokens(tokens);
    while let Some(stmt) = parser.next_statement(){
        ast.add_statement(stmt);
    }
    ast.visualize();
}
