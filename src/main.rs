mod lexer_scanner;
use crate::lexer_scanner::scanner::Lexer;
mod syntax_parser;
use crate::syntax_parser::Ast;
use crate::syntax_parser::parser::Parser;

// NOTES:
// VISUALIZE IS NOT PRINTING ANYTHING. (NOT WORKING or NOT RECEIVING ANYTHING)
// - Delimiters, Comments, and Unknowns are not properly handled.
// - Need to read through a specific string for objects like pointers and ;
// - Need to add delims, comments, and eof to parser.
// - Need to check syntax of original code.

fn main() {
    // Just for testing, the Parse will call upon the Lexer in actual runs.
    let input  = "2 + 5";
    // let mut lexer  = Lexer::new(input);
    // let mut tokens = Vec::new();
    // while let Some(token) = lexer.next_token(){
    //     tokens.push(token);
    // }
    let mut ast: Ast = Ast::new();
    let mut parser: Parser = Parser::from_tokens(input);
    while let Some(stmt) = parser.next_statement(){
        ast.add_statement(stmt);
    }
    ast.visualize();
}
