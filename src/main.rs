mod lexer_scanner;
use crate::lexer_scanner::scanner::{Lexer, TokenType};
mod syntax_parser;
use crate::syntax_parser::Ast;
use crate::syntax_parser::parser::Parser;

// Reformat Parsing through input.
// Current; Read Till whitespace, save, check value

// New:
// 1.Read each character
// 2.Check value(maybe next),
// 3.Save to temp
// 4.Process result.


fn main() {
    // Just for testing, the Parse will call upon the Lexer in actual runs
    let input  = "5";
    let mut lexer  = Lexer::new(input);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token();
        if token.kind == TokenType::EOF{
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
