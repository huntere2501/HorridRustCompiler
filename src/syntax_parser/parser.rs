use crate::syntax_parser::{ASTExpression, ASTStatement};
use crate::lexer_scanner::scanner::{Lexer, Token, TokenType};

pub struct Parser{
    tokens: Vec<Token>,
    current: usize,
}

impl Parser{
    pub fn new() -> Self{
        Self { tokens: Vec::new(), current: 0}
    }

    pub fn from_input(input: &str) -> Self {
        let mut lexer: Lexer::new(input);
        let mut tokens = Vec::new();
        while let Some(token) = lexer.next_token() {
            tokens.push(token);
        }
        Self {tokens, current: 0}
    }

    pub fn parse_statement(&mut self) -> Option<ASTStatement>{
        let token: &Token = self.current()?;
        let expr = self.parse_expression()?;
        return Some(ASTStatement::expression(expr));
    }

    pub fn parse_expression(&mut self) -> Option<ASTExpression>{
        let token: &Token = self.current()?;
        match token.kind{
            TokenType::Integer(number) =>{
                Some(ASTExpression::number(number))
            }
            _ => {
                None
            }
        }

    }



    pub fn next_statement(&mut self) -> Option<ASTStatement>{
        return self.parse_statement();
    }

    pub fn peek(&self, offset:usize) -> Option<Lexer::Token> {
        self.tokens.get(self.current+offset)
    }

    fn current(&self) -> Option<Lexer::Token>{
        self.peek(0)
    }

}