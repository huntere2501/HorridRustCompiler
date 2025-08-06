use crate::syntax_parser::{ASTExpression, ASTStatement};
use crate::lexer_scanner::scanner::{Lexer, Token, TokenType};

pub struct Parser{
    tokens: Vec<Token>,
    current: usize,
}

impl Parser{
    pub fn new(tokens: Vec<Token>) -> Self{
        Self { tokens: tokens.iter().filter(
            |token| token.kind != TokenType::Unknown
        ).map(|token| token.clone()).collect(), current: 0}
    }

    pub fn from_tokens(input: &str) -> Self {
        let mut lexer  = Lexer::new(input);
        let mut tokens = Vec::new();
        while let Some(token) = Some(lexer.next_token()){
            tokens.push(token);
        }
        Self {tokens, current: 0}
    }

    pub fn parse_statement(&mut self) -> Option<ASTStatement>{
        let expr = self.parse_expression()?;
        Some(ASTStatement::expression(expr))
    }

    pub fn parse_expression(&mut self) -> Option<ASTExpression>{
        self.parse_primary_expression()
    }

    fn parse_primary_expression(&mut self) -> Option<ASTExpression>{
        let token: &Token = self.consume_token()?;
        println!("{:?}",token.kind);
        // Betcha it has to do with the token.clone()
        match token.clone().kind {
            // // ISSUE WITH THIS MATCH STATEMENT!!
            // TokenType::Integer(number) =>{
            //     Some(ASTExpression::number(number))
            // }
            // TokenType::Float(float) =>{
            //     Some(ASTExpression::float(float))
            // }
            // TokenType::Identifier(identifier) =>{
            //     Some(ASTExpression::identifier(String::from(identifier)))
            // }
            // TokenType::Operator(operator) =>{
            //     Some(ASTExpression::operator(String::from(operator)))
            // }
            // TokenType::Keyword(keyword) =>{
            //     Some(ASTExpression::keyword(String::from(keyword)))
            // }
            // TokenType::WhiteSpace =>{
            //     Some(ASTExpression::whitespace())
            // }
            _ => {
                None
            }
        }
    }

    pub fn next_statement(&mut self) -> Option<ASTStatement>{
        self.parse_statement()
    }

    pub fn peek(&self, offset:isize) -> Option<&Token> {
        self.tokens.get((self.current as isize + offset) as usize)
    }

    fn current(&self) -> Option<&Token>{
        self.peek(0)
    }

    fn consume_token(&mut self) -> Option<&Token>{
        // Offset is negative One as to look at the previous value.
        self.current += 1;
        let token: &Token = self.peek(-1)?;
        Some(token)

    }
}