use std::cell::RefCell;
use std::rc::Rc;
use crate::syntax_parser::{whitespace, ASTExpression, ASTStatement};
use crate::lexer_scanner::scanner::{Lexer, Token, TokenType};
use crate::symbol_table::symbol_table::SymbolTable;

pub(crate) struct Parser{
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
        let symbol_tab = Rc::new(RefCell::new(SymbolTable::new()));
        let mut lexer  = Lexer::new(input, symbol_tab.clone());
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
            TokenType::Whitespace => {
                Some(whitespace())
            }
            _ => None
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