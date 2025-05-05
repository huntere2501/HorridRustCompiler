use crate::syntax_parser::{ASTBinaryOperator, ASTExpression, ASTStatement};
use crate::lexer_scanner::scanner::{Lexer, Token, TokenType};

pub struct Parser{
    tokens: Vec<Token>,
    current: usize,
}

impl Parser{
    pub fn new(tokens: Vec<Token>) -> Self{
        Self { tokens: tokens.iter().filter(
            |token| token.kind != TokenType::WhiteSpace
        ).map(|token| token.clone()).collect(), current: 0}
    }

    pub fn from_tokens(input: &str) -> Self {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        while let Some(token) = lexer.next_token(){
            tokens.push(token);
        }
        Self {tokens, current: 0}
    }

    pub fn parse_statement(&mut self) -> Option<ASTStatement>{
        let token: &Token = self.consume_token()?;
        if token.kind == TokenType::Unknown{
            return None
        }
        let expr = self.parse_expression()?;
        Some(ASTStatement::expression(expr))
    }

    pub fn parse_expression(&mut self) -> Option<ASTExpression>{
        self.parse_binary_expression(0)
    }

    pub fn parse_binary_expression(&mut self, precedence:u8) -> Option<ASTExpression>{
        let mut left = self.parse_primary_expression()?;
        while let Some(operator) =  self.parse_binary_operator(){
            let operator_precedent = operator.precedence();
            if operator_precedent < precedence{
                break
            }
            let right = self.parse_binary_expression(operator_precedent)?;
            left = ASTExpression::binary(operator, left, right);
        }
        Some(left)
    }

    // THIS NEEDS TO BE CORRECTED.
    fn parse_binary_operator(&mut self) -> Option<ASTBinaryOperator>{
        let token = self.consume_token()?;
        let kind = match token.kind {
            TokenType::Plus{
                Some(ASTBinaryOperatorKind::Plus)
            },
            TokenType::Minus{
                Some(ASTBinaryOperatorKind::Minus)
            },
            TokenType::Asterisk{
                Some(ASTBinaryOperatorKind::Multiply)
            },
            TokenType::Slash{
                Some(ASTBinaryOperatorKind::Divide)
            },
            _ => None
        }
        kind.map(|kind| ASTBinaryOperator::new(kind, token.clone()))
    }

    fn parse_primary_expression(&mut self) -> Option<ASTExpression>{
        let token: &Token = self.consume_token()?;
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
        self.parse_statement()
    }

    pub fn peek(&self, offset:usize) -> Option<&Token> {
        self.tokens.get((self.current as isize + offset as isize) as usize)
    }

    fn current(&self) -> Option<&Token>{
        self.peek(0)
    }

    fn consume_token(&mut self) -> Option<&Token>{
        self.current += 1;
        // Keep in mind about this offset
        let token: &Token = self.peek(1)?;
        return Some(token);

    }

}