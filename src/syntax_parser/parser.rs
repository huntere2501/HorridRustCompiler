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
            TokenType::Whitespace => {
                Some(ASTExpression::whitespace())
            }
            TokenType::LineComment { doc_style } => {
                Some(ASTExpression::line_comment(doc_style))
            }
            TokenType::BlockComment { doc_style, terminated } => {
                Some(ASTExpression::block_comment(doc_style, terminated))
            }
            TokenType::Frontmatter { has_invalid_preceding_whitespace, invalid_infostring } => {
                Some(ASTExpression::frontmatter(has_invalid_preceding_whitespace, invalid_infostring))
            }
            TokenType::Identifier => {
                Some(ASTExpression::identifier())
            }
            TokenType::RawIdentifier => {
                Some(ASTExpression::raw_identifier())
            }
            TokenType::InvalidIdentifier => {
                Some(ASTExpression::invalid_identifier())
            }
            TokenType::Keyword => {
                Some(ASTExpression::keyword())
            }
            TokenType::UnknownPrefix => {
                Some(ASTExpression::unknown_prefix())
            }
            TokenType::UnknownPrefixLifetime => {
                Some(ASTExpression::unknown_prefix_lifetime())
            }
            TokenType::RawLifetime => {
                Some(ASTExpression::raw_lifetime())
            }
            TokenType::GuardedStrPrefix => {
                Some(ASTExpression::guarded_str_prefix())
            }
            TokenType::CharLiteral { terminated } => {
                Some(ASTExpression::char_literal(terminated))
            }
            TokenType::StringLiteral => {
                Some(ASTExpression::string_literal())
            }
            TokenType::RawStringLiteral => {
                Some(ASTExpression::raw_string_literal())
            }
            TokenType::CStringLiteral => {
                Some(ASTExpression::c_string_literal())
            }
            TokenType::RawCStringLiteral => {
                Some(ASTExpression::raw_c_string_literal())
            }
            TokenType::ByteLiteral => {
                Some(ASTExpression::byte_literal())
            }
            TokenType::ByteStringLiteral { terminated } => {
                Some(ASTExpression::byte_string_literal(terminated))
            }
            TokenType::RawByteLiteral => {
                Some(ASTExpression::raw_byte_literal())
            }
            TokenType::IntegerLiteral { base, empty_int } => {
                Some(ASTExpression::integer_literal(base, empty_int))
            }
            TokenType::FloatLiteral { base, empty_exponent } => {
                Some(ASTExpression::float_literal(base, empty_exponent))
            }
            TokenType::Lifetime { starts_with_number } => {
                Some(ASTExpression::lifetime(starts_with_number))
            }
            TokenType::Semi => {
                Some(ASTExpression::semi())
            }
            TokenType::Comma => {
                Some(ASTExpression::comma())
            }
            TokenType::Dot => {
                Some(ASTExpression::dot())
            }
            TokenType::OpenParen => {
                Some(ASTExpression::open_paren())
            }
            TokenType::CloseParen => {
                Some(ASTExpression::close_paren())
            }
            TokenType::OpenBrace => {
                Some(ASTExpression::open_brace())
            }
            TokenType::CloseBrace => {
                Some(ASTExpression::close_brace())
            }
            TokenType::OpenBracket => {
                Some(ASTExpression::open_bracket())
            }
            TokenType::CloseBracket => {
                Some(ASTExpression::close_bracket())
            }
            TokenType::At => {
                Some(ASTExpression::at())
            }
            TokenType::Pound => {
                Some(ASTExpression::pound())
            }
            TokenType::Tilde => {
                Some(ASTExpression::tilde())
            }
            TokenType::Question => {
                Some(ASTExpression::question())
            }
            TokenType::Colon => {
                Some(ASTExpression::colon())
            }
            TokenType::SemiColon => {
                Some(ASTExpression::semicolon())
            }
            TokenType::Dollar => {
                Some(ASTExpression::dollar())
            }
            TokenType::Eq => {
                Some(ASTExpression::eq())
            }
            TokenType::Bang => {
                Some(ASTExpression::bang())
            }
            TokenType::Lt => {
                Some(ASTExpression::lt())
            }
            TokenType::Gt => {
                Some(ASTExpression::gt())
            }
            TokenType::Minus => {
                Some(ASTExpression::minus())
            }
            TokenType::And => {
                Some(ASTExpression::and())
            }
            TokenType::Or => {
                Some(ASTExpression::or())
            }
            TokenType::Plus => {
                Some(ASTExpression::plus())
            }
            TokenType::Star => {
                Some(ASTExpression::star())
            }
            TokenType::Slash => {
                Some(ASTExpression::slash())
            }
            TokenType::Caret => {
                Some(ASTExpression::caret())
            }
            TokenType::Percent => {
                Some(ASTExpression::percent())
            }
            TokenType::Unknown => {
                Some(ASTExpression::unknown())
            }
            TokenType::Error => {
                Some(ASTExpression::error())
            }
            TokenType::EOF => {
                Some(ASTExpression::eof())
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