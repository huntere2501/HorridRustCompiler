use crate::lexer_scanner::scanner::{Lexer, Token, TokenType};

pub(crate) struct Symbol_Table<'a> {
    lexeme: &'a str,
    token_type: &'a TokenType,
    data_type: &'a str,
    value: &'a i64,
    scope: &'a str,
}

impl <'a> Symbol_Table<'a>{
    pub fn new(self){
        0;
    }
    pub fn add(self){
        0;
    }
    pub fn remove(self){
        0;
    }
}