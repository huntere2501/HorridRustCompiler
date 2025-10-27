use std::collections::HashMap;
use crate::lexer_scanner::scanner::{TokenType};

pub(crate) struct SymbolTableEntry<'a> {
    token_type: &'a TokenType,
    data_type: &'a str,
    value: &'a i64,
    scope: &'a str,
}

pub(crate) struct SymbolTable<'a> {
    entries: HashMap<&'a str, SymbolTableEntry<'a>>,
}

impl <'a> SymbolTableEntry<'a>{
    pub fn new(token_type: &TokenType, data_type: &str, value: &i64, scope: &str) -> Self{
        Self {token_type, data_type, value, scope}
    }
}

impl <'a> SymbolTable<'a>{
    pub fn new(entries: HashMap<&'a str, SymbolTableEntry<'a>>){
        Self{entries};
    }

    pub fn insert(&mut self, lexeme: &str, token_type: &TokenType, data_type: &str, value: &i64, scope: &str){
        self.entries.insert(lexeme, SymbolTableEntry {
            token_type: &token_type,
            data_type,
            value,
            scope,
        });
    }
    pub fn remove(&mut self, lexeme: &str){
        self.entries.remove(lexeme);
    }
    pub fn search(&mut self, lexeme: &str, token_type: &TokenType, data_type: &str, value: &i64, scope: &str){
        if self.entries.contains_key(lexeme){
            // Do Nothing
        }
        else{
            self.insert(lexeme, token_type, data_type, value, scope);
        }
    }
}