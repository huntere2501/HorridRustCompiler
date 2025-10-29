use std::collections::HashMap;
use std::sync::LazyLock;
use std::collections::HashSet;
use crate::lexer_scanner::scanner::{TokenType};

static KEYWORDS: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
    HashSet::from([
        // Strict keywords (cannot be used as identifiers)
        "as", "async", "await", "break", "const", "continue", "crate", "dyn",
        "else", "enum", "extern", "false", "fn", "for", "if", "impl", "in",
        "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
        "self", "Self", "static", "struct", "super", "trait", "true", "type",
        "union", "unsafe", "use", "where", "while",
        // Reserved keywords
        "abstract", "become", "box", "do", "final", "macro", "override",
        "priv", "typeof", "unsized", "virtual", "yield",
        // Weak keywords
        "macro_rules", "try",
    ])
});

#[derive(Debug)]
pub(crate) struct SymbolTableEntry {
    token_type: TokenType,
    value: i64,
    scope: String,
}

#[derive(Debug)]
pub(crate) struct SymbolTable {
    entries: HashMap<String, SymbolTableEntry>,
}

impl SymbolTableEntry {
    pub fn new(token_type: TokenType, value: i64, scope: String) -> Self {
        Self { token_type, value, scope }
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut entries = HashMap::new();

        // Initialize with keywords
        for &keyword in KEYWORDS.iter() {
            entries.insert(
                keyword.to_string(),
                SymbolTableEntry {
                    token_type: TokenType::Keyword,
                    value: 0,
                    scope: "global".to_string(),
                }
            );
        }

        Self { entries }
    }

    pub fn insert(&mut self, lexeme: String, token_type: TokenType, value: i64, scope: String) {
        self.entries.insert(lexeme, SymbolTableEntry {
            token_type,
            value,
            scope,
        });
    }

    pub fn remove(&mut self, lexeme: &str) {
        self.entries.remove(lexeme);
    }

    pub fn search_or_enter(&mut self, lexeme: String, token_type: TokenType, value: i64, scope: String) {
        if self.entries.contains_key(&lexeme) {
            return;
        } else {
            self.insert(lexeme, token_type, value, scope);
        }
    }

    pub fn view_info(&self, lexeme: &str, info_type: &str) -> Option<String> {
        if let Some(entry) = self.entries.get(lexeme) {
            match info_type {
                "token_type" => Some(format!("{:?}", entry.token_type)),
                "value" => Some(entry.value.to_string()),
                "scope" => Some(entry.scope.clone()),
                _ => None
            }
        } else {
            None
        }
    }
}