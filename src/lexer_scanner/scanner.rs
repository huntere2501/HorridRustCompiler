/*
The Lexical Analyzer breaks up code into tokens, and runs operations based on the tokens it receives.
It will have to break up the values by type, operator, delimiter, numerics, strings, etc.
It will also need to work with the error_handler to call errors when they are found.
*/
use phf::phf_map;
use regex::Regex;

#[derive(Clone)]
pub enum Keyword {
    As,
    Continue,
    Break,
    Const,
    Crate,
    Else,
    Enum,
    Extern,
    False,
    Fn,
    For,
    If,
    Impl,
    In,
    Let,
    Loop,
    Match,
    Mod,
    Move,
    Mut,
    Pub,
    Ref,
    Return,
    Static,
    Struct,
    Super,
    Trait,
    True,
    Type,
    Unsafe,
    Use,
    Where,
    While,
    Async,
    Await,
    Dyn
}
static KEYWORDS: phf::Map<&'static str, Keyword> = phf_map! {
    "loop" => Keyword::Loop,
    "continue" => Keyword::Continue,
    "break" => Keyword::Break,
    "fn" => Keyword::Fn,
    "extern" => Keyword::Extern,
    "as" => Keyword::As,
    "const" => Keyword::Const,
    "crate" => Keyword::Crate,
    "else" => Keyword::Else,
    "enum" => Keyword::Enum,
    "false" => Keyword::False,
    "for" => Keyword::For,
    "if" => Keyword::If,
    "impl" => Keyword::Impl,
    "in" => Keyword::In,
    "let" => Keyword::Let,
    "match" => Keyword::Match,
    "mod" => Keyword::Mod,
    "move" => Keyword::Move,
    "mut" => Keyword::Mut,
    "pub" => Keyword::Pub,
    "ref" => Keyword::Ref,
    "return" => Keyword::Return,
    "static" => Keyword::Static,
    "struct" => Keyword::Struct,
    "super" => Keyword::Super,
    "trait" => Keyword::Trait,
    "true" => Keyword::True,
    "type" => Keyword::Type,
    "unsafe" => Keyword::Unsafe,
    "use" => Keyword::Use,
    "where" => Keyword::Where,
    "while" => Keyword::While,
    "async" => Keyword::Async,
    "await" => Keyword::Await,
    "dyn" => Keyword::Dyn,
};

// NEED TO UPDATE LIST TO INCLUDE ALL OPERATORS
// Gotta figure out how operators are seperated.
#[derive(Clone)]
enum Operators{
    MODULO,
    MODULO_EQUALS,
    BITWISE,
    BITWISE_EQUALS,
    MULTIPLY,
    MULT_EQUALS,
    ADD,
    ADD_EQUALS,
    SUB,
    SUB_EQUALS,
    DIVIDE,
    DIVIDE_EQUALS

}
static OPERATORS: phf::Map<&'static str, Operators> = phf_map! {
    "%" => Operators::MODULO,
    "%=" => Operators::MODULO_EQUALS,
    "&" => Operators::BITWISE,
    "&=" => Operators::BITWISE_EQUALS,
    "*" => Operators::MULTIPLY,
    "*=" => Operators::MULT_EQUALS,
    "+" => Operators::ADD,
    "+=" => Operators::ADD_EQUALS,
    "/" => Operators::DIVIDE,
    "/=" => Operators::DIVIDE_EQUALS,
};

#[derive(Debug, PartialEq, Clone)]
pub enum DocStyle {
    /// Used to handle all types of comment types that Rust has.
    Outer,
    Inner,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Base {
    /// Handles all integer base types.
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    /// Anything terminated represents items that need to have null termination == "/0"
    Int { base: Base, empty_int: bool },
    Float { base: Base, empty_exponent: bool },
    Char { terminated: bool },
    Byte { terminated: bool },
    Str { terminated: bool },
    ByteStr { terminated: bool },
    CStr { terminated: bool },
    RawStr { n_hashes: Option<u8> },
    RawByteStr { n_hashes: Option<u8> },
    RawCStr { n_hashes: Option<u8> },
}

// Enum of different types a token might be, covers most options for now.
#[derive(Debug, PartialEq, Clone)]
pub(crate) enum TokenType {
    LineComment {
        doc_style: Option<DocStyle>,
    },
    BlockComment {
        doc_style: Option<DocStyle>,
        terminated: bool,
    },
    WhiteSpace,    // Spaces, tabs, newlines
    Identifier(String),    // Variable/function names
    Operator(String),      // Mathematical or logical operators
    Keyword(String),       // Reserved language words
    Delimiter(String),     // Punctuation like parentheses or semicolon
    Comment(String),
    Unknown,
    EOF
}

#[derive(Debug, PartialEq, Clone)]
pub struct TextSpan {
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) literal : String,
}

impl TextSpan {
    pub fn new(start: usize, end: usize, literal: String) -> Self{
        Self {start, end, literal}
    }

    pub fn length(&self) -> usize {
        self.end - self.start
    }
}
// Token Struct to construct Token Values
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub(crate) kind: TokenType,
    pub(crate) span: TextSpan,
}

impl Token {
    pub fn new(kind: TokenType, span: TextSpan) -> Self{
        Self{kind, span}
    }
}
// The Lexer structure will be used identify the main parts of a value, will stop based on delimiter.
pub(crate) struct Lexer<'a> {
    input: &'a str,
    current_position: usize,
}


// Lexer structure as follows:
// Read character by character.
// Create Regex for each token type
// From regex create a DFA to match input text.
// In the case where you have similar texts, the text that matches the longest Regex pattern wins.
// Once finite automata checks, code is run for each specific case.


// Create a basic Lexer structure, start at zero for all values.
impl <'a> Lexer<'a>{
    pub fn new(input: & 'a str) -> Self {
        Self {input, current_position: 0}
    }
    // First fix keyword/operator finding
    // Then handle the operator/ keyword issue.
    pub fn next_token(&mut self) -> Option<Token>{
        // Check if we are at the end of our input.
        // Return EOF if we are and stop making tokens.
        if self.current_position == self.input.len() {
            let eof_char: char = '\0';
            self.current_position += 1;
            return Some(Token::new(
                TokenType::EOF,
                TextSpan::new(0,0,eof_char.to_string())
            ));
        }
        let c  = self.current_char();
        c.map(|c| {
            let start = self.current_position;
            let mut kind = TokenType::Unknown;
            if self.is_number_start() {
                let number: i64 = self.get_number();
                kind = TokenType::Integer(number);
            }
            else if self.is_float_start(){
                let float: f64 = self.get_float();
                kind = TokenType::Float(float);
            }
            else if self.is_keyword(){
                let keyword: String = self.get_keyword();
                kind = TokenType::Keyword(keyword);
            }
            else if self.is_operator(){
                let operator: String = self.get_operator();
                kind = TokenType::Operator(operator);
            }
            else if self.is_identifier(){
                let identifier: String = self.get_identifier();
                kind = TokenType::Identifier(identifier);
            }
            else if Self::get_whitespace(&c) {
                self.consume();
                kind = TokenType::WhiteSpace;
            }
            else if Self::is_delimiter(&c) {
                self.consume();
                kind = TokenType::Delimiter(String::from(c));
            }
            else if Self::is_comment(&c){
                let comment:String = self.get_comment();
                kind = TokenType::Comment(String::from(comment));
            }

            let end = self.current_position;
            let literal = self.input[start..end].to_string();
            let span = TextSpan::new(start, end, literal);
            Token::new(kind, span)
        })
    }

    // For checking single character values.
    fn current_char(&self) -> Option<char> {
        self.input.chars().nth(self.current_position)
    }
    fn next_char(&self) -> Option<char> {
        self.input.chars().nth(self.current_position+1)
    }

    // For grabbing the current character
    fn consume(&mut self) -> Option<char>{
        if self.current_position > self.input.len(){
            return None
        }
        let c = self.current_char();
        self.current_position += 1;
        c
    }
    // Ex: *var*5
    // Read until we hit a new type of token
    // Need to fix the way I am checking values for next time.
    fn check_value(&mut self) -> String {
        let mut add_string = String::new();
        let mut count = 0;
        while let Some(mut value) = self.current_char(){
            let next_val  = self.next_char();
            if next_val == Option::from('*') || next_val == Option::from('&') {
                value = self.consume().unwrap();
                add_string.push(value);
                count += 1;
                break
            }
            if value == '*' || value == '&'{
                value = self.consume().unwrap();
                add_string.push(value);
                count += 1;
                break
            }
            else if !value.is_whitespace(){
                // Match statement to handle regex?
                value = self.consume().unwrap();
                add_string.push(value);
                count += 1;
            }
        }
        self.current_position -= count;
        add_string
    }

    // For grabbing an entire value up to whitespace.
    // Check value should give a value to consume value.
    fn consume_value(&mut self) -> String {
        let mut add_string = String::new();
        while let Some(mut value) = self.current_char(){
            let next_val  = self.next_char();
            if next_val == Option::from('*') || next_val == Option::from('&') {
                value = self.consume().unwrap();
                add_string.push(value);
                break
            }
            if value == '*' || value == '&'{
                value = self.consume().unwrap();
                add_string.push(value);
                break
            }
            else if !value.is_whitespace(){
                value = self.consume().unwrap();
                add_string.push(value);
            }
        }
        add_string
    }

    pub fn is_whitespace(c: char) -> bool {
        /// Stolen from official rust compiler, since whitespace check will practically be the same.
        // This is Pattern_White_Space.
        //
        // Note that this set is stable (ie, it doesn't change with different
        // Unicode versions), so it's ok to just hard-code the values.

        matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
    }

    fn is_delimiter(c: &char) -> bool {
        if *c == ';'{
            true
        }
        else{
            false
        }
    }

    fn is_comment(c: &char) -> bool{
        if *c == '/'{
            true
        }
        else{
            false
        }
    }

    // NOT CORRECT, going to have to do more reading.
    fn get_comment(&mut self) -> String {
        let line = self.check_value();
        if line  == "//" || "//!".parse().unwrap() || "///".parse().unwrap() || "/*".parse().unwrap() {
            line
        }
        else{
            line
        }
    }

    fn is_identifier(&mut self) -> bool{
        let identifier = self.check_value();
        let word_reg = Regex::new(r"[a-zA-Z_]+").unwrap();
        if (identifier == "*" || identifier == "&") && word_reg.is_match(&*self.next_char().unwrap().to_string()){
            true
        }
        else if KEYWORDS.get(&*identifier).cloned().is_none() && OPERATORS.get(&*identifier).cloned().is_none() && !identifier.is_empty() {
           true
        }
        else{
            false
        }
    }

    fn get_identifier(&mut self) -> String{
        let identifier = self.consume_value();
        identifier
    }

    fn is_number_start(&mut self) -> bool {
        let comp_str: String = self.check_value();
        comp_str.parse::<i64>().is_ok()
    }

    fn get_number(&mut self) -> i64 {
        let comp_str: String = self.consume_value();
        let number: i64 = comp_str.parse::<i64>().unwrap();
        number
    }

    fn is_float_start(&mut self) -> bool {
        let comp_str: String = self.check_value();
        comp_str.parse::<f64>().is_ok()
    }

    fn get_float(&mut self) -> f64 {
        let comp_str: String = self.consume_value();
        let float: f64 = comp_str.parse::<f64>().unwrap();
        float
    }

    fn build_keyword_number_regex(&mut self, keywords: phf::Map<&'static str, Keyword>) -> String {
        let mut keys: Vec<&str> = keywords.keys().copied().collect();

        // Sort by length (longest first) to handle overlapping operators correctly
        // This ensures "==" is matched before "=" if both exist
        keys.sort_by(|a, b| b.len().cmp(&a.len()));

        // Escape special regex characters
        let escaped_keys: Vec<String> = keys.iter()
            .map(|&key| regex::escape(key))
            .collect();

        // Join with | (alternation) and add number pattern
        format!("({})", escaped_keys.join("|"))
    }

    fn is_keyword(&mut self) -> bool {
        let mut comp_str: String = self.check_value();
        if KEYWORDS.get(&*comp_str).cloned().is_some() {
            true
        } else {
            false
        }
    }

    fn get_keyword(&mut self) -> String{
        let comp_str: String = self.consume_value();
        comp_str
    }

    fn build_operator_number_regex(&mut self, operators: phf::Map<&'static str, Operators>) -> String {
        let mut keys: Vec<&str> = operators.keys().copied().collect();

        // Sort by length (longest first) to handle overlapping operators correctly
        // This ensures "==" is matched before "=" if both exist
        keys.sort_by(|a, b| b.len().cmp(&a.len()));

        // Escape special regex characters
        let escaped_keys: Vec<String> = keys.iter()
            .map(|&key| regex::escape(key))
            .collect();

        // Join with | (alternation) and add number pattern
        format!("({})[0-9]+", escaped_keys.join("|"))
    }

    fn is_operator(&mut self) -> bool {
        let operator: String = self.check_value();
        let num_reg = Regex::new(r"[0-9_]+").unwrap();
        // let op_grab = self.build_operator_number_regex(*OPERATORS);
        // let op_reg = Regex::new(&op_grab);
        if operator == "*" && num_reg.is_match(&*self.next_char().unwrap().to_string()){
            true
        }
        else if OPERATORS.get(&*operator).cloned().is_some() {
            true
        } else {
            false
        }
    }

    fn get_operator(&mut self) -> String{
        let operator: String = self.consume_value();
        operator
    }
}