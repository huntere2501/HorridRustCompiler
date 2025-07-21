/*
The Lexical Analyzer breaks up code into tokens, and runs operations based on the tokens it receives.
It will have to break up the values by type, operator, delimiter, numerics, strings, etc.
It will also need to work with the error_handler to call errors when they are found.
Much like the official rust compiler, the point of this Lexer is to break down Rust's main components to make actual tokenization easier later.
*/
use std::ffi::c_char;
use std::ops::Add;
use phf::phf_map;
use regex::Regex;
use TokenType::*;
use std::str::Chars;

pub use unicode_xid::UNICODE_VERSION as UNICODE_XID_VERSION;
use crate::lexer_scanner::scanner:: Whitespace;
use crate::lexer_scanner::scanner::LiteralKind::{Float, Int};

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct GuardedStr {
    pub n_hashes: u32,
    pub terminated: bool,
    pub token_len: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum RawStrError {
    InvalidStarter { bad_char: char },
    NoTerminator { expected: u32, found: u32, possible_terminator_offset: Option<u32> },
    TooManyDelimiters { found: u32 },
}

/// List of token types and properties.
/// Raw simply means any value that starts with r#
#[derive(Debug, PartialEq, Clone)]
pub(crate) enum TokenType {
    Whitespace,
    LineComment {
        doc_style: Option<DocStyle>,
    },
    BlockComment {
        doc_style: Option<DocStyle>,
        terminated: bool,
    },
    Frontmatter{
        has_invalid_preceding_whitespace: bool,
        invalid_infostring: bool,
    },
    Identifier_or_Keyword,
    RawIdentifier,
    InvalidIdentifier,
    UnknownPrefix,
    UnknownPrefixLifetime,
    RawLifetime,
    GuardedStrPrefix,
    Literal {
        kind: LiteralKind,
        suffix_start: u32,
    },
    Lifetime {
        starts_with_number: bool,
    },
    Semi,
    Comma,
    Dot,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    At,
    Pound,
    Tilde,
    Question,
    Colon,
    Dollar,
    Eq,
    Bang,
    Lt,
    Gt,
    Minus,
    And,
    Or,
    Plus,
    Star,
    Slash,
    Caret,
    Percent,
    Unknown,
    EOF,
}

// Useful constant values to keep for specific checks.
pub(crate) const EOF_CHAR: char = '\0';
pub const ZERO_WIDTH_JOINER: char = '\u{200d}';


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
/// The Lexer structure will be used identify the main parts of a value, will stop based on delimiter.
pub(crate) struct Lexer<'a> {
    input: &'a str,
    current_position: usize,
}


/// Multiple TODOs
/// TODO: Test Values one by one and see if they work with match statement
/// TODO: Spend time updating functions to work with your specific use cases.
/// TODO: Be able to read and tokenize a simple Hello World in Rust.
/// TODO: Update certain functions since Im using string input and not a direct Chars Iterator.
/// TODO: Main compiler takes in bytes directly instead of chars == Figure out why.

/// Create a basic Lexer structure, start at zero for all values.
impl <'a> Lexer<'a>{
    pub fn new(input: & 'a str) -> Self {
        Self {input, current_position: 0}
    }
    // Will read through each character in an input string and match the value to a token type.
    // Uses .map() to create an iterator and match statement to find correct token type.
    pub fn next_token(&mut self) -> Token{
        // Check if we are at the end of our input.
        // Return EOF if we are and stop making tokens.
        let Some(c) = self.next_char() else {
            return Token::new( EOF, TextSpan::new(0,0,String::from(EOF_CHAR)));
        };
        let c:char  = self.current_char();
        c.map(|c| {
            let start:usize = self.current_position;
            match c{
                /// Basic check of all values char by char.
                'a' =>  Literal,
                c if c.is_whitespace() => self.whitespace(),
                c if self.is_comment(&c) => self.comment(),
                c if self.check_keyword() => self.keyword(),
                c if self.check_identifier(c) => self.identifier(),
                'b' => self.byte_string_check(),
                'c' => self.byte_string_check(),
                ',' => Comma,
                '.' => Dot,
                '(' => OpenParen,
                ')' => CloseParen,
                '{' => OpenBrace,
                '}' => CloseBrace,
                '[' => OpenBracket,
                ']' => CloseBracket,
                '@' => At,
                '#' => Pound,
                '~' => Tilde,
                '?' => Question,
                ':' => Colon,
                '$' => Dollar,
                '=' => Eq,
                '!' => Bang,
                '<' => Lt,
                '>' => Gt,
                '-' => Minus,
                '&' => And,
                '|' => Or,
                '+' => Plus,
                '*' => Star,
                '/' => Slash,
                '^' => Caret,
                '%' => Percent,
                _ => Unknown
            }

            /// TODO After receiving TokenType need to input string to get literal and span with token.
            let end:usize = self.current_position;
            let literal:String = self.input[start..end].to_string();
            let span:TextSpan = TextSpan::new(start, end, literal);
            Token::new(c, span)
        })
    }

    /// For checking single character values.
    /// Using clone copies an iterator which means the value is not consumed until we want to consume it.
    /// Only current_char gets consumes, second and third_char are used for checks.
    fn current_char(&self) -> char {
        self.input.nth(self.current_position)
    }

    /// Move to next character and consume the current one.
    fn next_char(&self) -> char {
        self.input.nth(self.current_position+1)
    }

    /// Checks the previous character with clone as to not consume characters.
    fn prev_char(&self) -> char {
        self.input.clone().nth(self.current_position-1)
    }

    /// Move up input a certain number of characters.
    fn next_while(&self, count: usize) -> char {
        let mut i = 0;
        while i < count{
            self.input.nth(self.current_position+1);
            i += 1;
        }
    }

    /// Check current character, but use clone() so item doesn't get consumed.
    fn check_curr_char(&self) -> char {
        self.input.clone().nth(self.current_position)
    }

    /// Check next character, but use clone() so item doesn't get consumed.
    fn first_char(&self) -> char {
        self.input.clone().nth(self.current_position+1)
    }

    /// Check second character, but use clone() so item doesn't get consumed.
    fn second_char(&self) -> char {
        self.input.clone().nth(self.current_position+2)
    }

    /// Check third character, but use clone() so item doesn't get consumed.
    fn third_char(&self) -> char {
        self.input.clone().nth(self.current_position+3)
    }

    /// Moves a number of specified bytes
    /// Specific compiler instances require an exact jump to the correct byte number.
   fn move_bytes(&mut self, n: usize) {
        self.input = self.as_str()[n..].chars();
    }


    /// Reads each character until we hit a character we don't want to consume.
    fn consume_until(&mut self, c: char) {
        while c != self.first_char() && c != EOF_CHAR {
            self.next_char();
        }
    }
    /// Consumes while character value is equal to what is provided.
    /// Returns a bool from a function with a wildcard input(_).
    fn consume_while(&mut self, c: fn(_) -> bool)  {
        while self.current_char() == c {
            self.next_char();
        }
    }

    pub fn is_whitespace(c: char) -> bool {
        // Stolen from official rust compiler, since whitespace check will practically be the same.
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

    fn whitespace(&mut self) -> TokenType{
        self.consume_while(self.is_whitespace());
        Whitespace
    }

    fn line_comment(&mut self) -> TokenType {
        debug_assert!();
        if self.check_curr_char() == '/' && self.first_char() == '/' {
            self.next_char();
        }
        let check_doc:Option<DocStyle> = match self.first_char() {
            '!' => Some(DocStyle::Inner),
            '/' if self.second_char() != '/' => Some(DocStyle::Outer),
           _ => None,
        };
        self.consume_until('\n');
        LineComment { doc_style: check_doc,}
    }

    fn block_comment(&mut self) -> TokenType{
        debug_assert!();
        if self.check_curr_char() == '/' && self.first_char() == '*' {
            self.next_char();
        }
        let check_doc:Option<DocStyle> = match self.first_char() {
            '!' => Some(DocStyle::Inner),
            '*' if self.second_char() != '*' => Some(DocStyle::Outer),
            _ => None,
        };
        // Read until count is zero == block comment is finished.
        let mut count:usize = 1usize;
        while let Some(c) = self.next_char(){
            match c {
                '/' if self.first_char() == '*' => {
                    self.next_char();
                    count -= 1;
                    if count == 0{
                        break;
                    }
                }
                _ => (),
            }
        }

        self.consume_until('\n');
        BlockComment { doc_style: check_doc, terminated: false }
    }
    /// Used to check and remove metadata at the beginning of certain rust files.
    /// Will handle ---, use, //!, and #![ These are common pieces of metadata that are not compiled the same as traditional programming languages.
    fn frontmatter(&mut self) -> TokenType{
        debug_assert_eq!('-', self.prev());
        // Track size of starting delims, used later to match the ending delims since the count should be the same.
        let position:usize = self.input.len();
        self.consume_while(|c| c == '-');
        let opening:usize = self.input.len() - position + 1;
        debug_assert!(opening >= 3);

        // Read until we hit a '-' then check if we have found the final delimiter.
        self.consume_while(|c| c != '\n' && self.is_whitespace(c));

        if unicode_xid::UnicodeXID::is_xid_start(self.first_char()){
            self.next_char();
            self.consume_while(|c| unicode_xid::UnicodeXID::is_xid_continue(c) || c == '.');
        }
        self.consume_while(|c| c != '\n' && self.is_whitespace(c));
        self.first_char() != '\n';
        let mut s = self.input.as_str();
        let mut found:bool = false;
        let mut size:i32 = 0;

        // Find the closing delimiter
        while let Some(closing) = s.find(&"-".repeat(opening)){
            let prev_chars_start = s[..closing].rfind("\n").map_or(0, |i| i + 1);
            if s[prev_chars_start..closing].chars().all(self.is_whitespace()){
                self.next_while(size + closing);
                self.consume_until(b'\n');
                found = true;
                break;
            }
            else{
                s = &s[closing + opening..];
                size += closing + opening;
            }
        }
        if !found{

            let mut rest = self.input.as_str();

            let mut potential_closing = rest
                .find("\n---")
                .map(|x| x + 1)
                .or_else(|| rest.find("\nuse"))
                .or_else(|| rest.find("\n//!"))
                .or_else(|| rest.find("\n#!["));

            if potential_closing.is_none() {
                while let Some(closing) = rest.find("---") {
                    let preceding_chars_start = rest[..closing].rfind("\n").map_or(0, |i| i + 1);
                    if rest[preceding_chars_start..closing].chars().all(self.is_whitespace()) {
                        potential_closing = Some(closing);
                        break;
                    } else {
                        rest = &rest[closing + 3..];
                    }
                }
            }

            if let Some(potential_closing) = potential_closing {
                self.move_bytes(potential_closing);
                self.consume_until(b'\n');
            } else {
                // Consume everything else since it won't be frontmatter.
                self.consume_while(|_| true);
            }
        }

        Frontmatter{ has_invalid_preceding_whitespace, invalid_infostring }
    }

    /// Will be used in all identifier calls.
    /// Checks the first value and subsequent values for if they match the identifier.
    fn consume_full_identifier_or_keyword(&mut self){
        if !unicode_xid::UnicodeXID::is_xid_start(self.first_char()){
            return
        }
        else{
            self.consume_while(unicode_xid::UnicodeXID::is_xid_start);
        }
    }

    fn identifier_or_keyword(&mut self) -> TokenType{
        debug_assert!(unicode_xid::UnicodeXID::is_xid_start(self.first_char()));
        self.consume_full_identifier();
        Identifier_or_Keyword
    }

    /// Invalid identifiers include items that are not traditional rust identifiers
    /// Ex: let 8run =...... digits shouldn't start variable names.
    fn invalid_identifier_or_keyword(&mut self) -> TokenType {
        self.consume_while(|c| {
            unicode_xid::UnicodeXID::is_xid_continue(c) || !c.is_ascii() || c == ZERO_WIDTH_JOINER
        });
        InvalidIdentifier
    }

    /// Check for r# symbol if raw, then check rest of value for identifier match.
    fn raw_identifier(&mut self) -> TokenType{
        debug_assert!(self.prev() == 'r' && self.first() == '#' && unicode_xid::UnicodeXID::is_xid_start(self.second_char()));
        self.next_char();
        self.consume_full_identifier();
        RawIdentifier
    }

    fn unkwn_prefix(&mut self) -> TokenType{
        debug_assert!();
        UnknownPrefix
    }

    fn unkwn_prefix_lifetime(&mut self) -> TokenType{
        debug_assert!();
        UnknownPrefixLifetime
    }

    fn grd_str_prefix(&mut self) -> TokenType{
        debug_assert!();
        GuardedStrPrefix
    }

    fn lifetime(&mut self) -> TokenType{
        Lifetime
    }

    /// Check for r# symbol if raw, then check rest of value for lifetime
    /// Raw values evaluate cha by char while ignoring escape sequences.
    fn raw_lifetime(&mut self) -> TokenType{
        debug_assert!(self.prev_char() == 'r' && self.first_char() == '#');
        self.lifetime();
        RawLifetime
    }

    /// Check the byte or c string then, call identifier checks for token types.
    fn c_string_check(&mut self) -> LiteralKind{
        debug_assert!(self.prev_char() == 'c');
        LiteralKind::Char { terminated: false }
    }

    /// Check the byte or c string then, call identifier checks for token types.
    fn byte_string_check(&mut self) -> LiteralKind{
        debug_assert!(self.prev_char() == 'b');
        LiteralKind::ByteStr { terminated: false }
    }

    /// Helper functions ===========================================================================

    /// Check if a string should just a single character
    fn single_quote_string(&mut self) -> bool{
        if self.second_char() == '\'' && self.first_char() != '\\'{
            self.next_char();
            self.next_char();
            return true;
        }
        loop {
            match self.first_char(){
                '\'' => {
                    self.next_char();
                    return true;
                },
                // Check for the beginning of a comment.
                '/' => break,
                '\n' if self.second_char() => break,
                '\\' => {
                    self.next_char();
                    self.next_char();
                },
                EOF_CHAR => break,
                _ => {self.next_char();},
            }
        }
        false
    }

    /// Check if a string matches the String or &str type.
    /// Used for other token type checks (literals, keywords, etc.)
    fn double_quote_string(&mut self) -> bool{
        while let Some(c) = self.next_char(){
            match c{
                '"' => {return true},
                // Handle string escape sequences, read until we hit correct escape character.
                // Since \"Hello\\World\"" for example is a valid string, we want to stop at the correct '"'.
                '\\' if self.first_char() == '\\' || self.first_char() == '"' =>{
                    self.next_char();
                }
                _ => (),
            }
        }
        false
    }

    /// Used to handle raw string checking. Used in other functions for single and double string/character checking.
    /// Raw strings are strings that allow the usage of " or ' or ` while still creating an acceptable string.
    fn check_raw_string(&mut self, len: u32) -> Result<u32, RawStrError> {
        // Program will read through string char by char.
        // A count of the amount of # will be processed. These will be used to verify that they are the correct number of hashes.
        // If # numbers don't match then we error out since we don't have a valid raw string.
        debug_assert!(self.prev_char() == 'r');
        let mut count: u32 = 0;
        let mut max_count:u32 = 0;
        let start:usize = self.current_position;
        let mut possible_terminator_offset:Option<u32> = None;

        while self.first_char() == '#' {
            count += 1;
            self.next_char();
        }

        let start_count:u32 = count;

        match self.next_char(){
            Some('"') => (),
            c => {
                let c_option:Option<char> = Some(c);
                let c = c_option.unwrap_or(EOF_CHAR);
                return Err(RawStrError::InvalidStarter { bad_char: c });
            }
        }

        loop {
            self.consume_until(char::from(b'"'));
            if self.is_eof() {
                return Err(RawStrError::NoTerminator {
                    expected: start_count,
                    found: max_count,
                    possible_terminator_offset,
                });
            }

            self.next_char();
            let mut end_count = 0;
            while self.first_char() == '#' && end_count < start_count{
                end_count += 1;
                self.next_char();
            }

            if start_count == end_count{
                Ok(start_count)
            }
            else {
                possible_terminator_offset = Some(self.current_position - start - end_count + len);
                max_count = end_count;
            }
        }
    }

    // Cant simply call string function since raw strings ignore escape characters.
    fn raw_double_string(&mut self, len: u32) -> Result<u32, RawStrError> {
        match self.check_raw_string(len){
            Ok(1) => Ok(1),
            Err(_) => Err(RawStrError::TooManyDelimiters { found: 1}),
            _ => {}
        }
    }

    /// Breaks down supplied number to discern number type and Literal type.
    fn handle_number(&mut self, c: char) -> LiteralKind {
        // Check if previous value is a number between 0-9
        debug_assert!('0' <= self.prev_char() && self.prev_char() <= '9');
        let mut base = Base::Decimal;

        if c == '0' {
            match self.first_char() {
                'b' => {
                    self.next_char();
                    base = Base::Binary;
                    if !self.handle_decimal() {
                        Int { base: base, empty_int: true };
                    }
                },
                'o' => {
                    self.next_char();
                    base = Base::Octal;
                    if !self.handle_decimal() {
                        Int { base: base, empty_int: true };
                    }
                },
                'x' => {
                    self.next_char();
                    base = Base::Hexadecimal;
                    if !self.handle_hex() {
                        Int { base: base, empty_int: true };
                    }
                },
                '0'..='9' | '_' => {
                    self.handle_decimal();
                    Int { base: base, empty_int: false };
                }
                _ => return Int { base: base, empty_int: false },
            }
        } else {
            self.handle_decimal();
        }

        // Check for Float Values
        if self.second_char() == '.' && self.third_char().is_ascii_digit() {
            let mut empty_expo:bool = false;
            self.next_char();
            self.handle_decimal();

            match self.first_char() {
                // Handle scientific notation numbers.
                'e' | 'E' => {
                    self.next_char();
                    empty_expo = self.handle_float();
                    return Float { base: base, empty_exponent: empty_expo };
                }
                _ => return Float { base: base, empty_exponent: false }
            }
        }
        // Default case: return integer
        Int { base: base, empty_int: false }
    }

    fn handle_decimal(&mut self) -> bool{
        let mut dec_flag:bool = false;
        loop{
            match self.first_char(){
                '0'..='9' => {
                    dec_flag = true;
                    self.next_char();
                },
                '_' => {self.next_char();},
                _ => break,
            }
        }
        dec_flag
    }

    fn handle_hex(&mut self) -> bool {
        let mut hex_flag:bool = false;
        loop{
            match self.first_char(){
                // Pipe for pattern matching.
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    hex_flag = true;
                    self.next_char();
                },
                '_' => {self.next_char();},
                _ => break,
            }
        }
        hex_flag
    }

    fn handle_float(&mut self) -> bool{
        let mut flt_flag:bool = false;
        loop{
            match self.first_char(){
                '0'..='9' | '.' => {
                    flt_flag = true;
                    self.next_char();
                },
                '_' => {self.next_char();},
                _ => break,
            }
        }
        flt_flag
    }
}