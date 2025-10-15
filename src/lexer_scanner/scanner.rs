/*
The Lexical Analyzer breaks up code into tokens, and runs operations based on the tokens it receives.
It will have to break up the values by type, operator, delimiter, numerics, strings, etc.
It will also need to work with the error_handler to call errors when they are found.
Much like the official rust compiler, the point of this Lexer is to break down Rust's main components to make actual tokenization easier later.
*/
use TokenType::*;
use crate::lexer_scanner::scanner::Whitespace;
use std::collections::HashSet;
use std::sync::LazyLock;

static KEYWORDS: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
    HashSet::from([
        // Strict keywords (cannot be used as identifiers)
        "as",
        "async",
        "await",
        "break",
        "const",
        "continue",
        "crate",
        "dyn",
        "else",
        "enum",
        "extern",
        "false",
        "fn",
        "for",
        "if",
        "impl",
        "in",
        "let",
        "loop",
        "match",
        "mod",
        "move",
        "mut",
        "pub",
        "ref",
        "return",
        "self",
        "Self",
        "static",
        "struct",
        "super",
        "trait",
        "true",
        "type",
        "union",
        "unsafe",
        "use",
        "where",
        "while",

        // Reserved keywords (reserved for future use)
        "abstract",
        "become",
        "box",
        "do",
        "final",
        "macro",
        "override",
        "priv",
        "typeof",
        "unsized",
        "virtual",
        "yield",

        // Weak keywords (contextual, but good to include for completeness)
        "macro_rules",
        "try",
    ])
});

static BASIC_TYPES: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
    HashSet::from([
        "i8",
        "u8",
        "i16",
        "u16",
        "i32",
        "u32",
        "i64",
        "u64",
        "i128",
        "u128",
        "isize",
        "usize",
        "f32",
        "f64",
        "true",
        "false",
        "char",
        "String",
    ])
});

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
    Identifier,
    RawIdentifier,
    InvalidIdentifier,
    Keyword,
    UnknownPrefix,
    UnknownPrefixLifetime,
    RawLifetime,
    GuardedStrPrefix,
    CharLiteral{
        terminated: bool
    },
    StringLiteral,
    RawStringLiteral,
    CStringLiteral,
    RawCStringLiteral,
    ByteLiteral,
    ByteStringLiteral{
        terminated: bool
    },
    RawByteLiteral,
    IntegerLiteral{
        base: Base,
        empty_int: bool
    },
    FloatLiteral{
        base: Base,
        empty_exponent: bool
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
    SemiColon,
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
    Error,
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
    prev_character: char,
}

/// Create a basic Lexer structure, start at zero for all values.
impl <'a> Lexer<'a>{
    pub fn new(input: &'a str) -> Self {
        Self {input, current_position: 0, prev_character: EOF_CHAR}
    }
    /// Will read through each character in an input string and match the value to a token type.
    pub fn next_token(&mut self) -> Token{
        // Check if we are at the end of our input.
        // Return EOF if we are and stop making tokens.
        let c:char  = self.current_char();
        let start: usize = self.current_position;
        if c == EOF_CHAR {
            return Token::new(EOF, TextSpan::new(self.current_position, self.current_position, String::from(EOF_CHAR)));
        }
        let token_type = match c {
                c if Self::check_whitespace(c) => self.whitespace(),
                '/' => match self.next_char() {
                    '/' => self.line_comment(),
                    '*' => self.block_comment(),
                    _ => Slash,
                },
                '-' => match self.next_char(){
                    '-' => self.frontmatter(),
                    _ => Minus
                },
                '0'..='9' => self.handle_number(c),
                'c' => match self.next_char() {
                    '"' => self.c_string_check(),
                    'r' => match self.second_char() {
                        '"' => self.raw_c_string_check(self.input.len() as u32),
                        c => self.identifier_or_keyword(),
                    }
                    c if unicode_xid::UnicodeXID::is_xid_continue(c) => self.identifier_or_keyword(),
                    _ => Unknown
                }
                'b' => match self.next_char(){
                    '"' => self.byte_string_check(),
                    'r' => match self.second_char() {
                        '"' => self.raw_byte_string_check(self.input.len() as u32),
                        c => self.identifier_or_keyword(),
                    },
                    c if unicode_xid::UnicodeXID::is_xid_continue(c) => self.identifier_or_keyword(),
                    _ => Unknown
                },
                /// Using multiple match statements here since the raw strings and identifiers have multiple pieces of overlap.
                'r' => match self.next_char(){
                    '"' => self.raw_double_quote_string(self.input.len() as u32),
                    '#' => match self.second_char(){
                        '"' => self.raw_double_quote_string(self.input.len() as u32),
                        c => self.raw_identifier_or_lifetime(),
                    },
                    _ => Unknown
                },
                c if unicode_xid::UnicodeXID::is_xid_continue(c) => self.identifier_or_keyword(),
                '\'' => self.char_and_lifetime_check(),
                '"' => self.string_check(),
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
                ';' => SemiColon,
                '$' => Dollar,
                '=' => Eq,
                '!' => Bang,
                '<' => Lt,
                '>' => Gt,
                '&' => And,
                '|' => Or,
                '+' => Plus,
                '*' => Star,
                '^' => Caret,
                '%' => Percent,
                _ => Unknown
            };
            self.move_chars(1);
            let end: usize = self.current_position;
            let literal: String = self.input[start..end].to_string();
            let span: TextSpan = TextSpan::new(start, end, literal);
            Token::new(token_type, span)
        }

    /// For checking single character values.
    /// Grabs the current character.
    fn current_char(&mut self) -> char {
        self.input.chars().nth(self.current_position).unwrap_or(EOF_CHAR)
    }

    /// Move to next character and consume the current one.
    fn next_char(&mut self) -> char {
        let c = self.input.chars().nth(self.current_position+1).unwrap_or(EOF_CHAR);
        self.prev_character = c;
        c
    }

    /// Checks the previous character with clone as to not consume characters.
    fn prev_char(&self) -> char {
        self.prev_character
    }

    /// Move up input a certain number of characters.
    fn next_while(&mut self, count: usize) {
        let mut i = 0;
        while i < count{
            self.input.chars().nth(self.current_position+1).unwrap_or(EOF_CHAR);
            i += 1;
        }
    }

    /// Check two characters ahead, but use clone() so item doesn't get consumed.
    fn second_char(&self) -> char {
        self.input.chars().nth(self.current_position+2).unwrap_or(EOF_CHAR)
    }

    /// Check three characters ahead, but use clone() so item doesn't get consumed.
    fn third_char(&self) -> char {
        self.input.chars().nth(self.current_position+3).unwrap_or(EOF_CHAR)
    }

    /// Move a specified number of characters.
    fn move_chars(&mut self, n: usize) {
        for _ in 0..n {
            if let Some((next_pos, _)) = self.input[self.current_position..].char_indices().nth(1) {
                self.current_position += next_pos;
            } else {
                self.current_position = self.input.len();
                break;
            }
        }
    }

    /// Reads each character until we hit a character we don't want to consume.
    fn consume_until(&mut self, c: char) {
        while c != self.current_char() && c != EOF_CHAR {
            self.move_chars(1);
        }
        self.move_chars(1);
    }

    /// Consumes while character value is equal to what is provided.
    fn consume_while(&mut self, mut func:  impl FnMut(char) -> bool) {
        while func(self.next_char()) && self.next_char() != EOF_CHAR {
            self.move_chars(1);
        }
    }

    /// Allows for movement to a specific spot in the token.
    fn move_pos_in_token(&mut self, len:u32){
        self.current_position = len as usize;
    }


    /// Uses ASCII codes to check if a value is equal to multiple whitespace possibilities.
    fn check_whitespace(c: char) -> bool {
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

    /// Uses check_whitespace to consume chars until we hit a value that is not classified as whitespace.
    fn whitespace(&mut self) -> TokenType{
        self.consume_while(|c:char| Self::check_whitespace(c));
        Whitespace
    }

    /// Checks for if a value is a line comment. Returns LineComment TokenType.
    fn line_comment(&mut self) -> TokenType {
        if self.prev_char() == '/' && self.next_char() == '/' {
            self.next_char();
        }
        let check_doc:Option<DocStyle> = match self.next_char() {
            '!' => Some(DocStyle::Inner),
            '/' if self.second_char() != '/' => Some(DocStyle::Outer),
           _ => None,
        };
        self.consume_until('\u{000A}');
        LineComment { doc_style: check_doc,}
    }

    /// Checks for if a value is a block comment. Returns LineComment TokenType.
    fn block_comment(&mut self) -> TokenType{
        if self.prev_char() == '/' && self.next_char() == '*' {
            self.next_char();
        }
        let check_doc:Option<DocStyle> = match self.next_char() {
            '!' => Some(DocStyle::Inner),
            '*' if self.second_char() != '*' => Some(DocStyle::Outer),
            _ => None,
        };
        // Read until count is zero == block comment is finished.
        let mut count: usize = 1usize;
        while let Some(c) = Some(self.current_char()){
            match c {
                '/' if self.next_char() == '*' => {
                    self.next_char();
                    count -= 1;
                    if count == 0{
                        break;
                    }
                }
                _ => (),
            }
        }

        self.consume_until('\u{000A}');
        BlockComment { doc_style: check_doc, terminated: false }
    }

    /// Used to check and remove metadata at the beginning of certain rust files.
    /// Will handle ---, use, //!, and #![ These are common pieces of metadata that are not compiled the same as traditional programming languages.
    fn frontmatter(&mut self) -> TokenType{
        debug_assert_eq!('-', self.current_char());
        // Track size of starting delims, used later to match the ending delims since the count should be the same.
        let position:usize = self.current_position;
        self.consume_while(|c:char| c == '-');
        let opening:usize = self.current_position - position + 1;
        debug_assert!(opening >= 3);

        // Read until we hit a '-' then check if we have found the final delimiter.
        self.consume_while(|c:char| c != '\n' && Self::check_whitespace(c));

        if unicode_xid::UnicodeXID::is_xid_start(self.next_char()){
            self.next_char();
            self.consume_while(|c:char| unicode_xid::UnicodeXID::is_xid_continue(c) || c == '.');
        }
        self.consume_while(|c:char| c != '\n' && Self::check_whitespace(c));
        self.next_char() != '\n';
        let mut s: &str = self.input;
        let mut found:bool = false;
        let mut size:i32 = 0;

        // Find the closing delimiter
        while let Some(closing) = s.find(&"-".repeat(opening)){
            let prev_chars_start:usize = s[..closing].rfind("\n").map_or(0, |i| i + 1);
            if s[prev_chars_start..closing].chars().all(|c:char| Self::check_whitespace(c)){
                self.next_while((size + closing as i32).try_into().unwrap_or(0));
                self.consume_until('\n');
                found = true;
                break;
            }
            else{
                s = &s[closing + opening..];
                size += closing as i32 + opening as i32;
            }
        }
        if !found{

            let mut rest: &str = self.input;

            let mut potential_closing = rest
                .find("\n---")
                .map(|x| x + 1)
                .or_else(|| rest.find("\nuse"))
                .or_else(|| rest.find("\n//!"))
                .or_else(|| rest.find("\n#!["));

            if potential_closing.is_none() {
                while let Some(closing) = rest.find("---") {
                    let preceding_chars_start = rest[..closing].rfind("\n").map_or(0, |i| i + 1);
                    if rest[preceding_chars_start..closing].chars().all(|c| Self::check_whitespace(c)) {
                        potential_closing = Some(closing);
                        break;
                    } else {
                        rest = &rest[closing + 3..];
                    }
                }
            }

            if let Some(potential_closing) = potential_closing {
                self.move_chars(potential_closing);
                self.consume_until('\n');
            } else {
                // Consume everything else since it won't be frontmatter.
                self.consume_while(|_| true);
            }
        }

        Frontmatter{ has_invalid_preceding_whitespace: false, invalid_infostring: false }
    }

    /// Will be used in all identifier calls.
    /// Checks the first value and subsequent values for if they match the identifier.
    fn consume_full_identifier_or_keyword(&mut self){
        if !unicode_xid::UnicodeXID::is_xid_start(self.next_char()){
            return
        }
        else{
            self.consume_while(|c: char| {unicode_xid::UnicodeXID::is_xid_continue(c) && c.is_ascii()});
        }
    }

    /// Checks for identifier or keyword with use of a test string.
    /// If that test string matches a keyword we return keyword, otherwise we check if its invalid or identifier.
    ///
    /// CERTAIN KEYWORDS ARE BEING CUT OFF
    fn identifier_or_keyword(&mut self) -> TokenType{
        let mut test_string = String::new();
        test_string.push(self.current_char());
        while unicode_xid::UnicodeXID::is_xid_continue(self.next_char()) && self.next_char().is_alphabetic(){
            test_string.push(self.next_char());
            self.move_chars(1);
        }
        if KEYWORDS.contains(&*test_string) {
            Keyword
        }
        // We check for next character being anything that might indicate a macro or beginning of function creation/call.
        else if self.current_char() == EOF_CHAR || Self::check_whitespace(self.next_char()) || self.next_char() == '(' || self.next_char() == '!' {
            Identifier
        }
        else {
            InvalidIdentifier
        }
    }

    /// Check for r# symbol if raw, then check rest of value for identifier match.
    fn raw_identifier_or_lifetime(&mut self) -> TokenType{
        self.move_chars(1);
        let saved_position = self.current_position;
        if self.raw_lifetime(){
            RawLifetime
        }
        else {
            self.current_position = saved_position;
            while unicode_xid::UnicodeXID::is_xid_continue(self.next_char()) && self.next_char().is_ascii() {
                self.move_chars(1);
            }
            if self.current_char() == EOF_CHAR || Self::check_whitespace(self.next_char()) {
                RawIdentifier
            } else {
                InvalidIdentifier
            }
        }
    }

    /// Checks for if a value is a lifetime and returns true.
    /// Used in multiple functions to help check for base lifetime then other functions check for other params.
    fn lifetime(&mut self, ) -> bool{
        self.consume_while(|c: char| unicode_xid::UnicodeXID::is_xid_start(c));
        if Self::check_whitespace(self.next_char()) || self.next_char() == ',' || self.next_char() == ':' ||  self.next_char() == '>' {
            true
        }
        else {
            false
        }
    }

    /// Check for r# symbol if raw, then check rest of value for lifetime
    /// Raw values evaluate char by char while ignoring escape sequences.
    fn raw_lifetime(&mut self) -> bool{
        if self.next_char() == '\'' {
            self.move_chars(1);
            self.consume_while(|c: char| unicode_xid::UnicodeXID::is_xid_start(c));
            if Self::check_whitespace(self.next_char()) || self.next_char() == ',' || self.next_char() == ':' || self.next_char() == '>' {
                true
            }
            else{
                false
            }
        }
        else {
            false
        }
    }


    /// Chars a lifetimes use the same apostrophe to start so checks for tokentype by calling different bool functions.
    fn char_and_lifetime_check(&mut self) -> TokenType {
        let saved_position = self.current_position;
        if self.lifetime() {
            Lifetime { starts_with_number: false }
        } else {
            self.current_position = saved_position;
            if self.single_quote_string() {
                CharLiteral { terminated: false }
            } else {
                Unknown
            }
        }
    }

    /// Checks if token is a string literal via use of double_quote_string.
    fn string_check(&mut self) -> TokenType{
        if self.double_quote_string(){
            StringLiteral
        }
        else{
            Unknown
        }
    }

    /// Checks if item is a raw string Ex: r#"help"#
    fn raw_double_quote_string(&mut self, len: u32) -> TokenType {
        if self.check_raw_string(len) == Ok(len){
            RawStringLiteral
        }
        else{
            Unknown
        }
    }

    /// Checks if item is a c string Ex:c"help"
    fn c_string_check(&mut self) -> TokenType {
        self.move_chars(1);
        if self.double_quote_string(){
            CStringLiteral
        }
        else{
            Unknown
        }
    }

    /// Checks if item is a raw c string Ex:cr#"help"#
    fn raw_c_string_check(&mut self, len: u32) -> TokenType {
        self.move_chars(1);
        if self.check_raw_string(len) == Ok(len) {
            RawCStringLiteral
        }
        else{
            Unknown
        }
    }

    /// Checks if item is a byte string Ex:b"help"
    fn byte_string_check(&mut self) -> TokenType{
        self.move_chars(1);
        if self.double_quote_string(){
            ByteStringLiteral { terminated: false }
        }
        else{
            Unknown
        }
    }

    /// Checks if item is a raw byte string Ex:br#"help"#
    fn raw_byte_string_check(&mut self, len: u32) -> TokenType{
        self.move_chars(1);
        if self.check_raw_string(len) == Ok(len) {
            RawByteLiteral
        }
        else{
            Unknown
        }
    }

    /// Helper functions ===========================================================================

    /// Check if a string is just a single character
    fn single_quote_string(&mut self) -> bool{
        if self.second_char() == '\'' && self.next_char() != '\\'{
            self.move_chars(2);
            return true;
        }
        loop {
            match self.next_char(){
                '\'' => {
                    self.move_chars(1);
                    return true;
                },
                // Check for the beginning of a comment.
                '/' => break,
                '\n' if Some(self.second_char()).is_some() => break,
                '\\' => {
                    self.move_chars(2);
                },
                EOF_CHAR => break,
                _ => {self.move_chars(1);},
            }
        }
        false
    }

    /// Check if a string matches the String or &str type.
    /// Used for other token type checks (literals, keywords, etc.)
    fn double_quote_string(&mut self) -> bool{
        loop{
            match self.next_char(){
                '"' => {
                    self.move_chars(1);
                    return true
                },
                // Handle string escape sequences, read until we hit correct escape character.
                // Since \"Hello\\World\"" for example is a valid string, we want to stop at the correct '"'.
                '\\' if self.next_char() == '\\' || self.next_char() == '"' =>{
                    self.move_chars(1);
                }
                '/' => break,
                '\n' if Some(self.second_char()).is_some() => break,
                EOF_CHAR => break,
                _ => {self.move_chars(1)},
            }
        }
        false
    }

    /// In Rust, "guarded strings" refers to strings with a special syntax, #...#,
    /// that were reserved for future language or library features, not a standard string type.
    fn grd_double_quote_string(&mut self) -> Option<GuardedStr> {
        debug_assert!(self.prev_char() != '#');
        let mut start_count: u32 = 0;
        while self.next_char() == '#' {
            start_count += 1;
            self.next_char();
        }
        if self.next_char() != '"' {
            return None
        }
        self.next_char();
        debug_assert!(self.prev_char() == '"');

        let terminated = self.double_quote_string();
        if !terminated {
            let token_count: usize = self.current_position;
            // reset position within token.
            return Some(GuardedStr {n_hashes: start_count, terminated: false, token_len: token_count as u32 });
        }

        let mut end_count = 0;
        while self.next_char() == '#' && end_count < start_count {
            end_count += 1;
            self.next_char();
        }

        // eat literal suffix

        let token_count: usize = self.current_position;
        // reset position within token

        Some(GuardedStr {n_hashes: start_count, terminated: false, token_len: token_count as u32})
    }

    /// Used to handle raw string checking. Used in other functions for single and double string/character checking.
    /// Raw strings are strings that allow the usage of " or ' or ` while still creating an acceptable string.
    fn check_raw_string(&mut self, len: u32) -> Result<u32, RawStrError> {
        // Program will read through string char by char.
        // A count of the amount of # will be processed. These will be used to verify that they are the correct number of hashes.
        // If # numbers don't match then we error out since we don't have a valid raw string.
        let mut start_count: u32 = 256;
        let mut max_count:u32 = 0;
        let mut end_count = 0;
        let start:usize = self.current_position;
        let mut possible_terminator_offset:Option<u32> = None;

        if self.next_char() == '#' {
            start_count = 0;
            while self.next_char() == '#' {
                start_count += 1;
                self.move_chars(1);
            }
        }
        self.move_chars(1);

        println!("{:?}", self.current_char());

        match self.current_char(){
            '"' => (),
            c => {
                return Err(RawStrError::InvalidStarter { bad_char: c });
            }
        }
        self.move_chars(1);
        // Only ever returning OK when #'s are present.
        loop {
            if self.current_char() == '"'{
                while self.next_char() == '#' && end_count < start_count {
                    end_count += 1;
                    self.move_chars(1);
                }
                if Self::check_whitespace(self.next_char()) || self.next_char() == EOF_CHAR{
                    return Ok(len);
                }
            }
            if self.current_char() == EOF_CHAR {
                return Err(RawStrError::NoTerminator {
                    expected: start_count,
                    found: max_count,
                    possible_terminator_offset,
                });
            }
            if start_count == end_count{
                return Ok(len);
            }
            else {
                possible_terminator_offset = Some(self.current_position as u32 - start as u32 - end_count + len);
                max_count = end_count;

            }
            self.move_chars(1);
        }
    }


    /// Breaks down supplied number to discern number type and Literal type.
    fn handle_number(&mut self, c: char) -> TokenType {
        let mut base = Base::Decimal;

        // Check for Float Values
        if self.next_char() == '.' && self.second_char().is_ascii_digit() {
            let empty_expo:bool;
            self.next_char();
            self.handle_float();

            return match self.next_char() {
                // Handle scientific notation numbers.
                'e' | 'E' => {
                    self.next_char();
                    empty_expo = self.handle_float();
                    FloatLiteral { base, empty_exponent: empty_expo }
                }
                _ => FloatLiteral { base, empty_exponent: false }
            }
        }

        /// Float is getting caught here due to zeroes.
        if c == '0' && self.next_char() != 'i' && self.next_char() != 'u' {
                match self.next_char() {
                'b' => {
                    self.move_chars(1);
                    base = Base::Binary;
                    if !self.handle_bin() {
                        IntegerLiteral { base, empty_int: true };
                    }
                },
                'o' => {
                    self.move_chars(1);
                    base = Base::Octal;
                    if !self.handle_oct() {
                        IntegerLiteral { base, empty_int: true };
                    }
                },
                'x' => {
                    self.move_chars(1);
                    base = Base::Hexadecimal;
                    if !self.handle_hex() {
                        IntegerLiteral { base, empty_int: true };
                    }
                },
                '0'..='9' | '_' => {
                    self.handle_decimal();
                    IntegerLiteral { base, empty_int: false };
                }
                _ => ()
            }
        } else {
            self.handle_decimal();
        }
        // Default case: return integer
        IntegerLiteral { base, empty_int: false }
    }

    fn check_data_type(&mut self) -> bool{
        let mut data_flag: bool = true;
        println!("{:?}", self.current_char());
        if self.next_char() == 'i' || self.next_char() == 'u' {
            let mut type_string: String = String::new();
            while self.next_char() != EOF_CHAR && !Self::check_whitespace(self.next_char()) && self.next_char() != ';' {
                type_string.push(self.current_char());
                self.move_chars(1);
            }
            if !BASIC_TYPES.contains(&*type_string) {
                data_flag = false
            }
        }
        data_flag
    }

    fn handle_decimal(&mut self) -> bool{
        let mut dec_flag: bool = false;
        loop{
            match self.next_char(){
                '0'..='9' => {
                    dec_flag = true;
                    self.move_chars(1);
                },
                '_' => {self.move_chars(1);},
                'i' | 'u' => dec_flag = self.check_data_type(),
                _ => break,
            }
        }
        dec_flag
    }

    fn handle_hex(&mut self) -> bool {
        let mut hex_flag: bool = false;
        loop{
            match self.next_char(){
                // Pipe for pattern matching.
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    hex_flag = true;
                    self.move_chars(1);
                },
                '_' => {self.move_chars(1);},
                'i' | 'u' => hex_flag = self.check_data_type(),
                _ => break,
            }
        }
        hex_flag
    }

    fn handle_oct(&mut self) -> bool {
        let mut oct_flag: bool = false;
        loop{
            match self.next_char(){
                // Pipe for pattern matching.
                '0'..='7' => {
                    oct_flag = true;
                    self.move_chars(1);
                },
                '_' => {self.move_chars(1);},
                'i' | 'u' => oct_flag = self.check_data_type(),
                _ => break,
            }
        }
        oct_flag
    }

    fn handle_bin(&mut self) -> bool {
        let mut bin_flag: bool = false;
        loop{
            match self.next_char(){
                // Pipe for pattern matching.S
                '0'..='1' => {
                    bin_flag = true;
                    self.move_chars(1);
                },
                '_' => {self.move_chars(1);},
                'i' | 'u' => bin_flag = self.check_data_type(),
                _ => break,
            }
        }
        bin_flag
    }

    fn handle_float(&mut self) -> bool{
        let mut flt_flag: bool = false;
        loop{
            match self.next_char(){
                '0'..='9' | '.' => {
                    flt_flag = true;
                    self.move_chars(1);
                },
                '_' => {self.move_chars(1);},
                'i' | 'u' => flt_flag = self.check_data_type(),
                _ => break,
            }
        }
        flt_flag
    }
}