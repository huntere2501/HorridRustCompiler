/*
The Lexical Analyzer breaks up code into tokens, and runs operations based on the tokens it receives.
It will have to break up the values by type, operator, delimiter, numerics, strings, etc.
It will also need to work with the error_handler to call errors when they are found.
*/
use std::ffi::c_char;
use std::ops::Add;
use phf::phf_map;
use unicode_properties::UnicodeEmoji;
pub use unicode_xid::UNICODE_VERSION as UNICODE_XID_VERSION;


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
    /// Non `#` characters exist between `r` and `"`, e.g. `r##~"abcde"##`
    InvalidStarter { bad_char: char },
    /// The string was not terminated, e.g. `r###"abcde"##`.
    /// `possible_terminator_offset` is the number of characters after `r` or
    /// `br` where they may have intended to terminate it.
    NoTerminator { expected: u32, found: u32, possible_terminator_offset: Option<u32> },
    /// More than 255 `#`s exist.
    TooManyDelimiters { found: u32 },
}

/// List of tokentypes and properties.
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
    Frontmatter,
    Keyword,
    Identifier,
    InvalidIdentifier,
    RawIdentifier,
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

/// Useful constant values to keep for specific checks.
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
// The Lexer structure will be used identify the main parts of a value, will stop based on delimiter.
pub(crate) struct Lexer<'a> {
    input: &'a str,
    current_position: usize,
}

/// Create a basic Lexer structure, start at zero for all values.
impl <'a> Lexer<'a>{
    pub fn new(input: & 'a str) -> Self {
        Self {input, current_position: 0}
    }
    /// Will read through each character in an input string and match the value to a token type.
    /// Uses .map() to create an iterator and match statement to find correct token type.
    pub fn next_token(&mut self) -> Token{
        // Check if we are at the end of our input.
        // Return EOF if we are and stop making tokens.
        let Some(c) = self.next_char() else {
            return Token::new(TokenType::EOF, TextSpan::new(0,0,String::from(EOF_CHAR)));
        };
        let c  = self.current_char();
        c.map(|c| {
            let start = self.current_position;
            let mut kind = TokenType::Unknown;
            match c{
                /// Basic reset of values.
                'a' => TokenType::Literal,
                c if c.is_whitespace() => self.whitespace(),
                c if self.is_comment(&c) => self.comment(),
                ',' => TokenType::Comma,
                '.' => TokenType::Dot,
                '(' => TokenType::OpenParen,
                ')' => TokenType::CloseParen,
                '{' => TokenType::OpenBrace,
                '}' => TokenType::CloseBrace,
                '[' => TokenType::OpenBracket,
                ']' => TokenType::CloseBracket,
                '@' => TokenType::At,
                '#' => TokenType::Pound,
                '~' => TokenType::Tilde,
                '?' => TokenType::Question,
                ':' => TokenType::Colon,
                '$' => TokenType::Dollar,
                '=' => TokenType::Eq,
                '!' => TokenType::Bang,
                '<' => TokenType::Lt,
                '>' => TokenType::Gt,
                '-' => TokenType::Minus,
                '&' => TokenType::And,
                '|' => TokenType::Or,
                '+' => TokenType::Plus,
                '*' => TokenType::Star,
                '/' => TokenType::Slash,
                '^' => TokenType::Caret,
                '%' => TokenType::Percent,
                _ => TokenType::Unknown
            }

            let end = self.current_position;
            let literal = self.input[start..end].to_string();
            let span = TextSpan::new(start, end, literal);
            Token::new(kind, span)
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

    fn line_comment(&mut self) -> TokenType {
        if self.check_curr_char() == '/' && self.first_char() == '/' {
            self.next_char();
        }
        let check_doc = match self.first_char() {
            '!' => Some(DocStyle::Inner),
            '/' if self.second_char() != '/' => Some(DocStyle::Outer),
           _ => None,
        };
        self.consume_until('\n');
        TokenType::LineComment { doc_style: check_doc,}
    }

    fn block_comment(&mut self) -> TokenType{
        /// TODO: Block Comment needs more verbose checking.
        if self.check_curr_char() == '/' && self.first_char() == '*' {
            self.next_char();
        }
        let check_doc = match self.first_char() {
            '!' => Some(DocStyle::Inner),
            '*' if self.second_char() != '*' => Some(DocStyle::Outer),
            _ => None,
        };
        self.consume_until('\n');
        TokenType::BlockComment { doc_style: check_doc, terminated: false }
    }

    fn frontmatter(&mut self) -> TokenType{
        TokenType::Frontmatter
    }

    fn keyword(&mut self) -> TokenType{
        /// TODO: Keyword needs to be checked and stored in seperate function before deciding tokentype.
        let mut check: String = "".to_owned();
        while !Self::is_whitespace(self.check_curr_char()){
            check.push_str(&self.check_curr_char().to_string());
            self.first_char();
        }
        if KEYWORDS.get(&*check){
            TokenType::Keyword
        }
        else{
            TokenType::Unknown
        }
    }

    fn identifier(&mut self) -> TokenType{
        /// TODO: Identifier needs more verbose checking. More time needs to spent understanding how to check for it.
        // This is incorrect right now and doesn't match the proper checking.
        self.consume_while(|c| {
            unicode_xid::UnicodeXID::is_xid_continue(c) || !c.is_ascii() || c == ZERO_WIDTH_JOINER
        });
        TokenType::Identifier
    }

    /// Invalid identifiers include items that are not traditional rust identifiers
    /// Ex: let 8run =...... digits shouldnt start variable names.
    fn invalid_identifier(&mut self) -> TokenType {

        // This is just the start for the valid identifier.
        self.consume_while(|c| {
            unicode_xid::UnicodeXID::is_xid_continue(c) || !c.is_ascii()
        });
        TokenType::InvalidIdentifier
    }

    fn raw_identifier(&mut self) -> TokenType{
        TokenType::RawIdentifier
    }

    fn unkwn_prefix(&mut self) -> TokenType{
        TokenType::UnknownPrefix
    }

    fn unkwn_prefix_lifetime(&mut self) -> TokenType{
        TokenType::UnknownPrefixLifetime
    }

    fn raw_lifetime(&mut self) -> TokenType{
        TokenType::RawLifetime
    }

    fn grd_str_prefix(&mut self) -> TokenType{
        TokenType::GuardedStrPrefix
    }

    fn literal(&mut self) -> TokenType{
        TokenType::Literal
    }

    fn lifetime(&mut self) -> TokenType{
        TokenType::Lifetime
    }


}