/// Setup for Error Handling.
///
/// TODO: NEED TO CREATE LIST OF ERROR HANDLER CODES (CUSTOM)

// Need to create a template for errors
// Ex: Type of error, name of error, description

// Need to handle multiple error types.
// List of rust error codes = https://doc.rust-lang.org/error_codes/error-index.html

// Start with basics (DATA TYPE matching)
// Int types, Match ranges
// String type

// The question is do we trust the lexer to provide us with proper tokens and we only care about what we get or are we checking for errors as we grab tokens and therefore
// I would need to edit my lexer to handle errors based on the class I made here.

// Need to review parts of a compiler
/// Lexical Analyzer = Malformed Tokens, Incorrect Token Syntax, Invalid Characters
/// Syntax Analyzer = Basic syntax (duh), missing semicolons, unbalanced parentheses.
/// Semantic Anaylzer =
/// Intermediate Code =
/// Code Optimizer =
/// Code Generator =

use std::collections::HashMap;
use std::sync::LazyLock;

#[derive(Debug, Clone)]
enum IntRange {
    Signed(i128, i128),
    Unsigned(u128, u128),
}

static INTEGER_RANGES: LazyLock<HashMap<&'static str, IntRange>> = LazyLock::new(|| {
    HashMap::from([
        // Signed integers
        ("i8", IntRange::Signed(i8::MIN as i128, i8::MAX as i128)),
        ("i16", IntRange::Signed(i16::MIN as i128, i16::MAX as i128)),
        ("i32", IntRange::Signed(i32::MIN as i128, i32::MAX as i128)),
        ("i64", IntRange::Signed(i64::MIN as i128, i64::MAX as i128)),
        ("i128", IntRange::Signed(i128::MIN, i128::MAX)),
        ("isize", IntRange::Signed(isize::MIN as i128, isize::MAX as i128)),

        // Unsigned integers
        ("u8", IntRange::Unsigned(u8::MIN as u128, u8::MAX as u128)),
        ("u16", IntRange::Unsigned(u16::MIN as u128, u16::MAX as u128)),
        ("u32", IntRange::Unsigned(u32::MIN as u128, u32::MAX as u128)),
        ("u64", IntRange::Unsigned(u64::MIN as u128, u64::MAX as u128)),
        ("u128", IntRange::Unsigned(u128::MIN, u128::MAX)),
        ("usize", IntRange::Unsigned(usize::MIN as u128, usize::MAX as u128)),
    ])
});

pub(crate) struct ErrorHandler<'a>  {
    error_type: &'a str,
    description: &'a str,
    error_num: &'a str

}

impl <'a> ErrorHandler<'a> {
    pub fn new(error_type: &'a str, description: &'a str, error_num:&'a str) -> Self {
        ErrorHandler{error_type, description, error_num}
    }

    pub fn print_err(&self){
        println!("Error Type: {:?}\n", self.error_type);
        println!("Description: {:?}\n", self.description);
        println!("Error Num: {:?}\n", self.error_num);
    }
}
