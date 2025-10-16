use std::collections::{HashMap, HashSet};
use std::ffi::CString;
use std::sync::LazyLock;
/// Setup for Error Handling.

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
/// Lexical Analyzer =
/// Syntax Analyzer =
/// Semantic Anaylzer =
/// Intermediate Code =
/// Code Optimizer =
/// Code Generator =

use crate::lexer_scanner::scanner::{Lexer, TokenType};

static INTEGER_RANGES: HashMap<&'static str, (i128, i128)> = HashMap::from([
    // Signed integers
    ("i8", (-128, 127)),
    ("i16", (-32_768, 32_767)),
    ("i32", (-2_147_483_648, 2_147_483_647)),
    ("i64", (-9_223_372_036_854_775_808, 9_223_372_036_854_775_807)),
    ("i128", (-170_141_183_460_469_231_731_687_303_715_884_105_728, 170_141_183_460_469_231_731_687_303_715_884_105_727)),
    ("isize", (-9_223_372_036_854_775_808, 9_223_372_036_854_775_807)), // 64-bit

    // Unsigned integers
    ("u8", (0, 255)),
    ("u16", (0, 65_535)),
    ("u32", (0, 4_294_967_295)),
    ("u64", (0, 18_446_744_073_709_551_615)),
    ("u128", (0, 340_282_366_920_938_463_463_374_607_431_768_211_455)),
    ("usize", (0, 18_446_744_073_709_551_615)), // 64-bit
]);

struct ErrorHandler {
    error_type: str,
    description: str,
    error_num: str

}

impl ErrorHandler {


}
