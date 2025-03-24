/*
The Lexical Analyzer breaks up code into tokens, and runs operations based on the tokens it receives.
It will have to break up the values by type, operator, delimiter, numerics, strings, etc.
It will also need to work with the error_handler to call errors when they are found.
*/

// Enum of different types a token might be, covers most options for now.
#[derive(Debug, PartialEq, Clone)]
enum TokenType {
    // Primitive token types
    Integer,       // Numeric values
    Identifier,    // Variable/function names
    Operator,      // Mathematical or logical operators
    Keyword,       // Reserved language words
    Delimiter,     // Punctuation like parentheses or semicolons
    WhiteSpace,    // Spaces, tabs, newlines
    Comment,
    Unknown        // Catch-all for unrecognized tokens
}
// Token Struct to construct Token Values
#[derive(Debug, Clone)]
struct Token {
   token_type: TokenType,
    literal: String,
    line: usize,
    column: usize,
}
// The Lexer structure will be used identify the main parts of a value, will stop based on delimiter.
struct Lexer {
    input: Vec<char>,
    current_position: usize,
    current_line: usize,
    current_column: usize,
}

// Create a basic Lexer structure, start at zero for all values.
impl Lexer{
    fn new(source: &str) -> Self {
        Lexer {
            input: source.chars().collect(),
            current_position: 0,
            current_line: 1,
            current_column: 1,
        }
    }
    // Check if the current position i
    fn is_at_end(&self) -> bool {
        self.current_position >= self.input.len()
    }

    fn advance(&self) -> Option<str>{
        return "str";
    }

    // NEED TO SEE IF THIS ONLY CHECKS FOR I32/I64 TYPES!!!!!!!!!
    fn get_number(&mut self) -> Token {
        let start_pos = self.current_position- 1;
        let start_line = self.current_line;
        let start_column = self.current_column;

        while let Some(x) = self.peek(){
            if !x.is_numeric() && x != '_'{
                break;
            }
            self.advance();
        }
        let literal: String = self.input[start_pos..self.current_position].iter().collect();

        return Token {
            token_type: TokenType::Integer,
            literal,
            line: start_line,
            column: start_column,
        }
    }

    fn get_identifier(&mut self) -> Token {
        let start_pos = self.current_position - 1;
        let start_line = self.current_line;
        let start_column = self.current_column;

        while let Some(x) = self.peek() {
            if !x.is_alphanumeric() && x != '_'{
                break;
            }
            self.advance();
        }
        let literal: String = self.input[start_pos..self.current_position].iter().collect();
        let token_type = match literal.as_str() {
            "let" | "fn" | "if" | "else" | "match" | "if else" | "return" => TokenType::Keyword,
            _ => TokenType::Identifier,
        };
        return Token {
            token_type,
            literal,
            line: start_line,
            column: start_column,
        }
    }

    fn get_operator(&mut self) -> Token {
        let start_pos = self.current_position - 1;
        let start_line = self.current_line;
        let start_column = self.current_column;

        while let Some(x) = self.peek() {
            if !x.is_ascii() && x != '_' {
                break;
            }
            self.advance();
        }
        let literal: String = self.input[start_pos..self.current_position].iter().collect();
        let token_type = match literal.as_str() {
            "+" | "-" | "/" | "*" | "+=" | "-=" | "*=" | "/=" => TokenType::Operator,
            _ => TokenType::Identifier,
        };
        return Token {
            token_type,
            literal,
            line: start_line,
            column: start_column,
        }
    }

    fn get_delimiter(&mut self) -> Token {
        let start_pos = self.current_position - 1;
        let start_line = self.current_line;
        let start_column = self.current_column;

        while let Some(x) = self.peek() {
            if !x.is_ascii() && x != '_'{
                break;
            }
            self.advance();
        }
        let literal: String = self.input[start_pos..self.current_position].iter().collect();
        let token_type = match literal.as_str() {
            "{" | "}"| ";" | "(" | ")" => TokenType::Delimiter,
            _ => TokenType::Identifier,
        };
        return Token {
            token_type,
            literal,
            line: start_line,
            column: start_column,
        }
    }
    fn get_whitespace(&mut self) -> Token {
        let start_pos = self.current_position - 1;
        let start_line = self.current_line;
        let start_column = self.current_column;

        while let Some(x) = self.peek() {
            if !x.is_ascii_whitespace() && x != '_'{
                break;
            }
            self.advance();
        }
        let literal: String = self.input[start_pos..self.current_position].iter().collect();

        return Token {
            token_type: TokenType::WhiteSpace,
            literal,
            line: start_line,
            column: start_column,
        }
    }

    // Comments dont get saved, compilers dont care about them.
    fn get_comment(&mut self) -> Token {
        let start_pos = self.current_position - 1;
        let start_line = self.current_line;
        let start_column = self.current_column;
        while let Some(x) = self.peek() {
            // Check for first then second slash character side by side.
            if x.is_ascii && x = '/'{
                self.advance();
                if x.is_ascii && x = '/'{
                    break;
                }
            }
            self.advance();
        }
    }

    // Create vector to store tokens?
    // String must be broken up into chunks and stored.
    // What am I even returning?
    // Hashmap?
    // Ex: let x = 1;
    // Return -> (identifier, let), (symbol, x), (operation, =), (integer, 1)

    ////////////////////////////////////////////////////////////////
    // NEED TO READ MORE BEFORE WE CONTINUE CODING!!!!!!!!!!!!!!!
    ////////////////////////////////////////////////////////////////


    fn tokenize(){
        1
    }
}
