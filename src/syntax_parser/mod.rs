pub mod parser;


/// Steps in Syntax Analysis Phase
// The Syntax Analysis phase, also known as parsing, is a crucial step in the compilation process where the structure of the source code is verified according to the grammar of the programming language.
//
// Parsing: The tokens are analyzed according to the grammar rules of the programming language, and a parse tree or AST is constructed that represents the hierarchical structure of the program.
// Error handling: If the input program contains syntax errors, the syntax analyzer detects and reports them to the user, along with an indication of where the error occurred.
// Symbol table creation: The syntax analyzer creates a symbol table, which is a data structure that stores information about the identifiers used in the program, such as their type, scope, and location.
///
///
/// ex: impl ASTExpression {
//     pub fn whitespace() -> Self {
//         ASTExpression {
//             kind: ASTExpressionKind::Whitespace(ASTExpression),
//         }
//     }
//
//     pub fn line_comment(doc_style: Option<DocStyle>) -> Self {
//         ASTExpression {
//             kind: ASTExpressionKind::LineComment(ASTExpression { doc_style }),
//         }
//     }
//
//     pub fn block_comment(doc_style: Option<DocStyle>, terminated: bool) -> Self {
//         ASTExpression {
//             kind: ASTExpressionKind::BlockComment(ASTExpression { doc_style, terminated }),
//         }
//     }
/// PRIORITY: Learn to create grammars, then learn how to match it to AST. (If you can do it by hand then you can do it via code).
/// TODO: Take tokens create AST.
/// TODO: Create a symbol table from given tokens/grammar.
/// TODO: Symbol table can be created during (likely my choice) or after initial pass.


pub struct Ast {
    pub statements: Vec<ASTStatement>
}

impl Ast {
    pub fn new() -> Self{
        Self{statements:Vec::new()}
    }

    pub fn add_statement(&mut self, statement: ASTStatement){
        self.statements.push(statement);
    }

    pub fn visit(&mut self, visitor: &mut dyn ASTVisitor){
        for statement in &self.statements {
            visitor.visit_statement(statement)
        }
    }

    pub fn visualize(&mut self) -> () {
        let mut printer =  ASTPrinter{indent:0};
        self.visit(&mut printer);
    }
}

pub trait ASTVisitor {
    fn do_visit_statement(&mut self, statement: &ASTStatement){
        match &statement.kind {
            ASTStatementKind::Expression(expr) => {
                self.visit_expression(expr);
            }
        }
    }

    fn visit_statement(&mut self, statement: &ASTStatement) {
        self.do_visit_statement(statement);
    }

    fn do_visit_expression(&mut self, expr: &ASTExpression){
        match &expr.kind {
            ASTExpressionKind::Whitespace(e) => self.visit_whitespace(e),
            ASTExpressionKind::LineComment(e) => self.visit_line_comment(e),
            ASTExpressionKind::BlockComment(e) => self.visit_block_comment(e),
            ASTExpressionKind::Frontmatter(e) => self.visit_frontmatter(e),
            ASTExpressionKind::Identifier(e) => self.visit_identifier(e),
            ASTExpressionKind::RawIdentifier(e) => self.visit_raw_identifier(e),
            ASTExpressionKind::InvalidIdentifier(e) => self.visit_invalid_identifier(e),
            ASTExpressionKind::Keyword(e) => self.visit_keyword(e),
            ASTExpressionKind::UnknownPrefix(e) => self.visit_unknown_prefix(e),
            ASTExpressionKind::UnknownPrefixLifetime(e) => self.visit_unknown_prefix_lifetime(e),
            ASTExpressionKind::RawLifetime(e) => self.visit_raw_lifetime(e),
            ASTExpressionKind::GuardedStrPrefix(e) => self.visit_guarded_str_prefix(e),
            ASTExpressionKind::CharLiteral(e) => self.visit_char_literal(e),
            ASTExpressionKind::StringLiteral(e) => self.visit_string_literal(e),
            ASTExpressionKind::RawStringLiteral(e) => self.visit_raw_string_literal(e),
            ASTExpressionKind::CStringLiteral(e) => self.visit_c_string_literal(e),
            ASTExpressionKind::RawCStringLiteral(e) => self.visit_raw_c_string_literal(e),
            ASTExpressionKind::ByteLiteral(e) => self.visit_byte_literal(e),
            ASTExpressionKind::ByteStringLiteral(e) => self.visit_byte_string_literal(e),
            ASTExpressionKind::RawByteLiteral(e) => self.visit_raw_byte_literal(e),
            ASTExpressionKind::IntegerLiteral(e) => self.visit_integer_literal(e),
            ASTExpressionKind::FloatLiteral(e) => self.visit_float_literal(e),
            ASTExpressionKind::Lifetime(e) => self.visit_lifetime(e),
            ASTExpressionKind::Semi(e) => self.visit_semi(e),
            ASTExpressionKind::Comma(e) => self.visit_comma(e),
            ASTExpressionKind::Dot(e) => self.visit_dot(e),
            ASTExpressionKind::OpenParen(e) => self.visit_open_paren(e),
            ASTExpressionKind::CloseParen(e) => self.visit_close_paren(e),
            ASTExpressionKind::OpenBrace(e) => self.visit_open_brace(e),
            ASTExpressionKind::CloseBrace(e) => self.visit_close_brace(e),
            ASTExpressionKind::OpenBracket(e) => self.visit_open_bracket(e),
            ASTExpressionKind::CloseBracket(e) => self.visit_close_bracket(e),
            ASTExpressionKind::At(e) => self.visit_at(e),
            ASTExpressionKind::Pound(e) => self.visit_pound(e),
            ASTExpressionKind::Tilde(e) => self.visit_tilde(e),
            ASTExpressionKind::Question(e) => self.visit_question(e),
            ASTExpressionKind::Colon(e) => self.visit_colon(e),
            ASTExpressionKind::SemiColon(e) => self.visit_semicolon(e),
            ASTExpressionKind::Dollar(e) => self.visit_dollar(e),
            ASTExpressionKind::Eq(e) => self.visit_eq(e),
            ASTExpressionKind::Bang(e) => self.visit_bang(e),
            ASTExpressionKind::Lt(e) => self.visit_lt(e),
            ASTExpressionKind::Gt(e) => self.visit_gt(e),
            ASTExpressionKind::Minus(e) => self.visit_minus(e),
            ASTExpressionKind::And(e) => self.visit_and(e),
            ASTExpressionKind::Or(e) => self.visit_or(e),
            ASTExpressionKind::Plus(e) => self.visit_plus(e),
            ASTExpressionKind::Star(e) => self.visit_star(e),
            ASTExpressionKind::Slash(e) => self.visit_slash(e),
            ASTExpressionKind::Caret(e) => self.visit_caret(e),
            ASTExpressionKind::Percent(e) => self.visit_percent(e),
            ASTExpressionKind::Unknown(e) => self.visit_unknown(e),
            ASTExpressionKind::Error(e) => self.visit_error(e),
            ASTExpressionKind::EOF(e) => self.visit_eof(e),
        }
    }

    fn visit_expression(&mut self, expression: &ASTExpression) {
        self.do_visit_expression(expression);
    }

    fn visit_whitespace(&mut self, e: &ASTExpression);
    fn visit_line_comment(&mut self, e: &ASTExpression);
    fn visit_block_comment(&mut self, e: &ASTExpression);
    fn visit_frontmatter(&mut self, e: &ASTExpression);
    fn visit_identifier(&mut self, e: &ASTExpression);
    fn visit_raw_identifier(&mut self, e: &ASTExpression);
    fn visit_invalid_identifier(&mut self, e: &ASTExpression);
    fn visit_keyword(&mut self, e: &ASTExpression);
    fn visit_unknown_prefix(&mut self, e: &ASTExpression);
    fn visit_unknown_prefix_lifetime(&mut self, e: &ASTExpression);
    fn visit_raw_lifetime(&mut self, e: &ASTExpression);
    fn visit_guarded_str_prefix(&mut self, e: &ASTExpression);
    fn visit_char_literal(&mut self, e: &ASTExpression);
    fn visit_string_literal(&mut self, e: &ASTExpression);
    fn visit_raw_string_literal(&mut self, e: &ASTExpression);
    fn visit_c_string_literal(&mut self, e: &ASTExpression);
    fn visit_raw_c_string_literal(&mut self, e: &ASTExpression);
    fn visit_byte_literal(&mut self, e: &ASTExpression);
    fn visit_byte_string_literal(&mut self, e: &ASTExpression);
    fn visit_raw_byte_literal(&mut self, e: &ASTExpression);
    fn visit_integer_literal(&mut self, e: &ASTExpression);
    fn visit_float_literal(&mut self, e: &ASTExpression);
    fn visit_lifetime(&mut self, e: &ASTExpression);
    fn visit_semi(&mut self, e: &ASTExpression);
    fn visit_comma(&mut self, e: &ASTExpression);
    fn visit_dot(&mut self, e: &ASTExpression);
    fn visit_open_paren(&mut self, e: &ASTExpression);
    fn visit_close_paren(&mut self, e: &ASTExpression);
    fn visit_open_brace(&mut self, e: &ASTExpression);
    fn visit_close_brace(&mut self, e: &ASTExpression);
    fn visit_open_bracket(&mut self, e: &ASTExpression);
    fn visit_close_bracket(&mut self, e: &ASTExpression);
    fn visit_at(&mut self, e: &ASTExpression);
    fn visit_pound(&mut self, e: &ASTExpression);
    fn visit_tilde(&mut self, e: &ASTExpression);
    fn visit_question(&mut self, e: &ASTExpression);
    fn visit_colon(&mut self, e: &ASTExpression);
    fn visit_semicolon(&mut self, e: &ASTExpression);
    fn visit_dollar(&mut self, e: &ASTExpression);
    fn visit_eq(&mut self, e: &ASTExpression);
    fn visit_bang(&mut self, e: &ASTExpression);
    fn visit_lt(&mut self, e: &ASTExpression);
    fn visit_gt(&mut self, e: &ASTExpression);
    fn visit_minus(&mut self, e: &ASTExpression);
    fn visit_and(&mut self, e: &ASTExpression);
    fn visit_or(&mut self, e: &ASTExpression);
    fn visit_plus(&mut self, e: &ASTExpression);
    fn visit_star(&mut self, e: &ASTExpression);
    fn visit_slash(&mut self, e: &ASTExpression);
    fn visit_caret(&mut self, e: &ASTExpression);
    fn visit_percent(&mut self, e: &ASTExpression);
    fn visit_unknown(&mut self, e: &ASTExpression);
    fn visit_error(&mut self, e: &ASTExpression);
    fn visit_eof(&mut self, e: &ASTExpression);
}

pub struct ASTPrinter {
    indent: usize,
}

const INDENT_LEVEL: usize = 2;

impl ASTVisitor for ASTPrinter{
    fn visit_statement(&mut self, statement: &ASTStatement){
        self.print_with_indent("Statement:");
        self.indent += INDENT_LEVEL;
        ASTVisitor::do_visit_statement(self, statement);
        self.indent -= INDENT_LEVEL;
    }

    fn visit_expression(&mut self, expression: &ASTExpression){
        self.print_with_indent("Expression:");
        self.indent += INDENT_LEVEL;
        ASTVisitor::do_visit_expression(self, expression);
        self.indent -= INDENT_LEVEL;
    }

    fn visit_whitespace(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Whitespace");
    }
    fn visit_line_comment(&mut self, e: &ASTExpression) {
        self.print_with_indent(&format!("LineComment: doc_style={:?}", e.doc_style));
    }
    fn visit_block_comment(&mut self, e: &ASTExpression) {
        self.print_with_indent(&format!("BlockComment: doc_style={:?}, terminated={}", e.doc_style, e.terminated));
    }
    fn visit_frontmatter(&mut self, e: &ASTExpression) {
        self.print_with_indent(&format!("Frontmatter: invalid_ws={}, invalid_info={}", e.has_invalid_preceding_whitespace, e.invalid_infostring));
    }
    fn visit_identifier(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Identifier");
    }
    fn visit_raw_identifier(&mut self, _e: &ASTExpression) {
        self.print_with_indent("RawIdentifier");
    }
    fn visit_invalid_identifier(&mut self, _e: &ASTExpression) {
        self.print_with_indent("InvalidIdentifier");
    }
    fn visit_keyword(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Keyword");
    }
    fn visit_unknown_prefix(&mut self, _e: &ASTExpression) {
        self.print_with_indent("UnknownPrefix");
    }
    fn visit_unknown_prefix_lifetime(&mut self, _e: &ASTExpression) {
        self.print_with_indent("UnknownPrefixLifetime");
    }
    fn visit_raw_lifetime(&mut self, _e: &ASTExpression) {
        self.print_with_indent("RawLifetime");
    }
    fn visit_guarded_str_prefix(&mut self, _e: &ASTExpression) {
        self.print_with_indent("GuardedStrPrefix");
    }
    fn visit_char_literal(&mut self, e: &ASTExpression) {
        self.print_with_indent(&format!("CharLiteral: terminated={}", e.terminated));
    }
    fn visit_string_literal(&mut self, _e: &ASTExpression) {
        self.print_with_indent("StringLiteral");
    }
    fn visit_raw_string_literal(&mut self, _e: &ASTExpression) {
        self.print_with_indent("RawStringLiteral");
    }
    fn visit_c_string_literal(&mut self, _e: &ASTExpression) {
        self.print_with_indent("CStringLiteral");
    }
    fn visit_raw_c_string_literal(&mut self, _e: &ASTExpression) {
        self.print_with_indent("RawCStringLiteral");
    }
    fn visit_byte_literal(&mut self, _e: &ASTExpression) {
        self.print_with_indent("ByteLiteral");
    }
    fn visit_byte_string_literal(&mut self, e: &ASTExpression) {
        self.print_with_indent(&format!("ByteStringLiteral: terminated={}", e.terminated));
    }
    fn visit_raw_byte_literal(&mut self, _e: &ASTExpression) {
        self.print_with_indent("RawByteLiteral");
    }
    fn visit_integer_literal(&mut self, e: &ASTExpression) {
        self.print_with_indent(&format!("IntegerLiteral: base={:?}, empty_int={}", e.base, e.empty_int));
    }
    fn visit_float_literal(&mut self, e: &ASTExpression) {
        self.print_with_indent(&format!("FloatLiteral: base={:?}, empty_exponent={}", e.base, e.empty_exponent));
    }
    fn visit_lifetime(&mut self, e: &ASTExpression) {
        self.print_with_indent(&format!("Lifetime: starts_with_number={}", e.starts_with_number));
    }
    fn visit_semi(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Semi");
    }
    fn visit_comma(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Comma");
    }
    fn visit_dot(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Dot");
    }
    fn visit_open_paren(&mut self, _e: &ASTExpression) {
        self.print_with_indent("OpenParen");
    }
    fn visit_close_paren(&mut self, _e: &ASTExpression) {
        self.print_with_indent("CloseParen");
    }
    fn visit_open_brace(&mut self, _e: &ASTExpression) {
        self.print_with_indent("OpenBrace");
    }
    fn visit_close_brace(&mut self, _e: &ASTExpression) {
        self.print_with_indent("CloseBrace");
    }
    fn visit_open_bracket(&mut self, _e: &ASTExpression) {
        self.print_with_indent("OpenBracket");
    }
    fn visit_close_bracket(&mut self, _e: &ASTExpression) {
        self.print_with_indent("CloseBracket");
    }
    fn visit_at(&mut self, _e: &ASTExpression) {
        self.print_with_indent("At");
    }
    fn visit_pound(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Pound");
    }
    fn visit_tilde(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Tilde");
    }
    fn visit_question(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Question");
    }
    fn visit_colon(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Colon");
    }
    fn visit_semicolon(&mut self, _e: &ASTExpression) {
        self.print_with_indent("SemiColon");
    }
    fn visit_dollar(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Dollar");
    }
    fn visit_eq(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Eq");
    }
    fn visit_bang(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Bang");
    }
    fn visit_lt(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Lt");
    }
    fn visit_gt(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Gt");
    }
    fn visit_minus(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Minus");
    }
    fn visit_and(&mut self, _e: &ASTExpression) {
        self.print_with_indent("And");
    }
    fn visit_or(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Or");
    }
    fn visit_plus(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Plus");
    }
    fn visit_star(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Star");
    }
    fn visit_slash(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Slash");
    }
    fn visit_caret(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Caret");
    }
    fn visit_percent(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Percent");
    }
    fn visit_unknown(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Unknown");
    }
    fn visit_error(&mut self, _e: &ASTExpression) {
        self.print_with_indent("Error");
    }
    fn visit_eof(&mut self, _e: &ASTExpression) {
        self.print_with_indent("EOF");
    }
}

impl ASTPrinter{
    fn print_with_indent(&mut self, text: &str) {
        println!("{}{}", " ".repeat(self.indent), text);
    }
}

pub enum ASTStatementKind {
    Expression(ASTExpression),
}

pub struct ASTStatement {
    kind: ASTStatementKind
}

impl ASTStatement {
    pub fn new(kind: ASTStatementKind) -> Self {
        ASTStatement {kind}
    }

    pub fn expression(expr: ASTExpression) -> Self{
        ASTStatement::new(ASTStatementKind::Expression(expr))
    }
}

// Expression struct definitions
#[derive(Debug, PartialEq, Clone)]
pub struct ASTExpression {
    kind: ()
}

//     pub fn whitespace() -> Self {
//         ASTExpression {
//             kind: ASTExpressionKind::Whitespace(ASTExpression),
//         }
//     }
//
//     pub fn line_comment(doc_style: Option<DocStyle>) -> Self {
//         ASTExpression {
//             kind: ASTExpressionKind::LineComment(ASTExpression { doc_style }),
//         }
//     }
//
//     pub fn block_comment(doc_style: Option<DocStyle>, terminated: bool) -> Self {
//         ASTExpression {
//             kind: ASTExpressionKind::BlockComment(ASTExpression { doc_style, terminated }),
//         }
//     }

// Placeholder types - you'll need to define these based on your token types
#[derive(Debug, PartialEq, Clone)]
pub enum DocStyle {
    Inner,
    Outer,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Base {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTExpressionKind {
    Whitespace(ASTExpression),
    LineComment(ASTExpression),
    BlockComment(ASTExpression),
    Frontmatter(ASTExpression),
    Identifier(ASTExpression),
    RawIdentifier(ASTExpression),
    InvalidIdentifier(ASTExpression),
    Keyword(ASTExpression),
    UnknownPrefix(ASTExpression),
    UnknownPrefixLifetime(ASTExpression),
    RawLifetime(ASTExpression),
    GuardedStrPrefix(ASTExpression),
    CharLiteral(ASTExpression),
    StringLiteral(ASTExpression),
    RawStringLiteral(ASTExpression),
    CStringLiteral(ASTExpression),
    RawCStringLiteral(ASTExpression),
    ByteLiteral(ASTExpression),
    ByteStringLiteral(ASTExpression),
    RawByteLiteral(ASTExpression),
    IntegerLiteral(ASTExpression),
    FloatLiteral(ASTExpression),
    Lifetime(ASTExpression),
    Semi(ASTExpression),
    Comma(ASTExpression),
    Dot(ASTExpression),
    OpenParen(ASTExpression),
    CloseParen(ASTExpression),
    OpenBrace(ASTExpression),
    CloseBrace(ASTExpression),
    OpenBracket(ASTExpression),
    CloseBracket(ASTExpression),
    At(ASTExpression),
    Pound(ASTExpression),
    Tilde(ASTExpression),
    Question(ASTExpression),
    Colon(ASTExpression),
    SemiColon(ASTExpression),
    Dollar(ASTExpression),
    Eq(ASTExpression),
    Bang(ASTExpression),
    Lt(ASTExpression),
    Gt(ASTExpression),
    Minus(ASTExpression),
    And(ASTExpression),
    Or(ASTExpression),
    Plus(ASTExpression),
    Star(ASTExpression),
    Slash(ASTExpression),
    Caret(ASTExpression),
    Percent(ASTExpression),
    Unknown(ASTExpression),
    Error(ASTExpression),
    EOF(ASTExpression),
}