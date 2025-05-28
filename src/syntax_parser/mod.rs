use crate::lexer_scanner::scanner::{Token};

pub mod parser;

pub struct Ast {
    pub statements: Vec<ASTStatement>
}
// Implementing the abstract syntax tree class.
// Essentially a vector of ast statements.
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
    // Need to add other values to the EXPRESSIONS
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
            // In the future I will add multiple expression types to cover scope.
            ASTExpressionKind::Integer(number) => {
                self.visit_integer(number);
            }
            ASTExpressionKind::Float(float) => {
                self.visit_float(float);
            }
            ASTExpressionKind::Identifier(identifier) => {
                self.visit_identifier(identifier);
            }
            ASTExpressionKind::Operator(operator) => {
                self.visit_operator(operator);
            }
            ASTExpressionKind::Keyword(keyword) => {
                self.visit_keyword(keyword);
            }

            _ => {}
        }
    }
    fn visit_expression(&mut self, expression: &ASTExpression) {
        self.do_visit_expression(expression);
    }

    fn visit_integer(&mut self, number: &ASTIntegerExpression);
    fn visit_float(&mut self, number: &ASTFloatExpression);
    fn visit_identifier(&mut self, number: &ASTIdentifierExpression);
    fn visit_operator(&mut self, number: &ASTOperatorExpression);
    fn visit_keyword(&mut self, number: &ASTKeywordExpression);

}
// Prints out the AST when it is completed.
pub struct ASTPrinter {
    indent: usize,
}

const INDENT_LEVEL: usize = 2;
impl ASTVisitor for ASTPrinter{
    fn visit_statement(&mut self, statement: &ASTStatement){
        self.print_with_indent("Statement:");
        self.indent += INDENT_LEVEL;
        ASTVisitor::do_visit_statement(self, statement);
        self.indent += INDENT_LEVEL;
    }

    fn visit_expression(&mut self, expression: &ASTExpression){
        self.print_with_indent("Expression:");
        self.indent += INDENT_LEVEL;
        ASTVisitor::do_visit_expression(self, expression);
        self.indent += INDENT_LEVEL;
    }

    fn visit_integer(&mut self, number: &ASTIntegerExpression) {
        self.print_with_indent(&format!("Number: {}", number.number));
    }
    fn visit_float(&mut self, float: &ASTFloatExpression) {
        self.print_with_indent(&format!("Float: {}", float.float));
    }
    fn visit_identifier(&mut self, identifier: &ASTIdentifierExpression) {
        self.print_with_indent(&format!("Identifier: {}", identifier.identifier));
    }
    fn visit_operator(&mut self, operator: &ASTOperatorExpression) {
        self.print_with_indent(&format!("Operator: {}", operator.operator));
    }
    fn visit_keyword(&mut self, keyword: &ASTKeywordExpression) {
        self.print_with_indent(&format!("Keyword: {}", keyword.keyword));
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
// KEEPING AS AN EXAMPLE
// #[derive(Debug)]
// pub enum ASTBinaryOperatorKind{
//     Plus,
//     Minus,
//     Multiply,
//     Divide
// }
//
// pub struct ASTBinaryOperator{
//     kind: ASTBinaryOperatorKind,
//     token: Token,
// }
//
// impl ASTBinaryOperator{
//     pub fn new(kind: ASTBinaryOperatorKind, token: Token) -> Self{
//         ASTBinaryOperator {kind, token}
//     }
//
//     pub fn precedence(&self) -> u8{
//         match self.kind {
//             ASTBinaryOperatorKind::Plus => 1,
//             ASTBinaryOperatorKind::Minus => 1,
//             ASTBinaryOperatorKind::Multiply => 2,
//             ASTBinaryOperatorKind::Divide => 2,
//         }
//     }
// }

pub struct ASTIntegerExpression{
    number: i64,
}
pub struct ASTFloatExpression{
    float: f64,
}
pub struct ASTIdentifierExpression{
    identifier: String,
}
pub struct ASTOperatorExpression{
    operator: String,
}
pub struct ASTKeywordExpression{
    keyword: String,
}

pub enum ASTExpressionKind {
    Integer(
        ASTIntegerExpression
    ),
    Float(
        ASTFloatExpression
    ),
    Identifier(
        ASTIdentifierExpression
    ),
    Operator(
        ASTOperatorExpression
    ),
    Keyword(
        ASTKeywordExpression
    ),
}

pub struct ASTExpression{
    kind: ASTExpressionKind
}

// Will be used to take in tokens/tokentypes assign them the correct spot in the AST.
impl ASTExpression {
    // Need to add other values to the EXPRESSIONS
    pub fn new(kind: ASTExpressionKind)-> Self{
        ASTExpression{kind}
    }
    // Going to have to change this to integer when adding float values.
    pub fn number(number: i64) -> Self {
        ASTExpression::new(ASTExpressionKind::Integer(ASTIntegerExpression{number}))
    }
    pub fn float(float: f64) -> Self {
        ASTExpression::new(ASTExpressionKind::Float(ASTFloatExpression{float}))
    }
    pub fn identifier(identifier: String) -> Self {
        ASTExpression::new(ASTExpressionKind::Identifier(ASTIdentifierExpression{identifier}))
    }
    pub fn operator(operator: String) -> Self {
        ASTExpression::new(ASTExpressionKind::Operator(ASTOperatorExpression{operator}))
    }
    pub fn keyword(keyword: String) -> Self {
        ASTExpression::new(ASTExpressionKind::Keyword(ASTKeywordExpression{keyword}))
    }
}