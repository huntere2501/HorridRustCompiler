pub mod parser;

/// TODO: Take tokens create AST.
/// TODO: Create a symbol table from given tokens/grammar.
/// TODO: Symbol table can be created during (likely my choice) or after initial pass.


pub(crate) struct Ast {
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
            ASTExpressionKind::Whitespace => self.visit_whitespace(&ASTExpression { kind: ASTExpressionKind::Whitespace })
        }
    }

    fn visit_expression(&mut self, expression: &ASTExpression) {
        self.do_visit_expression(expression);
    }

    fn visit_whitespace(&mut self, e: &ASTExpression);
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
    kind: ASTExpressionKind
}

    pub fn whitespace() -> ASTExpression{
        ASTExpression {
            kind: ASTExpressionKind::Whitespace,
        }
    }

#[derive(Debug, PartialEq, Clone)]
pub enum ASTExpressionKind {
    Whitespace,
}