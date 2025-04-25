pub mod parser;

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

    pub fn visualize_tree(&self){
        println!("Tree AST {{");
        println!("     node [shape=box]");
        for (i, statement) in self.statements.iter().enumerate(){
            println!("     {} [label=\"{}\"];", i, statement);
        }
        println!("}}");
    }
}

trait ASTVisitor {
    fn visit_statement(&mut self, statement: &ASTStatement);
    fn visit_expression(&mut self, expr: &ASTExpression);
    fn visit_integer(&mut self, number: &ASTIntegerExpression);
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


pub enum ASTExpressionKind {
    Number(
        ASTIntegerExpression
    ),
}

pub struct ASTIntegerExpression{
    number: i64,
}

pub struct ASTExpression{
    kind: ASTExpressionKind
}

impl ASTExpression {
    pub fn new(kind: ASTExpressionKind)-> Self{
        ASTExpression{kind}
    }

    pub fn number(number: i64) -> Self {
        ASTExpression::new(ASTExpressionKind::Number(ASTIntegerExpression{number}))
    }
}