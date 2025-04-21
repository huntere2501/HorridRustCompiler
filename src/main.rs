mod lexer_scanner;

fn main() {
    let input  = "8.0 ";
    let mut lexer  = lexer_scanner::scanner::Lexer::new(input);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next_token(){
        tokens.push(token);
    }
    println!("{:?}", tokens);
}
