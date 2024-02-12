use regex::Regex;
use std::{
    env::{self},
    fs,
};

fn provide_error(line_num: i32, error_message: &str) {
    println!("Error at line {}: {}", line_num, error_message);
    panic!()
}
#[derive(Debug)]
enum TokenType {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    COLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    ELSE,
    FALSE,
    FOR,
    IF,
    OR,
    PRINT,
    RETURN,
    TRUE,
    WHILE,
    DEF,

    EOF,
}

#[derive(Debug)]
struct Token {
    lexeme: String,
    token_type: TokenType,
    line: i32,
    col: i32,
}
impl Token {
    fn new(lexeme: String, token_type: TokenType, line: i32, col: i32) -> Self {
        Token {
            lexeme,
            token_type,
            line,
            col,
        }
    }
}
struct LexicalAnalyzer {
    source: String,
    tokens: Vec<Token>,
}

impl LexicalAnalyzer {
    fn new(source: String) -> Self {
        LexicalAnalyzer {
            source,
            tokens: Vec::new(),
        }
    }

    fn tokenize(&mut self) {
        let lines: Vec<String> = self.source.lines().map(|line| line.to_string()).collect();
        for (line_num, line) in lines.iter().enumerate() {
            for (col_num, character) in line.chars().enumerate() {
                match character {
                    '(' => self.add_token(character, TokenType::LEFT_PAREN, line_num, col_num),
                    ')' => self.add_token(character, TokenType::RIGHT_PAREN, line_num, col_num),
                    '{' => self.add_token(character, TokenType::LEFT_BRACE, line_num, col_num),
                    '}' => self.add_token(character, TokenType::RIGHT_BRACE, line_num, col_num),
                    ',' => self.add_token(character, TokenType::COMMA, line_num, col_num),
                    ':' => self.add_token(character, TokenType::COLON, line_num, col_num),
                    '+' => self.add_token(character, TokenType::PLUS, line_num, col_num),
                    '-' => self.add_token(character, TokenType::MINUS, line_num, col_num),
                    '*' => self.add_token(character, TokenType::STAR, line_num, col_num),
                    _ => provide_error(line_num.try_into().unwrap(), "Invalid character"),
                }
            }
        }
    }
    fn add_token(&mut self, lexeme: char, token_type: TokenType, line_num: usize, col_num: usize) {
        self.tokens.push(Token::new(
            lexeme.to_string(),
            token_type,
            line_num.try_into().unwrap(),
            col_num.try_into().unwrap(),
        ));
    }
    fn get_tokens(&self) -> &[Token] {
        &self.tokens
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Please provide a file name as a command-line argument.");
        return;
    }
    let file_name = &args[1];
    let contents = match fs::read_to_string(file_name) {
        Ok(contents) => contents,
        Err(error) => {
            println!("Error reading file: {}", error);
            return;
        }
    };
    let mut lex_analyzer = LexicalAnalyzer::new(contents.to_string());
    lex_analyzer.tokenize();
    print!("{:?}", lex_analyzer.get_tokens());
}
