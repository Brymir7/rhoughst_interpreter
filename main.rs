use once_cell::sync::Lazy;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Keyword {
    Def,
    If,
    Then,
    End,
    And,
    Or,
    Not,
    Return,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Delimiter {
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    Colon,
    Comma,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Literal {
    Integer(i64),
    Character(char),
}
#[derive(Debug, Clone,Copy, PartialEq, Eq, Hash)]
enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Not,
    Equal,
    LessThan,
    GreaterThan,
}
#[derive(Debug, Clone,Copy, PartialEq, Eq, Hash)]
enum UnaryOperator {
    Not,
    Negate,
}
#[derive(Debug, Clone,Copy, PartialEq, Eq, Hash)]
enum Operator {
    BinaryOperator(BinaryOperator),
    UnaryOperator(UnaryOperator),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Token {
    Keyword(Keyword),
    Identifier(String),
    Delimiter(Delimiter),
    Operator(Operator),
    Literal(Literal),
    EndOfFile,
}

const KEYWORD_MAP: Lazy<HashMap<&'static str, Keyword>> = Lazy::new(|| {
    let mut map = HashMap::new();
    map.insert("def", Keyword::Def);
    map.insert("if", Keyword::If);
    map.insert("then", Keyword::Then);
    map.insert("end", Keyword::End);
    map.insert("and", Keyword::And);
    map.insert("or", Keyword::Or);
    map.insert("not", Keyword::Not);
    map.insert("return", Keyword::Return);
    map
});
const OPERATOR_MAP: Lazy<HashMap<char, BinaryOperator>> = Lazy::new(|| {
    let mut map = HashMap::new();
    map.insert('+', BinaryOperator::Add);
    map.insert('-', BinaryOperator::Subtract);
    map.insert('*', BinaryOperator::Multiply);
    map.insert('/', BinaryOperator::Divide);
    map.insert('%', BinaryOperator::Modulo);
    map.insert('!', BinaryOperator::Not);
    map.insert('=', BinaryOperator::Equal);
    map.insert('<', BinaryOperator::LessThan);
    map.insert('>', BinaryOperator::GreaterThan);
    map
});
const UNARY_OPERATOR_MAP: Lazy<HashMap<char, UnaryOperator>> = Lazy::new(|| {
    let mut map = HashMap::new();
    map.insert('!', UnaryOperator::Not);
    map.insert('-', UnaryOperator::Negate);
    map
});

fn parse_token(source: &str) -> (Token, &str) {
    // Returns token, remainder
    let source = source.trim_start();
    let Some(char) = source.chars().next() else {
        // No more characters, what do we return?
        return (Token::EndOfFile, "");
    };

    match char {
        '_' | 'a'..='z' | 'A'..='Z' => parse_keyword_or_identifier(source),
        '0'..='9' => parse_number_literal(source),
        '(' => (Token::Delimiter(Delimiter::LeftParenthesis), &source[1..]),
        ')' => (Token::Delimiter(Delimiter::RightParenthesis), &source[1..]),
        '{' => (Token::Delimiter(Delimiter::LeftBrace), &source[1..]),
        '}' => (Token::Delimiter(Delimiter::RightBrace), &source[1..]),
        ':' => (Token::Delimiter(Delimiter::Colon), &source[1..]),
        ',' => (Token::Delimiter(Delimiter::Comma), &source[1..]),
        _ => {
            if let Some(operator) = OPERATOR_MAP.get(&char) {
                (Token::Operator(Operator::BinaryOperator(*operator)), &source[1..])
            } else if let Some(operator) = UNARY_OPERATOR_MAP.get(&char) {
                (Token::Operator(Operator::UnaryOperator(*operator)), &source[1..])
            } else {
                panic!("unknown token at start of {:?}", source);
            }
        }
    }
}

fn parse_keyword_or_identifier(source: &str) -> (Token, &str) {
    debug_assert!(matches!(
        source.chars().next(),
        Some('_' | 'a'..='z' | 'A'..='Z')
    ));

    let mut characters_after_identifier = source
        .char_indices()
        .skip_while(|(_, c)| matches!(c, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'));
    let (identifier, remainder) = match characters_after_identifier.next() {
        None => (source, ""),
        Some((index_after, _)) => source.split_at(index_after),
    };
    let token = match KEYWORD_MAP.get(identifier) {
        Some(&keyword) => Token::Keyword(keyword),
        None => Token::Identifier(identifier.to_string()),
    };
    (token, remainder)
}

fn parse_number_literal(source: &str) -> (Token, &str) {
    debug_assert!(matches!(source.chars().next(), Some('0'..='9')));
    let mut character_after_literal = source
        .char_indices()
        .skip_while(|(_, c)| matches!(c, '0'..='9'));
    let (literal, remainder) = match character_after_literal.next() {
        None => (source, ""),
        Some((index_after, _)) => source.split_at(index_after),
    };
    (Token::Literal(Literal::Integer(literal.parse::<i64>().unwrap())), remainder)
}

fn tokenize(source: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut remainder = source;
    loop {
        let (token, next_remainder) = parse_token(remainder);
        if token == Token::EndOfFile {
            return tokens;
        }
        tokens.push(token);
        remainder = next_remainder;
    }
}

fn main() {
    let tokens = tokenize("def add(x, y): return 5123 + x + y");
    println!("{tokens:?}");
    println!("Hello, world!");
}
