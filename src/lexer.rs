pub fn tokenize(code: &str) -> Vec<Token> {
    let mut lex = Lexer::new(code);

    while let Some(cur) = lex.peek() {
        let token = match cur {
            n if n.is_ascii_alphabetic() => lex.match_str(),
            n if n.is_ascii_digit() => lex.get_number_literal(),
            CHAR_DELIM => lex.match_char_literal(),
            STR_DELIM => lex.match_str_literal(),
            b'(' => lex.consume(Token::OpenParen),
            b')' => lex.consume(Token::CloseParen),
            b'{' => lex.consume(Token::OpenBrace),
            b'}' => lex.consume(Token::CloseBrace),
            // todo: add double colon disambig
            b':' => lex.consume(Token::Colon),
            b';' => lex.consume(Token::Semicolon),
            b'+' => lex.consume(Token::Plus),
            // todo add thin arrow disambig
            b'-' => lex.consume(Token::Hyphen),
            b'*' => lex.consume(Token::Asterisk),
            // todo: add comments parsing
            b'/' => lex.consume(Token::Div),
            b'!' => lex.consume(Token::Bang),
            b'=' => lex.consume(Token::Equals),
            n if n.is_ascii_whitespace() => {
                lex.next();
                continue;
            }
            n => unreachable!("{}", n as char),
        };

        lex.tokens.push(token);
    }

    lex.tokens
}

const CHAR_DELIM: u8 = b'\'';
const STR_DELIM: u8 = b'\"';

struct Lexer {
    tokens: Vec<Token>,
    reader: Vec<u8>,
}

impl Lexer {
    fn new(code: &str) -> Self {
        let reader = code.bytes().rev().collect();

        Self {
            tokens: vec![],
            reader,
        }
    }

    fn peek(&self) -> Option<u8> {
        self.reader.last().copied()
    }

    fn next(&mut self) -> Option<u8> {
        self.reader.pop()
    }

    fn consume(&mut self, token: Token) -> Token {
        self.next();
        token
    }

    fn expect(&mut self, expect: u8) {
        self.next()
            .filter(|n| *n == expect)
            .unwrap_or_else(|| panic!("Expected {}", expect as char));
    }

    fn match_str(&mut self) -> Token {
        let word = self.get_str();

        match word.as_str() {
            "let" => Token::Let,
            "for" => Token::For,
            "loop" => Token::Loop,
            "fn" => Token::Fn,
            "use" => Token::Use,
            "struct" => Token::Struct,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Identifier(word),
        }
    }

    fn get_str(&mut self) -> String {
        let mut id = String::new();

        while let Some(c) = self.peek().filter(u8::is_ascii_alphanumeric) {
            id.push(c as char);
            self.next();
        }

        if id.chars().next().expect("Identifier").is_numeric() {
            panic!("Word can't start with a number",)
        } else {
            id
        }
    }

    fn get_number_literal(&mut self) -> Token {
        let mut id = String::new();

        while let Some(c) = self.peek().filter(u8::is_ascii_digit) {
            id.push(c as char);
            self.next();
        }

        Token::I32(id.parse().expect("Valid i32"))
    }

    fn match_str_literal(&mut self) -> Token {
        self.expect(STR_DELIM);

        let mut id = String::new();

        while let Some(c) = self.peek().filter(|n| *n != STR_DELIM) {
            id.push(c as char);
            self.next();
        }

        self.expect(STR_DELIM);

        Token::String(id)
    }

    fn match_char_literal(&mut self) -> Token {
        self.expect(CHAR_DELIM);

        let byte = self.next().expect("Expected char");

        self.expect(CHAR_DELIM);

        Token::Char(byte as char)
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    // keywords
    Fn,
    For,
    Loop,
    Let,
    True,
    False,
    Use,
    Struct,
    // others
    Identifier(String),
    // syntax symbols
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Colon,
    DoubleColon,
    Semicolon,
    // operators
    Plus,
    Hyphen,
    Asterisk,
    Div,
    Bang,
    Equals,
    // literals
    String(String),
    Char(char),
    I32(i32),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hello_world() {
        let tokens = tokenize(include_str!("../test_data/hello_world.rs"));

        let expected = vec![
            Token::Fn,
            Token::Identifier("main".to_string()),
            Token::OpenParen,
            Token::CloseParen,
            Token::OpenBrace,
            Token::Identifier("println".to_string()),
            Token::Bang,
            Token::OpenParen,
            Token::String("Hello, World!".to_string()),
            Token::CloseParen,
            Token::Semicolon,
            Token::CloseBrace,
        ];

        assert_eq!(tokens, expected)
    }
}
