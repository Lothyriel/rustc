pub fn tokenize(code: &str) -> Vec<Token> {
    let mut lex = Lexer::new(code);

    while let Some(cur) = lex.peek() {
        let token = match cur {
            n if n.is_ascii_digit() => lex.get_number_literal(),
            n if n.is_ascii_alphabetic() || n == b'_' => lex.match_str(),
            CHAR_DELIM => lex.match_char_literal(),
            STR_DELIM => lex.match_str_literal(),
            b'(' => lex.consume(Token::OpenParen),
            b')' => lex.consume(Token::CloseParen),
            b'{' => lex.consume(Token::OpenBrace),
            b'}' => lex.consume(Token::CloseBrace),
            // todo: add double colon disambig
            b':' => lex.match_colon(),
            b';' => lex.consume(Token::Semicolon),
            b',' => lex.consume(Token::Comma),
            b'+' => lex.consume(Token::Plus),
            // todo add thin arrow disambig
            b'-' => lex.consume(Token::Hyphen),
            b'>' => lex.consume(Token::ArrowRight),
            b'<' => lex.consume(Token::ArrowLeft),
            b'*' => lex.consume(Token::Asterisk),
            // todo: add comments parsing
            b'/' => lex.consume(Token::Div),
            b'!' => lex.consume(Token::Bang),
            b'.' => lex.consume(Token::Dot),
            b'=' => lex.consume(Token::Equals),
            b'&' => lex.consume(Token::Ampersand),
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

macro_rules! expect {
    ($s:expr, $e:expr) => {
        $s.next()
            .filter(|n| *n == $e)
            .unwrap_or_else(|| panic!("Expected {}", $e as char));
    };
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

    fn match_str(&mut self) -> Token {
        let word = self.get_str();

        match word.as_str() {
            "let" => Token::Let,
            "mut" => Token::Mut,
            "for" => Token::For,
            "loop" => Token::Loop,
            "fn" => Token::Fn,
            "use" => Token::Use,
            "in" => Token::In,
            "struct" => Token::Struct,
            "impl" => Token::Impl,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Identifier(word),
        }
    }

    fn get_str(&mut self) -> String {
        let mut id = String::new();

        while let Some(c) = self
            .peek()
            .filter(|c| c.is_ascii_alphanumeric() || *c == b'_')
        {
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
        expect!(self, STR_DELIM);

        let mut id = String::new();

        while let Some(c) = self.peek().filter(|n| *n != STR_DELIM) {
            id.push(c as char);
            self.next();
        }

        expect!(self, STR_DELIM);

        Token::String(id)
    }

    fn match_char_literal(&mut self) -> Token {
        expect!(self, CHAR_DELIM);

        let byte = self.next().expect("Expected char");

        let byte = match byte {
            b'\\' => self.match_escaped_char(byte),
            _ => byte,
        };

        expect!(self, CHAR_DELIM);

        Token::Char(byte as char)
    }

    fn match_escaped_char(&mut self, byte: u8) -> u8 {
        match self.next().expect("Unexpected end of input") {
            b'\\' => b'\\',
            b'\'' => b'\'',
            b'\"' => b'\"',
            b'n' => b'\n',
            b'0' => b'\0',
            _ => byte,
        }
    }

    fn match_colon(&mut self) -> Token {
        expect!(self, b':');
        match self.peek().expect("Unexpected end of input") {
            b':' => {
                expect!(self, b':');
                Token::DoubleColon
            }
            _ => Token::Colon,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    // keywords
    Fn,
    For,
    Loop,
    Let,
    Mut,
    True,
    False,
    Use,
    Struct,
    Impl,
    In,
    // others
    Identifier(String),
    // syntax symbols
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Colon,
    Comma,
    DoubleColon,
    Semicolon,
    // operators
    Plus,
    Hyphen,
    ArrowLeft,
    ArrowRight,
    Asterisk,
    Div,
    Bang,
    Equals,
    Dot,
    Ampersand,
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
