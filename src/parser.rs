use crate::lexer::Token;

pub fn parse(tokens: Vec<Token>) -> Ast {
    Parser::new(tokens).build()
}

macro_rules! err {
    ($t:expr, $tokens:expr) => {{
        eprintln!("Tokens: {:?}", $tokens);
        panic!("Unexpected token {:?}", $t)
    }};

    ($t:expr) => {{
        panic!("Unexpected token {:?}", $t);
    }};
}

macro_rules! expect {
    ($s:expr, $e:expr) => {
        $s.next()
            .filter(|n| *n == $e)
            .unwrap_or_else(|| panic!("Tokens: {:?}\n\nExpected {:?}", $s.tokens, $e))
    };
}

struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    fn new(mut tokens: Vec<Token>) -> Self {
        tokens.reverse();
        Self { tokens }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.last()
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.pop()
    }

    fn optional(&mut self, expect: Token) -> Option<()> {
        self.peek().filter(|n| **n == expect)?;

        self.next();

        Some(())
    }

    fn build(&mut self) -> Ast {
        let imports = self.get_imports();

        let definitions = self.get_definitions();

        Ast {
            imports,
            definitions,
        }
    }

    fn get_definitions(&mut self) -> Vec<Definition> {
        let mut definitions = vec![];

        while self.peek().is_some() {
            definitions.push(self.get_definition())
        }

        definitions
    }

    fn get_imports(&mut self) -> Vec<Identifier> {
        let mut imports = vec![];

        while self.peek().filter(|n| **n == Token::Use).is_some() {
            imports.push(self.get_import())
        }

        imports
    }

    fn get_fn_def(&mut self) -> Function {
        expect!(self, Token::Fn);

        let name = self.expect_single_identifier();

        let params = self.get_fn_params();

        let return_type = self.match_fn_return_type().unwrap_or("()".to_string());

        expect!(self, Token::OpenBrace);

        let mut body = vec![];

        while self.peek().filter(|n| **n != Token::CloseBrace).is_some() {
            body.push(self.get_statement());

            match self.peek().expect("Unexpected end of input") {
                Token::CloseBrace => {}
                _ => {
                    expect!(self, Token::Semicolon);
                }
            };
        }

        expect!(self, Token::CloseBrace);

        Function {
            params,
            name,
            body,
            return_type,
        }
    }

    fn expect_single_identifier(&mut self) -> String {
        match self.next().expect("Expected identifier") {
            Token::Identifier(id) => id,
            t => err!(t, self.tokens),
        }
    }

    fn get_fn_params(&mut self) -> Vec<Variable> {
        let mut params = vec![];

        expect!(self, Token::OpenParen);

        while self.peek().filter(|n| **n != Token::CloseParen).is_some() {
            params.push(self.get_fn_param())
        }

        expect!(self, Token::CloseParen);

        params
    }

    fn get_expression(&mut self) -> Expression {
        let expr = match self.next().expect("Expected statement") {
            Token::For => todo!(),
            Token::Loop => todo!(),
            Token::Plus => todo!(),
            Token::Hyphen => todo!(),
            Token::Asterisk => todo!(),
            Token::Bang => todo!(),
            Token::Ampersand => self.get_ref(),
            Token::True => Expression::Bool(true),
            Token::False => Expression::Bool(false),
            Token::String(s) => Expression::String(s),
            Token::Char(c) => Expression::Char(c),
            Token::I32(n) => Expression::I32(n),
            Token::Identifier(id) => self.match_id_expression(Identifier::new(id)),
            t => err!(t, self.tokens),
        };

        match self.peek().expect("Unexpected end of input") {
            Token::Dot => self.match_method_expression(expr),
            _ => expr,
        }
    }

    fn get_statement(&mut self) -> Statement {
        match self.peek().expect("Expected statement") {
            Token::Let => {
                expect!(self, Token::Let);

                _ = self.optional(Token::Mut);

                let id = self.expect_single_identifier();

                expect!(self, Token::Equals);

                Statement::Assignment(id, self.get_expression())
            }
            _ => Statement::Expression(self.get_expression()),
        }
    }

    fn get_fn_param(&mut self) -> Variable {
        let name = self.expect_single_identifier();
        expect!(self, Token::Colon);
        let var_type = self.expect_single_identifier();
        Variable { name, var_type }
    }

    fn get_import(&mut self) -> Identifier {
        let mut segments = vec![];
        expect!(self, Token::Use);
        segments.push(self.expect_single_identifier());

        while self.peek().filter(|n| **n == Token::DoubleColon).is_some() {
            expect!(self, Token::DoubleColon);
            segments.push(self.expect_single_identifier());
        }

        expect!(self, Token::Semicolon);

        Identifier { segments }
    }

    fn get_definition(&mut self) -> Definition {
        match self.peek().expect("Expected struct | fn Definition") {
            Token::Fn => Definition::Function(self.get_fn_def()),
            Token::Struct => Definition::Struct(self.get_struct_def()),
            Token::Impl => Definition::ImplBlock(self.get_impl_block()),
            t => err!(t),
        }
    }

    fn get_struct_def(&self) -> Struct {
        todo!()
    }

    fn match_fn_return_type(&mut self) -> Option<String> {
        match self.peek().expect("Expected -> or {") {
            Token::Hyphen => Some(self.get_fn_return_type()),
            Token::OpenBrace => None,
            t => err!(t),
        }
    }

    fn get_fn_return_type(&mut self) -> String {
        expect!(self, Token::Hyphen);
        expect!(self, Token::ArrowRight);

        self.expect_single_identifier()
    }

    fn match_id_expression(&mut self, id: Identifier) -> Expression {
        match self.peek().expect("Expected ; or ( or ! or ::") {
            Token::Bang => {
                expect!(self, Token::Bang);
                Expression::DeclMacroCall(id, self.get_fn_args())
            }
            Token::OpenParen => Expression::FunctionCall(id, self.get_fn_args()),
            Token::Semicolon | Token::Dot | Token::CloseParen | Token::CloseBrace => {
                Expression::Variable(id.get_single())
            }
            Token::DoubleColon => self.get_path_expression(id),
            t => err!(t),
        }
    }

    fn get_fn_args(&mut self) -> Vec<Expression> {
        expect!(self, Token::OpenParen);

        let mut args = vec![];

        while self.peek().filter(|n| **n != Token::CloseParen).is_some() {
            args.push(self.get_expression());
            _ = self.optional(Token::Comma);
        }

        expect!(self, Token::CloseParen);

        args
    }

    fn get_path_expression(&mut self, mut id: Identifier) -> Expression {
        expect!(self, Token::DoubleColon);

        id.segments.push(self.expect_single_identifier());

        self.match_id_expression(id)
    }

    fn match_method_expression(&mut self, target: Expression) -> Expression {
        expect!(self, Token::Dot);

        let id = self.expect_single_identifier();

        let args = self.get_fn_args();
        let expr = Expression::MethodCall(Box::new(target), id, args);

        match self.peek().expect("Unexpected end of input") {
            Token::Semicolon => expr,
            Token::Dot => self.match_method_expression(expr),
            t => err!(t),
        }
    }

    fn get_ref(&mut self) -> Expression {
        let mutable = self.optional(Token::Mut);

        let expr = Box::new(self.get_expression());

        match mutable {
            Some(_) => Expression::MutRef(expr),
            None => Expression::Ref(expr),
        }
    }

    fn get_impl_block(&self) -> ImplBlock {
        todo!()
    }
}

#[derive(Debug, PartialEq)]
pub struct Ast {
    pub imports: Vec<Identifier>,
    pub definitions: Vec<Definition>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    I32(i32),
    Char(char),
    String(String),
    Bool(bool),
    UnaryOp(UnaryOp, Box<Expression>),
    BinaryOp(Box<Expression>, BinaryOp, Box<Expression>),
    FunctionCall(Identifier, Vec<Expression>),
    MethodCall(Box<Expression>, String, Vec<Expression>),
    DeclMacroCall(Identifier, Vec<Expression>),
    Variable(String),
    Ref(Box<Expression>),
    MutRef(Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    And,
    Or,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    LogNeg,
    Ref,
    MutRef,
    Deref,
}

#[derive(Debug, PartialEq)]
pub enum Definition {
    Struct(Struct),
    Function(Function),
    ImplBlock(ImplBlock),
}

impl Definition {
    pub fn id(&self) -> &str {
        match self {
            Definition::Struct(s) => &s.name,
            Definition::Function(f) => &f.name,
            Definition::ImplBlock(_) => todo!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ImplBlock;

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Variable>,
    pub body: Vec<Statement>,
    pub return_type: String,
}

#[derive(Debug, PartialEq)]
pub struct Struct {
    name: String,
    fields: Vec<Variable>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Assignment(String, Expression),
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub name: String,
    pub var_type: String,
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub segments: Vec<String>,
}

impl Identifier {
    fn new(name: String) -> Self {
        Self {
            segments: vec![name],
        }
    }

    pub fn get_single(mut self) -> String {
        if self.segments.len() == 1 {
            self.segments.pop().unwrap()
        } else {
            panic!("Expected id {:?} to have a single element", self)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hello_world() {
        let input = vec![
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

        let ast = parse(input);

        let expected = Ast {
            imports: vec![],
            definitions: vec![Definition::Function(Function {
                name: "main".to_string(),
                params: vec![],
                body: vec![Statement::Expression(Expression::DeclMacroCall(
                    Identifier::new("println".to_string()),
                    vec![Expression::String("Hello, World!".to_string())],
                ))],
                return_type: "()".to_string(),
            })],
        };

        assert_eq!(ast, expected)
    }
}
