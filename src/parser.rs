use std::rc::Rc;

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
    ($s:expr, $e:expr) => {{
        let n = $s.next();

        n.as_ref()
            .filter(|n| **n == $e)
            .unwrap_or_else(|| panic!("Tokens: {:?}\n\nExpected {:?} | Got {:?}", $s.tokens, $e, n))
    }};
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

        expect!(self, Token::OpenParen);

        let mut params = vec![];
        self.get_fn_params(&mut params);

        let return_type = self
            .match_fn_return_type()
            .unwrap_or(Type::Owned("()".into()));

        let body = self.get_body();

        Function {
            params,
            name,
            body,
            return_type,
        }
    }

    fn get_body(&mut self) -> Vec<Statement> {
        expect!(self, Token::OpenBrace);

        let mut body = vec![];

        while self.peek().filter(|n| **n != Token::CloseBrace).is_some() {
            let stmt = self.get_statement();
            body.push(stmt);
            self.optional(Token::Semicolon);
        }

        expect!(self, Token::CloseBrace);

        body
    }

    fn expect_single_identifier(&mut self) -> Rc<str> {
        match self.next().expect("Expected identifier") {
            Token::Identifier(id) => id,
            t => err!(t, self.tokens),
        }
    }

    fn get_fn_params(&mut self, params: &mut Vec<Param>) {
        if self.peek().filter(|n| **n == Token::CloseParen).is_some() {
            expect!(self, Token::CloseParen);
            return;
        }

        params.push(self.get_fn_param());

        match self.peek().expect("Unexpected end of input") {
            Token::Comma => {
                expect!(self, Token::Comma);
                self.get_fn_params(params);
            }
            Token::CloseParen => {
                expect!(self, Token::CloseParen);
            }
            t => err!(t),
        };
    }

    fn get_expression(&mut self) -> Expression {
        let expr = match self.next().expect("Expected statement") {
            Token::For => Expression::For(self.get_for()),
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
            Token::Hyphen => {
                expect!(self, Token::Hyphen);
                Expression::BinaryOp(
                    Box::new(expr),
                    BinaryOp::Minus,
                    Box::new(self.get_expression()),
                )
            }
            Token::Dot => self.match_method_expression(expr),
            _ => expr,
        }
    }

    fn get_statement(&mut self) -> Statement {
        match self.peek().expect("Expected statement") {
            Token::Let => self.get_declaration(),
            _ => Statement::Expression(self.get_expression()),
        }
    }

    fn get_declaration(&mut self) -> Statement {
        expect!(self, Token::Let);

        _ = self.optional(Token::Mut);

        let id = self.expect_single_identifier();

        let typ = match self.peek().expect("Unexpected end of input") {
            Token::Colon => {
                expect!(self, Token::Colon);
                Some(self.expect_single_identifier())
            }
            _ => None,
        };

        expect!(self, Token::Equals);

        Statement::VarDeclaration(id, self.get_expression(), typ)
    }

    fn get_fn_param(&mut self) -> Param {
        let mutable = self.optional(Token::Mut);

        let var = self.expect_single_identifier();

        let name = match mutable {
            Some(_) => Variable::Mutable(var),
            None => Variable::Const(var),
        };

        expect!(self, Token::Colon);

        let param_type = self.get_type();

        Param { name, param_type }
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

    fn match_fn_return_type(&mut self) -> Option<Type> {
        match self.peek().expect("Expected -> or {") {
            Token::Hyphen => {
                expect!(self, Token::Hyphen);
                expect!(self, Token::ArrowRight);

                Some(self.get_type())
            }
            Token::OpenBrace => None,
            t => err!(t),
        }
    }

    fn match_id_expression(&mut self, id: Identifier) -> Expression {
        match self.peek().expect("Unexpected end of input") {
            Token::Bang => {
                expect!(self, Token::Bang);
                Expression::DeclMacroCall(id, self.get_fn_args())
            }
            Token::OpenParen => Expression::FunctionCall(id, self.get_fn_args()),
            Token::DoubleColon => self.get_path_expression(id),
            _ => Expression::Variable(id.get_single()),
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

    fn get_for(&mut self) -> For {
        let id = self.expect_single_identifier();

        expect!(self, Token::In);

        let start = self.get_expression();

        expect!(self, Token::DoubleDot);

        let range = match self.optional(Token::Equals) {
            Some(_) => Range::Inclusive,
            None => Range::Exclusive,
        };

        let end = self.get_expression();

        let body = self.get_body();

        For {
            indexer_name: id,
            range: (Box::new(start), Box::new(end), range),
            body,
        }
    }

    fn get_type(&mut self) -> Type {
        let ptr = self.optional(Token::Ampersand);
        let mutable = self.optional(Token::Mut);
        let name = self.expect_single_identifier();

        match ptr {
            Some(_) => match mutable {
                Some(_) => Type::MutRef(name),
                None => Type::Ref(name),
            },
            None => Type::Owned(name),
        }
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
    String(Rc<str>),
    Bool(bool),
    UnaryOp(UnaryOp, Box<Expression>),
    BinaryOp(Box<Expression>, BinaryOp, Box<Expression>),
    FunctionCall(Identifier, Vec<Expression>),
    MethodCall(Box<Expression>, Rc<str>, Vec<Expression>),
    DeclMacroCall(Identifier, Vec<Expression>),
    Variable(Rc<str>),
    Ref(Box<Expression>),
    MutRef(Box<Expression>),
    For(For),
}

#[derive(Debug, PartialEq)]
pub struct For {
    pub range: (Box<Expression>, Box<Expression>, Range),
    pub body: Vec<Statement>,
    pub indexer_name: Rc<str>,
}

#[derive(Debug, PartialEq)]
pub enum Range {
    Inclusive,
    Exclusive,
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
    pub name: Rc<str>,
    pub params: Vec<Param>,
    pub body: Vec<Statement>,
    pub return_type: Type,
}

#[derive(Debug, PartialEq)]
pub struct Struct {
    name: Rc<str>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    VarDeclaration(Rc<str>, Expression, Option<Rc<str>>),
}

#[derive(Debug, PartialEq)]
pub enum Variable {
    Const(Rc<str>),
    Mutable(Rc<str>),
}

#[derive(Debug, PartialEq)]
pub struct Param {
    pub name: Variable,
    pub param_type: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Owned(Rc<str>),
    Ref(Rc<str>),
    MutRef(Rc<str>),
}

impl Type {
    pub fn name(&self) -> &str {
        match self {
            Type::Owned(n) => n,
            Type::Ref(n) => n,
            Type::MutRef(n) => n,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub segments: Vec<Rc<str>>,
}

impl Identifier {
    fn new(name: Rc<str>) -> Self {
        Self {
            segments: vec![name],
        }
    }

    pub fn get_single(mut self) -> Rc<str> {
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
            Token::Identifier("main".into()),
            Token::OpenParen,
            Token::CloseParen,
            Token::OpenBrace,
            Token::Identifier("println".into()),
            Token::Bang,
            Token::OpenParen,
            Token::String("Hello, World!".into()),
            Token::CloseParen,
            Token::Semicolon,
            Token::CloseBrace,
        ];

        let ast = parse(input);

        let expected = Ast {
            imports: vec![],
            definitions: vec![Definition::Function(Function {
                name: "main".into(),
                params: vec![],
                body: vec![Statement::Expression(Expression::DeclMacroCall(
                    Identifier::new("println".into()),
                    vec![Expression::String("Hello, World!".into())],
                ))],
                return_type: Type::Owned("()".into()),
            })],
        };

        assert_eq!(ast, expected)
    }
}
