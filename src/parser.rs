use crate::lexer::Token;

pub fn parse(tokens: Vec<Token>) -> Ast {
    Parser::new(tokens).build()
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

    fn expect(&mut self, expect: Token) {
        self.next()
            .filter(|n| *n == expect)
            .unwrap_or_else(|| panic!("Expected {:?}", expect));
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

    fn get_imports(&mut self) -> Vec<Import> {
        let mut imports = vec![];

        while self.peek().filter(|n| **n == Token::Use).is_some() {
            imports.push(self.get_import())
        }

        imports
    }

    fn get_fn_def(&mut self) -> Function {
        self.expect(Token::Fn);

        let name = self.expect_identifier();

        let params = self.get_fn_params();

        let return_type = self.match_fn_return_type().unwrap_or(Type::Unit);

        self.expect(Token::OpenBrace);

        let mut body = vec![];

        while self.peek().filter(|n| **n != Token::CloseBrace).is_some() {
            body.push(self.get_statement())
        }

        self.expect(Token::CloseBrace);

        Function {
            params,
            name,
            body,
            return_type,
        }
    }

    fn expect_identifier(&mut self) -> String {
        match self.next().expect("Expected identifier") {
            Token::Identifier(id) => id,
            _ => unreachable!(),
        }
    }

    fn get_fn_params(&mut self) -> Vec<Variable> {
        let mut params = vec![];

        self.expect(Token::OpenParen);
        while self.peek().filter(|n| **n != Token::CloseParen).is_some() {
            params.push(self.get_fn_param())
        }
        self.expect(Token::CloseParen);
        params
    }

    fn get_expression(&mut self) -> Expression {
        match self.next().expect("Expected statement") {
            Token::For => todo!(),
            Token::Loop => todo!(),
            Token::Let => todo!(),
            Token::Plus => todo!(),
            Token::Hyphen => todo!(),
            Token::Asterisk => todo!(),
            Token::Div => todo!(),
            Token::Bang => todo!(),
            Token::True => Expression::Bool(true),
            Token::False => Expression::Bool(false),
            Token::Identifier(id) => self.match_id_expression(id),
            Token::String(s) => Expression::String(s),
            Token::Char(c) => Expression::Char(c),
            Token::I32(n) => Expression::I32(n),
            _ => unreachable!(),
        }
    }

    fn get_statement(&mut self) -> Statement {
        let stmt = match self.peek().expect("Expected statement") {
            Token::Let => {
                self.expect(Token::Let);

                let id = self.expect_identifier();

                self.expect(Token::Equals);

                Statement::Let(id, self.get_expression())
            }
            _ => Statement::Expression(self.get_expression()),
        };

        self.expect(Token::Semicolon);

        stmt
    }

    fn get_fn_param(&mut self) -> Variable {
        let name = self.expect_identifier();
        self.expect(Token::Colon);
        let var_type = self.get_type();
        Variable { name, var_type }
    }

    fn get_import(&mut self) -> Import {
        self.expect(Token::Use);
        todo!()
    }

    fn get_definition(&mut self) -> Definition {
        match self.peek().expect("Expected struct | fn Definition") {
            Token::Fn => Definition::Function(self.get_fn_def()),
            Token::Struct => Definition::Struct(self.get_struct_def()),
            _ => unreachable!(),
        }
    }

    fn get_struct_def(&self) -> Struct {
        todo!()
    }

    fn match_fn_return_type(&mut self) -> Option<Type> {
        match self.peek().expect("Expected -> or {") {
            Token::Hyphen => Some(self.get_fn_return_type()),
            Token::OpenBrace => None,
            _ => unreachable!(),
        }
    }

    fn get_fn_return_type(&self) -> Type {
        todo!()
    }

    fn match_id_expression(&mut self, id: String) -> Expression {
        match self.peek().expect("Expected ; or ( or !") {
            Token::Bang => {
                self.expect(Token::Bang);
                Expression::DeclMacroCall(id, self.get_fn_args())
            }
            Token::OpenBrace => Expression::FunctionCall(id, self.get_fn_args()),
            Token::Semicolon => Expression::Variable(id),
            _ => unreachable!(),
        }
    }

    fn get_fn_args(&mut self) -> Vec<Expression> {
        self.expect(Token::OpenParen);

        let mut args = vec![];

        while self.peek().filter(|n| **n != Token::CloseParen).is_some() {
            args.push(self.get_expression())
        }

        self.expect(Token::CloseParen);

        args
    }

    fn get_type(&self) -> Type {
        todo!()
    }
}

#[derive(Debug, PartialEq)]
pub struct Ast {
    pub imports: Vec<Import>,
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
    FunctionCall(String, Vec<Expression>),
    DeclMacroCall(String, Vec<Expression>),
    Variable(String),
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
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Variable>,
    pub body: Vec<Statement>,
    pub return_type: Type,
}

#[derive(Debug, PartialEq)]
pub struct Struct {
    name: String,
    fields: Vec<Variable>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Let(String, Expression),
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub name: String,
    pub var_type: Type,
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Unit,
}

#[derive(Debug, PartialEq)]
pub struct Import;

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
                    "println".to_string(),
                    vec![Expression::String("Hello, World!".to_string())],
                ))],
                return_type: Type::Unit,
            })],
        };

        assert_eq!(ast, expected)
    }
}
