mod codegen;
mod lexer;
mod parser;

pub fn transpile(code: &str) -> String {
    let tokens = lexer::tokenize(code);

    let ast = parser::parse(tokens);

    ast.generate()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hello_world() {
        let output = transpile(include_str!("../test_data/hello_world.rs"));

        let expected = include_str!("../test_data/hello_world.c");

        assert_eq!(output, expected)
    }
}
