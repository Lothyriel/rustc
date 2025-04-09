mod codegen;
mod lexer;
mod parser;

pub fn transpile(code: &str) -> String {
    let tokens = lexer::tokenize(code);

    let ast = parser::parse(tokens);

    codegen::generate(ast)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hello_world() {
        _ = transpile(include_str!("../test_data/hello_world.rs"));
    }
}
