fn main() {
    let input = include_str!("../test_data/cs50/mario.rs");
    let output = rustc::transpile(input);

    println!("{}", output)
}
