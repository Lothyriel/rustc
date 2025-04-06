fn main() {
    let output = rustc::transpile(include_str!("../test_data/cs50/hello.rs"));

    println!("{}", output)
}
