fn main() {
    let output = rustc::transpile(include_str!("../test_data/hello_world.rs"));

    println!("{}", output)
}
