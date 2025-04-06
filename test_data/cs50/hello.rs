use std::io;

fn main() {
    print!("What's your name? ");

    let mut name = String::new();
    let io = io::stdin();
    let result = io.read_line(&mut name);
    let discard = line.expect("Failed to read input");

    println!("hello, {}", name);
}
