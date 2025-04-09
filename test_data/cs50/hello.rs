fn main() {
    print!("What's your name? ");

    let name = read_line();

    println!("hello, {}", name);
}

fn read_line() -> String {
    let mut buf = String::new();

    std::io::stdin()
        .read_line(&mut buf)
        .expect("Failed to read input");

    buf
}
