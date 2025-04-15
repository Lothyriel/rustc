fn main() {
    mario()
}

fn mario() {
    let input = read_line();
    println!("Height: {}", input);

    let Ok(height) = input.parse::<i32>() else {
        return mario();
    };

    let mut buf = String::new();

    for h in 1..=height {
        repeat(&mut buf, ' ', height - h);
        repeat(&mut buf, '#', h);
        repeat(&mut buf, ' ', 2);
        repeat(&mut buf, '#', h);
        buf.push('\n');
    }

    println!("{}", buf);
}

fn repeat(output: &mut String, c: char, count: i32) {
    for _ in 0..count {
        output.push(c);
    }
}
