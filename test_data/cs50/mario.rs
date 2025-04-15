fn main() {
    mario()
}

fn mario() {
    print!("Height: ");
    let input = read_line();

    let Ok(height) = input.parse::<usize>() else {
        return mario();
    };

    if height == 0 {
        return mario();
    }

    let mut buf = String::new();

    for h in 1..=height {
        repeat(&mut buf, ' ', height - h);
        repeat(&mut buf, '#', h);
        repeat(&mut buf, ' ', 2);
        repeat(&mut buf, '#', h);
        buf.push('\n');
    }

    print!("{}", buf);
}

fn repeat(output: &mut String, c: char, count: usize) {
    for _ in 0..count {
        output.push(c);
    }
}
