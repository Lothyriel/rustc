fn main() {
    let height = read_usize();

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

fn repeat(output: &mut String, c: char, count: usize) {
    for _ in 0..count {
        output.push(c);
    }
}
