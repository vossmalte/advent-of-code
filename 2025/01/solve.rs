use std::fs;
fn main() {
    parse();
}

fn lrpm(c: &str) -> isize {
    match c {
        "L" => -1,
        _ => 1,
    }
}

fn parse_line(l: &str) -> isize {
    let (direction, steps) = l.split_at(1);
    let lr = lrpm(direction);
    let steps = steps.parse().unwrap_or(0);
    lr * steps
}

fn next(start: isize, steps: isize) -> isize {
    (start + steps) % 100
}

fn parse() {
    let (_,counter) = fs::read_to_string("input.txt")
        .expect("Should have been able to read the file")
        .lines()
        .map(parse_line)
        .fold((50, 0), |(step, counter), steps| {
            let n = next(step, steps);
            (n, if n == 0 { counter + 1 } else { counter })
        });

    println!("{}", counter)
}
