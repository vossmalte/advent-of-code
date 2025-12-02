use std::fs;

fn main() {
    let steps = get_lines();
    part1(&steps);
    part2(&steps);
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

fn get_lines() -> std::vec::Vec<isize> {
    fs::read_to_string("input.txt")
        .expect("Should have been able to read the file")
        .lines()
        .map(parse_line)
        .collect()
}

fn part1(lines: &[isize]) -> isize {
    let (_, counter) = lines.iter().fold((50, 0), |(step, counter), steps| {
        let n = next(step, *steps);
        (n, if n == 0 { counter + 1 } else { counter })
    });

    println!("Part 1: {}", counter);

    counter
}

fn next_with_counting_zeroes(start: isize, steps: isize) -> (isize, isize) {
    let overflowing = start + steps;
    let normalized = overflowing.rem_euclid(100);
    let overturned = match overflowing {
        o if o.is_positive() => o,
        o if start == 0 => o.abs(),
        o => o.abs() + 100,
    } / 100;
    (normalized, overturned)
}

fn part2(lines: &[isize]) -> isize {
    let (_, counter) = lines.iter().fold((50, 0), |(step, counter), steps| {
        let (n, c) = next_with_counting_zeroes(step, *steps);
        (n, counter + c)
    });

    println!("Part 2: {}", counter);

    counter
}

#[cfg(test)]
mod test {

    use crate::{next_with_counting_zeroes, parse_line, part1, part2};
    use std::fs;

    fn get_test_steps() -> std::vec::Vec<isize> {
        fs::read_to_string("input-test.txt")
            .expect("Should have been able to read the file")
            .lines()
            .map(parse_line)
            .collect()
    }

    #[test]
    fn test_part1() {
        let steps = get_test_steps();
        assert_eq!(3, part1(&steps));
    }

    #[test]
    fn test_part2() {
        let steps = get_test_steps();
        assert_eq!(6, part2(&steps));
    }

    #[test]
    fn test_part2_steps() {
        let steps = get_test_steps();
        let f = next_with_counting_zeroes;
        let scores = [1, 0, 1, 0, 1, 1, 0, 1, 0, 1];
        let mut current_pointer = 50;
        for (expected, step) in scores.iter().zip(steps.iter()) {
            let (new_pointer, score) = f(current_pointer, *step);
            assert_eq!(
                score, *expected,
                "Tried to turn {current_pointer} by {step} expecting {expected} but got {score}"
            );
            current_pointer = new_pointer;
        }
    }

    #[test]
    fn test_overflowing() {
        assert_eq!(3,next_with_counting_zeroes(0, -350).1)
    }
}
