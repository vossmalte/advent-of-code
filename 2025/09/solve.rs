use itertools::Itertools;
use std::fs;

const INPUT_FILE: &str = "input.txt";

fn main() {
    println!("Part 1: {:?}", part1(parse_input_file(INPUT_FILE)));
    println!("Part 2: {:?}", part2(parse_input_file(INPUT_FILE)));
}

type Position = (usize, usize);

fn parse_input_file(file: &str) -> Vec<Position> {
    fs::read_to_string(file)
        .expect("Open file")
        .lines()
        .flat_map(|line| {
            line.splitn(2, ',')
                .flat_map(|n| n.parse().ok())
                .collect_tuple()
        })
        .collect()
}

fn rectangle_size((a, b): Position, (x, y): Position) -> usize {
    (a.abs_diff(x) + 1) * (b.abs_diff(y) + 1)
}

fn part1(positions: Vec<Position>) -> Option<usize> {
    positions
        .clone()
        .into_iter()
        .cartesian_product(positions)
        .map(|(a, b)| rectangle_size(a, b))
        .sorted()
        .last()
}

fn part2(_input: Vec<Position>) -> Option<usize> {
    todo!()
}

#[cfg(test)]
mod test {
    use crate::parse_input_file;
    use crate::part1;
    use crate::part2;

    const INPUT_TEST_FILE: &str = "input-test.txt";

    #[test]
    fn test_part2() {
        assert_eq!(Some(24), part2(parse_input_file(INPUT_TEST_FILE)));
    }

    #[test]
    fn test_part1() {
        assert_eq!(Some(50), part1(parse_input_file(INPUT_TEST_FILE)))
    }
}
