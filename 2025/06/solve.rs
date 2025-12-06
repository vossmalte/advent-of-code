use itertools::multizip;
use std::fs;

const INPUT_FILE: &str = "input.txt";

fn main() {
    println!("Part 1: {:?}", part1(parse_input_file(INPUT_FILE)));
    println!("Part 2: {:?}", part2(parse_input_file(INPUT_FILE)));
}

fn parse_input_file(file: &str) -> String {
    fs::read_to_string(file).expect("Open file")
}

fn part1(input: String) -> Option<usize> {
    let (operands, operators) = input.trim().rsplit_once("\n")?;
    let mut lines = operands
        .lines()
        .map(|l| l.split_whitespace().flat_map(|n| n.parse::<usize>().ok()));
    Some(
        multizip((
            operators.split_whitespace(),
            lines.next()?,
            lines.next()?,
            lines.next()?,
            lines.next()?,
        ))
        .map(|(op, a, b, c, d)| match op {
            "*" => a * b * c * d,
            "+" => a + b + c + d,
            _ => panic!("wtf"),
        })
        .sum(),
    )
}

fn part2(_input: String) -> Option<usize> {
    Some(1)
}

#[cfg(test)]
mod test {
    use crate::parse_input_file;
    use crate::part1;
    use crate::part2;

    const INPUT_TEST_FILE: &str = "input-test.txt";

    #[test]
    fn test_part2() {
        assert_eq!(Some(1), part2(parse_input_file(INPUT_TEST_FILE)));
    }

    #[test]
    fn test_part1() {
        assert_eq!(Some(4277556), part1(parse_input_file(INPUT_TEST_FILE)))
    }
}
