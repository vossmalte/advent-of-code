use itertools::{Itertools, multizip};
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

fn part2(input: String) -> Option<usize> {
    let (operands, operators) = input.trim().rsplit_once("\n")?;
    let mut lines = operands.lines().map(|l| l.chars());
    let operands = multizip((lines.next()?, lines.next()?, lines.next()?, lines.next()?)).map(
        |(a, b, c, d)| {
            [a, b, c, d]
                .iter()
                .collect::<String>()
                .trim()
                .parse::<usize>()
                .ok()
        },
    );
    Some(
        operands
            .chunk_by(|x| x.is_some())
            .into_iter()
            .filter_map(|(is_ok, vs)| {
                if is_ok {
                    Some(vs.flatten().collect::<Vec<usize>>())
                } else {
                    None
                }
            })
            .zip(operators.split_whitespace())
            .map(|(x, op)| -> usize {
                match op {
                    "*" => x.iter().product(),
                    "+" => x.iter().sum(),
                    _ => panic!("wtf"),
                }
            })
            .sum(),
    )
}

#[cfg(test)]
mod test {
    use crate::parse_input_file;
    use crate::part1;
    use crate::part2;

    const INPUT_TEST_FILE: &str = "input-test.txt";
    const INPUT_TEST_FILE_FIXED: &str = "input-test-4-operands.txt";

    #[test]
    fn test_part2() {
        assert_eq!(Some(3263827), part2(parse_input_file(INPUT_TEST_FILE)));
    }

    #[test]
    fn test_part1() {
        assert_eq!(
            Some(4277556),
            part1(parse_input_file(INPUT_TEST_FILE_FIXED))
        )
    }
}
