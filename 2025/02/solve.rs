use std::{fs, ops::Range};

const INPUT_FILE: &str = "input.txt";

fn main() {
    println!("Part 1: {}", part1(INPUT_FILE));
    println!("Part 2: {}", part2(INPUT_FILE));
}

fn part1(file: &str) -> usize {
    fs::read_to_string(file)
        .expect("Open file")
        .trim()
        .split(",")
        .filter_map(parse_range)
        .flatten()
        .filter(|x| is_repeating(*x))
        .sum()
}
fn part2(file: &str) -> usize {
    fs::read_to_string(file)
        .expect("Open file")
        .trim()
        .split(",")
        .filter_map(parse_range)
        .flatten()
        .filter(|x| is_multi_repeating(*x))
        .sum()
}

fn parse_range(repr: &str) -> Option<Range<usize>> {
    let mut iter = repr.split("-").map(|x| x.parse::<usize>().ok());
    let start = iter.next().flatten()?;
    let end = iter.last().flatten()?;
    Some(start..end + 1)
}

fn is_repeating(x: usize) -> bool {
    let s = x.to_string();
    s == s[..s.len() / 2].repeat(2)
}

fn is_multi_repeating(x: usize) -> bool {
    let s = x.to_string();

    (1..s.len()).any(|n| s == s[..n].repeat(s.len() / n))
}

#[cfg(test)]
mod test {

    use crate::{is_multi_repeating, is_repeating, parse_range, part1, part2};
    const INPUT_TEST_FILE: &str = "input-test.txt";

    #[test]
    fn test_parse_range() {
        assert_eq!(parse_range("99-111"), Some(99..111 + 1));
        assert_eq!(parse_range("1-13534387"), Some(1..13534387 + 1));
    }

    #[test]
    fn test_is_repeating() {
        assert!(is_repeating(12341234));
        assert!(!is_repeating(12341235));
    }

    #[test]
    fn test_is_multi_repeating() {
        assert!(is_multi_repeating(12341234));
        assert!(is_multi_repeating(111));
        assert!(!is_multi_repeating(12341235));
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(INPUT_TEST_FILE), 1227775554);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(INPUT_TEST_FILE), 4174379265);
    }
}
