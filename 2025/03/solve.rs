use std::fs;
fn main() {
    println!("{}", part1(INPUT_FILE));
}

const INPUT_FILE: &str = "input.txt";

fn part1(file: &str) -> usize {
    fs::read_to_string(file)
        .expect("Open file")
        .lines()
        .map(|l| l.split("").filter_map(|c| c.parse().ok()).collect())
        .filter_map(find_max_joltage)
        .sum()
}

fn find_max_joltage(battery: Vec<usize>) -> Option<usize> {
    let mut maximum = battery.iter().max()?;
    if battery.iter().position(|x| x == maximum) == Some(battery.len() - 1) {
        maximum = battery.iter().take(battery.len() - 1).max()?;
    }
    let snd_digit = battery.iter().skip_while(|x| *x != maximum).skip(1).max()?;
    Some(maximum * 10 + snd_digit)
}

#[cfg(test)]
mod test {
    use crate::{find_max_joltage, part1};
    const INPUT_TEST_FILE: &str = "input-test.txt";
    #[test]
    fn test_part1() {
        assert_eq!(part1(INPUT_TEST_FILE), 357);
    }

    #[test]
    fn test_find_max_joltage() {
        assert_eq!(find_max_joltage(vec![1, 2, 3]), Some(23));
    }
}
