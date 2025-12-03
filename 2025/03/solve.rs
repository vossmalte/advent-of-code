use std::fs;
fn main() {
    println!("Part 1: {}", part1(INPUT_FILE));
    println!("Part 2: {}", part2(INPUT_FILE));
}

const INPUT_FILE: &str = "input.txt";

fn part1(file: &str) -> usize {
    fs::read_to_string(file)
        .expect("Open file")
        .lines()
        .map(|l| l.split("").filter_map(|c| c.parse().ok()).collect())
        .filter_map(find_max_joltage_for_two_batteries)
        .sum()
}
fn part2(file: &str) -> usize {
    fs::read_to_string(file)
        .expect("Open file")
        .lines()
        .map(|l| l.split("").filter_map(|c| c.parse().ok()).collect())
        .filter_map(find_max_joltage_for_twelve_batteries)
        .sum()
}


fn find_max_joltage_for_two_batteries(bank: Vec<usize>) -> Option<usize> {
    find_max_joltage(bank, 2)
}
fn find_max_joltage_for_twelve_batteries(bank: Vec<usize>) -> Option<usize> {
    find_max_joltage(bank, 12)
}

fn find_max_joltage(bank: Vec<usize>, num_batteries: usize) -> Option<usize> {
    if num_batteries == 0 {
        return Some(0);
    }
    let maximum = bank.iter().take(bank.len() - num_batteries + 1).max()?;
    let remaining_bank = bank
        .iter()
        .skip_while(|x| *x != maximum)
        .skip(1)
        .cloned()
        .collect();
    let digit = maximum * (10_usize.pow(num_batteries as u32 - 1));
    // println!(
    //     "took {}, left: {:?} for {} batteries",
    //     digit,
    //     remaining_bank,
    //     num_batteries - 1
    // );
    Some(digit + (find_max_joltage(remaining_bank, num_batteries - 1)?))
}

#[cfg(test)]
mod test {
    use crate::{find_max_joltage, find_max_joltage_for_two_batteries, part1, part2};

    const INPUT_TEST_FILE: &str = "input-test.txt";

    #[test]
    fn test_part1() {
        assert_eq!(part1(INPUT_TEST_FILE), 357);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(INPUT_TEST_FILE), 3121910778619);
    }

    #[test]
    fn test_find_max_joltage() {
        // assert_eq!(find_max_joltage_for_two_batteries(vec![1, 2, 3]), Some(23));
        assert_eq!(find_max_joltage_for_two_batteries(vec![1, 2]), Some(12));
        assert_eq!(find_max_joltage(vec![1, 2, 2, 1], 1), Some(2));
    }
}
