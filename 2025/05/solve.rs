use std::fs;

const INPUT_FILE: &str = "input.txt";

fn main() {
    println!("Part 1: {:?}", part1(parse_input_file(INPUT_FILE)));
    println!("Part 2: {:?}", part2(parse_input_file(INPUT_FILE)));
}

type FreshRange = (isize, isize);
type FreshRanges = Vec<FreshRange>;
type AvailableItems = Vec<isize>;

fn parse_input_file(file: &str) -> Option<(FreshRanges, AvailableItems)> {
    fs::read_to_string(file)
        .ok()?
        .split_once("\n\n")
        .map(|(fresh, available)| {
            let fresh = fresh
                .to_string()
                .lines()
                .filter_map(|l| {
                    let (start, end) = l.split_once("-")?;
                    Some((start.parse().ok()?, end.parse().ok()?))
                })
                .collect::<Vec<(isize, isize)>>();
            let available = available.lines().filter_map(|a| a.parse().ok()).collect();
            (fresh.to_vec(), available)
        })
}

fn part1(input: Option<(FreshRanges, AvailableItems)>) -> Option<usize> {
    let (fresh, available) = input?;
    Some(
        available
            .iter()
            .filter(|&id| fresh.iter().any(|r| in_range(*id, *r)))
            .count(),
    )
}

fn in_range(id: isize, (start, end): FreshRange) -> bool {
    start <= id && id <= end
}

fn range_len((start, end): FreshRange) -> isize {
    if start <= end {
        return end - start + 1;
    }
    0
}

fn count_duplicates((a_start, a_end): FreshRange, (b_start, b_end): FreshRange) -> isize {
    let overlap = (a_start.max(b_start), a_end.min(b_end));
    range_len(overlap)
}

fn push_range(rs: FreshRanges, a @ (a_start, a_end): FreshRange) -> FreshRanges {
    let (mergeable, mut disjunct): (FreshRanges, FreshRanges) =
        rs.into_iter().partition(|b @ (b_start, b_end)| {
            count_duplicates(a, *b) > 0
                || (a_start.abs_diff(*b_end) == 1 || (b_start.abs_diff(a_end) == 1))
        });

    let (starts, ends): (Vec<isize>, Vec<isize>) = mergeable.into_iter().unzip();
    let merged = (
        starts.iter().min().map_or(a_start, |x| a_start.min(*x)),
        ends.iter().max().map_or(a_end, |x| a_end.max(*x)),
    );
    disjunct.push(merged);
    disjunct
}
fn part2(input: Option<(FreshRanges, AvailableItems)>) -> Option<isize> {
    let (fresh, _) = input?;
    Some(
        fresh
            .iter()
            .fold(vec![], |r, a| push_range(r, *a))
            .iter()
            .map(|r| range_len(*r))
            .sum(),
    )
}

#[cfg(test)]
mod test {
    use crate::parse_input_file;
    use crate::part1;
    use crate::part2;

    const INPUT_TEST_FILE: &str = "input-test.txt";

    #[test]
    fn test_part2() {
        assert_eq!(Some(14), part2(parse_input_file(INPUT_TEST_FILE)));
    }

    #[test]
    fn test_part1() {
        assert_eq!(Some(3), part1(parse_input_file(INPUT_TEST_FILE)))
    }
}
