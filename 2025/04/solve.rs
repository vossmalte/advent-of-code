use std::fs;

const INPUT_FILE: &str = "input.txt";
const ROLL_OF_PAPER: char = '@';

fn main() {
    println!(
        "Part 1: {:?}",
        part1(parse_input_file(INPUT_FILE))
    );
    println!("Part 2: {:?}", part2(parse_input_file(INPUT_FILE)));
}

fn parse_input_file(file: &str) -> Vec<Vec<char>> {
    fs::read_to_string(file)
        .expect("Open file")
        .lines()
        .map(|l| l.chars().collect())
        .collect()
}

fn flatten_input(input: Vec<Vec<char>>) -> Option<(Vec<char>, usize)> {
    Some((input.concat(), input.first()?.len()))
}

fn get_neighbors(position: usize, width: usize) -> Vec<usize> {
    [1, width - 1, width, width + 1]
        .iter()
        .flat_map(|&offset| [position.checked_add(offset), position.checked_sub(offset)])
        .flatten()
        .filter(|pn| match position % width {
            0 => pn % width != width - 1,
            r if r == width - 1 => pn % width != 0,
            _ => true,
        })
        .collect()
}

fn get_accessible_paper_roles(input: &[char], width: usize) -> Option<Vec<usize>> {
    let paper_positions: Vec<_> = input
        .iter()
        .enumerate()
        .filter_map(|(i, c)| {
            if *c == ROLL_OF_PAPER {
                return Some(i);
            }
            None
        })
        .collect();
    Some(
        paper_positions
            .clone()
            .into_iter()
            .filter(|p| {
                4 > get_neighbors(*p, width)
                    .iter()
                    .filter(|pn| paper_positions.contains(pn))
                    .count()
            })
            .collect(),
    )
}

fn part1(input: Vec<Vec<char>>) -> Option<usize> {
    if let Some((grid, width)) = flatten_input(input) {
        Some(get_accessible_paper_roles(&grid, width)?.len())
    } else {
        None
    }
}

fn part2(input: Vec<Vec<char>>) -> Option<usize> {
    if let Some((grid, width)) = flatten_input(input) {
        Some(iteratively_count_paper_roles(&grid, width)?)
    } else {
        None
    }
}

fn iteratively_count_paper_roles(input: &[char], width: usize) -> Option<usize> {
    let accessible_paper_positions = get_accessible_paper_roles(input, width)?;
    let num_of_taken_rolls = accessible_paper_positions.len();
    if num_of_taken_rolls == 0 {
        return Some(0);
    }

    let updated_grid = input
        .iter()
        .enumerate()
        .map(|(i, c)| {
            if accessible_paper_positions.contains(&i) {
                'x'
            } else {
                *c
            }
        })
        .collect::<Vec<char>>();
    println!("Collected {} paper rolls", num_of_taken_rolls);
    Some(num_of_taken_rolls + iteratively_count_paper_roles(&updated_grid.to_vec(), width)?)
}

#[cfg(test)]
mod test {
    use crate::part1;
    use crate::get_neighbors;
    use crate::parse_input_file;
    use crate::part2;

    const INPUT_TEST_FILE: &str = "input-test.txt";

    #[test]
    fn test_part2() {
        assert_eq!(Some(43), part2(parse_input_file(INPUT_TEST_FILE)));
    }

    #[test]
    fn test_part1() {
        assert_eq!(
            Some(13),
            part1(parse_input_file(INPUT_TEST_FILE))
        )
    }

    #[test]
    fn test_get_neighbors() {
        assert_eq!(get_neighbors(0, 3), vec![1, 3, 4]);
        assert_eq!(get_neighbors(1, 3), vec![2, 0, 3, 4, 5]);
        assert_eq!(get_neighbors(2, 3), vec![1, 4, 5]);
    }
}
