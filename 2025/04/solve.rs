use std::fs;

const INPUT_FILE: &str = "input.txt";
const ROLL_OF_PAPER: char = '@';

fn main() {
    println!(
        "Part 1: {:?}",
        count_available_toilet_roles(parse_input_file(INPUT_FILE))
    )
}

fn parse_input_file(file: &str) -> Vec<Vec<char>> {
    fs::read_to_string(file)
        .expect("Open file")
        .lines()
        .map(|l| l.chars().collect())
        .collect()
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

fn count_available_toilet_roles(input: Vec<Vec<char>>) -> Option<usize> {
    let _height = input.len();
    let width = input.first()?.len();
    let paper_positions: Vec<_> = input
        .into_iter()
        .flatten()
        .enumerate()
        .filter_map(|(i, c)| {
            if c == ROLL_OF_PAPER {
                return Some(i);
            }
            None
        })
        .collect();
    Some(
        paper_positions
            .iter()
            .filter(|&&p| {
                4 > get_neighbors(p, width)
                    .iter()
                    .filter(|pn| paper_positions.contains(pn))
                    .count()
            })
            .count(),
    )
}

#[cfg(test)]
mod test {
    use crate::count_available_toilet_roles;
    use crate::get_neighbors;
    use crate::parse_input_file;

    const INPUT_TEST_FILE: &str = "input-test.txt";

    #[test]
    fn test_part1() {
        assert_eq!(
            Some(13),
            count_available_toilet_roles(parse_input_file(INPUT_TEST_FILE))
        )
    }

    #[test]
    fn test_get_neighbors() {
        assert_eq!(get_neighbors(0, 3), vec![1, 3, 4]);
        assert_eq!(get_neighbors(1, 3), vec![2, 0, 3, 4, 5]);
        assert_eq!(get_neighbors(2, 3), vec![1, 4, 5]);
    }
}
