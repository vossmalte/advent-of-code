use itertools::Itertools;
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
    let mut lines = input.lines();
    let start = vec![lines.next()?.chars().position(|c| c == 'S')?];
    println!("Start: {:?}", start);
    let (num_hits, _) = lines
        .map(|l| l.chars().positions(|c| c == '^').collect::<Vec<usize>>())
        .fold(
            (0, start),
            |(num_total_hit_splitters, beams), splitter_positions| {
                let num_hit_splitter = splitter_positions
                    .iter()
                    .filter(|p| beams.contains(p))
                    .count();
                let new_beams = beams
                    .iter()
                    .flat_map(|b| {
                        if splitter_positions.contains(b) {
                            return vec![b - 1, b + 1];
                        }
                        vec![*b]
                    })
                    .unique()
                    .collect();
                (num_total_hit_splitters + num_hit_splitter, new_beams)
            },
        );
    Some(num_hits)
}

fn part2(input: String) -> usize {
    let mut lines = input.lines();
    let start = lines
        .next()
        .unwrap_or("")
        .chars()
        .map(|c| match c {
            'S' => 1,
            _ => 0,
        })
        .collect::<Vec<usize>>();
    let beams = lines.map(|l| l.chars()).fold(start, |beams, splitters| {
        splitters
            .zip(beams)
            .map(|x| match x {
                ('^', b) => (b, 0, b),
                (_, b) => (0, b, 0),
            })
            .fold((vec![], 0), |(mut folded, for_next), current| {
                match current {
                    (0, b, 0) => folded.push(b + for_next),
                    (b, 0, _) => {
                        let prev = folded.pop().unwrap_or(0);
                        folded.push(prev + b);
                        folded.push(for_next);
                    }
                    _ => folded.push(for_next),
                };
                (folded, current.2)
            })
            .0
    });
    beams.iter().sum()
}

#[cfg(test)]
mod test {
    use crate::parse_input_file;
    use crate::part1;
    use crate::part2;

    const INPUT_TEST_FILE: &str = "input-test.txt";

    #[test]
    fn test_part2() {
        assert_eq!(40, part2(parse_input_file(INPUT_TEST_FILE)));
    }

    #[test]
    fn test_part1() {
        assert_eq!(Some(21), part1(parse_input_file(INPUT_TEST_FILE)))
    }
}
