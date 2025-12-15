use graph::Graph;
use itertools::Itertools;
use std::fs;

const INPUT_FILE: &str = "input.txt";

fn main() {
    println!("Part 1: {:?}", part1(parse_input_file(INPUT_FILE), 1000));
    println!("Part 2: {:?}", part2(parse_input_file(INPUT_FILE)));
}

type Position = (usize, usize, usize);

fn parse_input_file(file: &str) -> Vec<Position> {
    fs::read_to_string(file)
        .expect("Open file")
        .lines()
        .flat_map(|line| {
            line.splitn(3, ',')
                .flat_map(|n| n.parse().ok())
                .collect_tuple()
        })
        .collect()
}

fn distance_squared((a, b, c): &Position, (x, y, z): &Position) -> usize {
    (a.abs_diff(*x)).pow(2) + (b.abs_diff(*y)).pow(2) + (c.abs_diff(*z)).pow(2)
}

fn part1(positions: Vec<Position>, num_of_connections: usize) -> usize {
    let edges = positions
        .clone()
        .into_iter()
        .cartesian_product(positions.clone())
        .sorted_by(|(a1, a2), (b1, b2)| {
            Ord::cmp(&distance_squared(a1, a2), &distance_squared(b1, b2))
        })
        .skip_while(|(a, b)| a == b)
        .step_by(2)
        .take(num_of_connections)
        .collect();

    let graph = Graph::from_double_edges(edges);
    let subgraphs = graph.disjoint_subgraphs();

    subgraphs
        .iter()
        .map(|subgraph| subgraph.len())
        .sorted()
        .rev()
        .take(3)
        .product()
}

fn part2(_input: Vec<Position>) -> Option<usize> {
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
        assert_eq!(Some(25272), part2(parse_input_file(INPUT_TEST_FILE)));
    }

    #[test]
    fn test_part1() {
        assert_eq!((40), part1(parse_input_file(INPUT_TEST_FILE), 10))
    }
}
