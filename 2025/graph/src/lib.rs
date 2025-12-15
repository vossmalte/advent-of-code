use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

#[derive(Debug)]
pub struct Graph<V> {
    vertices: HashSet<V>,
    edges: HashMap<V, HashSet<V>>,
    // weights: HashMap<(V, V), W>,
}

impl<V: Hash + Eq + Copy> Graph<V> {
    pub fn new() -> Self {
        Graph {
            vertices: HashSet::new(),
            edges: HashMap::new(),
            // weights: HashMap::new(),
        }
    }

    pub fn from_vertices(vertices: HashSet<V>) -> Self {
        Graph {
            vertices: vertices,
            edges: HashMap::new(),
        }
    }

    pub fn from_double_edges(edges: Vec<(V, V)>) -> Self {
        let mut g = Graph::new();
        for (start, end) in edges {
            g.add_edge(start, end);
            g.add_edge(end, start);
        }
        g
    }

    pub fn vertices(&self) -> &HashSet<V> {
        &self.vertices
    }

    pub fn is_disjoint(&self) -> bool {
        if let Some(root) = self.vertices.iter().next() {
            return self.vertices.len() != self.reachable_vertices(*root).len();
        }
        panic!("there should be some root")
    }

    pub fn disjoint_subgraphs(&self) -> Vec<HashSet<V>> {
        let mut result = vec![];
        let mut vertices_to_visit = self.vertices.clone();
        while let Some(root) = vertices_to_visit.iter().cloned().next() {
            vertices_to_visit.remove(&root);
            let reachable_vertices = self.reachable_vertices(root);

            vertices_to_visit =
                HashSet::from_iter(vertices_to_visit.difference(&reachable_vertices).cloned());

            result.push(reachable_vertices);
        }
        result
    }

    fn reachable_vertices(&self, from: V) -> HashSet<V> {
        let mut visited = HashSet::new();
        let mut to_traverse = vec![from];
        while let Some(vertex) = to_traverse.pop() {
            visited.insert(vertex);
            if let Some(adjacent) = self.edges.get(&vertex) {
                let mut new_vertices: Vec<V> =
                    adjacent.difference(&visited).cloned().collect::<Vec<V>>();
                to_traverse.append(&mut new_vertices);
            }
        }

        visited
    }

    pub fn add_edge(&mut self, start: V, end: V) {
        self.vertices.insert(start);
        self.vertices.insert(end);
        self.edges
            .entry(start)
            .and_modify(|neighbors| {
                neighbors.insert(end);
            })
            .or_insert(HashSet::from([end]));
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use crate::Graph;

    #[test]
    fn test_disjoint_subgraphs() {
        let g = Graph::from_double_edges(vec![(1, 2), (2, 3), (4, 5)]);
        assert_eq!(
            g.disjoint_subgraphs(),
            vec![HashSet::from([1, 2, 3]), HashSet::from([4, 5])]
        )
    }
}
