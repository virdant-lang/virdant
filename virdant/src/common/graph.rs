use indexmap::IndexMap;
use std::hash::Hash;

#[derive(Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct VertIndex(usize);

pub struct Graph<V> {
    verts: Vec<V>,
    adj: Vec<Vec<VertIndex>>,
}

#[derive(Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct CycleError(pub Vec<VertIndex>);

struct TopoSortContext<'g, V> {
    graph: &'g Graph<V>,
    state: IndexMap<VertIndex, SortState>,
    stack: Vec<VertIndex>,
    rev_sorting: Vec<VertIndex>,
    cycle: Option<Vec<VertIndex>>,
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
enum SortState {
    Unvisited,
    Visiting,
    Visited,
}

impl<V: Clone> Graph<V> {
    pub fn new() -> Graph<V> {
        Graph {
            verts: vec![],
            adj: vec![],
        }
    }

    pub fn add_vert(&mut self, vert: V) -> VertIndex {
        let index = VertIndex(self.verts.len());
        self.verts.push(vert);
        self.adj.push(vec![]);
        index
    }

    pub fn add_edge(&mut self, from_index: VertIndex, to_index: VertIndex) {
        self.adj[from_index.0].push(to_index);
    }

    pub fn toposort(&self) -> Result<Vec<V>, CycleError> {
        let mut results = vec![];
        for vert_index in TopoSortContext::new(self).sort()? {
            results.push(self[vert_index].clone());
        }
        Ok(results)
    }
}

impl<V: Eq> Graph<V> {
    pub fn vertex(&self, search: &V) -> Option<VertIndex> {
        for (i, vert) in self.verts.iter().enumerate() {
            if vert == search {
                return Some(VertIndex(i));
            }
        }
        None
    }
}

impl<V> Graph<V> {
    /// Returns a path from `from` to `to` (exclusive of `from`, inclusive of `to`),
    /// or None if `to` is not reachable from `from`.
    pub fn path(&self, from: VertIndex, to: VertIndex) -> Option<Vec<VertIndex>> {
        let mut visited = vec![false; self.verts.len()];
        let mut stack: Vec<(VertIndex, usize)> = vec![(from, 0)];
        visited[from.0] = true;

        while let Some((current, child_idx)) = stack.last().copied() {
            let children = &self.adj[current.0];
            if child_idx < children.len() {
                let next_child = children[child_idx];
                // Bump the parent's child pointer.
                stack.last_mut().unwrap().1 = child_idx + 1;
                if next_child == to {
                    // Build the path from the stack plus `to`.
                    let mut path: Vec<VertIndex> = stack.iter().skip(1).map(|(v, _)| *v).collect();
                    path.push(to);
                    return Some(path);
                }
                if !visited[next_child.0] {
                    visited[next_child.0] = true;
                    stack.push((next_child, 0));
                }
            } else {
                stack.pop();
            }
        }
        None
    }
}

impl<'g, V> TopoSortContext<'g, V> {
    fn new(graph: &'g Graph<V>) -> Self {
        let state: IndexMap<VertIndex, SortState> = (0..graph.verts.len()).map(|i| (VertIndex(i), SortState::Unvisited)).collect();

        TopoSortContext {
            graph,
            state,
            rev_sorting: vec![],
            stack: vec![],
            cycle: None,
        }
    }

    fn sort(mut self) -> Result<Vec<VertIndex>, CycleError> {
        while let Some(vert_index) = self.choose_unvisited() {
            self.dfs(vert_index);
            if self.cycle.is_some() {
                break;
            }
        }

        if let Some(cycle) = self.cycle.take() {
            return Err(CycleError(cycle));
        }

        Ok(self.rev_sorting.into_iter().rev().collect())
    }

    fn choose_unvisited(&self) -> Option<VertIndex> {
        for (i, _v) in self.graph.verts.iter().enumerate() {
            if self.state[&VertIndex(i)] == SortState::Unvisited {
                return Some(VertIndex(i));
            }
        }
        None
    }

    fn dfs(&mut self, vert: VertIndex) {
        self.stack.push(vert);
        self.state.insert(vert, SortState::Visiting);

        for child in &self.graph.adj[vert.0] {
            let child_state = self.state[child];
            if child_state == SortState::Unvisited {
                self.dfs(*child);
            } else if child_state == SortState::Visiting {
                let stack = self.stack.clone();
                let child_position = stack.iter().position(|v| v == child).unwrap();
                let cycle: Vec<_> = stack[child_position..].to_vec();
                self.cycle = Some(cycle);
            }
        }

        self.rev_sorting.push(vert);
        self.state.insert(vert, SortState::Visited);
        self.stack.pop();
    }
}

impl<V> std::ops::Index<VertIndex> for Graph<V> {
    type Output = V;

    fn index(&self, index: VertIndex) -> &Self::Output {
        &self.verts[index.0]
    }
}
