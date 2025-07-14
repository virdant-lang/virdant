use hashbrown::HashMap;
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
    state: HashMap<VertIndex, SortState>,
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

impl<V> Graph<V> {
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

    pub fn toposort(&self) -> Result<Vec<VertIndex>, CycleError> {
        TopoSortContext::new(self).sort()
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

impl<'g, V> TopoSortContext<'g, V> {
    fn new(graph: &'g Graph<V>) -> Self {
        let state: HashMap<VertIndex, SortState> = (0..graph.verts.len()).map(|i| (VertIndex(i), SortState::Unvisited)).collect();

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
