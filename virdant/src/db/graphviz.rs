use super::*;

impl Db {
    pub fn save_graphviz<P: AsRef<std::path::Path>>(&self, filepath: P) {
        let map = self.map.lock().unwrap();

        let mut queries: Vec<Query> = map
            .keys()
            .cloned()
            .chain(map.values().flat_map(|cached_val| cached_val.deps.iter().cloned()))
            .collect::<HashSet<_>>()
            .into_iter()
            .collect();
        queries.sort_by_key(|query| format!("{query:?}"));

        let query_indices: HashMap<Query, usize> = queries
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, query)| (query, i))
            .collect();

        let mut dot = String::new();
        writeln!(&mut dot, "digraph db {{").unwrap();
        writeln!(&mut dot, "    rankdir=RL;").unwrap();
        writeln!(
            &mut dot,
            "    graph [label=\"Db Query Dependencies\", labelloc=t];",
        ).unwrap();

        for query in &queries {
            let node_id = query_indices[query];
            let shape = if query.is_input() { "folder" } else { "box" };
            let style = if query.is_input() { "filled" } else { "rounded" };

            let label = if let Some(cached_val) = map.get(query) {
                /*
                format!(
                    "{:?}\nrev={}\ndeps={}",
                    query,
                    cached_val.rev,
                    cached_val.deps.len(),
                )
                */
                format!("{:?}", query)
            } else {
                format!("{:?}\\n(not cached)", query)
            };
            let label = escape_graphviz_label(&label);

            writeln!(
                &mut dot,
                "    q{node_id} [shape={shape}, style=\"{style}\", label=\"{label}\"];",
            ).unwrap();
        }

        let mut input_node_ids: Vec<usize> = queries
            .iter()
            .filter(|query| query.is_input())
            .map(|query| query_indices[query])
            .collect();
        input_node_ids.sort_unstable();

        if !input_node_ids.is_empty() {
            writeln!(&mut dot, "    subgraph inputs {{").unwrap();
            writeln!(&mut dot, "        rank=max;").unwrap();
            for node_id in input_node_ids {
                writeln!(&mut dot, "        q{node_id};").unwrap();
            }
            writeln!(&mut dot, "    }}").unwrap();
        }

        let mut edges: Vec<(usize, usize)> = vec![];
        for (query, cached_val) in map.iter() {
            let from = query_indices[query];
            let mut deps = cached_val.deps.clone();
            deps.sort_by_key(|dep| format!("{dep:?}"));

            for dep in deps {
                let to = query_indices[&dep];
                edges.push((from, to));
            }
        }
        edges.sort_unstable();

        for (from, to) in edges {
            writeln!(&mut dot, "    q{from} -> q{to};").unwrap();
        }

        writeln!(&mut dot, "}}").unwrap();

        std::fs::write(filepath, dot).unwrap();
    }
}

fn escape_graphviz_label(label: &str) -> String {
    label
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
}
