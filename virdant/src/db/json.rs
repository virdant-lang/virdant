//! Adds `Db::dump_json`, returning the query build trace as a nested
//! JSON tree. The flat trace (a pre-order walk keyed by nesting level)
//! is reconstructed into a tree whose children are keyed `subqueries`.
//! Each node records the query (or `input` for input queries), whether
//! it was served from cache, the source location of the call, and (for
//! freshly built queries) the build duration in seconds.

use super::*;
use ::json::JsonValue;

impl Db {
    pub fn dump_json(&self) -> JsonValue {
        let trace = self.trace.lock().unwrap();

        let mut roots: Vec<TraceNode> = vec![];
        let mut stack: Vec<(usize, TraceNode)> = vec![];

        for element in trace.iter() {
            let level = element.level;
            while stack.last().is_some_and(|(l, _)| *l >= level) {
                let (_, node) = stack.pop().unwrap();
                if let Some((_, parent)) = stack.last_mut() {
                    parent.children.push(node);
                } else {
                    roots.push(node);
                }
            }
            stack.push((level, TraceNode { element, children: vec![] }));
        }

        while let Some((_, node)) = stack.pop() {
            if let Some((_, parent)) = stack.last_mut() {
                parent.children.push(node);
            } else {
                roots.push(node);
            }
        }

        let mut array = JsonValue::new_array();
        for root in &roots {
            array.push(node_to_json(root, self)).unwrap();
        }
        array
    }
}

struct TraceNode<'a> {
    element: &'a TraceElement,
    children: Vec<TraceNode<'a>>,
}

fn node_to_json(node: &TraceNode, db: &Db) -> JsonValue {
    let mut entry = ::json::object::Object::new();
    let element = node.element;

    if element.query.is_input() {
        entry.insert("input", format!("{:?}", element.query).into());
    } else {
        entry.insert("query", format!("{:?}", element.query).into());
        entry.insert("cached", element.cached.into());
    }

    let location = format!(
        "{}[{}:{}]",
        element.location.file(),
        element.location.line(),
        element.location.column(),
    );
    entry.insert("location", location.into());

    if !element.cached && !element.query.is_input() {
        let duration = {
            let map = db.map.try_lock().unwrap();
            map.get(&element.query).map(|cv| cv.duration).unwrap_or_default()
        };
        entry.insert("duration_secs", duration.as_secs_f64().into());
    }

    if !node.children.is_empty() {
        let mut subqueries = JsonValue::new_array();
        for child in &node.children {
            subqueries.push(node_to_json(child, db)).unwrap();
        }
        entry.insert("subqueries", subqueries);
    }

    JsonValue::Object(entry)
}
