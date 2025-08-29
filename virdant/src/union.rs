use hashbrown::HashMap;

#[derive(Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct UnionIndex(usize);

#[derive(Debug)]
pub struct Union<T> {
    items: Vec<T>,
    parents: Vec<UnionIndex>
}

impl<T> Union<T> {
    pub fn new() -> Union<T> {
        Union {
            items: Vec::new(),
            parents: Vec::new(),
        }
    }

    pub fn insert(&mut self, item: T) -> UnionIndex {
        let index = UnionIndex(self.items.len());
        self.items.push(item);
        self.parents.push(index);
        index
    }

    #[inline]
    fn parent(&self, i: UnionIndex) -> UnionIndex {
        self.parents[i.0]
    }

    #[inline]
    fn set_parent(&mut self, i: UnionIndex, j: UnionIndex) {
        self.parents[i.0] = j;
    }

    fn root(&self, i: UnionIndex) -> UnionIndex {
        let mut current = i;
        loop {
            let parent = self.parent(current);
            if parent != current {
                current = parent;
            } else {
                return current;
            }
        }
    }

    pub fn join(&mut self, i: UnionIndex, j: UnionIndex) {
        let mut current = i;

        loop {
            let parent = self.parent(current);
            self.set_parent(current, j);
            if parent != current {
                current = parent;
            } else {
                break;
            }
        }
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn normalize(&mut self) {
        for i in 0..self.len() {
            let i = UnionIndex(i);
            self.set_parent(i, self.root(i));
        }
    }

    pub fn into_iter(mut self) -> Vec<Vec<T>> {
        self.normalize();
        let mut group_for = HashMap::new();

        for (parent, item) in self.parents.into_iter().zip(self.items.into_iter()) {
            if !group_for.contains_key(&parent) {
                group_for.insert(parent, vec![]);
            }

            let group = group_for.get_mut(&parent).unwrap();
            group.push(item);
        }

        let mut results = vec![];
        for (_representative, group) in group_for.into_iter() {
            results.push(group);
        }
        results
    }
}

impl<T> std::ops::Index<UnionIndex> for Union<T> {
    type Output = T;

    fn index(&self, index: UnionIndex) -> &Self::Output {
        &self.items[index.0]
    }
}

#[test]
fn union_test() {
    let mut union = Union::new();
    let a = union.insert('a');
    let b = union.insert('b');
    let c = union.insert('c');
    let d = union.insert('d');
    let e = union.insert('e');
    let f = union.insert('f');
    let g = union.insert('g');
    let h = union.insert('h');
    let i = union.insert('i');
    let j = union.insert('j');

    union.join(a, b);
    union.join(c, d);
    union.join(e, f);
    union.join(g, h);
    union.join(i, j);
    union.join(j, g);

    union.normalize();
    //dbg!(&union);

    for group in union.into_iter() {
        //eprintln!("GROUP:");
        for _item in group {
            //eprintln!("    {item:?}");
        }
    }

    // TODO
    // NO TEST
}
