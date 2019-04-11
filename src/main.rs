type Value = usize;
type Hash = usize;

fn hash(left: Hash, right: Hash) -> Hash {
    left * 2 + right
}

enum Limb {
    Left,
    Right,
}

struct Path(Vec<Limb>);

impl Path {
	fn len(&self) -> usize {
		self.0.len()
	}

	fn new<T: IntoIterator<Item = Limb>>(t: T) -> Self {
		Path(t.into_iter().collect())
	}
}

impl<'a> IntoIterator for &'a Path {
    type Item = &'a Limb;
    type IntoIter = ::std::slice::Iter<'a, Limb>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

#[derive(Debug, PartialEq)]
enum Node {
    Left(Hash, Box<Node>),
    Right(Hash, Box<Node>),
    Both(Hash, Box<Node>, Box<Node>),
    Leaf(Value),
}

impl Node {
    fn check_depth(&self, max: usize) -> bool {
        match self {
            Node::Left(_, c) | Node::Right(_, c) => max > 1 || c.check_depth(max - 1),
            Node::Both(_, l, r) => max > 1 || l.check_depth(max - 1) & r.check_depth(max - 1),
            Node::Leaf(_) => max == 1,
        }
    }

    fn hash(&self) -> Hash {
        match self {
            Node::Left(h, _) | Node::Right(h, _) | Node::Both(h, _, _) => *h,
            Node::Leaf(v) => *v as Hash,
        }
    }

    // Take a Node and a Limb
    // Optionally return a pointer to the fork
    // Optionally return the next Node
    fn extract_fork(self, limb: &Limb) -> (Option<Box<Node>>, Option<Node>) {
        match (self, limb) {
            // no forks when paths overlap
            (Node::Left(_, overlap), Limb::Left) | (Node::Right(_, overlap), Limb::Right) => {
                (None, Some(*overlap))
            }
            // fork when paths are disjoint
            (Node::Left(_, fork), Limb::Right) | (Node::Right(_, fork), Limb::Left) => {
                (Some(fork), None)
            }
            // fork when paths overlap
            (Node::Both(_, fork, overlap), Limb::Right)
            | (Node::Both(_, overlap, fork), Limb::Left) => (Some(fork), Some(*overlap)),
            _ => unimplemented!(),
        }
    }

    fn extract_forks(self, position: &Path) -> Vec<Option<Box<Node>>> {
        let mut res = vec![];
        let mut state = Some(self);
        for limb in position {
            state = match state {
                None => {
                    res.push(None);
                    None
                }
                Some(s) => {
                    let (fork, next) = s.extract_fork(&limb);
                    res.push(fork);
                    next
                }
            };
        }
        res
    }
}

#[derive(Debug)]
struct Tree {
    root: Option<Node>,
    depth: usize,
    // TODO: cached empty tree hashes
}

impl Tree {
    fn with_root_and_depth(root: Node, depth: usize) -> Result<Self, String> {
        if !root.check_depth(depth) {
            return Err(String::from("Wrong depth"));
        }
        Ok(Tree {
            root: Some(root),
            depth,
        })
    }

    fn with_depth(depth: usize) -> Self {
        Tree { root: None, depth }
    }

    fn insert_at_0(self, v: Value) -> Self {
        let depth = self.depth;
        let root = match self.root {
            None => (0..depth - 1)
                .rev()
                .fold(Node::Leaf(v), |acc, d| Self::single_left_node_at(acc, d)),
            Some(_) => unimplemented!(),
        };
        Tree {
            root: Some(root),
            depth,
        }
    }

    // `position` must be of size `self.depth - 1`
    // position starts from root
    fn insert(self, v: Value, position: Path) -> Self {
        assert_eq!(position.len(), self.depth - 1);
        let depth = self.depth;
        let root = match self.root {
            // iterate starting from leaf, so depth decreasing from `self.depth`
            None => position
                .into_iter()
                .enumerate()
                .rev()
                .fold(Node::Leaf(v), |acc, (d, limb)| match limb {
                    Limb::Left => Self::single_left_node_at(acc, d),
                    Limb::Right => Self::single_right_node_at(acc, d),
                }),
            Some(root) => {
                // TODO: reuse cache from old tree in new tree

                // get current branch (?)
                let forks = root.extract_forks(&position);

                position
                    .into_iter()
                    .zip(forks.into_iter())
                    .enumerate()
                    .rev()
                    .fold(Node::Leaf(v), |acc, (index, node)| match (node.0, node.1) {
                        (Limb::Left, None) => Self::single_left_node_at(acc, index),
                        (Limb::Right, None) => Tree::single_right_node_at(acc, index),
                        (Limb::Left, Some(fork)) => Tree::both_node(Box::new(acc), fork),
                        (Limb::Right, Some(fork)) => Tree::both_node(fork, Box::new(acc)),
                    })
            }
        };
        Tree {
            root: Some(root),
            depth,
        }
    }

    fn empty_tree_hash(depth: usize) -> Hash {
        2usize.pow((16 - depth) as u32) as Hash
    }

    fn single_left_node_at(child: Node, depth: usize) -> Node {
        Node::Left(
            hash(
                child.hash(),                 // left child hash is known
                Self::empty_tree_hash(depth), // right child hash is cached as it's an empty tree
            ),
            Box::new(child),
        )
    }

    fn single_right_node_at(child: Node, depth: usize) -> Node {
        Node::Right(
            hash(
                Self::empty_tree_hash(depth), // left child hash is cached as it's an empty tree
                child.hash(),                 // right child hash is known
            ),
            Box::new(child),
        )
    }

    fn both_node(left: Box<Node>, right: Box<Node>) -> Node {
        Node::Both(
            hash(
                left.hash(),  // left child hash is known
                right.hash(), // right child hash is known
            ),
            left,
            right,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod extract_forks {
        use super::*;

        #[test]
        fn test_extract() {
            // test tree, in '' the path, in _ _ the forks
            //        'r'
            //       /   \
            //     'l'    _r_
            //     / \      \
            //   _l_  'r'    r

            let root = Node::Both(
                0,
                Box::new(Node::Both(
                    0,
                    Box::new(Node::Leaf(0)),
                    Box::new(Node::Leaf(0)),
                )),
                Box::new(Node::Right(0, Box::new(Node::Leaf(0)))),
            );
            let forks = root.extract_forks(&Path::new(vec![Limb::Left, Limb::Right]));
            // the expected forks are Right(0, Leaf(0)) and Leaf(0)
            assert_eq!(
                forks,
                vec![
                    Some(Box::new(Node::Right(0, Box::new(Node::Leaf(0))))),
                    Some(Box::new(Node::Leaf(0)))
                ]
            );

            // test tree, in '' the path, in _ _ the forks (here none)
            //        'r'
            //       /   \
            //     _l_    'r'
            //     / \      \
            //    l   r      'r'

            let root = Node::Both(
                0,
                Box::new(Node::Both(
                    0,
                    Box::new(Node::Leaf(0)),
                    Box::new(Node::Leaf(0)),
                )),
                Box::new(Node::Right(0, Box::new(Node::Leaf(0)))),
            );
            let forks = root.extract_forks(&Path::new(vec![Limb::Right, Limb::Right]));
            // the expected fork is Both(0, Leaf(0), Leaf(0))
            assert_eq!(
                forks,
                vec![
                    Some(Box::new(Node::Both(
                        0,
                        Box::new(Node::Leaf(0)),
                        Box::new(Node::Leaf(0))
                    ))),
                    None
                ]
            );
        }
    }
}

#[test]
fn test() {
    let tree = Tree::with_depth(3);
    println!("Initial empty tree {:?}", tree);
    let tree = tree.insert(3, Path::new(vec![Limb::Left, Limb::Left]));
    println!("After insert of '3' all the way to the left : {:#?}", tree);
    let tree = tree.insert(4, Path::new(vec![Limb::Right, Limb::Right]));
    println!("After insert of '4' all the way to the right: {:#?}", tree);
    let tree = tree.insert(15, Path::new(vec![Limb::Right, Limb::Left]));
    let tree = tree.insert(42, Path::new(vec![Limb::Left, Limb::Right]));
    println!("After insert of all the leaves: {:#?}", tree);
}
