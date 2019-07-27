use std::collections::HashMap;

pub struct Node<S> {
    accepts: bool,
    nexts: HashMap<S, usize>,
}

impl<S> Node<S>
where
    S: Eq + std::hash::Hash + Copy,
{
    pub fn new(accepts: bool) -> Self {
        Node {
            accepts,
            nexts: HashMap::new(),
        }
    }
    
    pub fn link(&mut self, symbol: S, nextn: usize) {
        self.nexts.insert(symbol, nextn);
    }
}

pub struct Dfa<S> {
    nodes: Vec<Node<S>>,
    start: usize,
}

impl<S> Dfa<S>
where
    S: Eq + std::hash::Hash + Copy,
{
    pub fn new() -> Self {
        Dfa {
            nodes: Vec::new(),
            start: 0,
        }
    }
    
    pub fn start(&mut self, node: usize) {
        self.start = node;
    }
    
    pub fn link(&mut self, from: usize, to: usize, symbol: S) {
        self.nodes[from].link(symbol, to);
    }
    
    pub fn add(&mut self, node: Node<S>) -> usize {
        self.nodes.push(node);
        self.nodes.len() - 1
    }
    
    pub fn parse<I>(&self, source: &mut I) -> bool
    where
        I: Iterator<Item = S>,
    {
        let nextn = |node: usize, sym: S| match self.nodes[node].nexts.get(&sym) {
            Some(u) => Some(*u),
            None => None,
        };
        
        let res = source.try_fold(self.start, nextn);
        match res {
            Some(r) => self.nodes[r].accepts,
            None => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut dfa = Dfa::<char>::new();
        let a = dfa.add(Node::new(false));
        let b = dfa.add(Node::new(true));
        let c = dfa.add(Node::new(false));
        dfa.link(a, a, '0');
        dfa.link(a, b, '1');
        dfa.link(b, b, '1');
        dfa.link(b, c, '0');
        dfa.link(c, b, '1');
        dfa.link(c, c, '0');

        assert!(dfa.parse(&mut "0001".to_string().chars()));
        assert!(dfa.parse(&mut "0101".to_string().chars()));
        assert!(!dfa.parse(&mut "1110".to_string().chars()));
    }
}
