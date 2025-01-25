use crate::vhdl::nodes_def::Node;

pub struct Chain {
    head : Node,
    tail : Node,
}

impl Chain {
    pub fn new() -> Self {
        Chain{head: Node::NULL, tail: Node::NULL}
    }

    pub fn append(self: &mut Self, n : Node) {
        if self.tail == Node::NULL {
            self.head = n;
        }
        else {
            self.tail.set_chain(n);
        }
        self.tail = n;
    }
    pub fn head(self: Self) -> Node {
        self.head
    }
}
