with PSL.Nodes; use PSL.Nodes;

package PSL.Rewrites is
   function Rewrite_Boolean (N : Node) return Node;
   function Rewrite_SERE (N : Node) return Node;
   function Rewrite_Property (N : Node) return Node;
   procedure Rewrite_Unit (N : Node);
end PSL.Rewrites;
