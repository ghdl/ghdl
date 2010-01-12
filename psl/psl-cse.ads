with PSL.Nodes; use PSL.Nodes;

package PSL.CSE is
   --  Build boolean expressions while trying to make the node uniq.
   function Build_Bool_And (L, R : Node) return Node;
   function Build_Bool_Or (L, R : Node) return Node;
   function Build_Bool_Not (N : Node) return Node;

   procedure Dump_Hash_Table (Level : Natural := 0);
end PSL.CSE;
