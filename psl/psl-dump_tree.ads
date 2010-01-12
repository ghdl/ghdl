with PSL.Nodes; use PSL.Nodes;

package PSL.Dump_Tree is
   procedure Dump_Tree (N : Node; Full : Boolean := False);

   --  Procedure to dump an HDL node.
   type Dump_Hdl_Node_Acc is access procedure (N : HDL_Node);
   Dump_Hdl_Node : Dump_Hdl_Node_Acc := null;
end PSL.Dump_Tree;
