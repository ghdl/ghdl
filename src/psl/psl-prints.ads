with PSL.Nodes; use PSL.Nodes;
with PSL.Priorities; use PSL.Priorities;

package PSL.Prints is
   procedure Print_Unit (Unit : Node);
   procedure Print_Property (Prop : Node;
                             Parent_Prio : Priority := Prio_Lowest);
   procedure Print_Expr (N : Node; Parent_Prio : Priority := Prio_Lowest);

   --  Procedure to display HDL_Expr nodes.
   type HDL_Expr_Printer_Acc is access procedure (N : HDL_Node);
   HDL_Expr_Printer : HDL_Expr_Printer_Acc;

   procedure Print_HDL_Expr (N : HDL_Node);

   --  Like Print_Expr but always put parenthesis.
   procedure Dump_Expr (N : Node);

end PSL.Prints;

