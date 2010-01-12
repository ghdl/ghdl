package PSL.NFAs.Utils is
   --  Sort outgoing edges by expression.
   procedure Sort_Src_Edges (S : NFA_State);
   procedure Sort_Src_Edges (N : NFA);

   procedure Sort_Dest_Edges (S : NFA_State);
   procedure Sort_Dest_Edges (N : NFA);

   --  Move incoming edges of S1 to S, remove S1 and its outgoing edges.
   procedure Merge_State_Dest (N : NFA; S : NFA_State; S1 : NFA_State);

   procedure Merge_State_Src (N : NFA; S : NFA_State; S1 : NFA_State);

   --  Return True if N or a child of N is EOS.
   --  N must be a boolean expression.
   function Has_EOS (N : Node) return Boolean;

   --  Raise Program_Error if N is not internally coherent.
   procedure Check_NFA (N : NFA);
end PSL.NFAs.Utils;

