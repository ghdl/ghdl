with PSL.NFAs; use PSL.NFAs;
with PSL.Nodes; use PSL.Nodes;

package PSL.Disp_NFAs is
   procedure Disp_Head (Name : String);
   procedure Disp_Tail;
   procedure Disp_Body (N : NFA);

   procedure Disp_State (S : NFA_State);

   procedure Disp_NFA (N : NFA; Name : String := "nfa");
end PSL.Disp_NFAs;
