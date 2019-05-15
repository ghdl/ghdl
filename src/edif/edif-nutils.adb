package body Edif.Nutils is
   procedure Init_Constr (Constr : out Constr_Type) is
   begin
      Constr := (Null_Node, Null_Node);
   end Init_Constr;

   procedure Append_Node (Constr : in out Constr_Type; N : Node) is
   begin
      if Constr.First = Null_Node then
         Constr.First := N;
      else
         Set_Chain (Constr.Last, N);
      end if;
      Constr.Last := N;
   end Append_Node;

   function Get_Constr_Chain (Constr : Constr_Type) return Node is
   begin
      return Constr.First;
   end Get_Constr_Chain;
end Edif.Nutils;
