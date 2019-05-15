with Edif.Nodes; use Edif.Nodes;

package Edif.Nutils is
   type Constr_Type is limited private;

   procedure Init_Constr (Constr : out Constr_Type);
   procedure Append_Node (Constr : in out Constr_Type; N : Node);
   function Get_Constr_Chain (Constr : Constr_Type) return Node;

private
   type Constr_Type is record
      First : Node;
      Last : Node;
   end record;
end Edif.Nutils;
