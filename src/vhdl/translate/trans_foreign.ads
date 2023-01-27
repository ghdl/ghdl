with System; use System;

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Back_End;

package Trans_Foreign is
   procedure Init;

   function Get_Foreign_Address
     (Decl : Iir; Info : Vhdl.Back_End.Foreign_Info_Type) return Address;
end Trans_Foreign;
