with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Back_End;
with Ortho_Nodes; use Ortho_Nodes;

package Trans_Foreign_Jit is
   procedure Init;

   procedure Foreign_Hook (Decl : Iir;
                           Info : Vhdl.Back_End.Foreign_Info_Type;
                           Ortho : O_Dnode);
end Trans_Foreign_Jit;
