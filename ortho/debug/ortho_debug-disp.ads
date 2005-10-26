with Interfaces.C_Streams;

package Ortho_Debug.Disp is
   --  Initialize the current context.
   --  Must be called before any use of the DISP_* subprograms.
   procedure Init_Context (File : Interfaces.C_Streams.FILEs);

   --  Disp nodes in a pseudo-language.
   procedure Disp_Ortho (Decls : O_Snode);

private
end Ortho_Debug.Disp;
