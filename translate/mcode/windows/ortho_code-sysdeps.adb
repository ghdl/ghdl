with Ortho_Code.X86.Flags;

package body Ortho_Code.Sysdeps is
   procedure Init is
   begin
      Ortho_Code.X86.Flags.Flag_Alloca_Call := True;
   end Init;
end Ortho_Code.Sysdeps;
