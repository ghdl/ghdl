with System;
with Ortho_Gcc_Front;
with Ada.Command_Line; use Ada.Command_Line;

procedure Ortho_Gcc.Main
is
   gnat_argc : Integer;
   gnat_argv : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);

   function Toplev_Main (Argc : Integer; Argv : System.Address)
                        return Integer;
   pragma Import (C, Toplev_Main);

   Status : Exit_Status;
begin
   Ortho_Gcc_Front.Init;

   --  Note: GCC set signal handlers...
   Status := Exit_Status (Toplev_Main (gnat_argc, gnat_argv));
   Set_Exit_Status (Status);
end Ortho_Gcc.Main;
