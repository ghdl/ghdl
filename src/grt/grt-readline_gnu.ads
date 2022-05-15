--  Binding to GNU readline
with Grt.Types; use Grt.Types;

package Grt.Readline_Gnu is
   function Readline (Prompt : Ghdl_C_String) return Ghdl_C_String;
   pragma Import (C, Readline);

   procedure Add_History (Line : Ghdl_C_String);
   pragma Import (C, Add_History);

   pragma Linker_Options ("-lreadline");
end Grt.Readline_Gnu;
