--  Although being part of GRT, the readline binding should be independent of
--  it (for easier reuse).

with System; use System;
with Grt.Types; use Grt.Types;

package Grt.Readline is
   function Readline (Prompt : Ghdl_C_String) return Ghdl_C_String;
   function Readline (Prompt : Address) return Ghdl_C_String;
   pragma Import (C, Readline);

   procedure Free (Buf : Ghdl_C_String);
   pragma Import (C, Free);

   procedure Add_History (Line : Ghdl_C_String);
   pragma Import (C, Add_History);

   pragma Linker_Options ("-lreadline");
end Grt.Readline;
