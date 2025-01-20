with Types; use Types;
with Flags;
with Name_Table;
with Ghdlrun;
with Errorout.Console;
with Verilog.Scans;

package Ghdl_Rust is
   --  Needed for linking.
   Gnat_Version : constant String := "unknown compiler version" & ASCII.NUL;
   pragma Export (C, Gnat_Version, "__gnat_version");

   --  Record to pass array of strings to Ada.
   type String_Acc_Record (Len : Natural) is record
      Str : String_Acc_Array (1 .. Len);
   end record;

   type String_Acc_Record_Acc is access String_Acc_Record;
   pragma Convention (C, String_Acc_Record_Acc);

   function New_String_Acc_Record (Len : Natural) return String_Acc_Record_Acc;
   pragma Export (C, New_String_Acc_Record);

   procedure Set_String_Acc_Record (Rec : String_Acc_Record_Acc;
                                    Pos : Positive;
                                    Str : Thin_String_Ptr;
                                    Len : Natural);
   pragma Export (C, Set_String_Acc_Record);

   procedure Free_String_Acc_Record (Rec : String_Acc_Record_Acc);
   pragma Export (C, Free_String_Acc_Record);

--   function Compile_Elab (Cmd_Str : Thin_String_Ptr;
--                          Cmd_Len : Natural;
--                          Args : String_Acc_Record_Acc) return Integer;
end Ghdl_Rust;
