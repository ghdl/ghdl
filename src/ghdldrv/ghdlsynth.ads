--  GHDL driver for synthesis
--  Copyright (C) 2016 Tristan Gingold
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.

with Types; use Types;
with Options; use Options;

with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Vhdl_Context; use Elab.Vhdl_Context;

with Synth.Flags; use Synth.Flags;
with Synth.Context; use Synth.Context;

with Ghdllocal; use Ghdllocal;

package Ghdlsynth is
   procedure Register_Commands;

   type Foreign_Resolve_Instances_Acc is access procedure;

   Foreign_Resolve_Instances : Foreign_Resolve_Instances_Acc;

   type Out_Format is
     (Format_Default,
      Format_Raw, Format_Dump, Format_Dot,
      Format_Vhdl, Format_Raw_Vhdl,
      Format_Verilog,
      Format_None);

   type Name_Id_Array is array (Natural range <>) of Name_Id;

   --  Command --synth
   type Command_Synth is new Command_Lib with record
      --  Control format of the output.
      Disp_Inline : Boolean := True;
      Disp_Id : Boolean := True;
      Oformat     : Out_Format := Format_Default;
      Ofile       : String_Acc := null;

      Flag_Stats : Boolean := False;

      --  Control name encoding of the top-entity.
      Top_Encoding : Name_Encoding := Name_Asis;

      --  If True, a failure is expected.  For tests.
      Expect_Failure : Boolean := False;

      Nbr_Vendor_Libraries : Natural := 0;
      Vendor_Libraries : Name_Id_Array (1 .. 8) := (others => No_Name_Id);
   end record;

   --  Set flags, load libraries.
   procedure Synth_Compile_Init (Load_Work : Boolean);

   procedure Mark_Vendor_Libraries (Vendor_Libraries : Name_Id_Array);

   --  Load files from ARGS (without semantic analysis).
   function Synth_Load_Files (Args : String_Acc_Array) return Boolean;

   --  Init, analyze and configure.
   --  Return the top configuration.
   function Ghdl_Synth_Configure (Init : Boolean;
                                  Vendor_Libraries : Name_Id_Array;
                                  Args : String_Acc_Array) return Node;

   procedure Disp_Design (Cmd : Command_Synth;
                          Default : Out_Format;
                          Res : Base_Instance_Acc;
                          Config : Iir;
                          Inst : Synth_Instance_Acc);

private
   function Decode_Command (Cmd : Command_Synth; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Synth) return String;
   procedure Disp_Long_Help (Cmd : Command_Synth);
   procedure Decode_Option (Cmd : in out Command_Synth;
                            Option : String;
                            Arg : String;
                            Res : out Option_State);
   procedure Perform_Action (Cmd : in out Command_Synth;
                             Args : String_Acc_Array;
                             Success : out Boolean);

end Ghdlsynth;
