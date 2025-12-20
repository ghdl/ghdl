--  Library interface for the synthesizer.
--  Copyright (C) 2025 Tristan Gingold
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

with Options; use Options;
with Errorout;
with Errorout.Console;
with Libraries;

with Bug;

with Netlists.Errors;

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Std_Package;
with Vhdl.Configuration;
with Vhdl.Errors;

with Elab.Vhdl_Objtypes;
with Elab.Vhdl_Insts;
with Elab.Vhdl_Context; use Elab.Vhdl_Context;
with Elab.Vhdl_Annotations;

with Synth;
with Synth.Vhdl_Foreign;
with Synth.Context; use Synth.Context;
with Synth.Vhdl_Context;

with Synthesis;

with Ghdlsynth; use Ghdlsynth;
with Ghdlmain; use Ghdlmain;

package body Libghdl_Synth is
   procedure Init_For_Ghdl_Synth is
   begin
      Ghdlsynth.Register_Commands;
      Errorout.Console.Install_Handler;
      Options.Initialize;
      Netlists.Errors.Initialize;
      Synth.Vhdl_Foreign.Initialize;
   end Init_For_Ghdl_Synth;

   function Ghdl_Synth
     (Init : Natural; Argc : Natural; Argv : C_String_Array_Acc)
     return Module
   is
      use Vhdl.Configuration;
      use Elab.Vhdl_Objtypes;
      Args : String_Acc_Array (1 .. Argc);
      Res : Base_Instance_Acc;
      Cmd : Command_Synth;
      First_Arg : Natural;
      Config : Node;
      Inst : Synth_Instance_Acc;
   begin
      --  Create arguments list.
      for I in 0 .. Argc - 1 loop
         declare
            Arg : constant Ghdl_C_String := Argv (I);
         begin
            Args (I + 1) := new String'(Arg (1 .. strlen (Arg)));
         end;
      end loop;

      --  Forget any previous errors.
      Errorout.Nbr_Errors := 0;

      --  Decode the options.  This is a little bit convoluted...
      Decode_Command_Options (Cmd, Args, First_Arg);

      --  Do the real work!
      Config := Ghdl_Synth_Configure
        (Init /= 0,
         Cmd.Vendor_Libraries (1 .. Cmd.Nbr_Vendor_Libraries),
         Args (First_Arg .. Args'Last));
      if Config = Null_Iir then
         return No_Module;
      end if;

      Inst := Elab.Vhdl_Insts.Elab_Top_Unit (Get_Library_Unit (Config));

      pragma Assert (Is_Expr_Pool_Empty);

      Res := Synthesis.Synth_Design (Config, Inst, Cmd.Top_Encoding);
      if Res = null then
         return No_Module;
      end if;

      pragma Assert (Is_Expr_Pool_Empty);

      Disp_Design (Cmd, Format_None, Res, Config, Inst);

      --  De-elaborate all packages, so that they could be re-used for
      --  synthesis of a second design.
      --  FIXME: move to vhdl.configure ?
      for I in Design_Units.First .. Design_Units.Last loop
         Set_Elab_Flag (Design_Units.Table (I), False);
      end loop;
      Set_Elab_Flag (Vhdl.Std_Package.Std_Standard_Unit, False);

      pragma Assert (Is_Expr_Pool_Empty);

      Elab.Vhdl_Annotations.Finalize_Annotate;
      Synth.Vhdl_Context.Free_Base_Instance;
      return Res.Top_Module;

   exception
      when Option_Error
        | Errorout.Compilation_Error =>
         return No_Module;
      when E: others =>
         --  Avoid possible issues with exceptions...
         Bug.Disp_Bug_Box (E);
         return No_Module;
   end Ghdl_Synth;

   function Ghdl_Synth_Read (Init : Natural;
                             Argc : Natural;
                             Argv : C_String_Array_Acc;
                             Cb : Ghdl_Synth_Read_Cb;
                             Cb_Arg : Pointer_Generic) return Integer
   is
      use Errorout;
      use Vhdl.Errors;
      Args : String_Acc_Array (1 .. Argc);
      Cmd : Command_Synth;
      First_Arg : Natural;
      File, Design, Unit : Iir;
   begin
      --  Create arguments list.
      for I in 0 .. Argc - 1 loop
         declare
            Arg : constant Ghdl_C_String := Argv (I);
         begin
            Args (I + 1) := new String'(Arg (1 .. strlen (Arg)));
         end;
      end loop;

      --  Forget any previous errors.
      Errorout.Nbr_Errors := 0;

      --  Decode the options.  This is a little bit convoluted...
      Decode_Command_Options (Cmd, Args, First_Arg);

      if Init /= 0 then
         Synth_Compile_Init (First_Arg <= Args'Last);
      end if;

      Mark_Vendor_Libraries
        (Cmd.Vendor_Libraries (1 .. Cmd.Nbr_Vendor_Libraries));
      if Errorout.Nbr_Errors > 0 then
         return -1;
      end if;

      if First_Arg <= Args'Last then
         if not Synth_Load_Files (Args (First_Arg .. Args'Last)) then
            return -1;
         end if;
      end if;

      File := Get_Design_File_Chain (Libraries.Work_Library);
      while File /= Null_Iir loop
         Design := Get_First_Design_Unit (File);
         while Design /= Null_Iir loop
            Unit := Get_Library_Unit (Design);
            if Get_Kind (Unit) = Iir_Kind_Entity_Declaration then
               Report_Msg (Msgid_Note, Option, No_Source_Coord,
                 "import %n", (1 => +Unit));

               Cb.all (Uns32 (Get_Identifier (Unit)), Uns32 (Unit), Cb_Arg);

            end if;
            Design := Get_Chain (Design);
         end loop;
         File := Get_Chain (File);
      end loop;

      return 0;
   end Ghdl_Synth_Read;
end Libghdl_Synth;
