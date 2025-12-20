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

with Name_Table;
with Files_Map;
with Ghdlcomp; use Ghdlcomp;
with Ghdlmain; use Ghdlmain;
with Ghdlverilog;
with Errorout;
with Simple_IO;
with Outputs;
with Libraries;
with Flags;

with Vhdl.Scanner;
with Vhdl.Canon;
with Vhdl.Configuration;
with Vhdl.Utils;

with Netlists; use Netlists;
with Netlists.Dump;
with Netlists.Disp_Vhdl;
with Netlists.Disp_Verilog;
with Netlists.Disp_Dot;
with Netlists.Inference;
with Netlists.Rename;
with Netlists.Errors;

with Elab.Debugger;
with Elab.Vhdl_Errors;
with Elab.Vhdl_Annotations;
with Elab.Vhdl_Insts;

with Synthesis;
with Synth.Disp_Vhdl;
with Synth.Vhdl_Context;
with Synth.Vhdl_Foreign;

package body Ghdlsynth is
   function Decode_Command (Cmd : Command_Synth; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "synth"
        or else Name = "--synth";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Synth) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "synth [FILES... -e] UNIT [ARCH]"
        & ASCII.LF & "  Synthesis from UNIT"
        & ASCII.LF & "  alias: --synth";
   end Get_Short_Help;

   procedure Disp_Long_Help (Cmd : Command_Synth)
   is
      pragma Unreferenced (Cmd);
      procedure P (Str : String) renames Simple_IO.Put_Line;
   begin
      P ("You can directly pass the list of files to synthesize:");
      P ("   --synth [OPTIONS] { [--work=NAME] FILE } -e [UNIT]");
      P (" If UNIT is not present, the top unit is automatically found");
      P (" You can use --work=NAME to change the library between files");
      P ("Or use already analysed files:");
      P ("   --synth [OPTIONS] -e UNIT");
      P ("In addition to analyze options, you can use:");
      P ("  -gNAME=VALUE");
      P ("    Override the generic NAME of the top unit");
      P ("  --vendor-library=NAME");
      P ("    Any unit from library NAME is a black box");
      P ("  --no-formal");
      P ("    Neither synthesize assert nor PSL");
      P ("  --no-assert-cover");
      P ("    Cover PSL assertion activation");
      P ("  --assert-assumes");
      P ("    Treat all PSL asserts like PSL assumes");
      P ("  --assume-asserts");
      P ("    Treat all PSL assumes like PSL asserts");
   end Disp_Long_Help;

   procedure Decode_Option (Cmd : in out Command_Synth;
                            Option : String;
                            Arg : String;
                            Res : out Option_State)
   is
      pragma Assert (Option'First = 1);
   begin
      Res := Option_Ok;

      if Option'Last > 3
        and then Option (2) = 'g'
        and then Is_Generic_Override_Option (Option)
      then
         Res := Decode_Generic_Override_Option (Option);
      elsif Option = "--no-formal" then
         Synth.Flags.Flag_Formal := False;
      elsif Option = "--formal" then
         Synth.Flags.Flag_Formal := True;
      elsif Option = "--latches" then
         Netlists.Inference.Flag_Latches := True;
      elsif Option = "--no-assert-cover" then
         Synth.Flags.Flag_Assert_Cover := False;
      elsif Option = "--assert-cover" then
         Synth.Flags.Flag_Assert_Cover := True;
      elsif Option = "--assert-assumes" then
         Synth.Flags.Flag_Assert_As_Assume := True;
      elsif Option = "--assume-asserts" then
         Synth.Flags.Flag_Assume_As_Assert := True;
      elsif Option = "--top-name=hash" then
         Cmd.Top_Encoding := Name_Hash;
      elsif Option = "--top-name=asis" then
         Cmd.Top_Encoding := Name_Asis;
      elsif Option'Last >= 16 and then Option (1 .. 16) = "--keep-hierarchy"
      then
         if Option'Last = 16
           or else Option (17 .. Option'Last) = "=yes"
         then
            Synth.Flags.Flag_Keep_Hierarchy := True;
         elsif Option (17 .. Option'Last) = "=no" then
            Synth.Flags.Flag_Keep_Hierarchy := False;
         else
            Res := Option_Unknown;
         end if;
      elsif Option'Last > 17
        and then Option (1 .. 17) = "--vendor-library="
      then
         if Cmd.Nbr_Vendor_Libraries >= Cmd.Vendor_Libraries'Last then
            --  FIXME: use a table/vector ?
            Errorout.Error_Msg_Option ("too many vendor libraries");
            Res := Option_Err;
         else
            declare
               Name : String := Option (18 .. Option'Last);
               Err : Boolean;
            begin
               Vhdl.Scanner.Convert_Identifier (Name, Err);
               if Err then
                  Res := Option_Err;
               else
                  Cmd.Nbr_Vendor_Libraries := Cmd.Nbr_Vendor_Libraries + 1;
                  Cmd.Vendor_Libraries (Cmd.Nbr_Vendor_Libraries) :=
                    Name_Table.Get_Identifier (Name);
               end if;
            end;
         end if;
      elsif Option = "--expect-failure" then
         Cmd.Expect_Failure := True;
      elsif Option = "--disp-noinline" then
         Cmd.Disp_Inline := False;
      elsif Option = "--disp-noid" then
         Cmd.Disp_Id := False;
      elsif Option'Length > 3 and then Option (1 .. 3) = "-o=" then
         Cmd.Ofile := new String'(Option (4 .. Option'Last));
      elsif Option'Length > 6 and then Option (1 .. 6) = "--out=" then
         if Option (7 .. Option'Last) = "raw" then
            Cmd.Oformat := Format_Raw;
         elsif Option (7 .. Option'Last) = "dump" then
            Cmd.Oformat := Format_Dump;
         elsif Option (7 .. Option'Last) = "dot" then
            Cmd.Oformat := Format_Dot;
         elsif Option (7 .. Option'Last) = "none" then
            Cmd.Oformat := Format_None;
         elsif Option (7 .. Option'Last) = "vhdl" then
            Cmd.Oformat := Format_Vhdl;
         elsif Option (7 .. Option'Last) = "raw-vhdl" then
            Cmd.Oformat := Format_Raw_Vhdl;
         elsif Option (7 .. Option'Last) = "verilog" then
            Cmd.Oformat := Format_Verilog;
         else
            Res := Option_Unknown;
         end if;
         return;
      elsif Option = "-di" then
         Flag_Debug_Noinference := True;
      elsif Option = "-dc" then
         Flag_Debug_Nocleanup := True;
      elsif Option = "-dm" then
         Flag_Debug_Nomemory1 := True;
         Flag_Debug_Nomemory2 := True;
      elsif Option = "-dm2" then
         --  Reduce muxes, but do not create memories.
         Flag_Debug_Nomemory2 := True;
      elsif Option = "-le" then
         Flag_Debug_Elaborate := True;
      elsif Option = "-de" then
         Flag_Debug_Noexpand := True;
      elsif Option = "-dn" then
         Flag_Debug_Nonull := True;
      elsif Option = "-ds" then
         Flag_Debug_Stats := True;
      elsif Option = "-t" then
         Flag_Trace_Statements := True;
      elsif Option = "-i" then
         Flag_Debug_Init := True;
      elsif Option = "-g" then
         Elab.Debugger.Flag_Debug_Enable := True;
      elsif Option = "-v" then
         if not Synth.Flags.Flag_Verbose then
            Synth.Flags.Flag_Verbose := True;
         else
            Flags.Verbose := True;
         end if;
      elsif Option = "--stats" then
         Cmd.Flag_Stats := True;
      else
         Decode_Option (Command_Lib (Cmd), Option, Arg, Res);
      end if;
   end Decode_Option;

   --  Return the position of "-e", or ARGS'FIRST -1 if none.
   function Find_Dash_E (Args : String_Acc_Array) return Integer is
   begin
      for I in Args'Range loop
         if Args (I).all = "-e" then
            return I;
         end if;
      end loop;
      return Args'First - 1;
   end Find_Dash_E;

   --  Set flags, load libraries.
   procedure Synth_Compile_Init (Load_Work : Boolean) is
   begin
      --  Enable translate_off
      Vhdl.Scanner.Flag_Comment_Keyword := True;
      Vhdl.Scanner.Flag_Pragma_Comment := True;

      Common_Compile_Init (False);
      --  Will elaborate.
      Flags.Flag_Elaborate := True;

      Elab.Vhdl_Errors.Debug_Handler := Elab.Debugger.Debug_Error'Access;

      --  Load content only if there are no files.
      Libraries.Load_Work_Library (Load_Work);

      --  Do not canon concurrent statements.
      Vhdl.Canon.Canon_Flag_Concurrent_Stmts := False;
      Vhdl.Canon.Canon_Flag_Add_Suspend_State := False;

      if Ghdlcomp.Init_Verilog_Options /= null then
         Ghdlcomp.Init_Verilog_Options.all (False);
      end if;
   end Synth_Compile_Init;

   --  Mark vendor libraries.
   procedure Mark_Vendor_Libraries (Vendor_Libraries : Name_Id_Array) is
   begin
      for I in Vendor_Libraries'Range loop
         declare
            Lib : Node;
         begin
            Lib := Libraries.Get_Library
              (Vendor_Libraries (I), Libraries.Command_Line_Location);
            Set_Vendor_Library_Flag (Lib, True);
         end;
      end loop;
   end Mark_Vendor_Libraries;

   function Synth_Load_Files (Args : String_Acc_Array) return Boolean
   is
      use Errorout;
      Has_Vhdl, Has_Verilog : Boolean;
   begin
      Flags.Flag_Elaborate_With_Outdated := True;

      Has_Vhdl := False;
      Has_Verilog := False;

      --  Import files
      for I in Args'Range loop
         declare
            Arg : String renames Args (I).all;
            pragma Assert (Arg'First = 1);
            Id : Name_Id;
         begin
            if Arg'Last > 7 and then Arg (1 .. 7) = "--work=" then
               Id := Libraries.Decode_Work_Option (Arg);
               if Id = Null_Identifier then
                  return False;
               end if;
               Libraries.Work_Library_Name := Id;
               Libraries.Load_Work_Library (True);
            else
               case Files_Map.Find_Language (Arg) is
                  when Language_Vhdl
                    | Language_Psl =>
                     Has_Vhdl := True;
                     Ghdlcomp.Compile_Load_Vhdl_File (Arg);
                  when Language_Verilog =>
                     Has_Verilog := True;
                     Ghdlverilog.Load_Verilog_File (Arg);
                  when others =>
                     Errorout.Report_Msg
                       (Warnid_Library, Option, No_Source_Coord,
                        "unexpected extension for file %i",
                        (1 => +Name_Table.Get_Identifier (Arg)));
               end case;
            end if;
         end;
      end loop;

      if Nbr_Errors > 0 then
         --  No need to configure if there are missing units.
         return False;
      end if;

      if Has_Verilog then
         --   Always export verilog units to find the top unit.
         Ghdlverilog.Export_Verilog_Units;
      end if;

      if Has_Vhdl and Has_Verilog then
         Ghdlverilog.Export_Vhdl_Units;
      end if;

      return True;
   end Synth_Load_Files;

   --  Init, analyze and configure.
   --  Return the top configuration.
   function Ghdl_Synth_Configure (Init : Boolean;
                                  Vendor_Libraries : Name_Id_Array;
                                  Args : String_Acc_Array) return Node
   is
      use Errorout;
      E_Opt : Integer;
      Opt_Arg : Natural;
      Config : Iir;
      Lib_Id : Name_Id;
      Prim_Id : Name_Id;
      Sec_Id : Name_Id;
   begin
      --  If the '-e' switch is present, there is a list of files.
      E_Opt := Find_Dash_E (Args);

      if Init then
         Synth_Compile_Init (E_Opt >= Args'First);
      end if;

      --  Mark vendor libraries.
      Mark_Vendor_Libraries (Vendor_Libraries);

      --  Maybe a vendor library is unknown.
      if Errorout.Nbr_Errors > 0 then
         return Null_Iir;
      end if;

      if E_Opt >= Args'First then
         if not Synth_Load_Files (Args (Args'First .. E_Opt - 1)) then
            return Null_Iir;
         end if;
      end if;

      --  Elaborate
      Extract_Elab_Unit
        ("--synth", True, Args (E_Opt + 1 .. Args'Last), Opt_Arg,
         Lib_Id, Prim_Id, Sec_Id);
      if Prim_Id = Null_Identifier then
         return Null_Iir;
      end if;
      if Opt_Arg <= Args'Last then
         Ghdlmain.Error ("extra options ignored");
         return Null_Iir;
      end if;

      Config := Vhdl.Configuration.Configure (Lib_Id, Prim_Id, Sec_Id);

      if Nbr_Errors > 0 then
         --  No need to configure if there are missing units.
         return Null_Iir;
      end if;

      Vhdl.Configuration.Add_Verification_Units;

      if Foreign_Resolve_Instances /= null then
         Foreign_Resolve_Instances.all;
      end if;

      --  Check (and possibly abandon) if entity can be at the top of the
      --  hierarchy.
      declare
         Config_Unit : constant Iir := Get_Library_Unit (Config);
         Top : Iir;
      begin
         if Get_Kind (Config_Unit) = Iir_Kind_Foreign_Module then
            Top := Config_Unit;
            Vhdl.Configuration.Apply_Generic_Override (Top);
            --  No Check_Entity_Declaration (yet).
         else
            Top := Vhdl.Utils.Get_Entity_From_Configuration (Config);
            Vhdl.Configuration.Apply_Generic_Override (Top);
            Vhdl.Configuration.Check_Entity_Declaration_Top (Top, False);
         end if;
         if Nbr_Errors > 0 then
            return Null_Iir;
         end if;
      end;
      return Config;
   end Ghdl_Synth_Configure;

   procedure Disp_Design (Cmd : Command_Synth;
                          Default : Out_Format;
                          Res : Base_Instance_Acc;
                          Config : Iir;
                          Inst : Synth_Instance_Acc)
   is
      Top : constant Module := Res.Top_Module;
      Format : Out_Format;
      Ent : Iir;
   begin
      Format := Cmd.Oformat;
      if Format = Format_Default then
         Format := Default;
      end if;

      if Format = Format_None then
         return;
      end if;

      if not Outputs.Open_File (Cmd.Ofile) then
         Errorout.Error_Msg_Option ("cannot open '" & Cmd.Ofile.all & "'");
         return;
      end if;

      case Format is
         when Format_Default =>
            raise Internal_Error;
         when Format_None =>
            null;
         when Format_Raw =>
            Netlists.Dump.Flag_Disp_Inline := Cmd.Disp_Inline;
            Netlists.Dump.Flag_Disp_Id := Cmd.Disp_Id;
            Netlists.Dump.Disp_Module (Top);
         when Format_Dump =>
            Netlists.Dump.Flag_Disp_Inline := Cmd.Disp_Inline;
            Netlists.Dump.Dump_Module (Top);
         when Format_Dot =>
            Netlists.Disp_Dot.Disp_Dot_Top_Module (Top);
         when Format_Vhdl =>
            Netlists.Rename.Rename_Module
              (Res.Builder, Top, Language_Vhdl);

            if Get_Kind (Get_Library_Unit (Config)) = Iir_Kind_Foreign_Module
            then
               --  Not a VHDL design.
               Netlists.Disp_Vhdl.Disp_Vhdl (Top);
            else
               Ent := Vhdl.Utils.Get_Entity_From_Configuration (Config);
               Synth.Disp_Vhdl.Disp_Vhdl_Wrapper (Ent, Top, Inst);
            end if;
         when Format_Raw_Vhdl =>
            Netlists.Disp_Vhdl.Disp_Vhdl (Top);
         when Format_Verilog =>
            Netlists.Rename.Rename_Module
              (Res.Builder, Top, Language_Verilog);
            Netlists.Disp_Verilog.Disp_Verilog (Top);
      end case;

      Outputs.Close;
   end Disp_Design;

   procedure Perform_Action (Cmd : in out Command_Synth;
                             Args : String_Acc_Array;
                             Success : out Boolean)
   is
      Res : Base_Instance_Acc;
      Inst : Synth_Instance_Acc;
      Config : Iir;
      Lib_Unit : Iir;
   begin
      Config := Ghdl_Synth_Configure
        (True, Cmd.Vendor_Libraries (1 .. Cmd.Nbr_Vendor_Libraries),
         Args);

      if Config = Null_Iir then
         Success := Cmd.Expect_Failure;
         return;
      end if;

      Lib_Unit := Get_Library_Unit (Config);
      if Get_Kind (Lib_Unit) /= Iir_Kind_Foreign_Module then
         Inst := Elab.Vhdl_Insts.Elab_Top_Unit (Lib_Unit);
      else
         Inst := null;
      end if;

      if Errorout.Nbr_Errors > 0 then
         Res := null;
      else
         Netlists.Errors.Initialize;
         Synth.Vhdl_Foreign.Initialize;
         Res := Synthesis.Synth_Design (Config, Inst, Cmd.Top_Encoding);
      end if;

      if Res = null then
         Success := Cmd.Expect_Failure;
         return;
      elsif Cmd.Expect_Failure then
         Success := False;
         return;
      end if;

      Disp_Design (Cmd, Format_Vhdl, Res, Config, Inst);

      if Cmd.Flag_Stats then
         Netlists.Disp_Stats;
      end if;

      Elab.Vhdl_Annotations.Finalize_Annotate;
      Synth.Vhdl_Context.Free_Base_Instance;

      Success := True;
   end Perform_Action;

   procedure Register_Commands is
   begin
      Ghdlmain.Register_Command (new Command_Synth);
   end Register_Commands;
end Ghdlsynth;
