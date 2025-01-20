--  Verilog driver for synthesis
--  Copyright (C) 2023 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.

with Types; use Types;
with Name_Table;
with Files_Map;
with Errorout; use Errorout;
with Libraries; use Libraries;
with Std_Names;
with Ghdlcomp;
with Ghdlsynth;

with Vhdl.Types; use Vhdl.Types;
with Vhdl.Nodes;
with Vhdl.Configuration;
with Vhdl.Sem_Lib;

with Verilog.Flags; use Verilog.Flags;
with Verilog.Nodes; use Verilog.Nodes;
with Verilog.Scans; use Verilog.Scans;
with Verilog.Parse;
with Verilog.Sem;
with Verilog.Sem_Types;
with Verilog.Sem_Scopes;
with Verilog.Errors;
with Verilog.Find_Top;
with Verilog.Elaborate;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Vpi;
with Verilog.Sem_Instances;
with Verilog.Vhdl_Export;

with Synthesis;
with Synth.Verilog_Insts;

with Elab.Vhdl_Insts;
with Synth.Vhdl_Insts;

package body Ghdlverilog is
   --  Chain of all verilog compilation unit.
   First_File : Node;
   Last_File : Node;

   --  Verilog front-end specific initialization before options decoding.
   procedure Init_Options_Verilog (Analyze_Only : Boolean)
   is
      pragma Unreferenced (Analyze_Only);
   begin
      Verilog.Errors.Initialize;
      Verilog.Scans.Init_Paths;
      Verilog.Sem_Scopes.Init;
      Verilog.Sem_Types.Create_Basetypes;

      --  For synthesis
      Verilog.Scans.Flag_Pragma_Comment := True;
      Verilog.Sem.Flag_Synthesis := True;

      Verilog.Vpi.Initialize;

      Init_Chain (First_File, Last_File);
   end Init_Options_Verilog;

   --  Load a verilog file.
   procedure Load_Verilog_File (Filename : String)
   is
      Id : Name_Id;
      Sfe : Source_File_Entry;
      Res : Vlg_Node;
   begin
      --  By default, use the same standard for keywords.
      Verilog.Scans.Keywords_Std := Verilog.Flags.Std;

      --  Load the file.
      Id := Name_Table.Get_Identifier (Filename);
      Sfe := Files_Map.Read_Source_File_Normalize (Null_Identifier, Id);
      if Sfe = No_Source_File_Entry then
         Error_Msg_Option ("cannot open %i", (1 => +Id));
         return;
      end if;

      Res := Verilog.Parse.Parse_File (Sfe);

      --  Add to the list of source files.
      Append_Chain (First_File, Last_File, Res);
      Verilog.Elaborate.Units_Chain := First_File;

      --  Analyze the compilation unit.
      --  FIXME: add support for one compilation unit ?
      Verilog.Sem.Sem_Compilation_Unit (Res);
   end Load_Verilog_File;

   --  Create a foreign module for VHDL library for each module.
   procedure Export_Verilog_Units
   is
      use Vhdl.Nodes;
      File : Vlg_Node;
      Design : Vhdl_Node;
      Unit : Vhdl_Node;
      N : Vlg_Node;
      Vhdn : Vhdl_Node;
      Last : Vhdl_Node;
   begin
      File := First_File;
      while File /= Null_Vlg_Node loop
         Design := Create_Iir (Iir_Kind_Design_File);

         declare
            use Files_Map;
            Loc : constant Location_Type := Get_Location (File);
            Sfe : constant Source_File_Entry := Location_To_File (Loc);
         begin
            Set_Location (Design, Loc);
            Set_Design_File_Source (Design, Sfe);
            Set_Design_File_Filename (Design, Get_File_Name (Sfe));
            Set_Design_File_Directory (Design, Get_Directory_Name (Sfe));
         end;

         N := Get_Descriptions (File);
         Last := Null_Vhdl_Node;
         while N /= Null_Vlg_Node loop
            case Get_Kind (N) is
               when N_Module =>
                  Unit := Create_Iir (Iir_Kind_Design_Unit);
                  Set_Location (Unit, Get_Location (N));
                  Set_Design_File (Unit, Design);
                  Set_Identifier (Unit, Get_Identifier (N));
                  Set_Date (Unit, Date_Parsed);
                  Set_Date_State (Unit, Date_Extern);

                  Vhdn := Create_Iir (Iir_Kind_Foreign_Module);
                  Set_Location (Vhdn, Get_Location (N));
                  Set_Library_Unit (Unit, Vhdn);
                  Set_Identifier (Vhdn, Get_Identifier (N));
                  Set_Foreign_Node (Vhdn, Int32 (N));
                  Set_Design_Unit (Vhdn, Unit);

                  if Last = Null_Vhdl_Node then
                     Set_First_Design_Unit (Design, Unit);
                  else
                     Set_Chain (Last, Unit);
                  end if;
                  Last := Unit;

               when others =>
                  null;
            end case;
            N := Get_Chain (N);
         end loop;
         Set_Last_Design_Unit (Design, Last);
         Add_Design_File_Into_Library (Design);

         File := Get_Chain (File);
      end loop;
   end Export_Verilog_Units;

   procedure Export_Vhdl_Units
   is
      use Vhdl.Nodes;
      File, Des_Unit, Unit : Vhdl_Node;
      Cu, Vlgn, Last : Vlg_Node;
   begin
      --  Create a compilation unit containing all foreign modules.
      Cu := Create_Node (N_Compilation_Unit);
      Set_Identifier (Cu, Std_Names.Name_D_Unit);
      Last := Null_Vlg_Node;

      --  Add it to the list of files.
      Append_Chain (First_File, Last_File, Cu);

      File := Get_Design_File_Chain (Work_Library);
      while File /= Null_Vhdl_Node loop
         Des_Unit := Get_First_Design_Unit (File);
         while Des_Unit /= Null_Vhdl_Node loop
            Unit := Get_Library_Unit (Des_Unit);

            --  For each entity, cereate a foreign module.
            if Get_Kind (Unit) = Iir_Kind_Entity_Declaration then
               Vlgn := Create_Node (N_Foreign_Module);
               Set_Location (Vlgn, Get_Location (Unit));
               Set_Identifier (Vlgn, Get_Identifier (Unit));
               Set_Foreign_Node (Vlgn, Int32 (Des_Unit));
               Set_Parent (Vlgn, Cu);

               if Last = Null_Vlg_Node then
                  Set_Descriptions (Cu, Vlgn);
               else
                  Set_Chain (Last, Vlgn);
               end if;
               Last := Vlgn;
            end if;

            Des_Unit := Get_Chain (Des_Unit);
         end loop;
         File := Get_Chain (File);
      end loop;
   end Export_Vhdl_Units;

   procedure Verilog_Resolve_Instances is
   begin
      Verilog.Elaborate.Resolve_Instantiations (First_File);
   end Verilog_Resolve_Instances;

   procedure Complete_Verilog_Foreign_Module (N : Node)
   is
      use Vhdl.Nodes;
      Vn : constant Vhdl_Node := Vhdl_Node (Get_Foreign_Node (N));
      Funit : constant Vhdl_Node := Get_Library_Unit (Vn);
      Fport : Vhdl_Node;
      Port, Last : Vlg_Node;
   begin
      Set_Ansi_Port_Flag (N, True);

      Last := Null_Vlg_Node;
      Fport := Get_Port_Chain (Funit);
      while Fport /= Null_Vhdl_Node loop
         case Get_Mode (Fport) is
            when Iir_In_Mode =>
               Port := Create_Node (N_Input);
            when Iir_Out_Mode
              | Iir_Buffer_Mode =>
               Port := Create_Node (N_Output);
            when Iir_Inout_Mode =>
               Port := Create_Node (N_Inout);
            when others =>
               raise Internal_Error;
         end case;
         Set_Location (Port, Get_Location (Fport));
         Set_Identifier (Port, Get_Identifier (Fport));
         Set_Parent (Port, N);

         if Last = Null_Vlg_Node then
            Set_Ports_Chain (N, Port);
         else
            Set_Chain (Last, Port);
         end if;
         Last := Port;

         Fport := Get_Chain (Fport);
      end loop;
   end Complete_Verilog_Foreign_Module;

   procedure Set_Hooks is
   begin
      Ghdlcomp.Init_Verilog_Options := Init_Options_Verilog'Access;
      Vhdl.Configuration.Mark_Foreign_Module :=
        Verilog.Find_Top.Mark_Module'Access;
      Vhdl.Configuration.Apply_Foreign_Override :=
        Synth.Verilog_Insts.Verilog_Override_Generic'Access;
      Synthesis.Synth_Top_Foreign :=
        Synth.Verilog_Insts.Synth_Top_Module'Access;
      Synthesis.Synth_Initialize_Foreign :=
        Synth.Verilog_Insts.Initialize'Access;
      Elab.Vhdl_Insts.Elab_Foreign_Instance :=
        Synth.Verilog_Insts.Elab_Foreign_Instance'Access;
      Synth.Vhdl_Insts.Synth_Foreign_Module :=
        Synth.Verilog_Insts.Synth_Foreign_Module'Access;
      Vhdl.Sem_Lib.Convert_Foreign_Unit :=
        Verilog.Vhdl_Export.Convert_Unit_To_Vhdl'Access;
      Ghdlsynth.Foreign_Resolve_Instances :=
        Verilog_Resolve_Instances'Access;

      Verilog.Sem_Instances.Complete_Foreign_Module :=
        Complete_Verilog_Foreign_Module'Access;
   end Set_Hooks;

   procedure Register_Commands is
   begin
      null;
   end Register_Commands;

begin
   Set_Hooks;
end Ghdlverilog;
