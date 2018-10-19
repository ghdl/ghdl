--  GHDL driver - JIT commands.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with System; use System;

with Ada.Unchecked_Conversion;
with Ada.Command_Line;
with Ada.Text_IO;

with Interfaces;
with Interfaces.C;

with Ghdlmain; use Ghdlmain;
with Ghdllocal; use Ghdllocal;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Ortho_Jit;
with Ortho_Nodes; use Ortho_Nodes;
with Trans_Decls;
with Iirs; use Iirs;
with Std_Package;
with Flags;
with Errorout; use Errorout;
with Libraries;
with Canon;
with Configuration;
with Trans_Be;
with Translation;
with Ieee.Std_Logic_1164;

with Lists;
with Str_Table;
with Nodes;
with Files_Map;
with Name_Table;

with Grt.Main;
with Grt.Modules;
with Grt.Lib;
with Grt.Processes;
with Grt.Rtis;
with Grt.Files;
with Grt.Signals;
with Grt.Options;
with Grt.Types;
with Grt.Images;
with Grt.Values;
with Grt.Names;
with Grt.Std_Logic_1164;
with Grt.Errors;
with Grt.Backtraces.Jit;

with Ghdlcomp; use Ghdlcomp;
with Foreigns;
with Grtlink;

package body Ghdlrun is
   --  Elaboration mode.
   type Elab_Mode_Type is
     (--  Static elaboration (or pre-elaboration).
      Elab_Static,

      --  Dynamic elaboration: design is elaborated just before being run.
      Elab_Dynamic);

   --  Default elaboration mode is dynamic.
   Elab_Mode : constant Elab_Mode_Type := Elab_Dynamic;

   procedure Foreign_Hook (Decl : Iir;
                           Info : Translation.Foreign_Info_Type;
                           Ortho : O_Dnode);

   procedure Compile_Init (Analyze_Only : Boolean) is
   begin
      if Analyze_Only then
         Setup_Libraries (True);
      else
         Setup_Libraries (False);
         Libraries.Load_Std_Library;
         --  WORK library is not loaded.  FIXME: why ?
      end if;

      if Time_Resolution /= 'a' then
         Std_Package.Set_Time_Resolution (Time_Resolution);
      end if;

      if Analyze_Only then
         return;
      end if;

      Translation.Foreign_Hook := Foreign_Hook'Access;

      --  FIXME: add a flag to force unnesting.
      --  Translation.Flag_Unnest_Subprograms := True;

      --  The design is always analyzed in whole.
      Flags.Flag_Whole_Analyze := True;

      case Elab_Mode is
         when Elab_Static =>
            Canon.Canon_Flag_Add_Labels := True;
            Canon.Canon_Flag_Sequentials_Stmts := True;
            Canon.Canon_Flag_Expressions := True;
            Canon.Canon_Flag_All_Sensitivity := True;
         when Elab_Dynamic =>
            Canon.Canon_Flag_Add_Labels := True;
      end case;
   end Compile_Init;

   procedure Compile_Elab
     (Cmd_Name : String; Args : Argument_List; Opt_Arg : out Natural)
   is
      Config : Iir;
   begin
      Extract_Elab_Unit (Cmd_Name, Args, Opt_Arg);
      if Sec_Name = null then
         Sec_Name := new String'("");
      end if;

      Flags.Flag_Elaborate := True;

      Config := Configuration.Configure (Prim_Name.all, Sec_Name.all);
      if Config = Null_Iir then
         raise Compilation_Error;
      end if;

      if Time_Resolution = 'a' then
         Time_Resolution := Std_Package.Get_Minimal_Time_Resolution;
         if Time_Resolution = '?' then
            Time_Resolution := 'f';
         end if;
         if Flag_Verbose then
            declare
               use Ada.Text_IO;
            begin
               Put ("Time resolution is 1 ");
               case Time_Resolution is
                  when 'f' => Put ("fs");
                  when 'p' => Put ("ps");
                  when 'n' => Put ("ns");
                  when 'u' => Put ("us");
                  when 'm' => Put ("ms");
                  when 's' => Put ("sec");
                  when others => Put ("??");
               end case;
               New_Line;
            end;
         end if;
      end if;
      Std_Package.Set_Time_Resolution (Time_Resolution);

      --  Overwrite time resolution in flag string.
      Flags.Flag_String (5) := Time_Resolution;

      Ortho_Jit.Init;

      Translation.Initialize;

      case Elab_Mode is
         when Elab_Static =>
            raise Program_Error;
         when Elab_Dynamic =>
            Translation.Elaborate (Config, "", True);
      end case;

      if Errorout.Nbr_Errors > 0 then
         --  This may happen (bad entity for example).
         raise Compilation_Error;
      end if;
   end Compile_Elab;

   --  Set options.
   --  This is a little bit over-kill: from C to Ada and then again to C...
   procedure Set_Run_Options (Args : Argument_List)
   is
      use Interfaces.C;
      use Grt.Options;
      use Grt.Types;

      function Malloc (Size : size_t) return Argv_Type;
      pragma Import (C, Malloc);

      function Strdup (Str : String) return Ghdl_C_String;
      pragma Import (C, Strdup);
--        is
--           T : Grt.Types.String_Access;
--        begin
--           T := new String'(Str & Ghdllocal.Nul);
--           return To_Ghdl_C_String (T.all'Address);
--        end Strdup;
   begin
      Argc := 1 + Args'Length;
      Argv := Malloc
        (size_t (Argc * (Ghdl_C_String'Size / System.Storage_Unit)));
      Argv (0) := Strdup (Ada.Command_Line.Command_Name & Ghdllocal.Nul);
      Progname := Argv (0);
      for I in Args'Range loop
         Argv (1 + I - Args'First) := Strdup (Args (I).all & Ghdllocal.Nul);
      end loop;
   end Set_Run_Options;

   procedure Ghdl_Elaborate;
   pragma Export (C, Ghdl_Elaborate, "__ghdl_ELABORATE");

   type Elaborate_Acc is access procedure;
   pragma Convention (C, Elaborate_Acc);
   Elaborate_Proc : Elaborate_Acc := null;

   procedure Ghdl_Elaborate is
   begin
      --Ada.Text_IO.Put_Line (Standard_Error, "ghdl_elaborate");
      Elaborate_Proc.all;
   end Ghdl_Elaborate;

   procedure Def (Decl : O_Dnode; Addr : Address)
     renames Ortho_Jit.Set_Address;

   procedure Foreign_Hook (Decl : Iir;
                           Info : Translation.Foreign_Info_Type;
                           Ortho : O_Dnode)
   is
      use Translation;
      Res : Address;
   begin
      case Info.Kind is
         when Foreign_Vhpidirect =>
            declare
               Name : constant String :=
                 Info.Subprg_Name (1 .. Info.Subprg_Len);
            begin
               Res := Foreigns.Find_Foreign (Name);
               if Res /= Null_Address then
                  Def (Ortho, Res);
               else
                  Error_Msg_Sem
                    (+Decl, "unknown foreign VHPIDIRECT '" & Name & "'");
               end if;
            end;
         when Foreign_Intrinsic =>

            declare
               Name : constant String :=
                 Name_Table.Image (Get_Identifier (Decl));
            begin
               if Name = "untruncated_text_read" then
                  Def (Ortho, Grt.Files.Ghdl_Untruncated_Text_Read'Address);
               elsif Name = "textio_read_real" then
                  Def (Ortho, Grt.Lib.Textio_Read_Real'Address);
               elsif Name = "textio_write_real" then
                  Def (Ortho, Grt.Lib.Textio_Write_Real'Address);
               elsif Name = "control_simulation" then
                  Def (Ortho, Grt.Lib.Ghdl_Control_Simulation'Address);
               elsif Name = "get_resolution_limit" then
                  Def (Ortho, Grt.Lib.Ghdl_Get_Resolution_Limit'Address);
               else
                  Error_Msg_Sem
                    (+Decl, "unknown foreign intrinsic %i", +Decl);
               end if;
            end;
         when Foreign_Unknown =>
            null;
      end case;
   end Foreign_Hook;

   procedure Run
   is
      function Conv is new Ada.Unchecked_Conversion
        (Source => Address, Target => Elaborate_Acc);
      Err : Boolean;
      Decl : O_Dnode;
   begin
      if Flag_Verbose then
         Ada.Text_IO.Put_Line ("Linking in memory");
      end if;

      Def (Trans_Decls.Ghdl_Memcpy,
           Grt.Lib.Ghdl_Memcpy'Address);
      Def (Trans_Decls.Ghdl_Bound_Check_Failed,
           Grt.Lib.Ghdl_Bound_Check_Failed'Address);
      Def (Trans_Decls.Ghdl_Direction_Check_Failed,
           Grt.Lib.Ghdl_Direction_Check_Failed'Address);
      Def (Trans_Decls.Ghdl_Malloc0,
           Grt.Lib.Ghdl_Malloc0'Address);
      Def (Trans_Decls.Ghdl_Std_Ulogic_To_Boolean_Array,
           Grt.Lib.Ghdl_Std_Ulogic_To_Boolean_Array'Address);

      Def (Trans_Decls.Ghdl_Report,
           Grt.Lib.Ghdl_Report'Address);
      Def (Trans_Decls.Ghdl_Assert_Failed,
           Grt.Lib.Ghdl_Assert_Failed'Address);
      Def (Trans_Decls.Ghdl_Ieee_Assert_Failed,
           Grt.Lib.Ghdl_Ieee_Assert_Failed'Address);
      Def (Trans_Decls.Ghdl_Psl_Assert_Failed,
           Grt.Lib.Ghdl_Psl_Assert_Failed'Address);
      Def (Trans_Decls.Ghdl_Psl_Cover,
           Grt.Lib.Ghdl_Psl_Cover'Address);
      Def (Trans_Decls.Ghdl_Psl_Cover_Failed,
           Grt.Lib.Ghdl_Psl_Cover_Failed'Address);
      Def (Trans_Decls.Ghdl_Program_Error,
           Grt.Lib.Ghdl_Program_Error'Address);
      Def (Trans_Decls.Ghdl_Malloc,
           Grt.Lib.Ghdl_Malloc'Address);
      Def (Trans_Decls.Ghdl_Deallocate,
           Grt.Lib.Ghdl_Deallocate'Address);
      Def (Trans_Decls.Ghdl_Real_Exp,
           Grt.Lib.Ghdl_Real_Exp'Address);
      Def (Trans_Decls.Ghdl_Integer_Exp,
           Grt.Lib.Ghdl_Integer_Exp'Address);

      Def (Trans_Decls.Ghdl_Sensitized_Process_Register,
           Grt.Processes.Ghdl_Sensitized_Process_Register'Address);
      Def (Trans_Decls.Ghdl_Process_Register,
           Grt.Processes.Ghdl_Process_Register'Address);
      Def (Trans_Decls.Ghdl_Postponed_Sensitized_Process_Register,
           Grt.Processes.Ghdl_Postponed_Sensitized_Process_Register'Address);
      Def (Trans_Decls.Ghdl_Postponed_Process_Register,
           Grt.Processes.Ghdl_Postponed_Process_Register'Address);
      Def (Trans_Decls.Ghdl_Finalize_Register,
           Grt.Processes.Ghdl_Finalize_Register'Address);

      Def (Trans_Decls.Ghdl_Stack2_Allocate,
           Grt.Processes.Ghdl_Stack2_Allocate'Address);
      Def (Trans_Decls.Ghdl_Stack2_Mark,
           Grt.Processes.Ghdl_Stack2_Mark'Address);
      Def (Trans_Decls.Ghdl_Stack2_Release,
           Grt.Processes.Ghdl_Stack2_Release'Address);
      Def (Trans_Decls.Ghdl_Process_Wait_Exit,
           Grt.Processes.Ghdl_Process_Wait_Exit'Address);
      Def (Trans_Decls.Ghdl_Process_Wait_Suspend,
           Grt.Processes.Ghdl_Process_Wait_Suspend'Address);
      Def (Trans_Decls.Ghdl_Process_Wait_Timed_Out,
           Grt.Processes.Ghdl_Process_Wait_Timed_Out'Address);
      Def (Trans_Decls.Ghdl_Process_Wait_Timeout,
           Grt.Processes.Ghdl_Process_Wait_Timeout'Address);
      Def (Trans_Decls.Ghdl_Process_Wait_Set_Timeout,
           Grt.Processes.Ghdl_Process_Wait_Set_Timeout'Address);
      Def (Trans_Decls.Ghdl_Process_Wait_Add_Sensitivity,
           Grt.Processes.Ghdl_Process_Wait_Add_Sensitivity'Address);
      Def (Trans_Decls.Ghdl_Process_Wait_Close,
           Grt.Processes.Ghdl_Process_Wait_Close'Address);

      Def (Trans_Decls.Ghdl_Process_Add_Sensitivity,
           Grt.Processes.Ghdl_Process_Add_Sensitivity'Address);

      Def (Trans_Decls.Ghdl_Now,
           Grt.Types.Current_Time'Address);

      Def (Trans_Decls.Ghdl_Process_Add_Driver,
           Grt.Signals.Ghdl_Process_Add_Driver'Address);
      Def (Trans_Decls.Ghdl_Signal_Add_Direct_Driver,
           Grt.Signals.Ghdl_Signal_Add_Direct_Driver'Address);

      Def (Trans_Decls.Ghdl_Signal_Add_Source,
           Grt.Signals.Ghdl_Signal_Add_Source'Address);
      Def (Trans_Decls.Ghdl_Signal_In_Conversion,
           Grt.Signals.Ghdl_Signal_In_Conversion'Address);
      Def (Trans_Decls.Ghdl_Signal_Out_Conversion,
           Grt.Signals.Ghdl_Signal_Out_Conversion'Address);
      Def (Trans_Decls.Ghdl_Signal_Effective_Value,
           Grt.Signals.Ghdl_Signal_Effective_Value'Address);
      Def (Trans_Decls.Ghdl_Signal_Create_Resolution,
           Grt.Signals.Ghdl_Signal_Create_Resolution'Address);

      Def (Trans_Decls.Ghdl_Signal_Disconnect,
           Grt.Signals.Ghdl_Signal_Disconnect'Address);
      Def (Trans_Decls.Ghdl_Signal_Set_Disconnect,
           Grt.Signals.Ghdl_Signal_Set_Disconnect'Address);
      Def (Trans_Decls.Ghdl_Signal_Merge_Rti,
           Grt.Signals.Ghdl_Signal_Merge_Rti'Address);
      Def (Trans_Decls.Ghdl_Signal_Name_Rti,
           Grt.Signals.Ghdl_Signal_Name_Rti'Address);
      Def (Trans_Decls.Ghdl_Signal_Read_Port,
           Grt.Signals.Ghdl_Signal_Read_Port'Address);
      Def (Trans_Decls.Ghdl_Signal_Read_Driver,
           Grt.Signals.Ghdl_Signal_Read_Driver'Address);

      Def (Trans_Decls.Ghdl_Signal_Driving,
           Grt.Signals.Ghdl_Signal_Driving'Address);
      Def (Trans_Decls.Ghdl_Signal_Driving_Value_B1,
           Grt.Signals.Ghdl_Signal_Driving_Value_B1'Address);
      Def (Trans_Decls.Ghdl_Signal_Driving_Value_E8,
           Grt.Signals.Ghdl_Signal_Driving_Value_E8'Address);
      Def (Trans_Decls.Ghdl_Signal_Driving_Value_E32,
           Grt.Signals.Ghdl_Signal_Driving_Value_E32'Address);
      Def (Trans_Decls.Ghdl_Signal_Driving_Value_I32,
           Grt.Signals.Ghdl_Signal_Driving_Value_I32'Address);
      Def (Trans_Decls.Ghdl_Signal_Driving_Value_I64,
           Grt.Signals.Ghdl_Signal_Driving_Value_I64'Address);
      Def (Trans_Decls.Ghdl_Signal_Driving_Value_F64,
           Grt.Signals.Ghdl_Signal_Driving_Value_F64'Address);

      Def (Trans_Decls.Ghdl_Signal_Create_Guard,
           Grt.Signals.Ghdl_Signal_Create_Guard'Address);
      Def (Trans_Decls.Ghdl_Signal_Guard_Dependence,
           Grt.Signals.Ghdl_Signal_Guard_Dependence'Address);

      Def (Trans_Decls.Ghdl_Signal_Simple_Assign_Error,
           Grt.Signals.Ghdl_Signal_Simple_Assign_Error'Address);
      Def (Trans_Decls.Ghdl_Signal_Start_Assign_Error,
           Grt.Signals.Ghdl_Signal_Start_Assign_Error'Address);
      Def (Trans_Decls.Ghdl_Signal_Next_Assign_Error,
           Grt.Signals.Ghdl_Signal_Next_Assign_Error'Address);

      Def (Trans_Decls.Ghdl_Signal_Start_Assign_Null,
           Grt.Signals.Ghdl_Signal_Start_Assign_Null'Address);

      Def (Trans_Decls.Ghdl_Signal_Direct_Assign,
           Grt.Signals.Ghdl_Signal_Direct_Assign'Address);

      Def (Trans_Decls.Ghdl_Create_Signal_B1,
           Grt.Signals.Ghdl_Create_Signal_B1'Address);
      Def (Trans_Decls.Ghdl_Signal_Init_B1,
           Grt.Signals.Ghdl_Signal_Init_B1'Address);
      Def (Trans_Decls.Ghdl_Signal_Simple_Assign_B1,
           Grt.Signals.Ghdl_Signal_Simple_Assign_B1'Address);
      Def (Trans_Decls.Ghdl_Signal_Start_Assign_B1,
           Grt.Signals.Ghdl_Signal_Start_Assign_B1'Address);
      Def (Trans_Decls.Ghdl_Signal_Next_Assign_B1,
           Grt.Signals.Ghdl_Signal_Next_Assign_B1'Address);
      Def (Trans_Decls.Ghdl_Signal_Associate_B1,
           Grt.Signals.Ghdl_Signal_Associate_B1'Address);
      Def (Trans_Decls.Ghdl_Signal_Add_Port_Driver_B1,
           Grt.Signals.Ghdl_Signal_Add_Port_Driver_B1'Address);

      Def (Trans_Decls.Ghdl_Create_Signal_E8,
           Grt.Signals.Ghdl_Create_Signal_E8'Address);
      Def (Trans_Decls.Ghdl_Signal_Init_E8,
           Grt.Signals.Ghdl_Signal_Init_E8'Address);
      Def (Trans_Decls.Ghdl_Signal_Simple_Assign_E8,
           Grt.Signals.Ghdl_Signal_Simple_Assign_E8'Address);
      Def (Trans_Decls.Ghdl_Signal_Start_Assign_E8,
           Grt.Signals.Ghdl_Signal_Start_Assign_E8'Address);
      Def (Trans_Decls.Ghdl_Signal_Next_Assign_E8,
           Grt.Signals.Ghdl_Signal_Next_Assign_E8'Address);
      Def (Trans_Decls.Ghdl_Signal_Associate_E8,
           Grt.Signals.Ghdl_Signal_Associate_E8'Address);
      Def (Trans_Decls.Ghdl_Signal_Add_Port_Driver_E8,
           Grt.Signals.Ghdl_Signal_Add_Port_Driver_E8'Address);

      Def (Trans_Decls.Ghdl_Create_Signal_E32,
           Grt.Signals.Ghdl_Create_Signal_E32'Address);
      Def (Trans_Decls.Ghdl_Signal_Init_E32,
           Grt.Signals.Ghdl_Signal_Init_E32'Address);
      Def (Trans_Decls.Ghdl_Signal_Simple_Assign_E32,
           Grt.Signals.Ghdl_Signal_Simple_Assign_E32'Address);
      Def (Trans_Decls.Ghdl_Signal_Start_Assign_E32,
           Grt.Signals.Ghdl_Signal_Start_Assign_E32'Address);
      Def (Trans_Decls.Ghdl_Signal_Next_Assign_E32,
           Grt.Signals.Ghdl_Signal_Next_Assign_E32'Address);
      Def (Trans_Decls.Ghdl_Signal_Associate_E32,
           Grt.Signals.Ghdl_Signal_Associate_E32'Address);
      Def (Trans_Decls.Ghdl_Signal_Add_Port_Driver_E32,
           Grt.Signals.Ghdl_Signal_Add_Port_Driver_E32'Address);

      Def (Trans_Decls.Ghdl_Create_Signal_I32,
           Grt.Signals.Ghdl_Create_Signal_I32'Address);
      Def (Trans_Decls.Ghdl_Signal_Init_I32,
           Grt.Signals.Ghdl_Signal_Init_I32'Address);
      Def (Trans_Decls.Ghdl_Signal_Simple_Assign_I32,
           Grt.Signals.Ghdl_Signal_Simple_Assign_I32'Address);
      Def (Trans_Decls.Ghdl_Signal_Start_Assign_I32,
           Grt.Signals.Ghdl_Signal_Start_Assign_I32'Address);
      Def (Trans_Decls.Ghdl_Signal_Next_Assign_I32,
           Grt.Signals.Ghdl_Signal_Next_Assign_I32'Address);
      Def (Trans_Decls.Ghdl_Signal_Associate_I32,
           Grt.Signals.Ghdl_Signal_Associate_I32'Address);
      Def (Trans_Decls.Ghdl_Signal_Add_Port_Driver_I32,
           Grt.Signals.Ghdl_Signal_Add_Port_Driver_I32'Address);

      Def (Trans_Decls.Ghdl_Create_Signal_I64,
           Grt.Signals.Ghdl_Create_Signal_I64'Address);
      Def (Trans_Decls.Ghdl_Signal_Init_I64,
           Grt.Signals.Ghdl_Signal_Init_I64'Address);
      Def (Trans_Decls.Ghdl_Signal_Simple_Assign_I64,
           Grt.Signals.Ghdl_Signal_Simple_Assign_I64'Address);
      Def (Trans_Decls.Ghdl_Signal_Start_Assign_I64,
           Grt.Signals.Ghdl_Signal_Start_Assign_I64'Address);
      Def (Trans_Decls.Ghdl_Signal_Next_Assign_I64,
           Grt.Signals.Ghdl_Signal_Next_Assign_I64'Address);
      Def (Trans_Decls.Ghdl_Signal_Associate_I64,
           Grt.Signals.Ghdl_Signal_Associate_I64'Address);
      Def (Trans_Decls.Ghdl_Signal_Add_Port_Driver_I64,
           Grt.Signals.Ghdl_Signal_Add_Port_Driver_I64'Address);

      Def (Trans_Decls.Ghdl_Create_Signal_F64,
           Grt.Signals.Ghdl_Create_Signal_F64'Address);
      Def (Trans_Decls.Ghdl_Signal_Init_F64,
           Grt.Signals.Ghdl_Signal_Init_F64'Address);
      Def (Trans_Decls.Ghdl_Signal_Simple_Assign_F64,
           Grt.Signals.Ghdl_Signal_Simple_Assign_F64'Address);
      Def (Trans_Decls.Ghdl_Signal_Start_Assign_F64,
           Grt.Signals.Ghdl_Signal_Start_Assign_F64'Address);
      Def (Trans_Decls.Ghdl_Signal_Next_Assign_F64,
           Grt.Signals.Ghdl_Signal_Next_Assign_F64'Address);
      Def (Trans_Decls.Ghdl_Signal_Associate_F64,
           Grt.Signals.Ghdl_Signal_Associate_F64'Address);
      Def (Trans_Decls.Ghdl_Signal_Add_Port_Driver_F64,
           Grt.Signals.Ghdl_Signal_Add_Port_Driver_F64'Address);

      Def (Trans_Decls.Ghdl_Signal_Attribute_Register_Prefix,
           Grt.Signals.Ghdl_Signal_Attribute_Register_Prefix'Address);
      Def (Trans_Decls.Ghdl_Create_Stable_Signal,
           Grt.Signals.Ghdl_Create_Stable_Signal'Address);
      Def (Trans_Decls.Ghdl_Create_Quiet_Signal,
           Grt.Signals.Ghdl_Create_Quiet_Signal'Address);
      Def (Trans_Decls.Ghdl_Create_Transaction_Signal,
           Grt.Signals.Ghdl_Create_Transaction_Signal'Address);
      Def (Trans_Decls.Ghdl_Create_Delayed_Signal,
           Grt.Signals.Ghdl_Create_Delayed_Signal'Address);

      Def (Trans_Decls.Ghdl_Rti_Add_Package,
           Grt.Rtis.Ghdl_Rti_Add_Package'Address);
      Def (Trans_Decls.Ghdl_Rti_Add_Top,
           Grt.Rtis.Ghdl_Rti_Add_Top'Address);

      Def (Trans_Decls.Ghdl_Init_Top_Generics,
           Grt.Main.Ghdl_Init_Top_Generics'Address);

      Def (Trans_Decls.Ghdl_Protected_Enter,
           Grt.Processes.Ghdl_Protected_Enter'Address);
      Def (Trans_Decls.Ghdl_Protected_Leave,
           Grt.Processes.Ghdl_Protected_Leave'Address);
      Def (Trans_Decls.Ghdl_Protected_Init,
           Grt.Processes.Ghdl_Protected_Init'Address);
      Def (Trans_Decls.Ghdl_Protected_Fini,
           Grt.Processes.Ghdl_Protected_Fini'Address);

      Def (Trans_Decls.Ghdl_Text_File_Elaborate,
           Grt.Files.Ghdl_Text_File_Elaborate'Address);
      Def (Trans_Decls.Ghdl_Text_File_Finalize,
           Grt.Files.Ghdl_Text_File_Finalize'Address);
      Def (Trans_Decls.Ghdl_Text_File_Open,
           Grt.Files.Ghdl_Text_File_Open'Address);
      Def (Trans_Decls.Ghdl_Text_File_Open_Status,
           Grt.Files.Ghdl_Text_File_Open_Status'Address);
      Def (Trans_Decls.Ghdl_Text_Write,
           Grt.Files.Ghdl_Text_Write'Address);
      Def (Trans_Decls.Ghdl_Text_Read_Length,
           Grt.Files.Ghdl_Text_Read_Length'Address);
      Def (Trans_Decls.Ghdl_Text_File_Close,
           Grt.Files.Ghdl_Text_File_Close'Address);

      Def (Trans_Decls.Ghdl_File_Elaborate,
           Grt.Files.Ghdl_File_Elaborate'Address);
      Def (Trans_Decls.Ghdl_File_Finalize,
           Grt.Files.Ghdl_File_Finalize'Address);
      Def (Trans_Decls.Ghdl_File_Open,
           Grt.Files.Ghdl_File_Open'Address);
      Def (Trans_Decls.Ghdl_File_Open_Status,
           Grt.Files.Ghdl_File_Open_Status'Address);
      Def (Trans_Decls.Ghdl_File_Close,
           Grt.Files.Ghdl_File_Close'Address);
      Def (Trans_Decls.Ghdl_File_Flush,
           Grt.Files.Ghdl_File_Flush'Address);
      Def (Trans_Decls.Ghdl_Write_Scalar,
           Grt.Files.Ghdl_Write_Scalar'Address);
      Def (Trans_Decls.Ghdl_Read_Scalar,
           Grt.Files.Ghdl_Read_Scalar'Address);

      Def (Trans_Decls.Ghdl_File_Endfile,
           Grt.Files.Ghdl_File_Endfile'Address);

      Def (Trans_Decls.Ghdl_Image_B1,
           Grt.Images.Ghdl_Image_B1'Address);
      Def (Trans_Decls.Ghdl_Image_E8,
           Grt.Images.Ghdl_Image_E8'Address);
      Def (Trans_Decls.Ghdl_Image_E32,
           Grt.Images.Ghdl_Image_E32'Address);
      Def (Trans_Decls.Ghdl_Image_I32,
           Grt.Images.Ghdl_Image_I32'Address);
      Def (Trans_Decls.Ghdl_Image_I64,
           Grt.Images.Ghdl_Image_I64'Address);
      Def (Trans_Decls.Ghdl_Image_F64,
           Grt.Images.Ghdl_Image_F64'Address);
      Def (Trans_Decls.Ghdl_Image_P64,
           Grt.Images.Ghdl_Image_P64'Address);
      Def (Trans_Decls.Ghdl_Image_P32,
           Grt.Images.Ghdl_Image_P32'Address);

      Def (Trans_Decls.Ghdl_Value_B1,
           Grt.Values.Ghdl_Value_B1'Address);
      Def (Trans_Decls.Ghdl_Value_E8,
           Grt.Values.Ghdl_Value_E8'Address);
      Def (Trans_Decls.Ghdl_Value_E32,
           Grt.Values.Ghdl_Value_E32'Address);
      Def (Trans_Decls.Ghdl_Value_I32,
           Grt.Values.Ghdl_Value_I32'Address);
      Def (Trans_Decls.Ghdl_Value_I64,
           Grt.Values.Ghdl_Value_I64'Address);
      Def (Trans_Decls.Ghdl_Value_F64,
           Grt.Values.Ghdl_Value_F64'Address);
      Def (Trans_Decls.Ghdl_Value_P32,
           Grt.Values.Ghdl_Value_P32'Address);
      Def (Trans_Decls.Ghdl_Value_P64,
           Grt.Values.Ghdl_Value_P64'Address);

      Def (Trans_Decls.Ghdl_Get_Path_Name,
           Grt.Names.Ghdl_Get_Path_Name'Address);
      Def (Trans_Decls.Ghdl_Get_Instance_Name,
           Grt.Names.Ghdl_Get_Instance_Name'Address);

      Def (Trans_Decls.Ghdl_Std_Ulogic_Match_Eq,
           Grt.Std_Logic_1164.Ghdl_Std_Ulogic_Match_Eq'Address);
      Def (Trans_Decls.Ghdl_Std_Ulogic_Match_Ne,
           Grt.Std_Logic_1164.Ghdl_Std_Ulogic_Match_Ne'Address);
      Def (Trans_Decls.Ghdl_Std_Ulogic_Match_Lt,
           Grt.Std_Logic_1164.Ghdl_Std_Ulogic_Match_Lt'Address);
      Def (Trans_Decls.Ghdl_Std_Ulogic_Match_Le,
           Grt.Std_Logic_1164.Ghdl_Std_Ulogic_Match_Le'Address);

      Def (Trans_Decls.Ghdl_Std_Ulogic_Array_Match_Eq,
           Grt.Std_Logic_1164.Ghdl_Std_Ulogic_Array_Match_Eq'Address);
      Def (Trans_Decls.Ghdl_Std_Ulogic_Array_Match_Ne,
           Grt.Std_Logic_1164.Ghdl_Std_Ulogic_Array_Match_Ne'Address);

      Def (Trans_Decls.Ghdl_To_String_I32,
           Grt.Images.Ghdl_To_String_I32'Address);
      Def (Trans_Decls.Ghdl_To_String_I64,
           Grt.Images.Ghdl_To_String_I64'Address);
      Def (Trans_Decls.Ghdl_To_String_F64,
           Grt.Images.Ghdl_To_String_F64'Address);
      Def (Trans_Decls.Ghdl_To_String_F64_Digits,
           Grt.Images.Ghdl_To_String_F64_Digits'Address);
      Def (Trans_Decls.Ghdl_To_String_F64_Format,
           Grt.Images.Ghdl_To_String_F64_Format'Address);
      Def (Trans_Decls.Ghdl_To_String_B1,
           Grt.Images.Ghdl_To_String_B1'Address);
      Def (Trans_Decls.Ghdl_To_String_E8,
           Grt.Images.Ghdl_To_String_E8'Address);
      Def (Trans_Decls.Ghdl_To_String_E32,
           Grt.Images.Ghdl_To_String_E32'Address);
      Def (Trans_Decls.Ghdl_To_String_Char,
           Grt.Images.Ghdl_To_String_Char'Address);
      Def (Trans_Decls.Ghdl_To_String_P32,
           Grt.Images.Ghdl_To_String_P32'Address);
      Def (Trans_Decls.Ghdl_To_String_P64,
           Grt.Images.Ghdl_To_String_P64'Address);
      Def (Trans_Decls.Ghdl_Time_To_String_Unit,
           Grt.Images.Ghdl_Time_To_String_Unit'Address);
      Def (Trans_Decls.Ghdl_BV_To_Ostring,
           Grt.Images.Ghdl_BV_To_Ostring'Address);
      Def (Trans_Decls.Ghdl_BV_To_Hstring,
           Grt.Images.Ghdl_BV_To_Hstring'Address);
      Def (Trans_Decls.Ghdl_Array_Char_To_String_B1,
           Grt.Images.Ghdl_Array_Char_To_String_B1'Address);
      Def (Trans_Decls.Ghdl_Array_Char_To_String_E8,
           Grt.Images.Ghdl_Array_Char_To_String_E8'Address);
      Def (Trans_Decls.Ghdl_Array_Char_To_String_E32,
           Grt.Images.Ghdl_Array_Char_To_String_E32'Address);

      Ortho_Jit.Link (Err);
      if Err then
         raise Compile_Error;
      end if;

      Grtlink.Std_Standard_Boolean_RTI_Ptr :=
        Ortho_Jit.Get_Address (Trans_Decls.Std_Standard_Boolean_Rti);
      Grtlink.Std_Standard_Bit_RTI_Ptr :=
        Ortho_Jit.Get_Address (Trans_Decls.Std_Standard_Bit_Rti);
      if Ieee.Std_Logic_1164.Resolved /= Null_Iir then
         Decl := Translation.Get_Resolv_Ortho_Decl
           (Ieee.Std_Logic_1164.Resolved);
         if Decl /= O_Dnode_Null then
            Grtlink.Ieee_Std_Logic_1164_Resolved_Resolv_Ptr :=
              Ortho_Jit.Get_Address (Decl);
         end if;
      end if;

      Grtlink.Flag_String := Flags.Flag_String;

      Grt.Backtraces.Jit.Symbolizer_Proc := Ortho_Jit.Symbolize'Access;

      Elaborate_Proc :=
        Conv (Ortho_Jit.Get_Address (Trans_Decls.Ghdl_Elaborate));

      Ortho_Jit.Finish;

      Translation.Finalize;
      Lists.Initialize;
      Str_Table.Initialize;
      Nodes.Initialize;
      Files_Map.Initialize;
      Name_Table.Finalize;

      if Flag_Verbose then
         Ada.Text_IO.Put_Line ("Starting simulation");
      end if;

      Grt.Main.Run;

      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Exit_Status (Grt.Errors.Exit_Status));
   end Run;


   --  Command run help.
   type Command_Run_Help is new Command_Type with null record;
   function Decode_Command (Cmd : Command_Run_Help; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Run_Help) return String;
   procedure Perform_Action (Cmd : in out Command_Run_Help;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Run_Help; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--run-help";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Run_Help) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--run-help         Disp help for RUNOPTS options";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Run_Help;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      use Ada.Text_IO;
   begin
      if Args'Length /= 0 then
         Error
           ("warning: command '--run-help' does not accept any argument");
      end if;
      Put_Line ("These options can only be placed at [RUNOPTS]");
      --  Register modules, since they add commands.
      Grt.Modules.Register_Modules;
      --  Bypass usual help header.
      Grt.Options.Argc := 0;
      Grt.Options.Help;
   end Perform_Action;

   procedure Register_Commands
   is
   begin
      Ghdlcomp.Hooks := (Compile_Init'Access,
                         Compile_Elab'Access,
                         Set_Run_Options'Access,
                         Run'Access,
                         Ortho_Jit.Decode_Option'Access,
                         Ortho_Jit.Disp_Help'Access);
      Ghdlcomp.Register_Commands;
      Register_Command (new Command_Run_Help);
      Trans_Be.Register_Translation_Back_End;
   end Register_Commands;
end Ghdlrun;
