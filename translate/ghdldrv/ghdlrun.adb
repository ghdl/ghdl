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
with Interfaces.C;

with Ghdlmain; use Ghdlmain;
with Ghdllocal; use Ghdllocal;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Ada.Unchecked_Conversion;
with Ada.Command_Line;
with Ada.Text_IO;

with Binary_File; use Binary_File;
with Binary_File.Memory;
with Ortho_Mcode; use Ortho_Mcode;
with Ortho_Code.Flags; use Ortho_Code.Flags;
with Binary_File;
with Interfaces;
with System; use System;
with Trans_Decls;
with Ortho_Code.Binary;
with Ortho_Code.Debug;
with Ortho_Code.Abi;
with Types;
with Iirs; use Iirs;
with Flags;
with Back_End;
with Errorout; use Errorout;
with Libraries;
with Canon;
with Trans_Be;
with Translation;
with Std_Names;
with Ieee.Std_Logic_1164;
with Interfaces.C;

with Binary_File.Elf;

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

with Ghdlcomp;
with Foreigns;

package body Ghdlrun is
   Snap_Filename : GNAT.OS_Lib.String_Access := null;

   procedure Foreign_Hook (Decl : Iir;
                           Info : Translation.Foreign_Info_Type;
                           Ortho : O_Dnode);

   procedure Compile_Init (Analyze_Only : Boolean) is
   begin
      Back_End.Sem_Foreign := Trans_Be.Sem_Foreign'Access;

      if Analyze_Only then
         return;
      end if;

      Translation.Foreign_Hook := Foreign_Hook'Access;

      -- Initialize.
      Back_End.Finish_Compilation := Trans_Be.Finish_Compilation'Access;

      --  The design is always analyzed in whole.
      Flags.Flag_Whole_Analyze := True;

      Setup_Libraries (False);
      Libraries.Load_Std_Library;

      Ortho_Mcode.Init;
      Binary_File.Memory.Write_Memory_Init;

      Translation.Initialize;
      Canon.Canon_Flag_Add_Labels := True;
   end Compile_Init;

   procedure Compile_Elab
     (Cmd_Name : String; Args : Argument_List; Opt_Arg : out Natural)
   is
   begin
      Extract_Elab_Unit (Cmd_Name, Args, Opt_Arg);
      if Sec_Name = null then
         Sec_Name := new String'("");
      end if;

      Flags.Flag_Elaborate := True;
      Translation.Chap12.Elaborate (Prim_Name.all, Sec_Name.all, "", True);

      if Errorout.Nbr_Errors > 0 then
         --  This may happen (bad entity for example).
         raise Compilation_Error;
      end if;

      Ortho_Mcode.Finish;
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

   --  Toplevel function, defined by grt.
   Flag_String : String (1 .. 5);
   pragma Export (C, Flag_String, "__ghdl_flag_string");

   procedure Ghdl_Elaborate;
   pragma Export (C, Ghdl_Elaborate, "__ghdl_ELABORATE");

   type Elaborate_Acc is access procedure;
   Elaborate_Proc : Elaborate_Acc := null;

   procedure Ghdl_Elaborate is
   begin
      --Ada.Text_IO.Put_Line (Standard_Error, "ghdl_elaborate");
      Elaborate_Proc.all;
   end Ghdl_Elaborate;

   Std_Standard_Bit_RTI_Ptr : Address := Null_Address;

   Std_Standard_Boolean_RTI_Ptr : Address := Null_Address;

   pragma Export (C, Std_Standard_Bit_RTI_Ptr,
                  "std__standard__bit__RTI_ptr");

   pragma Export (C, Std_Standard_Boolean_RTI_Ptr,
                  "std__standard__boolean__RTI_ptr");

   Ieee_Std_Logic_1164_Resolved_Resolv_Ptr : Address := Null_Address;
   pragma Export (C, Ieee_Std_Logic_1164_Resolved_Resolv_Ptr,
                  "ieee__std_logic_1164__resolved_RESOLV_ptr");

   function Find_Untruncated_Text_Read return O_Dnode
   is
      use Types;
      use Std_Names;
      File, Unit, Lib, Decl : Iir;
   begin
      if Libraries.Std_Library = Null_Iir then
         return O_Dnode_Null;
      end if;
      File := Get_Design_File_Chain (Libraries.Std_Library);
      L1 : loop
         if File = Null_Iir then
            return O_Dnode_Null;
         end if;
         Unit := Get_First_Design_Unit (File);
         while Unit /= Null_Iir loop
            Lib := Get_Library_Unit (Unit);
            if Get_Kind (Lib) = Iir_Kind_Package_Body
              and then Get_Identifier (Lib) = Name_Textio
            then
               exit L1;
            end if;
            Unit := Get_Chain (Unit);
         end loop;
         File := Get_Chain (File);
      end loop L1;

      Decl := Get_Declaration_Chain (Lib);
      while Decl /= Null_Iir loop
         if Get_Kind (Decl) = Iir_Kind_Procedure_Declaration
           and then Get_Identifier (Decl) = Name_Untruncated_Text_Read
         then
            if not Get_Foreign_Flag (Decl) then
               raise Program_Error;
            end if;
            return Translation.Get_Ortho_Decl (Decl);
         end if;
         Decl := Get_Chain (Decl);
      end loop;
      return O_Dnode_Null;
   end Find_Untruncated_Text_Read;

   procedure Def (Decl : O_Dnode; Addr : Address)
   is
      use Ortho_Code.Binary;
   begin
      Binary_File.Memory.Set_Symbol_Address (Get_Decl_Symbol (Decl), Addr);
   end Def;

   function Get_Address (Decl : O_Dnode) return Address
   is
      use Interfaces;
      use Ortho_Code.Binary;

      function Conv is new Ada.Unchecked_Conversion
        (Source => Unsigned_32, Target => Address);
   begin
      return Conv (Get_Symbol_Vaddr (Get_Decl_Symbol (Decl)));
   end Get_Address;

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
               Name : String := Name_Table.Name_Buffer (Info.Subprg_First
                                                        .. Info.Subprg_Last);
            begin
               Res := Foreigns.Find_Foreign (Name);
               if Res /= Null_Address then
                  Def (Ortho, Res);
               else
                  Error_Msg_Sem ("unknown foreign VHPIDIRECT '" & Name & "'",
                                 Decl);
               end if;
            end;
         when Foreign_Intrinsic =>
            null;
         when Foreign_Unknown =>
            null;
      end case;
   end Foreign_Hook;

   procedure Run
   is
      use Binary_File;
      use Interfaces;
      use Ortho_Code.Binary;

      function Conv is new Ada.Unchecked_Conversion
        (Source => Address, Target => Elaborate_Acc);
      Err : Boolean;
      Decl : O_Dnode;
   begin
      if Flag_Verbose then
         Ada.Text_IO.Put_Line ("Linking in memory");
      end if;

      if Ortho_Code.Debug.Flag_Debug_Hli then
         --  Can't generate code in HLI.
         raise Compile_Error;
      end if;

      Ortho_Code.Abi.Link_Intrinsics;

      Def (Trans_Decls.Ghdl_Memcpy,
           Grt.Lib.Ghdl_Memcpy'Address);
      Def (Trans_Decls.Ghdl_Bound_Check_Failed_L0,
           Grt.Lib.Ghdl_Bound_Check_Failed_L0'Address);
      Def (Trans_Decls.Ghdl_Bound_Check_Failed_L1,
           Grt.Lib.Ghdl_Bound_Check_Failed_L1'Address);
      Def (Trans_Decls.Ghdl_Malloc0,
           Grt.Lib.Ghdl_Malloc0'Address);
      Def (Trans_Decls.Ghdl_Assert_Default_Report,
           Grt.Lib.Ghdl_Assert_Default_Report'Address);

      Def (Trans_Decls.Ghdl_Report,
           Grt.Lib.Ghdl_Report'Address);
      Def (Trans_Decls.Ghdl_Assert_Failed,
           Grt.Lib.Ghdl_Assert_Failed'Address);
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
      Def (Trans_Decls.Ghdl_Signal_Driving_Value_B2,
           Grt.Signals.Ghdl_Signal_Driving_Value_B2'Address);
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

      Def (Trans_Decls.Ghdl_Signal_Start_Assign_Null,
           Grt.Signals.Ghdl_Signal_Start_Assign_Null'Address);

      Def (Trans_Decls.Ghdl_Create_Signal_B2,
           Grt.Signals.Ghdl_Create_Signal_B2'Address);
      Def (Trans_Decls.Ghdl_Signal_Init_B2,
           Grt.Signals.Ghdl_Signal_Init_B2'Address);
      Def (Trans_Decls.Ghdl_Signal_Simple_Assign_B2,
           Grt.Signals.Ghdl_Signal_Simple_Assign_B2'Address);
      Def (Trans_Decls.Ghdl_Signal_Start_Assign_B2,
           Grt.Signals.Ghdl_Signal_Start_Assign_B2'Address);
      Def (Trans_Decls.Ghdl_Signal_Next_Assign_B2,
           Grt.Signals.Ghdl_Signal_Next_Assign_B2'Address);
      Def (Trans_Decls.Ghdl_Signal_Associate_B2,
           Grt.Signals.Ghdl_Signal_Associate_B2'Address);

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

      Def (Trans_Decls.Ghdl_Rti_Top_Instance,
           Grt.Rtis.Ghdl_Rti_Top_Instance'Address);
      Def (Trans_Decls.Ghdl_Rti_Top_Ptr,
           Grt.Rtis.Ghdl_Rti_Top_Ptr'Address);

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
      Def (Trans_Decls.Ghdl_Write_Scalar,
           Grt.Files.Ghdl_Write_Scalar'Address);
      Def (Trans_Decls.Ghdl_Read_Scalar,
           Grt.Files.Ghdl_Read_Scalar'Address);

      Def (Trans_Decls.Ghdl_File_Endfile,
           Grt.Files.Ghdl_File_Endfile'Address);

      Def (Trans_Decls.Ghdl_Image_B2,
           Grt.Images.Ghdl_Image_B2'Address);
      Def (Trans_Decls.Ghdl_Image_E8,
           Grt.Images.Ghdl_Image_E8'Address);
      Def (Trans_Decls.Ghdl_Image_E32,
           Grt.Images.Ghdl_Image_E32'Address);
      Def (Trans_Decls.Ghdl_Image_I32,
           Grt.Images.Ghdl_Image_I32'Address);
      Def (Trans_Decls.Ghdl_Image_F64,
           Grt.Images.Ghdl_Image_F64'Address);
      Def (Trans_Decls.Ghdl_Image_P64,
           Grt.Images.Ghdl_Image_P64'Address);
      Def (Trans_Decls.Ghdl_Image_P32,
           Grt.Images.Ghdl_Image_P32'Address);

      Def (Trans_Decls.Ghdl_Value_I32,
           Grt.Values.Ghdl_Value_I32'Address);

      Def (Trans_Decls.Ghdl_Get_Path_Name,
           Grt.Names.Ghdl_Get_Path_Name'Address);
      Def (Trans_Decls.Ghdl_Get_Instance_Name,
           Grt.Names.Ghdl_Get_Instance_Name'Address);

      --  Find untruncated_text_read, if any.
      Decl := Find_Untruncated_Text_Read;
      if Decl /= O_Dnode_Null then
         Def (Decl, Grt.Files.Ghdl_Untruncated_Text_Read'Address);
      end if;

      Binary_File.Memory.Write_Memory_Relocate (Err);
      if Err then
         raise Compile_Error;
      end if;

      Std_Standard_Boolean_RTI_Ptr :=
        Get_Address (Trans_Decls.Std_Standard_Boolean_Rti);
      Std_Standard_Bit_RTI_Ptr :=
        Get_Address (Trans_Decls.Std_Standard_Bit_Rti);
      if Ieee.Std_Logic_1164.Resolved /= Null_Iir then
         Decl := Translation.Get_Resolv_Ortho_Decl
           (Ieee.Std_Logic_1164.Resolved);
         if Decl /= O_Dnode_Null then
            Ieee_Std_Logic_1164_Resolved_Resolv_Ptr := Get_Address (Decl);
         end if;
      end if;

      Flag_String := Flags.Flag_String;

      Elaborate_Proc := Conv (Get_Address (Trans_Decls.Ghdl_Elaborate));

      if Snap_Filename /= null then
         declare
            Fd : File_Descriptor;
         begin
            Fd := Create_File (Snap_Filename.all, Binary);
            if Fd = Invalid_FD then
               Error_Msg_Option ("can't open '" & Snap_Filename.all & "'");
            else
               Binary_File.Elf.Write_Elf (Fd);
               Close (Fd);
            end if;
         end;
      end if;

      --  Free all the memory.
      Ortho_Mcode.Free_All;

      Translation.Finalize;
      Lists.Initialize;
      Str_Table.Initialize;
      Nodes.Initialize;
      Files_Map.Initialize;
      Name_Table.Initialize;
      Binary_File.Finish;

      if Flag_Verbose then
         Ada.Text_IO.Put_Line ("Starting simulation");
      end if;

      Grt.Main.Run;
      --V := Ghdl_Main (1, Gnat_Argv);
   end Run;

   function Decode_Option (Option : String) return Boolean
   is
   begin
      if Option = "-g" then
         Flag_Debug := Debug_Dwarf;
         return True;
      elsif Option'Length > 5 and then Option (1 .. 5) = "--be-" then
         Ortho_Code.Debug.Set_Be_Flag (Option);
         return True;
      elsif Option'Length > 7 and then Option (1 .. 7) = "--snap=" then
         Snap_Filename := new String'(Option (8 .. Option'Last));
         return True;
      else
         return False;
      end if;
   end Decode_Option;

   procedure Disp_Long_Help
   is
      use Ada.Text_IO;
   begin
      Put_Line (" -g             Generate debugging informations");
      Put_Line (" --debug-be=X   Set X internal debugging flags");
      Put_Line (" --snap=FILE    Write memory snapshot to FILE");
   end Disp_Long_Help;


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
                         Decode_Option'Access,
                         Disp_Long_Help'Access);
      Ghdlcomp.Register_Commands;
      Register_Command (new Command_Run_Help);
   end Register_Commands;
end Ghdlrun;
