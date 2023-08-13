--  Link imported routines in Trans_Decls with GRT.
--  Copyright (C) 2002-2023 Tristan Gingold
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
with System; use System;

with Ghdllocal; use Ghdllocal;
with Simple_IO; use Simple_IO;

with Ortho_Jit;
with Ortho_Nodes; use Ortho_Nodes;
with Trans_Decls;

with Grt.Main;
with Grt.Lib;
with Grt.Processes;
with Grt.Rtis;
with Grt.Files_Lib;
with Grt.Signals;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Images;
with Grt.Values;
with Grt.Names;
with Grt.Std_Logic_1164;

package body Trans_Link is
   procedure Def (Decl : O_Dnode; Addr : Address)
     renames Ortho_Jit.Set_Address;

   procedure Link is
   begin
      if Flag_Verbose then
         Put_Line ("Linking in memory");
      end if;

      Def (Trans_Decls.Ghdl_Memcpy,
           Grt.Lib.Ghdl_Memcpy'Address);
      Def (Trans_Decls.Ghdl_Bound_Check_Failed,
           Grt.Lib.Ghdl_Bound_Check_Failed'Address);
      Def (Trans_Decls.Ghdl_Direction_Check_Failed,
           Grt.Lib.Ghdl_Direction_Check_Failed'Address);
      Def (Trans_Decls.Ghdl_Access_Check_Failed,
           Grt.Lib.Ghdl_Access_Check_Failed'Address);
      Def (Trans_Decls.Ghdl_Integer_Index_Check_Failed,
           Grt.Lib.Ghdl_Integer_Index_Check_Failed'Address);

      Def (Trans_Decls.Ghdl_Malloc,
           Grt.Lib.Ghdl_Malloc'Address);
      Def (Trans_Decls.Ghdl_Malloc0,
           Grt.Lib.Ghdl_Malloc0'Address);
      Def (Trans_Decls.Ghdl_Free_Mem,
           Grt.Lib.Ghdl_Free_Mem'Address);

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
      Def (Trans_Decls.Ghdl_Psl_Assume_Failed,
           Grt.Lib.Ghdl_Psl_Assume_Failed'Address);
      Def (Trans_Decls.Ghdl_Psl_Cover,
           Grt.Lib.Ghdl_Psl_Cover'Address);
      Def (Trans_Decls.Ghdl_Psl_Cover_Failed,
           Grt.Lib.Ghdl_Psl_Cover_Failed'Address);
      Def (Trans_Decls.Ghdl_Program_Error,
           Grt.Lib.Ghdl_Program_Error'Address);

      Def (Trans_Decls.Ghdl_Real_Exp,
           Grt.Lib.Ghdl_Real_Exp'Address);
      Def (Trans_Decls.Ghdl_I32_Exp,
           Grt.Lib.Ghdl_I32_Exp'Address);
      Def (Trans_Decls.Ghdl_I64_Exp,
           Grt.Lib.Ghdl_I64_Exp'Address);
      Def (Trans_Decls.Ghdl_I32_Div,
           Grt.Lib.Ghdl_I32_Div'Address);
      Def (Trans_Decls.Ghdl_I64_Div,
           Grt.Lib.Ghdl_I64_Div'Address);
      Def (Trans_Decls.Ghdl_I32_Mod,
           Grt.Lib.Ghdl_I32_Mod'Address);
      Def (Trans_Decls.Ghdl_I64_Mod,
           Grt.Lib.Ghdl_I64_Mod'Address);
      Def (Trans_Decls.Ghdl_Check_Stack_Allocation,
           Grt.Lib.Ghdl_Check_Stack_Allocation'Address);

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
           Grt.Vhdl_Types.Current_Time'Address);

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
      Def (Trans_Decls.Ghdl_Signal_Set_Mode,
           Grt.Signals.Ghdl_Signal_Set_Mode'Address);
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

      Def (Trans_Decls.Ghdl_Signal_Release_Eff,
           Grt.Signals.Ghdl_Signal_Release_Eff'Address);
      Def (Trans_Decls.Ghdl_Signal_Release_Drv,
           Grt.Signals.Ghdl_Signal_Release_Drv'Address);

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
      Def (Trans_Decls.Ghdl_Signal_Force_Drv_B1,
           Grt.Signals.Ghdl_Signal_Force_Driving_B1'Address);
      Def (Trans_Decls.Ghdl_Signal_Force_Eff_B1,
           Grt.Signals.Ghdl_Signal_Force_Effective_B1'Address);

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
      Def (Trans_Decls.Ghdl_Signal_Force_Drv_E8,
           Grt.Signals.Ghdl_Signal_Force_Driving_E8'Address);
      Def (Trans_Decls.Ghdl_Signal_Force_Eff_E8,
           Grt.Signals.Ghdl_Signal_Force_Effective_E8'Address);

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
      Def (Trans_Decls.Ghdl_Signal_Force_Drv_E32,
           Grt.Signals.Ghdl_Signal_Force_Driving_E32'Address);
      Def (Trans_Decls.Ghdl_Signal_Force_Eff_E32,
           Grt.Signals.Ghdl_Signal_Force_Effective_E32'Address);

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
      Def (Trans_Decls.Ghdl_Signal_Force_Drv_I32,
           Grt.Signals.Ghdl_Signal_Force_Driving_I32'Address);
      Def (Trans_Decls.Ghdl_Signal_Force_Eff_I32,
           Grt.Signals.Ghdl_Signal_Force_Effective_I32'Address);

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
      Def (Trans_Decls.Ghdl_Signal_Force_Drv_I64,
           Grt.Signals.Ghdl_Signal_Force_Driving_I64'Address);
      Def (Trans_Decls.Ghdl_Signal_Force_Eff_I64,
           Grt.Signals.Ghdl_Signal_Force_Effective_I64'Address);

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
      Def (Trans_Decls.Ghdl_Signal_Force_Drv_F64,
           Grt.Signals.Ghdl_Signal_Force_Driving_F64'Address);
      Def (Trans_Decls.Ghdl_Signal_Force_Eff_F64,
           Grt.Signals.Ghdl_Signal_Force_Effective_F64'Address);

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
           Grt.Files_Lib.Ghdl_Text_File_Elaborate'Address);
      Def (Trans_Decls.Ghdl_Text_File_Finalize,
           Grt.Files_Lib.Ghdl_Text_File_Finalize'Address);
      Def (Trans_Decls.Ghdl_Text_File_Open,
           Grt.Files_Lib.Ghdl_Text_File_Open'Address);
      Def (Trans_Decls.Ghdl_Text_File_Open_Status,
           Grt.Files_Lib.Ghdl_Text_File_Open_Status'Address);
      Def (Trans_Decls.Ghdl_Text_Write,
           Grt.Files_Lib.Ghdl_Text_Write'Address);
      Def (Trans_Decls.Ghdl_Text_Read_Length,
           Grt.Files_Lib.Ghdl_Text_Read_Length'Address);
      Def (Trans_Decls.Ghdl_Text_File_Close,
           Grt.Files_Lib.Ghdl_Text_File_Close'Address);

      Def (Trans_Decls.Ghdl_File_Elaborate,
           Grt.Files_Lib.Ghdl_File_Elaborate'Address);
      Def (Trans_Decls.Ghdl_File_Finalize,
           Grt.Files_Lib.Ghdl_File_Finalize'Address);
      Def (Trans_Decls.Ghdl_File_Open,
           Grt.Files_Lib.Ghdl_File_Open'Address);
      Def (Trans_Decls.Ghdl_File_Open_Status,
           Grt.Files_Lib.Ghdl_File_Open_Status'Address);
      Def (Trans_Decls.Ghdl_File_Close,
           Grt.Files_Lib.Ghdl_File_Close'Address);
      Def (Trans_Decls.Ghdl_File_Flush,
           Grt.Files_Lib.Ghdl_File_Flush'Address);
      Def (Trans_Decls.Ghdl_Write_Scalar,
           Grt.Files_Lib.Ghdl_Write_Scalar'Address);
      Def (Trans_Decls.Ghdl_Read_Scalar,
           Grt.Files_Lib.Ghdl_Read_Scalar'Address);

      Def (Trans_Decls.Ghdl_File_Endfile,
           Grt.Files_Lib.Ghdl_File_Endfile'Address);

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
      Def (Trans_Decls.Ghdl_Std_Ulogic_Match_Ge,
           Grt.Std_Logic_1164.Ghdl_Std_Ulogic_Match_Ge'Address);
      Def (Trans_Decls.Ghdl_Std_Ulogic_Match_Gt,
           Grt.Std_Logic_1164.Ghdl_Std_Ulogic_Match_Gt'Address);

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
   end Link;
end Trans_Link;
