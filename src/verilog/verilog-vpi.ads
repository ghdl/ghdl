--  Verilog vpi
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
with Grt.Types; use Grt.Types;
with Interfaces; use Interfaces;
with Verilog.Nodes; use Verilog.Nodes;
with Verilog.Storages; use Verilog.Storages;

package Verilog.Vpi is
   type VpiHandle_Type is tagged limited private;
   type VpiHandle is access all VpiHandle_Type'Class;
   pragma Convention (C, VpiHandle);
   pragma Warnings (Off, "* does not correspond to any C type");
   pragma Warnings (Off, "* does not correspond to C type");

   subtype PLI_INT32 is Integer;
   subtype PLI_UINT32 is Unsigned_32;
   subtype Pli_Double is Interfaces.IEEE_Float_64;

   --  Strong typing for Typ of S_Vpi_Time.
   type PLI_INT32_Time is new PLI_INT32;
   pragma Convention (C, PLI_INT32_Time);

   type S_Vpi_Time is record
      Typ : PLI_INT32_Time;
      High, Low : PLI_UINT32;
      Real : Pli_Double;
   end record;
   pragma Convention (C, S_Vpi_Time);
   type P_Vpi_Time is access S_Vpi_Time;

   VpiScaledRealTime : constant PLI_INT32_Time := 1;
   VpiSimTime        : constant PLI_INT32_Time := 2;
   VpiSuppressTime   : constant PLI_INT32_Time := 3;

   type S_Vpi_Vecval is record
      --  Bit encoding: AB 00=0, 10=1, 11=X, 01=Z.
      Aval, Bval : PLI_UINT32;
   end record;
   pragma Convention (C, S_Vpi_Vecval);

   type S_Vpi_Vecval_Arr is array (Natural range <>) of S_Vpi_Vecval;
   type P_Vpi_Vecval is access S_Vpi_Vecval_Arr (Natural);
   pragma Convention (C, P_Vpi_Vecval);

   --  Strong typing for Format of S_Vpi_Value.
   type PLI_INT32_Format is new PLI_INT32;
   pragma Convention (C, PLI_INT32_Format);

   VpiBinStrVal   : constant PLI_INT32_Format := 1;
   VpiOctStrVal   : constant PLI_INT32_Format := 2;
   VpiDecStrVal   : constant PLI_INT32_Format := 3;
   VpiHexStrVal   : constant PLI_INT32_Format := 4;
   VpiScalarVal   : constant PLI_INT32_Format := 5;
   VpiIntVal      : constant PLI_INT32_Format := 6;
   VpiRealVal     : constant PLI_INT32_Format := 7;
   VpiStringVal   : constant PLI_INT32_Format := 8;
   VpiVectorVal   : constant PLI_INT32_Format := 9;
   VpiStrengthVal : constant PLI_INT32_Format := 10;
   VpiTimeVal     : constant PLI_INT32_Format := 11;
   VpiObjTypeVal  : constant PLI_INT32_Format := 12;
   VpiSuppressVal : constant PLI_INT32_Format := 13;

   type T_Vpi_Value_Union (Disc : PLI_INT32_Format := 0) is record
      case Disc is
         when VpiBinStrVal
           | VpiOctStrVal
           | VpiDecStrVal
           | VpiHexStrVal
           | VpiStringVal =>
            Str : Ghdl_C_String;
         when VpiScalarVal =>
            Scalar : PLI_INT32;
         when VpiIntVal =>
            Intege : PLI_INT32;
         when VpiRealVal =>
            Real : Pli_Double;
         when VpiTimeVal =>
            Time : P_Vpi_Time;
         when VpiVectorVal =>
            Vector : P_Vpi_Vecval;
         when VpiSuppressVal =>
            null;
         when others =>
            Misc : Ghdl_C_String;
      end case;
   end record;
   pragma Unchecked_Union (T_Vpi_Value_Union);
   pragma Convention (C, T_Vpi_Value_Union);

   type S_Vpi_Value is record
      Format : PLI_INT32_Format;
      Value : T_Vpi_Value_Union;
   end record;
   pragma Convention (C, S_Vpi_Value);

   type P_Vpi_Value is access S_Vpi_Value;
   pragma Convention (C, P_Vpi_Value);

   type Ghdl_C_String_Arr is array (Natural range <>) of Ghdl_C_String;
   type Argv_Type is access Ghdl_C_String_Arr (Natural);
   pragma Convention (C, Argv_Type);

   type S_Vpi_Vlog_Info is record
      Argc : PLI_INT32;
      Argv : Argv_Type;
      Product : Ghdl_C_String;
      Version : Ghdl_C_String;
   end record;
   pragma Convention (C, S_Vpi_Vlog_Info);

   type P_Vpi_Vlog_Info is access S_Vpi_Vlog_Info;
   pragma Convention (C, P_Vpi_Vlog_Info);

   type S_Cb_Data;
   type P_Cb_Data is access all S_Cb_Data;
   pragma Convention (C, P_Cb_Data);

   type Cb_Rtn_Type is access function (Cb : P_Cb_Data) return PLI_INT32;
   pragma Convention (C, Cb_Rtn_Type);

   type Void_Type is null record;
   type P_Void_Type is access Void_Type;
   pragma Convention (C, P_Void_Type);

   --  Use strong typing for Reason.
   type PLI_INT32_Reason is new PLI_INT32;
   pragma Convention (C, PLI_INT32_Reason);

   --  Possible values for Reason:
   CbValueChange            : constant PLI_INT32_Reason := 1;
   CbStmt                   : constant PLI_INT32_Reason := 2;
   CbForce                  : constant PLI_INT32_Reason := 3;
   CbRelease                : constant PLI_INT32_Reason := 4;
   CbAtStartOfSimTime       : constant PLI_INT32_Reason := 5;
   CbReadWriteSynch         : constant PLI_INT32_Reason := 6;
   CbReadOnlySynch          : constant PLI_INT32_Reason := 7;
   CbNextSimTime            : constant PLI_INT32_Reason := 8;
   CbAfterDelay             : constant PLI_INT32_Reason := 9;
   CbEndOfCompile           : constant PLI_INT32_Reason := 10;
   CbStartOfSimulation      : constant PLI_INT32_Reason := 11;
   CbEndOfSimulation        : constant PLI_INT32_Reason := 12;
   CbError                  : constant PLI_INT32_Reason := 13;
   CbTchkViolation          : constant PLI_INT32_Reason := 14;
   CbStartOfSave            : constant PLI_INT32_Reason := 15;
   CbEndOfSave              : constant PLI_INT32_Reason := 16;
   CbStartOfRestart         : constant PLI_INT32_Reason := 17;
   CbEndOfRestart           : constant PLI_INT32_Reason := 18;
   CbStartOfReset           : constant PLI_INT32_Reason := 19;
   CbEndOfReset             : constant PLI_INT32_Reason := 20;
   CbEnterInteractive       : constant PLI_INT32_Reason := 21;
   CbExitInteractive        : constant PLI_INT32_Reason := 22;
   CbInteractiveScopeChange : constant PLI_INT32_Reason := 23;
   CbUnresolvedSystf        : constant PLI_INT32_Reason := 24;

   type S_Cb_Data is record
      Reason : PLI_INT32_Reason;
      Cb_Rtn : Cb_Rtn_Type;
      Obj : VpiHandle;
      Time : P_Vpi_Time;
      Value : P_Vpi_Value;
      Index : PLI_INT32;
      User_Data : P_Void_Type;
   end record;
   pragma Convention (C, S_Cb_Data);

   type Systf_Func_Type is access
     function (User_Data : P_Void_Type) return PLI_INT32;
   pragma Convention (C, Systf_Func_Type);

   --  Use strong typing for Systf type.
   type PLI_INT32_Systf is new PLI_INT32;
   pragma Convention (C, PLI_INT32_Systf);

   --  A SysTf hook is either a task or a function.
   VpiSysTask : constant PLI_INT32_Systf := 1;
   VpiSysFunc : constant PLI_INT32_Systf := 2;

   type PLI_INT32_FuncType is new PLI_INT32;
   pragma Convention (C, PLI_INT32_FuncType);
   VpiIntFunc         : constant PLI_INT32_FuncType := 1;
   VpiRealFunc        : constant PLI_INT32_FuncType := 2;
   VpiTimeFunc        : constant PLI_INT32_FuncType := 3;
   VpiSizedFunc       : constant PLI_INT32_FuncType := 4;
   VpiSizedSignedFunc : constant PLI_INT32_FuncType := 5;
   VpiOtherFunc       : constant PLI_INT32_FuncType := 6;

   --  From iverilog.
   VpiStringFunc      : constant PLI_INT32_FuncType := 10;

   type S_Vpi_Systf_Data is record
      Typ : PLI_INT32_Systf;
      Sysfunctype : PLI_INT32_FuncType;
      Tfname : Ghdl_C_String;
      Calltf : Systf_Func_Type;
      Compiletf : Systf_Func_Type;
      Sizetf : Systf_Func_Type;
      User_Data : P_Void_Type;
   end record;
   pragma Convention (C, S_Vpi_Systf_Data);

   type P_Vpi_Systf_Data is access S_Vpi_Systf_Data;
   pragma Convention (C, P_Vpi_Systf_Data);

   function Vpi_Get_Vlog_Info (Vlog_Info_P : P_Vpi_Vlog_Info) return PLI_INT32;
   pragma Export (C, Vpi_Get_Vlog_Info, "ghdlvlg_vpi_get_vlog_info");

   type PLI_INT32_Control is new PLI_INT32;
   pragma Convention (C, PLI_INT32_Control);
   VpiStop                : constant PLI_INT32_Control := 66;
   VpiFinish              : constant PLI_INT32_Control := 67;
   VpiReset               : constant PLI_INT32_Control := 68;
   VpiSetInteractiveScope : constant PLI_INT32_Control := 69;

   procedure Vpi_Control (Op : PLI_INT32_Control; Status : PLI_INT32);
   pragma Export (C, Vpi_Control, "ghdlvlg_vpi_control");

   --  Strong typing for Vpi_Handle Typ.
   type PLI_INT32_Obj is new PLI_INT32;
   pragma Convention (C, PLI_INT32_Obj);

   VpiConstant      : constant PLI_INT32_Obj := 7;
   VpiFuncCall      : constant PLI_INT32_Obj := 19;
   VpiFunction      : constant PLI_INT32_Obj := 20;
   VpiIntegerVar    : constant PLI_INT32_Obj := 25;
   VpiIterator      : constant PLI_INT32_Obj := 27;
   VpiMemory        : constant PLI_INT32_Obj := 29;
   VpiMemoryWord    : constant PLI_INT32_Obj := 30;
   VpiModPath       : constant PLI_INT32_Obj := 31;
   VpiModule        : constant PLI_INT32_Obj := 32;
   VpiNamedBegin    : constant PLI_INT32_Obj := 33;
   VpiNamedEvent    : constant PLI_INT32_Obj := 34;
   VpiNamedFork     : constant PLI_INT32_Obj := 35;
   VpiNet           : constant PLI_INT32_Obj := 36;
   VpiOperation     : constant PLI_INT32_Obj := 39;
   VpiParameter     : constant PLI_INT32_Obj := 41;
   VpiPartSelect    : constant PLI_INT32_Obj := 42;
   VpiPathTerm      : constant PLI_INT32_Obj := 43;
   VpiPort          : constant PLI_INT32_Obj := 44;
   VpiRealVar       : constant PLI_INT32_Obj := 47;
   VpiReg           : constant PLI_INT32_Obj := 48;
   VpiRegBit        : constant PLI_INT32_Obj := 49;
   VpiSysFuncCall   : constant PLI_INT32_Obj := 56;
   VpiSysTaskCall   : constant PLI_INT32_Obj := 57;
   VpiTask          : constant PLI_INT32_Obj := 59;
   VpiTimeVar       : constant PLI_INT32_Obj := 63;
   VpiUdpDefn       : constant PLI_INT32_Obj := 66;
   VpiUserSystf     : constant PLI_INT32_Obj := 67;
   VpiVarSelect     : constant PLI_INT32_Obj := 68;
   VpiIndex         : constant PLI_INT32_Obj := 78;
   VpiLeftRange     : constant PLI_INT32_Obj := 79;
   VpiParent        : constant PLI_INT32_Obj := 81;
   VpiRightRange    : constant PLI_INT32_Obj := 83;
   VpiScope         : constant PLI_INT32_Obj := 84;
   VpiSysTfCall     : constant PLI_INT32_Obj := 85;
   VpiArgument      : constant PLI_INT32_Obj := 89;
   VpiInternalScope : constant PLI_INT32_Obj := 92;
   VpiModPathIn     : constant PLI_INT32_Obj := 95;
   VpiModPathOut    : constant PLI_INT32_Obj := 96;
   VpiVariables     : constant PLI_INT32_Obj := 100;
   VpiExpr          : constant PLI_INT32_Obj := 102;
   VpiBitSelect     : constant PLI_INT32_Obj := 106;
   VpiNetArray      : constant PLI_INT32_Obj := 114;
   VpiRegArray      : constant PLI_INT32_Obj := 116;

   VpiTypespec      : constant PLI_INT32_Obj := 605;

   --  Variables
   VpiVarBit               : constant PLI_INT32_Obj := VpiRegBit;
   VpiLongIntVar           : constant PLI_INT32_Obj := 610;
   VpiShortIntVar          : constant PLI_INT32_Obj := 611;
   VpiIntVar               : constant PLI_INT32_Obj := 612;
   VpiShortRealVar         : constant PLI_INT32_Obj := 613;
   VpiByteVar              : constant PLI_INT32_Obj := 614;
   VpiClassVar             : constant PLI_INT32_Obj := 615;
   VpiStringVar            : constant PLI_INT32_Obj := 616;
   VpiEnumVar              : constant PLI_INT32_Obj := 617;
   VpiStructVar            : constant PLI_INT32_Obj := 618;
   VpiUnionVar             : constant PLI_INT32_Obj := 619;
   VpiBitVar               : constant PLI_INT32_Obj := 620;
   VpiLogicVar             : constant PLI_INT32_Obj := VpiReg;
   VpiArrayVar             : constant PLI_INT32_Obj := VpiRegArray;
   VpiClassObj             : constant PLI_INT32_Obj := 621;
   VpiChandleVar           : constant PLI_INT32_Obj := 622;
   VpiPackedArrayVar       : constant PLI_INT32_Obj := 623;
   VpiVirtualInterfaceVar  : constant PLI_INT32_Obj := 728;

   --  Typespecs
   VpiStringTypespec  : constant PLI_INT32_Obj := 631;

   VpiMethodFuncCall  : constant PLI_INT32_Obj := 648;

   VpiClassDefn       : constant PLI_INT32_Obj := 652;

   function Vpi_Handle (Typ : PLI_INT32_Obj; Ref : VpiHandle) return VpiHandle;
   pragma Export (C, Vpi_Handle, "ghdlvlg_vpi_handle");

   function Vpi_Handle_By_Index(Ref : VpiHandle; Idx : PLI_INT32)
                               return VpiHandle;
   pragma Export (C, Vpi_Handle_By_Index, "ghdlvlg_vpi_handle_by_index");

   function Vpi_Iterate (Typ : PLI_INT32_Obj; Ref : VpiHandle)
                        return VpiHandle;
   pragma Export (C, Vpi_Iterate, "ghdlvlg_vpi_iterate");

   function Vpi_Scan (Iter : VpiHandle) return VpiHandle;
   pragma Export (C, Vpi_Scan, "ghdlvlg_vpi_scan");

   function Vpi_Free_Object (Ref : VpiHandle) return PLI_INT32;
   pragma Export (C, Vpi_Free_Object, "ghdlvlg_vpi_free_object");

   --  Strong typing for vpi properties.
   type PLI_INT32_Prop is new PLI_INT32;
   pragma Convention (C, PLI_INT32_Prop);

   --  Properties
   VpiUndefined      : constant PLI_INT32_Prop := -1;
   VpiType           : constant PLI_INT32_Prop := 1;
   VpiName           : constant PLI_INT32_Prop := 2;
   VpiFullName       : constant PLI_INT32_Prop := 3;
   VpiSize           : constant PLI_INT32_Prop := 4;
   VpiFile           : constant PLI_INT32_Prop := 5;
   VpiLineNo         : constant PLI_INT32_Prop := 6;
   VpiTopModule      : constant PLI_INT32_Prop := 7;
   VpiCellInstance   : constant PLI_INT32_Prop := 8;
   VpiDefName        : constant PLI_INT32_Prop := 9;
   VpiTimeUnit       : constant PLI_INT32_Prop := 11;
   VpiTimePrecision  : constant PLI_INT32_Prop := 12;
   VpiDefFile        : constant PLI_INT32_Prop := 15;
   VpiDefLineNo      : constant PLI_INT32_Prop := 16;
   VpiScalar         : constant PLI_INT32_Prop := 17;
   VpiVector         : constant PLI_INT32_Prop := 18;
   VpiDirection      : constant PLI_INT32_Prop := 20;
   VpiNetType        : constant PLI_INT32_Prop := 22;
   VpiArray          : constant PLI_INT32_Prop := 28;
   VpiPortIndex      : constant PLI_INT32_Prop := 29;
   VpiEdge           : constant PLI_INT32_Prop := 36;
   VpiConstType      : constant PLI_INT32_Prop := 40;
   VpiFuncType       : constant PLI_INT32_Prop := 44;
   VpiSysFuncType    : constant PLI_INT32_Prop := VpiFuncType;
   VpiUserDefn       : constant PLI_INT32_Prop := 45;
   VpiAutomatic      : constant PLI_INT32_Prop := 50;
   VpiConstantSelect : constant PLI_INT32_Prop := 53;
   VpiSigned         : constant PLI_INT32_Prop := 65;
   VpiLocalParam     : constant PLI_INT32_Prop := 70;

   --  For VpiConstType
   VpiDecConst    : constant := 1;
   VpiRealConst   : constant := 2;
   VpiBinaryConst : constant := 3;
   VpiOctConst    : constant := 4;
   VpiHexConst    : constant := 5;
   VpiStringConst : constant := 6;

   function Vpi_Get (Prop : PLI_INT32_Prop; Ref : VpiHandle) return PLI_INT32;
   pragma Export (C, Vpi_Get, "ghdlvlg_vpi_get");

   function Vpi_Get_Str (Prop : PLI_INT32_Prop; Ref : VpiHandle)
                        return Ghdl_C_String;
   pragma Export (C, Vpi_Get_Str, "ghdlvlg_vpi_get_str");

   procedure Vpi_Get_Time (Obj : VpiHandle; T : P_Vpi_Time);
   pragma Export (C, Vpi_Get_Time, "ghdlvlg_vpi_get_time");

   procedure Vpi_Get_Value (Expr : VpiHandle; Value : P_Vpi_Value);
   pragma Export (C, Vpi_Get_Value, "ghdlvlg_vpi_get_value");

   type PLI_INT32_Put_Flags is new PLI_INT32;
   pragma Convention (C, PLI_INT32_Put_Flags);

   VpiNoDelay            : constant PLI_INT32_Put_Flags := 1;
   VpiInertialDelay      : constant PLI_INT32_Put_Flags := 2;
   VpiTransportDelay     : constant PLI_INT32_Put_Flags := 3;
   VpiPureTransportDelay : constant PLI_INT32_Put_Flags := 4;
   VpiForceFlag          : constant PLI_INT32_Put_Flags := 5;
   VpiReleaseFlag        : constant PLI_INT32_Put_Flags := 6;

   function Vpi_Put_Value (Obj : VpiHandle;
                           Value : P_Vpi_Value;
                           Whe : P_Vpi_Time;
                           Flags : PLI_INT32_Put_Flags) return VpiHandle;
   pragma Export (C, Vpi_Put_Value, "ghdlvlg_vpi_put_value");

   function Vpi_Register_Cb (Data : P_Cb_Data) return VpiHandle;
   pragma Export (C, Vpi_Register_Cb, "ghdlvlg_vpi_register_cb");

   function Vpi_Remove_Cb (Ref : VpiHandle) return PLI_INT32;
   pragma Export (C, Vpi_Remove_Cb, "ghdlvlg_vpi_remove_cb");

   function Vpi_Register_Systf (Sd : P_Vpi_Systf_Data) return VpiHandle;
   pragma Export (C, Vpi_Register_Systf, "ghdlvlg_vpi_register_systf");

   --  For compilation
   function Find_Systask (Name : Name_Id) return Sys_Tf_Id;
   function Find_Sysfunc (Name : Name_Id) return Sys_Tf_Id;
   function Get_Sysfunc_Type (Id : Sys_Tf_Id) return PLI_INT32_FuncType;
   function Call_Systf_Sizetf (Id : Sys_Tf_Id) return PLI_INT32;
   procedure Call_Systf_Compiletf (Id : Sys_Tf_Id; N : Node);

   --  For simulation

   type Blocking_Assign_Acc is access procedure
     (Frame : Frame_Ptr; Target : Node; Value : Data_Ptr; Typ : Node);

   --  Procedure to be called for Put_Value.
   Blocking_Assign : Blocking_Assign_Acc;

   procedure Call_Systask_Calltf
     (Frame : Frame_Ptr; Id : Sys_Tf_Id; N : Node);
   procedure Call_Sysfunc_Calltf
     (Frame : Frame_Ptr; Id : Sys_Tf_Id; N : Node; Data : Data_Ptr);

   procedure Initialize;

   --  Call end of compile callbacks.
   procedure End_Of_Compile;

   --  Execute ReadOnlySynch callbacks.
   procedure Execute_Read_Only_Synch_Cb;

   procedure End_Of_Simulation;

   procedure Execute_Cb (Cb : P_Cb_Data);

   --  Simulation control.
   Vpip_Control : Natural := 0;

   --  Exit status
   Vpip_Exit_Status : Integer := 0;

   --  Interractive scope.
   Interractive_Scope : Node;
   Interractive_Frame : Frame_Ptr;
private
   procedure Vpip_Make_Systf_System_Defined (Ref : VpiHandle);
   pragma Export (C, Vpip_Make_Systf_System_Defined,
                  "ghdlvlg_vpip_make_systf_system_defined");

   type VpiHandle_Type is tagged limited null record;
   function Get (Prop : PLI_INT32_Prop; Ref : VpiHandle_Type)
                return PLI_INT32;
   function Get_Str (Prop : PLI_INT32_Prop; Ref : VpiHandle_Type)
                    return Ghdl_C_String;
   procedure Get_Value (Ref : VpiHandle_Type; Value : P_Vpi_Value);
   function Put_Value (Ref : VpiHandle_Type;
                       Value : P_Vpi_Value;
                       Whe : P_Vpi_Time;
                       Flags : PLI_INT32_Put_Flags) return VpiHandle;
   procedure Free_Handle (Ref : VpiHandle_Type);

   type VpiHandle_Node_Type is new VpiHandle_Type with record
      N : Node;
   end record;
   function Get (Prop : PLI_INT32_Prop; Ref : VpiHandle_Node_Type)
                return PLI_INT32;
   function Get_Str (Prop : PLI_INT32_Prop; Ref : VpiHandle_Node_Type)
                    return Ghdl_C_String;
   procedure Get_Value (Ref : VpiHandle_Node_Type; Value : P_Vpi_Value);
   function Put_Value (Ref : VpiHandle_Node_Type;
                       Value : P_Vpi_Value;
                       Whe : P_Vpi_Time;
                       Flags : PLI_INT32_Put_Flags) return VpiHandle;
   function Handle (Typ : PLI_INT32_Obj; Ref : VpiHandle_Node_Type)
                    return VpiHandle;
   function Iterate (Typ : PLI_INT32_Obj; Ref : VpiHandle_Node_Type)
                    return VpiHandle;
   procedure Free_Handle (Ref : VpiHandle_Node_Type);

   --  Like VpiHandle_Node_Type, but also contains an handle that has to be
   --  unref-ed when the vpihandle is free.
   type VpiHandle_Node_Handle_Type is new VpiHandle_Node_Type with record
      Defined : Boolean;
      Handle : Data_Ptr;
   end record;
   procedure Free_Handle (Ref : VpiHandle_Node_Handle_Type);

   type VpiHandle_Cst_Type is new VpiHandle_Type with record
      Typ : PLI_INT32_Obj;
      Val : Int32;
   end record;
   procedure Get_Value (Ref : VpiHandle_Cst_Type; Value : P_Vpi_Value);

   --  Iterable VPI handle.
   type VpiHandle_Iterate_Type is abstract new VpiHandle_Type
     with null record;
   function Scan (Ref : access VpiHandle_Iterate_Type) return VpiHandle is
      abstract;

   type VpiHandle_Tf_Call_Type is new VpiHandle_Node_Type with null record;
   function Put_Value (Ref : VpiHandle_Tf_Call_Type;
                       Value : P_Vpi_Value;
                       Whe : P_Vpi_Time;
                       Flags : PLI_INT32_Put_Flags) return VpiHandle;
   procedure Free_Handle (Ref : VpiHandle_Tf_Call_Type);

   type VpiHandle_Iterate_Type_Acc is
     access all VpiHandle_Iterate_Type'Class;

   pragma Warnings (On, "* does not correspond to any C type");
   pragma Warnings (On, "* does not correspond to C type");
end Verilog.Vpi;
