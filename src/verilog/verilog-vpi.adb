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
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
with Tables;
with Files_Map;
with Name_Table;
with Str_Table;
with Name_Maps;
with Std_Names;
with Verilog.Types; use Verilog.Types;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Sv_Strings; use Verilog.Sv_Strings;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Standard;
with Verilog.Bignums; use Verilog.Bignums;
with Verilog.Simulation;
with Verilog.Executions;
with Verilog.Allocates; use Verilog.Allocates;

package body Verilog.Vpi is
   Vpi_Location : Location_Type;

   Product_String : constant String := "GHDL vlg" & NUL;
   Version_String : constant String := "v0.0" & NUL;

   function To_Uns32 is new Ada.Unchecked_Conversion
     (PLI_INT32, Uns32);

   function To_PLI_INT32 is new Ada.Unchecked_Conversion
     (Uns32, PLI_INT32);

   function To_Argv_Type is new Ada.Unchecked_Conversion
     (System.Address, Argv_Type);

   Vlog_Info : S_Vpi_Vlog_Info;

   type Systf_Context is record
      Call : Node;
      Value : Data_Ptr;
      Frame : Frame_Ptr;
   end record;

   Current_Systf_Context : Systf_Context := (Null_Node, No_Data_Ptr, null);

   type Systf_Entry is record
      Typ : PLI_INT32_Systf;
      Func_Type : PLI_INT32_FuncType;
      Calltf : Systf_Func_Type;
      Compiletf : Systf_Func_Type;
      Sizetf : Systf_Func_Type;
      User_Data : P_Void_Type;
   end record;

   package Systf_Table is new Tables
     (Table_Component_Type => Systf_Entry,
      Table_Index_Type => Sys_Tf_Id,
      Table_Low_Bound => Sys_Tf_User_Id,
      Table_Initial => 16);

   package Systf_Maps is
      new Name_Maps (T => Sys_Tf_Id, No_Element => No_Sys_Tf_Id);

   Systf_Map : Systf_Maps.Map_Type;

   procedure Set_Vpi_Location (N : Node) is
   begin
      Set_Location (N, Vpi_Location);
   end Set_Vpi_Location;

   function Vpi_Get_Vlog_Info (Vlog_Info_P : P_Vpi_Vlog_Info) return PLI_INT32
   is
   begin
      Vlog_Info_P.all := Vlog_Info;
      return 0;
   end Vpi_Get_Vlog_Info;

   function Get (Prop : PLI_INT32_Prop; Ref : VpiHandle_Type)
                return PLI_INT32 is
   begin
      raise Program_Error;
      return -1;
   end Get;

   function Get_Str (Prop : PLI_INT32_Prop; Ref : VpiHandle_Type)
                    return Ghdl_C_String is
   begin
      raise Program_Error;
      return null;
   end Get_Str;

   procedure Get_Value (Ref : VpiHandle_Type; Value : P_Vpi_Value) is
   begin
      raise Program_Error;
   end Get_Value;

   function Put_Value (Ref : VpiHandle_Type;
                       Value : P_Vpi_Value;
                       Whe : P_Vpi_Time;
                       Flags : PLI_INT32_Put_Flags) return VpiHandle is
   begin
      raise Program_Error;
      return null;
   end Put_Value;

   procedure Free_Handle (Ref : VpiHandle_Type) is
   begin
      raise Program_Error;
   end Free_Handle;

   procedure Free_Handle (Ref : VpiHandle_Node_Handle_Type) is
   begin
      if Ref.Defined then
         Executions.Finalize_Data (Ref.Handle'Address, Get_Expr_Type (Ref.N));
      end if;
   end Free_Handle;

   --  VPI handle to iterate on chain N.
   type VpiHandle_Iterate_Node_Type is new VpiHandle_Iterate_Type with record
      --  Next node to return.
      N : Node;
   end record;
   procedure Free_Handle (Ref : VpiHandle_Iterate_Node_Type);
   function Scan (Ref : access VpiHandle_Iterate_Node_Type) return VpiHandle;

   function Scan (Ref : access VpiHandle_Iterate_Node_Type) return VpiHandle
   is
      Res : Node;
   begin
      Res := Ref.N;
      if Res = Null_Node then
         return null;
      end if;
      Ref.N := Get_Chain (Ref.N);
      return new VpiHandle_Node_Handle_Type'(N => Get_Expression (Res),
                                             Defined => False,
                                             Handle => No_Data_Ptr);
   end Scan;

   procedure Free_Handle (Ref : VpiHandle_Iterate_Node_Type) is
   begin
      null;
   end Free_Handle;

   function Strip_Name (N : Node) return Node is
   begin
      case Get_Kind (N) is
         when N_Name
           | N_Hierarchical =>
            return Get_Declaration (N);
         when others =>
            return N;
      end case;
   end Strip_Name;

   function Get_Node (Ref : VpiHandle_Node_Type) return Node is
   begin
      return Strip_Name (Ref.N);
   end Get_Node;

   function Handle_Range (Typ : PLI_INT32_Obj; N : Node) return VpiHandle
   is
      Ntype : Node;
   begin
      case Get_Kind (N) is
         when N_Var =>
            Ntype := Get_Type_Data_Type (N);
         when others =>
            Error_Kind ("verilog.vpi.handle_range", N);
      end case;

      case Get_Kind (Ntype) is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Array_Cst =>
            null;
         when others =>
            Error_Kind ("verilog.vpi.handle_range (type)", Ntype);
      end case;

      case Typ is
         when VpiLeftRange =>
            return new VpiHandle_Cst_Type'(Typ => VpiLeftRange,
                                           Val => Get_Msb_Cst (Ntype));
         when VpiRightRange =>
            return new VpiHandle_Cst_Type'(Typ => VpiRightRange,
                                           Val => Get_Lsb_Cst (Ntype));
         when others =>
            raise Program_Error;
      end case;
   end Handle_Range;

   procedure Get_Value (Ref : VpiHandle_Cst_Type; Value : P_Vpi_Value) is
   begin
      if Value.Format = VpiIntVal then
         Value.Value.Intege := PLI_INT32 (Ref.Val);
      else
         raise Program_Error;
      end if;
   end Get_Value;

   function Handle_Scope (N : Node) return Node is
   begin
      case Get_Kind (N) is
         when N_Subroutine_Call_Stmt
           | N_Class =>
            return Get_Parent (N);
         when N_System_Call =>
            return Get_Call_Scope (N);
         when others =>
            Error_Kind ("verilog.vpi.handle_scope", N);
      end case;
   end Handle_Scope;

   function Handle_Typespec (N : Node) return Node is
   begin
      case Get_Kind (N) is
         when N_Call
           | N_System_Call
           | N_Concatenation =>
            return Get_Expr_Type (N);
         when others =>
            Error_Kind ("verilog.vpi.handle_typespec", N);
      end case;
   end Handle_Typespec;

   function Handle (Typ : PLI_INT32_Obj; Ref : VpiHandle_Node_Type)
                   return VpiHandle
   is
      N : constant Node := Get_Node (Ref);
   begin
      case Typ is
         when VpiScope =>
            return new VpiHandle_Node_Type'(N => Handle_Scope (N));
         when VpiLeftRange
           | VpiRightRange =>
            return Handle_Range (Typ, N);
         when VpiTypespec =>
            return new VpiHandle_Node_Type'(N => Handle_Typespec (N));
         when others =>
            raise Program_Error;
      end case;
   end Handle;

   procedure Free_Handle (Ref : VpiHandle_Node_Type) is
   begin
      --  TODO: free nodes that have been created in vpi.
      null;
   end Free_Handle;

   --  Return True iff ATYPE applies to vpiMemory.
   function Is_Memory_Type (Atype : Node) return Boolean is
   begin
      --  IEEE1800-2017 36.12.1 VPI Incompatibilities with other standard
      --  versions
      --  1) vpiMemory exists as object:
      --     Unpacked unidimensional REG arrays were exclusively characterized
      --     as vpiMemory objects in IEEE Std 1364-1995.
      if Get_Kind (Atype) /= N_Array_Cst then
         return False;
      end if;
      case Get_Kind (Get_Type_Element_Type (Atype)) is
         when N_Log_Packed_Array_Cst
           | N_Logic_Type =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Memory_Type;

   --  Return a vpi*Var according to TYP.
   function Get_Var_Vpitype (Typ : Node) return PLI_INT32_Obj
   is
      use Verilog.Standard;
   begin
      case Get_Kind (Typ) is
         when N_Array_Cst =>
            --  IEEE1800-2017 36.12.1 VPI Incompatibilities with other
            --  standard versions
            --  1) vpiMemory exists as object:
            --     Unpacked unidimensional REG arrays were exclusively
            --     characterized as vpiMemory objects in IEEE Std 1364-1995.
            case Get_Kind (Get_Type_Element_Type (Typ)) is
               when N_Log_Packed_Array_Cst
                 | N_Logic_Type =>
                  return VpiMemory;
               when others =>
                  return VpiArrayVar;
            end case;
         when N_Bit_Packed_Array_Cst =>
            case Typ is
               when Signed_Byte_Type =>
                  return VpiByteVar;
               when Signed_Shortint_Type =>
                  return VpiShortIntVar;
               when Signed_Int_Type =>
                  return VpiIntVar;
               when Signed_Longint_Type =>
                  return VpiLongIntVar;
               when others =>
                  return VpiPackedArrayVar;
            end case;
         when N_Log_Packed_Array_Cst =>
            case Typ is
               when Signed_Integer_Type =>
                  return VpiIntegerVar;
               when others =>
                  if Get_Type_Element_Type (Typ) = Unsigned_Logic_Type then
                     return VpiReg;
                  else
                     return VpiPackedArrayVar;
                  end if;
            end case;
         when N_Logic_Type =>
            return VpiLogicVar; --  same as VpiReg
         when N_Bit_Type =>
            return VpiBitVar;
         when N_String_Type =>
            return VpiStringVar;
         when N_Class =>
            return VpiClassVar;
         when N_Queue_Cst =>
            return VpiArrayVar;
         when N_Enum_Type =>
            return VpiEnumVar;
         when others =>
            Error_Kind ("get_var_vpitype", Typ);
      end case;
   end Get_Var_Vpitype;

   function Get_Vpitype (N : Node) return PLI_INT32_Obj is
   begin
      case Get_Kind (N) is
         when N_Parenthesis_Expr =>
            return Get_Vpitype (Get_Expression (N));
         when N_String_Literal
           | N_Time_Literal =>
            return VpiConstant;
         when N_Concatenation
           | N_Replication_Cst
           | N_Post_Increment
           | N_Binary_Op
           | N_Short_Circuit_Op
           | N_Unary_Op
           | N_Cond_Op =>
            return VpiOperation;
         when N_Number =>
            --  FIXME: not correct.
            return VpiReg;
         when N_Var =>
            return Get_Var_Vpitype (Get_Type_Data_Type (N));
         when N_Foreach_Variable =>
            return Get_Var_Vpitype (Get_Expr_Type (N));
         when N_Property_Name
           | N_Return_Var =>
            if Is_Memory_Type (Get_Expr_Type (N)) then
               return VpiMemory;
            else
               --  FIXME: unless real ?
               return VpiReg;
            end if;
         when Nkinds_Nets =>
            return VpiNet;
         when N_Parameter
           | N_Localparam =>
            return VpiParameter;
         when Nkinds_Net_Port
           | Nkinds_Tf_Port =>
            --  FIXME: or vpiIODecl
            return VpiReg;
         when N_String_Type =>
            return VpiStringTypespec;
         when N_Dotted_Name
            | N_Member_Name =>
            --  FIXME: type member, method...
            return VpiPartSelect;
         when N_Class_Qualified_Name
            | N_This_Name =>
            return Get_Vpitype (Get_Declaration (N));
         when N_Bit_Select =>
            --  FIXME: refine:
            return VpiBitSelect;
         when N_Part_Select
           | N_Part_Select_Cst =>
            return VpiPartSelect;
         when N_Indexed_Name
           | N_Associative_Index =>
            return VpiVarSelect;
         when N_System_Call =>
            return VpiSysFuncCall;
         when N_Call =>
            return VpiFuncCall;
         when N_Method_Name =>
            return VpiMethodFuncCall;
         when N_Module =>
            return VpiModule;
         when N_Class =>
            return VpiClassDefn;
         when others =>
            Error_Kind ("verilog.vpi.get(Type)", N);
      end case;
   end Get_Vpitype;

   --  1800-2017 37.17 Variables
   --  9) vpiSize for a variable array shall return the number of variables in
   --     the array.  For variables belonging to an integer data type, for
   --     enum vars, and for packed struct and union variables, vpiSize shall
   --     return the size of the variable in bits.  For a string var, it shall
   --     return the number of characters that the variable currently contains.
   --     For unpacked structures and unions, the size indicates the number of
   --     fields in the structure or union.  For a var bit, vpiSize shall
   --     return 1.  For all other variables, the behavior of the vpiSize
   --     property is not defined.
   function Get_Vpisize (Ref : VpiHandle_Node_Type) return PLI_INT32
   is
      Rtype : constant Node := Get_Expr_Type (Ref.N);
   begin
      case Get_Kind (Rtype) is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst =>
            return PLI_INT32 (Get_Type_Width (Rtype));
         when N_Logic_Type
           | N_Bit_Type =>
            return 1;
         when N_String_Type =>
            declare
               Res : Sv_String;
               Len : PLI_INT32;
            begin
               --  FIXME: evaluate only once.
               Executions.Execute_Expression
                 (Current_Systf_Context.Frame, Res'Address, Ref.N);
               Len := PLI_INT32 (Get_Length (Res));
               Unref (Res);
               return Len;
            end;
         when others =>
            Error_Kind ("verilog.vpi.get(Size)", Rtype);
      end case;
   end Get_Vpisize;

   function Get_Vpiconsttype (N : Node) return PLI_INT32 is
   begin
      case Get_Kind (N) is
         when N_String_Literal =>
            return VpiStringConst;
         when N_Class_Qualified_Name =>
            return Get_Vpiconsttype (Get_Declaration (N));
         when others =>
            Error_Kind ("verilog.vpi.getconsttype", N);
      end case;
   end Get_Vpiconsttype;

   function Get (Prop : PLI_INT32_Prop; Ref : VpiHandle_Node_Type)
                return PLI_INT32 is
   begin
      case Prop is
         when VpiType =>
            return PLI_INT32 (Get_Vpitype (Get_Node (Ref)));
         when VpiConstType =>
            return Get_Vpiconsttype (Get_Node (Ref));
         when VpiLineNo =>
            declare
               Loc : constant Location_Type := Get_Location (Ref.N);
               File : Source_File_Entry;
               Line_Pos : Source_Ptr;
               Line : Natural;
               Offset : Natural;
            begin
               Files_Map.Location_To_Coord (Loc, File, Line_Pos, Line, Offset);
               return PLI_INT32 (Line);
            end;

         when VpiAutomatic =>
            --  Not yet handled.
            return 0;

         when VpiTimeUnit =>
            return -9;  --  1 ns
         when VpiTimePrecision =>
            return -9;  --  1 ns

         when VpiSigned =>
            if Get_Signed_Flag (Get_Expr_Type (Ref.N)) then
               return 1;
            else
               return 0;
            end if;
         when VpiSize =>
            return Get_Vpisize (Ref);

         when VpiFuncType =>
            declare
               use Verilog.Standard;
               Ftype : constant Node := Get_Expr_Type (Ref.N);
               Etype : Node;
            begin
               case Ftype is
                  when Signed_Integer_Type =>
                     return Integer (VpiIntFunc);
                  when Real_Type =>
                     return Integer (VpiRealFunc);
                  when Signed_Time_Type =>
                     return Integer (VpiTimeFunc);
                  when others =>
                     null;
               end case;
               if Get_Kind (Ftype) in Nkinds_Vector_Types then
                  Etype := Get_Type_Element_Type (Ftype);
                  if Etype = Unsigned_Logic_Type or Etype = Unsigned_Bit_Type
                  then
                     if Get_Signed_Flag (Ftype) then
                        return Integer (VpiSizedSignedFunc);
                     else
                        return Integer (VpiSizedFunc);
                     end if;
                  end if;
               end if;
               return Integer (VpiOtherFunc);
            end;
         when others =>
            raise Program_Error;
      end case;
   end Get;

   procedure Vpi_Free_Object (Ref : VpiHandle)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (VpiHandle_Type'Class, VpiHandle);
      Copy : VpiHandle;
   begin
      if Ref = null then
         return;
      end if;

      Free_Handle (Ref.all);

      Copy := Ref;
      Free (Copy);
   end Vpi_Free_Object;

   function Vpi_Free_Object (Ref : VpiHandle) return PLI_INT32 is
   begin
      Vpi_Free_Object (Ref);
      return 0;
   end Vpi_Free_Object;

   Str_Buf : String (1 .. 1024);
   Str_Buf_Len : Natural := 0;

   procedure Init_Str_Buf is
   begin
      Str_Buf_Len := 0;
   end Init_Str_Buf;

   procedure Append_Str_Buf (C : Character) is
   begin
      Str_Buf_Len := Str_Buf_Len + 1;
      pragma Assert (Str_Buf_Len < Str_Buf'Last);
      Str_Buf (Str_Buf_Len) := C;
   end Append_Str_Buf;

   procedure Append_Str_Buf (S : String) is
   begin
      for I in S'Range loop
         Append_Str_Buf (S (I));
      end loop;
   end Append_Str_Buf;

   function Return_Str_Buf return Ghdl_C_String is
   begin
      Str_Buf (Str_Buf_Len + 1) := NUL;
      return To_Ghdl_C_String (Str_Buf'Address);
   end Return_Str_Buf;

   Vec_Buf : S_Vpi_Vecval_Arr (0 .. 1023);
   Vec_Res : Natural;
   Vec_Pos : Natural := 0;
   Vec_Len : Natural := 0;

   procedure Init_Vec_Buf (Len : Natural) is
   begin
      if Vec_Pos + Len > Vec_Buf'Last then
         Vec_Pos := 0;
      end if;
      if Vec_Pos + Len > Vec_Buf'Last then
         --  Buffer is too small.
         raise Program_Error;
      end if;
      Vec_Len := Len;
      Vec_Res := Vec_Pos;
   end Init_Vec_Buf;

   procedure Append_Vec_Buf (Val : Uns32; Zx : Uns32) is
   begin
      pragma Assert (Vec_Len > 0);
      Vec_Buf (Vec_Pos) := (Aval => PLI_UINT32 (Val), Bval => PLI_UINT32 (Zx));
      Vec_Pos := Vec_Pos + 1;
      Vec_Len := Vec_Len - 1;
   end Append_Vec_Buf;

   function Return_Vec_Buf return P_Vpi_Vecval
   is
      function To_P_Vpi_Vecval is new Ada.Unchecked_Conversion
        (System.Address, P_Vpi_Vecval);
   begin
      return To_P_Vpi_Vecval (Vec_Buf (Vec_Res)'Address);
   end Return_Vec_Buf;

   procedure Get_Str_File (N : Node)
   is
      use Files_Map;
      Loc : constant Location_Type := Get_Location (N);
      File : Source_File_Entry;
      Pos : Source_Ptr;
   begin
      Location_To_File_Pos (Loc, File, Pos);
      Append_Str_Buf (Name_Table.Image (Get_File_Name (File)));
   end Get_Str_File;

   procedure Get_Str_Full_Name (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Debug =>
            --  This is not a scope.
            Get_Str_Full_Name (Get_Parent (N));
         when N_Module =>
            if Get_Parent (N) /= Null_Node then
               Get_Str_Full_Name (Get_Parent (N));
               Append_Str_Buf ('.');
            end if;
            Append_Str_Buf (Name_Table.Image (Get_Identifier (N)));
         when others =>
            Error_Kind ("get_str_full_name", N);
      end case;
   end Get_Str_Full_Name;

   procedure Get_Str_Name (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Module =>
            Append_Str_Buf (Name_Table.Image (Get_Identifier (N)));
         when others =>
            Error_Kind ("get_str_name", N);
      end case;
   end Get_Str_Name;

   procedure Get_Str_Var_Type (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Class =>
            Append_Str_Buf ("vpiClassVar");
         when N_Queue_Cst =>
            Append_Str_Buf ("vpiArrayVar");
         when N_Enum_Type =>
            Append_Str_Buf ("vpiEnumVar");
         when others =>
            Error_Kind ("get_str_var_type", N);
      end case;
   end Get_Str_Var_Type;

   procedure Get_Str_Type (N : Node) is
   begin
      case Get_Kind (N) is
         when N_This_Name =>
            Get_Str_Var_Type (Get_Expr_Type (N));
         when N_Method_Name =>
            Append_Str_Buf ("vpiMethodFuncCall");
         when others =>
            Error_Kind ("get_str_type", N);
      end case;
   end Get_Str_Type;

   function Get_Str (Prop : PLI_INT32_Prop; Ref : VpiHandle_Node_Type)
                    return Ghdl_C_String is
   begin
      Init_Str_Buf;
      case Prop is
         when VpiType =>
            Get_Str_Type (Ref.N);
         when VpiName =>
            Get_Str_Name (Ref.N);
         when VpiFullName =>
            Get_Str_Full_Name (Ref.N);
         when VpiFile =>
            Get_Str_File (Ref.N);
         when others =>
            raise Program_Error;
            return null;
      end case;
      Append_Str_Buf (NUL);
      return Return_Str_Buf;
   end Get_Str;

   Hex : constant array (0 .. 15) of Character := "0123456789abcdef";

   procedure Get_Value_Of_Lv_Value
     (Res : P_Vpi_Value; Data : Logvec_Ptr; Typ : Node)
   is
      W : constant Width_Type := Get_Type_Width (Typ);
      Last : constant Digit_Index := To_Last (W);
      Hi : Natural;
      Mask : Uns32;
   begin
      case Res.Format is
         when VpiBinStrVal =>
            Init_Str_Buf;
            Hi := Natural ((W - 1) mod 32);
            for I in reverse 0 .. Last loop
               declare
                  Val : constant Uns32 := Data (I).Val;
                  Zx : constant Uns32 := Data (I).Zx;
                  V : Uns32;
               begin
                  for J in reverse 0 .. Hi loop
                     V := (Shift_Right (Val, J) and 1)
                       + (Shift_Right (Zx, J) and 1) * 2;
                     Append_Str_Buf (Log_To_Char (Logic_Type'Val (V)));
                  end loop;
                  Hi := 31;
               end;
            end loop;
            Res.Value.Str := Return_Str_Buf;
            return;
         when VpiHexStrVal =>
            Init_Str_Buf;
            Hi := Natural (W mod 32);
            if Hi = 0 then
               Mask := 16#ffff_ffff#;
               Hi := 28;
            else
               Mask := Shift_Left (1, Hi) - 1;
               Hi := 4 * ((Hi + 3) / 4 - 1);
            end if;
            for I in reverse 0 .. Last loop
               declare
                  Val : constant Uns32 := Data (I).Val and Mask;
                  Zx : constant Uns32 := Data (I).Zx and Mask;
                  H_Val : Uns32;
                  H_Zx : Uns32;
                  C : Character;
               begin
                  loop
                     H_Zx := Shift_Right (Zx, Hi) and 15;
                     H_Val := Shift_Right (Val, Hi) and 15;
                     if H_Zx = 0 then
                        C := Hex (Natural (H_Val));
                     elsif H_Zx = 15 then
                        if H_Val = 15 then
                           C := 'x';
                        elsif H_Val = 0 then
                           C := 'z';
                        else
                           --  Mix of z and x.
                           C := 'X';
                        end if;
                     elsif (H_Val and H_Zx) = 0 then
                        --  0, 1 and Z.
                        C := 'Z';
                     else
                        C := 'X';
                     end if;
                     Append_Str_Buf (C);
                     exit when Hi = 0;
                     Hi := Hi - 4;
                  end loop;
                  Mask := not 0;
                  Hi := 28;
               end;
            end loop;
            Res.Value.Str := Return_Str_Buf;
            return;
         when VpiDecStrVal =>
            declare
               type Nat_Array is array (Natural range <>) of Nat8;
               Nlen : constant Natural := 1 + Natural ((W + 2) / 3);
               Pow2 : Nat_Array (1 .. Nlen);
               Res : Nat_Array (1 .. Nlen) := (others => 0);
               Plen : Natural;
               Carry : Nat8 range 0 .. 1;
               Only_Zero : Boolean;
            begin
               --  FIXME: unknown values ?
               Pow2 (1) := 1;
               Res (1) := 0;
               Plen := 1;
               for I in 0 .. Last loop
                  declare
                     Val : constant Uns32 := Data (I).Val;
                     Hi : Natural;
                  begin
                     if I = Last then
                        Hi := Natural ((W - 1) mod 32);
                     else
                        Hi := 31;
                     end if;
                     for J in 0 .. Hi loop
                        if (Shift_Right (Val, J) and 1) /= 0 then
                           --  Add Pow2 to Res.
                           Carry := 0;
                           for K in 1 .. Plen loop
                              Res (K) := Res (K) + Pow2 (K) + Carry;
                              if Res (K) >= 10 then
                                 Carry := 1;
                                 Res (K) := Res (K) - 10;
                              else
                                 Carry := 0;
                              end if;
                           end loop;
                           if Carry /= 0 then
                              Res (Plen + 1) := Carry;
                           end if;
                        end if;
                        --  Multiply pow2 by 2.
                        Carry := 0;
                        for K in 1 .. Plen loop
                           Pow2 (K) := 2 * Pow2 (K) + Carry;
                           if Pow2 (K) >= 10 then
                              Carry := 1;
                              Pow2 (K) := Pow2 (K) - 10;
                           else
                              Carry := 0;
                           end if;
                        end loop;
                        if Carry = 1 then
                           Plen := Plen + 1;
                           Pow2 (Plen) := 1;
                        end if;
                     end loop;
                  end;
               end loop;
               --  Write result.
               Init_Str_Buf;
               Only_Zero := True;
               for I in reverse 1 .. Plen loop
                  if not (Only_Zero and Res (I) = 0) then
                     Only_Zero := False;
                     Append_Str_Buf
                       (Character'Val (Character'Pos ('0') + Res (I)));
                  end if;
               end loop;
               if Only_Zero then
                  Append_Str_Buf ('0');
               end if;
            end;
            Res.Value.Str := Return_Str_Buf;
            return;
         when VpiRealVal =>
            declare
               Val : Pli_Double;
               Pow : Pli_Double;
               V : Uns32;
            begin
               Val := 0.0;
               Pow := 1.0;
               for I in 0 .. Last loop
                  V := Data (I).Val;
                  Val := Val + Pli_Double (V) * Pow;
                  Pow := Pow * 2.0**32;
               end loop;
               Res.Value.Real := Val;
            end;
         when VpiStringVal =>
            Init_Str_Buf;
            Hi := Natural (W mod 32);
            if Hi = 0 then
               Mask := 16#ffff_ffff#;
               Hi := 24;
            else
               Mask := Shift_Left (1, Hi) - 1;
               Hi := 8 * ((Hi + 7) / 8 - 1);
            end if;
            for I in reverse 0 .. Last loop
               declare
                  Val : constant Uns32 := Data (I).Val and Mask;
                  Zx : constant Uns32 := Data (I).Zx and Mask;
                  H_Val : Uns32;
                  H_Zx : Uns32;
                  C : Character;
               begin
                  loop
                     H_Zx := Shift_Right (Zx, Hi) and 255;
                     H_Val := Shift_Right (Val, Hi) and 255;
                     if H_Zx = 0 then
                        C := Character'Val (H_Val);
                     else
                        --  FIXME: how are x/z represented ?
                        C := ' ';
                     end if;
                     Append_Str_Buf (C);
                     exit when Hi = 0;
                     Hi := Hi - 8;
                  end loop;
                  Mask := not 0;
                  Hi := 24;
               end;
            end loop;
            Res.Value.Str := Return_Str_Buf;
            return;
         when VpiIntVal =>
            --  FIXME: overflow ? X/z ?
            if W >= 32 then
               Res.Value.Intege := To_PLI_INT32 (Data (0).Val);
            else
               Mask := Shift_Left (1, Natural (W)) - 1;
               Res.Value.Intege := To_PLI_INT32 (Data (0).Val and Mask);
            end if;
            return;
         when VpiVectorVal =>
            Init_Vec_Buf (Natural ((W + 31) / 32));
            Hi := Natural (W mod 32);
            if Hi = 0 then
               Mask := 16#ffff_ffff#;
            else
               Mask := Shift_Left (1, Hi) - 1;
            end if;
            for I in 0 .. Last loop
               Append_Vec_Buf (Data (I).Val and Mask, Data (I).Zx and Mask);
               Mask := not 0;
            end loop;
            Res.Value.Vector := Return_Vec_Buf;
            return;
         when others =>
            raise Program_Error;
      end case;
   end Get_Value_Of_Lv_Value;

   procedure Get_Value_Of_Bv_Value
     (Res : P_Vpi_Value; Data : Bitvec_Ptr; Typ : Node)
   is
      W : constant Width_Type := Get_Type_Width (Typ);
      Last : constant Digit_Index := To_Last (W);
      Hi : Natural;
      Mask : Uns32;
   begin
      case Res.Format is
         when VpiBinStrVal =>
            Init_Str_Buf;
            Hi := Natural ((W - 1) mod 32);
            for I in reverse 0 .. Last loop
               declare
                  Val : constant Uns32 := Data (I);
                  V : Uns32;
               begin
                  for J in reverse 0 .. Hi loop
                     V := Shift_Right (Val, J) and 1;
                     Append_Str_Buf (Bit_To_Char (Bit_Type'Val (V)));
                  end loop;
                  Hi := 31;
               end;
            end loop;
            Res.Value.Str := Return_Str_Buf;
            return;
         when VpiHexStrVal =>
            Init_Str_Buf;
            Hi := Natural (W mod 32);
            if Hi = 0 then
               Mask := 16#ffff_ffff#;
               Hi := 28;
            else
               Mask := Shift_Left (1, Hi) - 1;
               Hi := 4 * ((Hi + 3) / 4 - 1);
            end if;
            for I in reverse 0 .. Last loop
               declare
                  Val : constant Uns32 := Data (I) and Mask;
                  H_Val : Uns32;
               begin
                  loop
                     H_Val := Shift_Right (Val, Hi) and 15;
                     Append_Str_Buf (Hex (Natural (H_Val)));
                     exit when Hi = 0;
                     Hi := Hi - 4;
                  end loop;
                  Mask := not 0;
                  Hi := 28;
               end;
            end loop;
            Res.Value.Str := Return_Str_Buf;
            return;
         when VpiDecStrVal =>
            declare
               type Nat_Array is array (Natural range <>) of Nat8;
               Nlen : constant Natural := 1 + Natural ((W + 2) / 3);
               Pow2 : Nat_Array (1 .. Nlen);
               Res : Nat_Array (1 .. Nlen) := (others => 0);
               Plen : Natural;
               Carry : Nat8 range 0 .. 1;
               Only_Zero : Boolean;
            begin
               --  FIXME: unknown values ?
               Pow2 (1) := 1;
               Res (1) := 0;
               Plen := 1;
               for I in 0 .. Last loop
                  declare
                     Val : constant Uns32 := Data (I);
                     Hi : Natural;
                  begin
                     if I = Last then
                        Hi := Natural ((W - 1) mod 32);
                     else
                        Hi := 31;
                     end if;
                     for J in 0 .. Hi loop
                        if (Shift_Right (Val, J) and 1) /= 0 then
                           --  Add Pow2 to Res.
                           Carry := 0;
                           for K in 1 .. Plen loop
                              Res (K) := Res (K) + Pow2 (K) + Carry;
                              if Res (K) >= 10 then
                                 Carry := 1;
                                 Res (K) := Res (K) - 10;
                              else
                                 Carry := 0;
                              end if;
                           end loop;
                           if Carry /= 0 then
                              Res (Plen + 1) := Carry;
                           end if;
                        end if;
                        --  Multiply pow2 by 2.
                        Carry := 0;
                        for K in 1 .. Plen loop
                           Pow2 (K) := 2 * Pow2 (K) + Carry;
                           if Pow2 (K) >= 10 then
                              Carry := 1;
                              Pow2 (K) := Pow2 (K) - 10;
                           else
                              Carry := 0;
                           end if;
                        end loop;
                        if Carry = 1 then
                           Plen := Plen + 1;
                           Pow2 (Plen) := 1;
                        end if;
                     end loop;
                  end;
               end loop;
               --  Write result.
               Init_Str_Buf;
               Only_Zero := True;
               for I in reverse 1 .. Plen loop
                  if not (Only_Zero and Res (I) = 0) then
                     Only_Zero := False;
                     Append_Str_Buf
                       (Character'Val (Character'Pos ('0') + Res (I)));
                  end if;
               end loop;
               if Only_Zero then
                  Append_Str_Buf ('0');
               end if;
            end;
            Res.Value.Str := Return_Str_Buf;
            return;
         when VpiIntVal =>
            --  FIXME: overflow ? X/z ?
            if W >= 32 then
               Res.Value.Intege := To_PLI_INT32 (Data (0));
            else
               Mask := Shift_Left (1, Natural (W)) - 1;
               Res.Value.Intege := To_PLI_INT32 (Data (0) and Mask);
            end if;
            return;
         when VpiRealVal =>
            declare
               Val : Pli_Double;
               Pow : Pli_Double;
               V : Uns32;
            begin
               Val := 0.0;
               Pow := 1.0;
               for I in 0 .. Last loop
                  V := Data (I);
                  Val := Val + Pli_Double (V) * Pow;
                  Pow := Pow * 2.0**32;
               end loop;
               Res.Value.Real := Val;
            end;
         when VpiVectorVal =>
            Init_Vec_Buf (Natural ((W + 31) / 32));
            Hi := Natural (W mod 32);
            if Hi = 0 then
               Mask := 16#ffff_ffff#;
            else
               Mask := Shift_Left (1, Hi) - 1;
            end if;
            for I in 0 .. Last loop
               Append_Vec_Buf (Data (I) and Mask, 0);
               Mask := not 0;
            end loop;
            Res.Value.Vector := Return_Vec_Buf;
            return;
         when others =>
            raise Program_Error;
      end case;
   end Get_Value_Of_Bv_Value;

   procedure Get_Value_Of_Logic_Value (Res : P_Vpi_Value; Data : Logic_Ptr) is
   begin
      case Res.Format is
         when VpiBinStrVal
           | VpiDecStrVal =>
            Init_Str_Buf;
            Append_Str_Buf (Log_To_Char (Data.all));
            Res.Value.Str := Return_Str_Buf;
            return;
         when others =>
            raise Program_Error;
      end case;
   end Get_Value_Of_Logic_Value;

   procedure Get_Value_Of_Bit_Value (Res : P_Vpi_Value; Data : Bit_Ptr) is
   begin
      case Res.Format is
         when VpiBinStrVal
           | VpiHexStrVal
           | VpiDecStrVal =>
            Init_Str_Buf;
            Append_Str_Buf (Bit_To_Char (Data.all));
            Res.Value.Str := Return_Str_Buf;
            return;
         when others =>
            raise Program_Error;
      end case;
   end Get_Value_Of_Bit_Value;

   procedure Get_Value_Of_String (Res : P_Vpi_Value; Data : Sv_String) is
   begin
      case Res.Format is
         when VpiStringVal =>
            Init_Str_Buf;
            Append_Str_Buf (Get_String (Data));
            Res.Value.Str := Return_Str_Buf;
            return;
         when others =>
            raise Program_Error;
      end case;
   end Get_Value_Of_String;

   procedure Get_Value_Of_Real (Res : P_Vpi_Value; Data : Fp64) is
   begin
      case Res.Format is
         when VpiRealVal =>
            Res.Value.Real := Pli_Double (Data);
            return;
         when others =>
            raise Program_Error;
      end case;
   end Get_Value_Of_Real;

   --  Set VALUE from DATA (of type TYP).
   procedure Set_Vpi_Value
     (Value : P_Vpi_Value; Data : Data_Ptr; Typ : Node) is
   begin
      case Get_Kind (Typ) is
         when N_Log_Packed_Array_Cst =>
            Get_Value_Of_Lv_Value (Value, To_Logvec_Ptr (Data), Typ);
         when N_Bit_Packed_Array_Cst =>
            Get_Value_Of_Bv_Value (Value, To_Bitvec_Ptr (Data), Typ);
         when N_Logic_Type =>
            Get_Value_Of_Logic_Value (Value, To_Logic_Ptr (Data));
         when N_Bit_Type =>
            Get_Value_Of_Bit_Value (Value, To_Bit_Ptr (Data));
         when N_String_Type =>
            Get_Value_Of_String (Value, To_Sv_String_Ptr (Data).all);
         when N_Real_Type =>
            Get_Value_Of_Real (Value, To_Fp64_Ptr (Data).all);
         when N_Shortreal_Type =>
            Get_Value_Of_Real (Value, Fp64 (To_Fp32_Ptr (Data).all));
         when N_Enum_Type =>
            Set_Vpi_Value (Value, Data, Get_Enum_Base_Type (Typ));
         when others =>
            raise Program_Error;
      end case;
   end Set_Vpi_Value;

   procedure Get_Value (Ref : VpiHandle_Node_Type; Value : P_Vpi_Value) is
   begin
      case Get_Kind (Ref.N) is
         when N_String_Literal =>
            if Value.Format = VpiStringVal then
               --  Shortcut.
               Value.Value.Str := To_Ghdl_C_String
                 (Str_Table.String8_Address (Get_String_Id (Ref.N)));
               return;
            end if;
         when N_System_Call
           | N_Member_Name
           | N_Indexed_Name
           | N_Property_Name
           | N_This_Name
           | N_Bit_Select
           | N_Part_Select_Cst
           | N_Call
           | N_Binary_Op
           | N_Short_Circuit_Op
           | N_Unary_Op
           | N_Concatenation
           | N_Replication_Cst
           | N_Cond_Op
           | Nkinds_Inc_Dec
           | N_Name
           | N_Hierarchical
           | N_Number =>
            null;
         when others =>
            Error_Kind ("vpi.get_value", Ref.N);
      end case;
      declare
         Typ : constant Node := Get_Expr_Type (Ref.N);
         Ssize : constant Storage_Index := Get_Storage_Size (Typ);
         Res : Storage_Type (0 .. Ssize - 1);
      begin
         Executions.Execute_Expression
           (Current_Systf_Context.Frame, Res'Address, Ref.N);
         Set_Vpi_Value (Value, Res'Address, Typ);
         Executions.Finalize_Data (Res'Address, Typ);
      end;
   end Get_Value;

   --  Copy value of VALUE to DATA.
   procedure Extract_Value
     (Value : P_Vpi_Value; Data : Data_Ptr; Etype : Node) is
   begin
      case Value.Format is
         when VpiVectorVal =>
            case Get_Kind (Etype) is
               when N_Log_Packed_Array_Cst =>
                  declare
                     Dest : constant Logvec_Ptr := To_Logvec_Ptr (Data);
                     Width : constant Width_Type := Get_Type_Width (Etype);
                     Last : constant Digit_Index := To_Last (Width);
                     V : constant P_Vpi_Vecval := Value.Value.Vector;
                  begin
                     for I in 0 .. Last loop
                        Dest (I) := (Val => Uns32 (V (Natural (I)).Aval),
                                     Zx => Uns32 (V (Natural (I)).Bval));
                     end loop;
                  end;
               when others =>
                  Error_Kind ("extract_value(vectorval)", Etype);
            end case;
         when VpiStringVal =>
            case Get_Kind (Etype) is
               when N_Log_Packed_Array_Cst =>
                  declare
                     Dest : constant Logvec_Ptr := To_Logvec_Ptr (Data);
                     Width : constant Width_Type := Get_Type_Width (Etype);
                     Last : constant Digit_Index := To_Last (Width);
                     S : constant Ghdl_C_String := Value.Value.Str;
                     Didx : Digit_Index;
                     C : Character;
                  begin
                     --  Clear.
                     Set_0 (Dest, Width);
                     for I in S'Range loop
                        C := S (I);
                        exit when C = NUL;
                        Didx := Last - Digit_Index (I / 4);
                        Dest (Didx).Val := Dest (Didx).Val
                          or Shift_Left (Character'Pos (C),
                                         Natural ((I mod 4) * 8));
                     end loop;
                  end;
               when N_String_Type =>
                  declare
                     Dest : constant Sv_String_Ptr := To_Sv_String_Ptr (Data);
                  begin
                     Dest.all := New_Sv_String (Value.Value.Str);
                  end;
               when others =>
                  Error_Kind ("extract_value(stringval)", Etype);
            end case;
         when VpiHexStrVal =>
            case Get_Kind (Etype) is
               when N_Bit_Packed_Array_Cst =>
                  declare
                     Dest : constant Bitvec_Ptr := To_Bitvec_Ptr (Data);
                     Width : constant Width_Type := Get_Type_Width (Etype);
                     Last : constant Digit_Index := To_Last (Width);
                     S : constant Ghdl_C_String := Value.Value.Str;
                     Len : constant Natural := strlen (S);
                     C : Character;
                     V : Uns32;
                     Didx : Digit_Index;
                     Sh : Natural;
                  begin
                     Set_0 (Dest, Width);
                     Sh := 0;
                     Didx := 0;
                     for I in reverse 1 .. Len loop
                        C := S (I);
                        if C >= '0' and C <= '9' then
                           V := Character'Pos (C) - Character'Pos ('0');
                        elsif C >= 'a' and C <= 'f' then
                           V := Character'Pos (C) - Character'Pos ('a') + 10;
                        elsif C >= 'A' and C <= 'F' then
                           V := Character'Pos (C) - Character'Pos ('A') + 10;
                        else
                           --  Error ?
                           V := 0;
                        end if;
                        Dest (Didx) := Dest (Didx) or Shift_Left (V, Sh);
                        Sh := Sh + 4;
                        if Sh = 32 then
                           Sh := 0;
                           Didx := Didx + 1;
                           exit when Didx > Last;
                        end if;
                     end loop;
                  end;
               when others =>
                  Error_Kind ("extract_value(hexstrval)", Etype);
            end case;
         when VpiIntVal =>
            case Get_Kind (Etype) is
               when N_Bit_Packed_Array_Cst =>
                  declare
                     Dest : constant Bitvec_Ptr := To_Bitvec_Ptr (Data);
                     Width : constant Width_Type := Get_Type_Width (Etype);
                  begin
                     Set_0 (Dest, Width);
                     --  FIXME: signed/unsigned ?
                     Dest (0) := To_Uns32 (Value.Value.Intege);
                  end;
               when N_Log_Packed_Array_Cst =>
                  declare
                     Dest : constant Logvec_Ptr := To_Logvec_Ptr (Data);
                     Width : constant Width_Type := Get_Type_Width (Etype);
                  begin
                     Set_0 (Dest, Width);
                     --  FIXME: signed/unsigned ?
                     Dest (0).Val := To_Uns32 (Value.Value.Intege);
                  end;
               when others =>
                  Error_Kind ("extract_value(intval)", Etype);
            end case;
         when others =>
            raise Program_Error;
      end case;
   end Extract_Value;

   function Put_Value (Ref : VpiHandle_Node_Type;
                       Value : P_Vpi_Value;
                       Whe : P_Vpi_Time;
                       Flags : PLI_INT32_Put_Flags) return VpiHandle
   is
      pragma Unreferenced (Whe);
   begin
      case Get_Kind (Ref.N) is
         when N_System_Call
           | N_Dotted_Name
           | N_Indexed_Name
           | N_Bit_Select
           | N_Call
           | N_Binary_Op
           | N_Name
           | N_Hierarchical
           | N_Number =>
            declare
               Typ : constant Node := Get_Expr_Type (Ref.N);
               Ssize : constant Storage_Index := Get_Storage_Size (Typ);
               Res : Storage_Type (0 .. Ssize - 1);
            begin
               Extract_Value (Value, Res'Address, Typ);
               if Flags /= VpiNoDelay then
                  raise Program_Error;
               end if;
               Blocking_Assign.all
                 (Current_Systf_Context.Frame, Ref.N, Res'Address, Typ);
               return null;
            end;
         when others =>
            Error_Kind ("vpi.put_value", Ref.N);
      end case;
   end Put_Value;

   function Iterate (Typ : PLI_INT32_Obj; Ref : VpiHandle_Node_Type)
                    return VpiHandle is
   begin
      case Typ is
         when VpiArgument =>
            declare
               Res : constant Node := Get_Arguments (Ref.N);
            begin
               if Res = Null_Node then
                  return null;
               else
                  return new VpiHandle_Iterate_Node_Type'(N => Res);
               end if;
            end;
         when others =>
            raise Program_Error;
      end case;
   end Iterate;

   function Root_Vpi_Handle (Typ : PLI_INT32_Obj) return VpiHandle is
   begin
      case Typ is
         when VpiSysTfCall =>
            if Current_Systf_Context.Call = Null_Node then
               raise Program_Error;
            end if;
            case Get_Kind (Current_Systf_Context.Call) is
               when N_System_Call =>
                  return new VpiHandle_Tf_Call_Type'
                    (N => Current_Systf_Context.Call);
               when others =>
                  Error_Kind ("root_vpi_handle", Current_Systf_Context.Call);
            end case;
         when others =>
            raise Program_Error;
      end case;
   end Root_Vpi_Handle;

   function Vpi_Handle (Typ : PLI_INT32_Obj; Ref : VpiHandle)
                       return VpiHandle is
   begin
      if Ref = null then
         return Root_Vpi_Handle (Typ);
      else
         if Ref.all in VpiHandle_Node_Type'Class then
            return Handle (Typ, VpiHandle_Node_Type (Ref.all));
         else
            raise Program_Error;
         end if;
      end if;
   end Vpi_Handle;

   function Vpi_Handle_By_Index(Ref : VpiHandle; Idx : PLI_INT32)
                               return VpiHandle
   is
      N_Orig : constant Node := VpiHandle_Node_Type (Ref.all).N;
      N : constant Node := Strip_Name (N_Orig);
      Ntype : constant Node := Get_Type_Data_Type (N);
      Res_N : Node;
      Expr_N : Node;
   begin
      case Get_Kind (N) is
         when N_Var =>
            case Get_Kind (Ntype) is
               when N_Array_Cst =>
                  Res_N := Create_Node (N_Indexed_Name);
                  Set_Vpi_Location (Res_N);
                  Set_Expr_Type (Res_N, Get_Type_Element_Type (Ntype));
                  Set_Name (Res_N, N_Orig);

                  Expr_N := Create_Node (N_Number);
                  Set_Vpi_Location (Expr_N);
                  Set_Number_Lo_Val (Expr_N, Uns32 (Idx)); --  FIXME: convert.
                  Set_Number_Hi_Val (Expr_N, 0);
                  Set_Number_Lo_Zx (Expr_N, 0);
                  Set_Number_Hi_Zx (Expr_N, 0);
                  Set_Expr_Type (Expr_N, Verilog.Standard.Signed_Integer_Type);
                  Set_Expression (Res_N, Expr_N);
                  return new VpiHandle_Node_Type'(N => Res_N);
               when others =>
                  Error_Kind ("vpi_handle_by_index/var", Ntype);
            end case;
         when others =>
            Error_Kind ("vpi_handle_by_index", N);
      end case;
   end Vpi_Handle_By_Index;

   function Vpi_Iterate (Typ : PLI_INT32_Obj; Ref : VpiHandle)
                        return VpiHandle is
   begin
      if Ref = null then
         raise Program_Error;
      else
         if Ref.all in VpiHandle_Node_Type'Class then
            return Iterate (Typ, VpiHandle_Node_Type (Ref.all));
         else
            raise Program_Error;
         end if;
      end if;
   end Vpi_Iterate;

   function Vpi_Scan (Iter : VpiHandle) return VpiHandle
   is
      Res : VpiHandle;
   begin
      if Iter = null then
         raise Program_Error;
      end if;
      if Iter.all not in VpiHandle_Iterate_Type'Class then
         raise Program_Error;
      end if;
      Res := Scan (VpiHandle_Iterate_Type_Acc (Iter));
      if Res = null then
         Vpi_Free_Object (Iter);
      end if;
      return Res;
   end Vpi_Scan;

   function Root_Vpi_Get (Prop : PLI_INT32_Prop) return PLI_INT32 is
   begin
      case Prop is
         when VpiTimePrecision =>
            --  FIXME.
            return -9; --  1 ns
         when others =>
            raise Program_Error;
      end case;
   end Root_Vpi_Get;

   function Vpi_Get (Prop : PLI_INT32_Prop; Ref : VpiHandle)
                    return PLI_INT32 is
   begin
      if Ref = null then
         return Root_Vpi_Get (Prop);
      else
         return Get (Prop, Ref.all);
      end if;
   end Vpi_Get;

   function Vpi_Get_Str (Prop : PLI_INT32_Prop; Ref : VpiHandle)
                        return Ghdl_C_String is
   begin
      if Ref = null then
         raise Program_Error;
      else
         return Get_Str (Prop, Ref.all);
      end if;
   end Vpi_Get_Str;

   procedure Vpi_Get_Time (Obj : VpiHandle; T : P_Vpi_Time) is
   begin
      if Obj /= null then
         raise Program_Error;
      end if;
      case T.Typ is
         when VpiSimTime =>
            T.High := 0;
            T.Low := PLI_UINT32 (Simulation.Get_Current_Time);
         when others =>
            raise Program_Error;
      end case;
   end Vpi_Get_Time;

   procedure Vpip_Make_Systf_System_Defined (Ref : VpiHandle) is
   begin
      if Vpi_Free_Object (Ref) /= 0 then
         raise Internal_Error;
      end if;
      --  Nothing to do.
   end Vpip_Make_Systf_System_Defined;

   procedure Vpi_Get_Value (Expr : VpiHandle; Value : P_Vpi_Value) is
   begin
      if Expr = null then
         raise Program_Error;
      end if;

      Get_Value (Expr.all, Value);
   end Vpi_Get_Value;

   function Vpi_Put_Value (Obj : VpiHandle;
                           Value : P_Vpi_Value;
                           Whe : P_Vpi_Time;
                           Flags : PLI_INT32_Put_Flags) return VpiHandle is
   begin
      if Obj = null then
         raise Program_Error;
      end if;

      return Put_Value (Obj.all, Value, Whe, Flags);
   end Vpi_Put_Value;

   function Put_Value (Ref : VpiHandle_Tf_Call_Type;
                       Value : P_Vpi_Value;
                       Whe : P_Vpi_Time;
                       Flags : PLI_INT32_Put_Flags) return VpiHandle
   is
      pragma Unreferenced (Ref);
      --  TODO: Check type ?
      Dest : constant Logvec_Ptr :=
        To_Logvec_Ptr (Current_Systf_Context.Value);
   begin
      if Whe /= null or else Flags /= VpiNoDelay or else Value = null then
         raise Program_Error;
      end if;
      case Value.Format is
         when VpiTimeVal =>
            case Value.Value.Time.Typ is
               when VpiSimTime =>
                  Dest (0) := (Val => Uns32 (Value.Value.Time.Low),
                               Zx => 0);
                  Dest (1) := (Val => Uns32 (Value.Value.Time.High),
                               Zx => 0);
                  return null;
               when others =>
                  raise Program_Error;
            end case;
         when others =>
            Extract_Value (Value, Current_Systf_Context.Value,
                           Get_Expr_Type (Current_Systf_Context.Call));
            return null;
      end case;
   end Put_Value;

   procedure Free_Handle (Ref : VpiHandle_Tf_Call_Type) is
   begin
      null;
   end Free_Handle;

   procedure Vpi_Control (Op : PLI_INT32_Control; Status : PLI_INT32) is
   begin
      case Op is
         when VpiStop =>
            Vpip_Control := Natural (VpiStop);
            Interractive_Scope := Handle_Scope (Current_Systf_Context.Call);
            Interractive_Frame := Current_Systf_Context.Frame;
         when VpiFinish =>
            Vpip_Control := Natural (VpiFinish);
            Vpip_Exit_Status := Integer (Status / 16);
         when others =>
            raise Program_Error;
      end case;
   end Vpi_Control;

   type VpiHandle_Cb_ValueChange_Type is new VpiHandle_Type with record
      Data : aliased S_Cb_Data;
      Update : Update_El_Acc;
   end record;

   type VpiHandle_Cb_ValueChange_Type_Acc is
     access all VpiHandle_Cb_ValueChange_Type;

   function Register_Valuechange_Cb (Data : P_Cb_Data) return VpiHandle
   is
      N : Node;
      Upd : Update_El_Acc;
      Res : VpiHandle_Cb_ValueChange_Type_Acc;
   begin
      if Data.Obj.all not in VpiHandle_Node_Type'Class then
         raise Program_Error;
      end if;

      N := VpiHandle_Node_Type (Data.Obj.all).N;
      N := Strip_Name (N);
      case Get_Kind (N) is
         when Nkinds_Nets | N_Var =>
            null;
         when others =>
            raise Program_Error;
      end case;

      Res := new VpiHandle_Cb_ValueChange_Type'
        (Data => Data.all, Update => Upd);

      Upd := new Update_El'(Kind => Update_Vpi,
                            Next => null,
                            Cb => Res.Data'Access);
      Add_Updates (N, Upd);

      return VpiHandle (Res);
   end Register_Valuechange_Cb;

   type Cb_Data_Entry;
   type Cb_Data_Entry_Acc is access Cb_Data_Entry;
   type Cb_Data_Entry is record
      Data : aliased S_Cb_Data;
      Next : Cb_Data_Entry_Acc;
   end record;

   type Cb_Data_Chain is record
      First, Last : Cb_Data_Entry_Acc := null;
   end record;

   procedure Append (Chain : in out Cb_Data_Chain; Ent : Cb_Data_Entry_Acc) is
   begin
      if Chain.First = null then
         Chain.First := Ent;
      else
         Chain.Last.Next := Ent;
      end if;
      Chain.Last := Ent;
   end Append;

   type VpiHandle_Cb_Type is new VpiHandle_Type with record
      Cb_Data : Cb_Data_Entry_Acc;
   end record;

   procedure Free_Handle (Ref : VpiHandle_Cb_Type) is
   begin
      null;
   end Free_Handle;

   End_Of_Compile_Cb_Chain : Cb_Data_Chain;
   End_Of_Simulation_Cb_Chain : Cb_Data_Chain;
   Read_Only_Synch_Cb_Chain : Cb_Data_Chain;

   procedure Vpi_Register_Global_Cb
     (Chain : in out Cb_Data_Chain; Data : P_Cb_Data; Res : out VpiHandle)
   is
      Ent : Cb_Data_Entry_Acc;
   begin
      Ent := new Cb_Data_Entry'(Data => Data.all, Next => null);
      Append (Chain, Ent);
      Res := new VpiHandle_Cb_Type'(Cb_Data => Ent);
   end Vpi_Register_Global_Cb;

   function Vpi_Register_Cb (Data : P_Cb_Data) return VpiHandle
   is
      Res : VpiHandle;
   begin
      case Data.Reason is
         when CbEndOfCompile =>
            Vpi_Register_Global_Cb (End_Of_Compile_Cb_Chain, Data, Res);
         when CbEndOfSimulation =>
            Vpi_Register_Global_Cb (End_Of_Simulation_Cb_Chain, Data, Res);
         when CbValueChange =>
            Res := Register_Valuechange_Cb (Data);
         when CbReadOnlySynch =>
            --  FIXME: only 'now' is supported.
            if Data.Time = null
              or else Data.Time.Typ /= VpiSimTime
              or else Data.Time.High /= 0
              or else Data.Time.Low /= 0
            then
               raise Program_Error;
            end if;
            Vpi_Register_Global_Cb (Read_Only_Synch_Cb_Chain, Data, Res);
         when others =>
            raise Program_Error;
      end case;
      return Res;
   end Vpi_Register_Cb;

   function Vpi_Remove_Cb (Ref : VpiHandle) return PLI_INT32 is
   begin
      raise Program_Error;
      return 0;
   end Vpi_Remove_Cb;

   procedure Execute_Cb (Cb : P_Cb_Data)
   is
      Res : PLI_INT32;
      pragma Unreferenced (Res);
   begin
      Res := Cb.Cb_Rtn (Cb);
   end Execute_Cb;

   procedure Call_Cb_Chain (Chain : Cb_Data_Chain)
   is
      El : Cb_Data_Entry_Acc;
   begin
      El := Chain.First;
      while El /= null loop
         Execute_Cb (El.Data'Access);
         El := El.Next;
      end loop;
   end Call_Cb_Chain;

   procedure Call_Cb_Chain_And_Free (Chain : in out Cb_Data_Chain)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Cb_Data_Entry, Cb_Data_Entry_Acc);
      El, Next : Cb_Data_Entry_Acc;
   begin
      El := Chain.First;
      Chain := (null, null);
      while El /= null loop
         Execute_Cb (El.Data'Access);
         Next := El.Next;
         Free (El);
         El := Next;
      end loop;
   end Call_Cb_Chain_And_Free;

   procedure End_Of_Compile is
   begin
      Call_Cb_Chain (End_Of_Compile_Cb_Chain);
   end End_Of_Compile;

   procedure Execute_Read_Only_Synch_Cb is
   begin
      Call_Cb_Chain_And_Free (Read_Only_Synch_Cb_Chain);
   end Execute_Read_Only_Synch_Cb;

   procedure End_Of_Simulation is
   begin
      Call_Cb_Chain_And_Free (End_Of_Simulation_Cb_Chain);
   end End_Of_Simulation;

   type Vpihandle_Systask_Type is new VpiHandle_Type with record
      Id : Sys_Tf_Id;
   end record;
   procedure Free_Handle (Ref : Vpihandle_Systask_Type) is null;

   type Vpihandle_Sysfunc_Type is new VpiHandle_Type with record
      Id : Sys_Tf_Id;
   end record;
   procedure Free_Handle (Ref : Vpihandle_Sysfunc_Type) is null;

   function Find_Systf (Name : Name_Id; Typ : PLI_INT32_Systf)
                       return Sys_Tf_Id
   is
      Res : Sys_Tf_Id;
   begin
      Res := Systf_Maps.Get_Element (Systf_Map, Name);

      if Res = No_Sys_Tf_Id then
         return No_Sys_Tf_Id;
      end if;

      if Res < Sys_Tf_User_Id then
         --  Reserved id (currently only function except $cast).
         if Typ /= VpiSysFunc and then Res /= Sys_Tf_Cast_Id then
            return Bad_Sys_Tf_Id;
         else
            return Res;
         end if;
      end if;
      if Systf_Table.Table (Res).Typ /= Typ then
         return Bad_Sys_Tf_Id;
      else
         return Res;
      end if;
   end Find_Systf;

   function Find_Sysfunc (Name : Name_Id) return Sys_Tf_Id is
   begin
      return Find_Systf (Name, VpiSysFunc);
   end Find_Sysfunc;

   function Find_Systask (Name : Name_Id) return Sys_Tf_Id is
   begin
      return Find_Systf (Name, VpiSysTask);
   end Find_Systask;

   function Get_Sysfunc_Type (Id : Sys_Tf_Id) return PLI_INT32_FuncType is
   begin
      return Systf_Table.Table (Id).Func_Type;
   end Get_Sysfunc_Type;

   function Call_Systf_Sizetf (Id : Sys_Tf_Id) return PLI_INT32
   is
      Ent : Systf_Entry renames Systf_Table.Table (Id);
   begin
      return Ent.Sizetf (Ent.User_Data);
   end Call_Systf_Sizetf;

   procedure Call_Systf_Compiletf (Id : Sys_Tf_Id; N : Node)
   is
      Prev_Context : constant Systf_Context := Current_Systf_Context;
      Ent : Systf_Entry renames Systf_Table.Table (Id);
      Res : PLI_INT32;
      pragma Unreferenced (Res);
   begin
      Current_Systf_Context :=
        (Call => N, Value => No_Data_Ptr, Frame => null);
      Res := Ent.Compiletf (Ent.User_Data);
      Current_Systf_Context := Prev_Context;
   end Call_Systf_Compiletf;

   procedure Call_Systask_Calltf (Frame : Frame_Ptr; Id : Sys_Tf_Id; N : Node)
   is
      Prev_Context : constant Systf_Context := Current_Systf_Context;
      Ent : Systf_Entry renames Systf_Table.Table (Id);
      Res : PLI_INT32;
      pragma Unreferenced (Res);
   begin
      Current_Systf_Context :=
        (Call => N, Value => No_Data_Ptr, Frame => Frame);
      Res := Ent.Calltf (Ent.User_Data);
      Current_Systf_Context := Prev_Context;
   end Call_Systask_Calltf;

   procedure Call_Sysfunc_Calltf
     (Frame : Frame_Ptr; Id : Sys_Tf_Id; N : Node; Data: Data_Ptr)
   is
      Prev_Context : constant Systf_Context := Current_Systf_Context;
      Ent : Systf_Entry renames Systf_Table.Table (Id);
      Res : PLI_INT32;
      pragma Unreferenced (Res);
   begin
      Current_Systf_Context := (Call => N, Value => Data, Frame => Frame);
      Res := Ent.Calltf (Ent.User_Data);
      Current_Systf_Context := Prev_Context;
   end Call_Sysfunc_Calltf;

   function Vpi_Register_Systf (Sd : P_Vpi_Systf_Data) return VpiHandle
   is
      use Name_Table;
      Length : Natural;
      Name : Name_Id;
      C : Character;
      Id : Sys_Tf_Id;
   begin
      pragma Assert (Sd.Typ = VpiSysTask or Sd.Typ = VpiSysFunc);

      --  Convert to Name_Id.
      for I in Positive loop
         C := Sd.Tfname (I);
         if C = NUL then
            Length := I - 1;
            exit;
         end if;
      end loop;
      pragma Assert (Length > 1);
      pragma Assert (Sd.Tfname (1) = '$');
      Name := Get_Identifier (Sd.Tfname (2 .. Length));

      Id := Find_Systask (Name);
      if Id /= No_Sys_Tf_Id then
         --  Redefinition.  Note that we don't know if this is a function
         --  or a task.  FIXME: share same namespace ?
         raise Program_Error;
      else
         Systf_Table.Increment_Last;
         Id := Systf_Table.Last;
      end if;
      Systf_Maps.Set_Element (Systf_Map, Name, Id);

      Systf_Table.Table (Id) := (Typ => Sd.Typ,
                                 Func_Type => Sd.Sysfunctype,
                                 Calltf => Sd.Calltf,
                                 Compiletf => Sd.Compiletf,
                                 Sizetf => Sd.Sizetf,
                                 User_Data => Sd.User_Data);

      case Sd.Typ is
         when VpiSysTask =>
            return new Vpihandle_Systask_Type'(Id => Id);
         when VpiSysFunc =>
            return new Vpihandle_Sysfunc_Type'(Id => Id);
         when others =>
            raise Program_Error;
      end case;
   end Vpi_Register_Systf;

   --  System entries to register.
   procedure Sys_Register
   is
   begin
      null;
   end Sys_Register;

--   procedure Vpip_Mcd_Init (Log : System.Address);
--   pragma Import (C, Vpip_Mcd_Init);

   gnat_argc : Integer;
   gnat_argv : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);

   procedure Initialize
   is
      use Files_Map;
      Vpi_Source_File : Source_File_Entry;
   begin
      --  Initialize vlog info.
      Vlog_Info := (Argc => gnat_argc,
                    Argv => To_Argv_Type (gnat_argv),
                    Product => To_Ghdl_C_String (Product_String'Address),
                    Version => To_Ghdl_C_String (Version_String'Address));

      Vpi_Source_File := Create_Virtual_Source_File
        (Name_Table.Get_Identifier ("*vpi*"));
      Vpi_Location := File_To_Location (Vpi_Source_File);

      --  Initialize mcd (can be used during compilation).
--      Vpip_Mcd_Init (System.Null_Address);

      Systf_Maps.Init (Systf_Map);

      --  Reserve special TF.
      Systf_Maps.Set_Element
        (Systf_Map, Std_Names.Name_Signed, Sys_Tf_Signed_Id);
      Systf_Maps.Set_Element
        (Systf_Map, Std_Names.Name_Unsigned, Sys_Tf_Unsigned_Id);
      Systf_Maps.Set_Element
        (Systf_Map, Std_Names.Name_Cast, Sys_Tf_Cast_Id);
      Systf_Maps.Set_Element
        (Systf_Map, Std_Names.Name_Typename, Sys_Tf_Typename_Id);
      Systf_Maps.Set_Element
        (Systf_Map, Std_Names.Name_Left, Sys_Tf_Left_Id);
      Systf_Maps.Set_Element
        (Systf_Map, Std_Names.Name_Right, Sys_Tf_Right_Id);
      Systf_Maps.Set_Element
        (Systf_Map, Std_Names.Name_Low, Sys_Tf_Low_Id);
      Systf_Maps.Set_Element
        (Systf_Map, Std_Names.Name_High, Sys_Tf_High_Id);
      Systf_Maps.Set_Element
        (Systf_Map, Std_Names.Name_Size, Sys_Tf_Size_Id);

      --  Register system TF.
      Sys_Register;
   end Initialize;
end Verilog.Vpi;
