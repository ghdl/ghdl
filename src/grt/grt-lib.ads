--  GHDL Run Time (GRT) -  misc subprograms.
--  Copyright (C) 2002 - 2016 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;

package Grt.Lib is
   procedure Ghdl_Memcpy
     (Dest : Ghdl_Ptr; Src : Ghdl_Ptr; Size : Ghdl_Index_Type);

   procedure Ghdl_Assert_Failed
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr);
   procedure Ghdl_Ieee_Assert_Failed
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr);

   procedure Ghdl_Psl_Assert_Failed
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr);

   procedure Ghdl_Psl_Assume_Failed (Loc : Ghdl_Location_Ptr);

   --  Called when a sequence is covered (in a cover directive)
   procedure Ghdl_Psl_Cover
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr);

   procedure Ghdl_Psl_Cover_Failed
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr);

   procedure Ghdl_Report
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr);

   --  Bound / Direction error.
   procedure Ghdl_Bound_Check_Failed (Filename : Ghdl_C_String;
                                      Line: Ghdl_I32);
   procedure Ghdl_Direction_Check_Failed (Filename : Ghdl_C_String;
                                          Line: Ghdl_I32);
   procedure Ghdl_Access_Check_Failed;

   procedure Ghdl_Integer_Index_Check_Failed
     (Filename : Ghdl_C_String;
      Line     : Ghdl_I32;
      Val      : Std_Integer;
      Rng      : Std_Integer_Range_Ptr);

   --  Program error has occurred:
   --  * configuration of an already configured block.
   procedure Ghdl_Program_Error (Filename : Ghdl_C_String;
                                 Line : Ghdl_I32;
                                 Code : Ghdl_Index_Type);

   function Ghdl_I32_Exp (V : Ghdl_I32; E : Std_Integer) return Ghdl_I32;
   function Ghdl_I64_Exp (V : Ghdl_I64; E : Std_Integer) return Ghdl_I64;

   --  Called before allocation of large (complex) objects.
   procedure Ghdl_Check_Stack_Allocation (Size : Ghdl_Index_Type);

   function Ghdl_Malloc (Size : Ghdl_Index_Type) return Ghdl_Ptr;

   --  Allocate and clear SIZE bytes.
   function Ghdl_Malloc0 (Size : Ghdl_Index_Type) return Ghdl_Ptr;

   procedure Ghdl_Deallocate (Ptr : Ghdl_Ptr);

   function Ghdl_Real_Exp (X : Ghdl_Real; Exp : Ghdl_I32)
     return Ghdl_Real;

   type Ghdl_Std_Ulogic_Boolean_Array_Type is array (Ghdl_E8 range 0 .. 8)
     of Ghdl_B1;

   Ghdl_Std_Ulogic_To_Boolean_Array :
     constant Ghdl_Std_Ulogic_Boolean_Array_Type := (False, --  U
                                                     False, --  X
                                                     False, --  0
                                                     True,  --  1
                                                     False, --  Z
                                                     False, --  W
                                                     False, --  L
                                                     True,  --  H
                                                     False  --  -
                                                    );

   function Textio_Read_Real (Str : Std_String_Ptr) return Ghdl_F64;

   procedure Textio_Write_Real (Str : Std_String_Ptr;
                                Len : Std_Integer_Acc;
                                V : Ghdl_F64;
                                Ndigits : Std_Integer);

   function Ghdl_Get_Resolution_Limit return Std_Time;

   procedure Ghdl_Control_Simulation
     (Stop : Ghdl_B1; Has_Status : Ghdl_B1; Status : Std_Integer);
private
   pragma Export (C, Ghdl_Memcpy, "__ghdl_memcpy");

   pragma Export (C, Ghdl_Assert_Failed, "__ghdl_assert_failed");
   pragma Export (C, Ghdl_Ieee_Assert_Failed, "__ghdl_ieee_assert_failed");
   pragma Export (C, Ghdl_Psl_Assume_Failed, "__ghdl_psl_assume_failed");
   pragma Export (C, Ghdl_Psl_Assert_Failed, "__ghdl_psl_assert_failed");
   pragma Export (C, Ghdl_Psl_Cover, "__ghdl_psl_cover");
   pragma Export (C, Ghdl_Psl_Cover_Failed, "__ghdl_psl_cover_failed");
   pragma Export (C, Ghdl_Report, "__ghdl_report");

   pragma Export (C, Ghdl_Bound_Check_Failed,
                  "__ghdl_bound_check_failed");
   pragma Export (C, Ghdl_Direction_Check_Failed,
                  "__ghdl_direction_check_failed");
   pragma Export (C, Ghdl_Integer_Index_Check_Failed,
                  "__ghdl_integer_index_check_failed");
   pragma Export (C, Ghdl_Access_Check_Failed,
                  "__ghdl_access_check_failed");

   pragma Export (C, Ghdl_Program_Error, "__ghdl_program_error");

   pragma Export (C, Ghdl_Check_Stack_Allocation,
                  "__ghdl_check_stack_allocation");

   pragma Export (C, Ghdl_Malloc, "__ghdl_malloc");
   pragma Export (C, Ghdl_Malloc0, "__ghdl_malloc0");
   pragma Export (C, Ghdl_Deallocate, "__ghdl_deallocate");

   pragma Export (C, Ghdl_I32_Exp, "__ghdl_i32_exp");
   pragma Export (C, Ghdl_I64_Exp, "__ghdl_i64_exp");
   pragma Export (C, Ghdl_Real_Exp, "__ghdl_real_exp");

   pragma Export (C, Ghdl_Std_Ulogic_To_Boolean_Array,
                  "__ghdl_std_ulogic_to_boolean_array");

   pragma Export (C, Textio_Read_Real,
                  "std__textio__textio_read_real");
   pragma Export (C, Textio_Write_Real,
                  "std__textio__textio_write_real");

   pragma Export (C, Ghdl_Get_Resolution_Limit,
                  "std__env__get_resolution_limit");
   pragma Export (Ada, Ghdl_Control_Simulation,
                  "std__env__control_simulation");
end Grt.Lib;
