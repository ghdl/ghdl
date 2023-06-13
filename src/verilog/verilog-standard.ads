--  Verilog builtin types
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

with Verilog.Nodes; use Verilog.Nodes;

package Verilog.Standard is
   --  The fundamental types.
   Unsigned_Logic_Type : constant Node := 2;
   Signed_Logic_Type   : constant Node := 3;
   Unsigned_Bit_Type   : constant Node := 4;
   Signed_Bit_Type     : constant Node := 5;

   --  Real types.
   --  1800-2017 6.12 Real, shortreal, and realtime data types
   --  The REALTIME declarations shall be treated synonymously with real
   --  declarations and can be used interchangeably.
   Real_Type              : constant Node := 6;
   Realtime_Type          : constant Node := 6;
   Shortreal_Type         : constant Node := 7;

   --  The integer types.
   Unsigned_Byte_Type     : constant Node := 8;
   Signed_Byte_Type       : constant Node := 9;
   Unsigned_Shortint_Type : constant Node := 10;
   Signed_Shortint_Type   : constant Node := 11;
   Unsigned_Int_Type      : constant Node := 12;
   Signed_Int_Type        : constant Node := 13;
   Unsigned_Longint_Type  : constant Node := 14;
   Signed_Longint_Type    : constant Node := 15;
   Unsigned_Integer_Type  : constant Node := 16;
   Signed_Integer_Type    : constant Node := 17;
   Unsigned_Time_Type     : constant Node := 18;
   Signed_Time_Type       : constant Node := 19;

   --  Typedef-s for logic.
   Signed_Logic_Typedef     : constant Node := 20;
   Unsigned_Logic_Typedef   : constant Node := 21;
   Integer_Typedef          : constant Node := 22;
   Signed_Integer_Typedef   : constant Node := 23;
   Unsigned_Integer_Typedef : constant Node := 24;
   Reg_Typedef              : constant Node := 25;
   Signed_Reg_Typedef       : constant Node := 26;
   Unsigned_Reg_Typedef     : constant Node := 27;
   Time_Typedef             : constant Node := 28;
   Signed_Time_Typedef      : constant Node := 29;
   Unsigned_Time_Typedef    : constant Node := 30;

   --  Typedef-s for bit.
   Signed_Bit_Typedef        : constant Node := 31;
   Unsigned_Bit_Typedef      : constant Node := 32;
   Byte_Typedef              : constant Node := 33;
   Signed_Byte_Typedef       : constant Node := 34;
   Unsigned_Byte_Typedef     : constant Node := 35;
   Shortint_Typedef          : constant Node := 36;
   Signed_Shortint_Typedef   : constant Node := 37;
   Unsigned_Shortint_Typedef : constant Node := 38;
   Int_Typedef               : constant Node := 39;
   Signed_Int_Typedef        : constant Node := 40;
   Unsigned_Int_Typedef      : constant Node := 41;
   Longint_Typedef           : constant Node := 42;
   Signed_Longint_Typedef    : constant Node := 43;
   Unsigned_Longint_Typedef  : constant Node := 44;

   --  Typedef of logic types used for implicit (ie missing) type.
   Implicit_Typedef          : constant Node := 45;
   Implicit_Unsigned_Typedef : constant Node := 46;
   Implicit_Signed_Typedef   : constant Node := 47;

   Real_Typedef              : constant Node := 48;
   Shortreal_Typedef         : constant Node := 49;
   Realtime_Typedef          : constant Node := 50;

   --  Other types.
   First_Other_Type       : constant Node := 51;
   String_Type            : constant Node := First_Other_Type + 0;
   Chandle_Type           : constant Node := First_Other_Type + 1;
   Event_Type             : constant Node := First_Other_Type + 2;
   Void_Type              : constant Node := First_Other_Type + 3;
   Error_Type             : constant Node := First_Other_Type + 4;
   Null_Type              : constant Node := First_Other_Type + 5;

   String_Typedef         : constant Node := First_Other_Type + 6;
   Chandle_Typedef        : constant Node := First_Other_Type + 7;
   Event_Typedef          : constant Node := First_Other_Type + 8;
   Void_Typedef           : constant Node := First_Other_Type + 9;
   Error_Typedef          : constant Node := First_Other_Type + 10;

   First_Wildcard_Type    : constant Node := First_Other_Type + 11;
   Wildcard_Type_Type     : constant Node := First_Wildcard_Type + 0;
   Wildcard_Element_Type  : constant Node := First_Wildcard_Type + 1;
   Wildcard_Index_Type    : constant Node := First_Wildcard_Type + 2;
   Wildcard_Element_Queue_Type : constant Node := First_Wildcard_Type + 3;
   Wildcard_Index_Queue_Type   : constant Node := First_Wildcard_Type + 4;

   Wildcard_Type_Typedef    : constant Node := First_Wildcard_Type + 5;
   Wildcard_Element_Typedef : constant Node := First_Wildcard_Type + 6;
   Wildcard_Index_Typedef   : constant Node := First_Wildcard_Type + 7;
   Wildcard_Element_Queue_Typedef : constant Node := First_Wildcard_Type + 8;
   Wildcard_Index_Queue_Typedef   : constant Node := First_Wildcard_Type + 9;
   Last_Wildcard_Type       : constant Node := Wildcard_Index_Queue_Typedef;

   --  Builtin methods (2 nodes per function).
   First_Builtin_Method    : constant Node := Last_Wildcard_Type + 1;

   First_String_Method     : constant Node := First_Builtin_Method;
   String_Len_Method       : constant Node := First_String_Method + 0;
   String_Substr_Method    : constant Node := First_String_Method + 2;
   String_Atoi_Method      : constant Node := First_String_Method + 4;
   String_Atohex_Method    : constant Node := First_String_Method + 6;
   String_Atooct_Method    : constant Node := First_String_Method + 8;
   String_Atobin_Method    : constant Node := First_String_Method + 10;
   String_Itoa_Method      : constant Node := First_String_Method + 12;
   String_Toupper_Method   : constant Node := First_String_Method + 14;
   String_Tolower_Method   : constant Node := First_String_Method + 16;
   Last_String_Method      : constant Node := String_Tolower_Method;

   First_Enum_Method       : constant Node := Last_String_Method + 2;
   Enum_First_Method       : constant Node := First_Enum_Method + 0;
   Enum_Last_Method        : constant Node := First_Enum_Method + 2;
   Enum_Next_Method        : constant Node := First_Enum_Method + 4;
   Enum_Prev_Method        : constant Node := First_Enum_Method + 6;
   Enum_Num_Method         : constant Node := First_Enum_Method + 8;
   Enum_Name_Method        : constant Node := First_Enum_Method + 10;
   Last_Enum_Method        : constant Node := Enum_Name_Method;

   First_Queue_Method        : constant Node := Last_Enum_Method + 2;
   Queue_Size_Method         : constant Node := First_Queue_Method + 0;
   Queue_Insert_Method       : constant Node := First_Queue_Method + 2;
   Queue_Delete_Method       : constant Node := First_Queue_Method + 4;
   Queue_Delete_Index_Method : constant Node := First_Queue_Method + 6;
   Queue_Pop_Front_Method    : constant Node := First_Queue_Method + 8;
   Queue_Pop_Back_Method     : constant Node := First_Queue_Method + 10;
   Queue_Push_Front_Method   : constant Node := First_Queue_Method + 12;
   Queue_Push_Back_Method    : constant Node := First_Queue_Method + 14;
   Last_Queue_Method         : constant Node := Queue_Push_Back_Method;

   First_Associative_Method  : constant Node := Last_Queue_Method + 2;
   Associative_Num_Method    : constant Node := First_Associative_Method + 0;
   Associative_Size_Method   : constant Node := First_Associative_Method + 2;
   Associative_Delete_Method : constant Node := First_Associative_Method + 4;
   Associative_Delete_Index_Method : constant Node :=
     First_Associative_Method + 6;
   Associative_Exists_Method : constant Node := First_Associative_Method + 8;
   Associative_First_Method  : constant Node := First_Associative_Method + 10;
   Associative_Last_Method   : constant Node := First_Associative_Method + 12;
   Associative_Next_Method   : constant Node := First_Associative_Method + 14;
   Associative_Prev_Method   : constant Node := First_Associative_Method + 16;
   Last_Associative_Method   : constant Node := Associative_Prev_Method;

   First_Dynamic_Method      : constant Node := Last_Associative_Method + 2;
   Dynamic_Size_Method       : constant Node := First_Dynamic_Method + 0;
   Dynamic_Delete_Method     : constant Node := First_Dynamic_Method + 2;
   Last_Dynamic_Method       : constant Node := Dynamic_Delete_Method;

   --  Array manipulation methods
   First_Array_Method        : constant Node := Last_Dynamic_Method + 2;
   Array_Find_Method         : constant Node := First_Array_Method + 0;
   Array_Find_Index_Method   : constant Node := First_Array_Method + 2;
   Array_Find_First_Index_Method   : constant Node := First_Array_Method + 4;
   Array_Min_Method          : constant Node := First_Array_Method + 6;
   Array_Max_Method          : constant Node := First_Array_Method + 8;
   Array_Unique_Method       : constant Node := First_Array_Method + 10;
   Array_Sort_Method         : constant Node := First_Array_Method + 12;
   Last_Array_Method         : constant Node := Array_Sort_Method;

   Event_Triggered_Method    : constant Node := Last_Array_Method + 2;
   Rand_Mode_Func_Method     : constant Node := Last_Array_Method + 4;
   Rand_Mode_Task_Method     : constant Node := Last_Array_Method + 6;

   Last_Builtin_Method       : constant Node := Rand_Mode_Task_Method;

   --  The base root class.
   Base_Root_Class           : constant Node := Last_Builtin_Method + 2;

   --  Base root class methods.
   Base_Randomize_Method      : constant Node := Base_Root_Class + 2;
   Base_Pre_Randomize_Method  : constant Node := Base_Root_Class + 4;
   Base_Post_Randomize_Method : constant Node := Base_Root_Class + 6;
   Base_Srandom_Method        : constant Node := Base_Root_Class + 8;
   Base_Get_Randstate_Method  : constant Node := Base_Root_Class + 10;
   Base_Set_Randstate_Method  : constant Node := Base_Root_Class + 12;

   --  The std unit.
   Built_In_Std_Unit : Node;
   Built_In_Std_Package : Node;
end Verilog.Standard;
