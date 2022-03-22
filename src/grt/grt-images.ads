--  GHDL Run Time (GRT) -  'image subprograms.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
with Grt.Rtis; use Grt.Rtis;

package Grt.Images is
   --  For all images procedures, the result is allocated on the secondary
   --  stack.

   procedure Ghdl_Image_B1
     (Res : Std_String_Ptr; Val : Ghdl_B1; Rti : Ghdl_Rti_Access);
   procedure Ghdl_Image_E8
     (Res : Std_String_Ptr; Val : Ghdl_E8; Rti : Ghdl_Rti_Access);
   procedure Ghdl_Image_E32
     (Res : Std_String_Ptr; Val : Ghdl_E32; Rti : Ghdl_Rti_Access);
   procedure Ghdl_Image_I32 (Res : Std_String_Ptr; Val : Ghdl_I32);
   procedure Ghdl_Image_I64 (Res : Std_String_Ptr; Val : Ghdl_I64);
   procedure Ghdl_Image_F64 (Res : Std_String_Ptr; Val : Ghdl_F64);
   procedure Ghdl_Image_P64
     (Res : Std_String_Ptr; Val : Ghdl_I64; Rti : Ghdl_Rti_Access);
   procedure Ghdl_Image_P32
     (Res : Std_String_Ptr; Val : Ghdl_I32; Rti : Ghdl_Rti_Access);

   procedure Ghdl_To_String_I32 (Res : Std_String_Ptr; Val : Ghdl_I32);
   procedure Ghdl_To_String_I64 (Res : Std_String_Ptr; Val : Ghdl_I64);
   procedure Ghdl_To_String_F64 (Res : Std_String_Ptr; Val : Ghdl_F64);
   procedure Ghdl_To_String_F64_Digits
     (Res : Std_String_Ptr; Val : Ghdl_F64; Nbr_Digits : Ghdl_I32);
   procedure Ghdl_To_String_F64_Format
     (Res : Std_String_Ptr; Val : Ghdl_F64; Format : Std_String_Ptr);
   procedure Ghdl_To_String_B1
     (Res : Std_String_Ptr; Val : Ghdl_B1; Rti : Ghdl_Rti_Access);
   procedure Ghdl_To_String_E8
     (Res : Std_String_Ptr; Val : Ghdl_E8; Rti : Ghdl_Rti_Access);
   procedure Ghdl_To_String_E32
     (Res : Std_String_Ptr; Val : Ghdl_E32; Rti : Ghdl_Rti_Access);
   procedure Ghdl_To_String_Char
     (Res : Std_String_Ptr; Val : Std_Character);
   procedure Ghdl_To_String_P32
     (Res : Std_String_Ptr; Val : Ghdl_I32; Rti : Ghdl_Rti_Access);
   procedure Ghdl_To_String_P64
     (Res : Std_String_Ptr; Val : Ghdl_I64; Rti : Ghdl_Rti_Access);
   procedure Ghdl_Time_To_String_Unit
     (Res : Std_String_Ptr;
      Val : Std_Time; Unit : Std_Time; Rti : Ghdl_Rti_Access);
   procedure Ghdl_Array_Char_To_String_B1
     (Res : Std_String_Ptr;
      Val : Ghdl_Ptr; Len : Ghdl_Index_Type; Rti : Ghdl_Rti_Access);
   procedure Ghdl_Array_Char_To_String_E8
     (Res : Std_String_Ptr;
      Val : Ghdl_Ptr; Len : Ghdl_Index_Type; Rti : Ghdl_Rti_Access);
   procedure Ghdl_Array_Char_To_String_E32
     (Res : Std_String_Ptr;
      Val : Ghdl_Ptr; Len : Ghdl_Index_Type; Rti : Ghdl_Rti_Access);

   procedure Ghdl_BV_To_Ostring (Res : Std_String_Ptr;
                                 Base : Std_Bit_Vector_Basep;
                                 Len : Ghdl_Index_Type);
   procedure Ghdl_BV_To_Hstring (Res : Std_String_Ptr;
                                 Base : Std_Bit_Vector_Basep;
                                 Len : Ghdl_Index_Type);

private
   pragma Export (Ada, Ghdl_Image_B1, "__ghdl_image_b1");
   pragma Export (C, Ghdl_Image_E8, "__ghdl_image_e8");
   pragma Export (C, Ghdl_Image_E32, "__ghdl_image_e32");
   pragma Export (C, Ghdl_Image_I32, "__ghdl_image_i32");
   pragma Export (C, Ghdl_Image_I64, "__ghdl_image_i64");
   pragma Export (C, Ghdl_Image_F64, "__ghdl_image_f64");
   pragma Export (C, Ghdl_Image_P64, "__ghdl_image_p64");
   pragma Export (C, Ghdl_Image_P32, "__ghdl_image_p32");

   pragma Export (C, Ghdl_To_String_I32, "__ghdl_to_string_i32");
   pragma Export (C, Ghdl_To_String_I64, "__ghdl_to_string_i64");
   pragma Export (C, Ghdl_To_String_F64, "__ghdl_to_string_f64");
   pragma Export (C, Ghdl_To_String_F64_Digits, "__ghdl_to_string_f64_digits");
   pragma Export (C, Ghdl_To_String_F64_Format, "__ghdl_to_string_f64_format");
   pragma Export (Ada, Ghdl_To_String_B1, "__ghdl_to_string_b1");
   pragma Export (C, Ghdl_To_String_E8, "__ghdl_to_string_e8");
   pragma Export (C, Ghdl_To_String_E32, "__ghdl_to_string_e32");
   pragma Export (C, Ghdl_To_String_Char, "__ghdl_to_string_char");
   pragma Export (C, Ghdl_To_String_P32, "__ghdl_to_string_p32");
   pragma Export (C, Ghdl_To_String_P64, "__ghdl_to_string_p64");
   pragma Export (C, Ghdl_Time_To_String_Unit, "__ghdl_time_to_string_unit");
   pragma Export (C, Ghdl_Array_Char_To_String_B1,
                  "__ghdl_array_char_to_string_b1");
   pragma Export (C, Ghdl_Array_Char_To_String_E8,
                  "__ghdl_array_char_to_string_e8");
   pragma Export (C, Ghdl_Array_Char_To_String_E32,
                  "__ghdl_array_char_to_string_e32");
   pragma Export (C, Ghdl_BV_To_Ostring, "__ghdl_bv_to_ostring");
   pragma Export (C, Ghdl_BV_To_Hstring, "__ghdl_bv_to_hstring");
end Grt.Images;
