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
with System; use System;
with Ada.Unchecked_Conversion;
with Grt.Rtis_Utils; use Grt.Rtis_Utils;
with Grt.Processes; use Grt.Processes;
with Grt.Errors; use Grt.Errors;
with Grt.Errors_Exec; use Grt.Errors_Exec;
with Grt.To_Strings; use Grt.To_Strings;

package body Grt.Images is
   function To_Std_String_Basep is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Std_String_Basep);

   function To_Std_String_Boundp is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Std_String_Boundp);

   procedure Set_String_Bounds (Res : Std_String_Ptr; Len : Ghdl_Index_Type)
   is
   begin
      Res.Bounds := To_Std_String_Boundp
        (Ghdl_Stack2_Allocate (Std_String_Bound'Size / System.Storage_Unit));
      Res.Bounds.Dim_1 := (Left => 1,
                           Right => Std_Integer (Len),
                           Dir => Dir_To,
                           Length => Len);
   end Set_String_Bounds;

   procedure Return_String (Res : Std_String_Ptr; Str : String)
   is
   begin
      Res.Base := To_Std_String_Basep (Ghdl_Stack2_Allocate (Str'Length));
      for I in 0 .. Str'Length - 1 loop
         Res.Base (Ghdl_Index_Type (I)) := Str (Str'First + I);
      end loop;
      Set_String_Bounds (Res, Str'Length);
   end Return_String;

   procedure Return_Enum
     (Res : Std_String_Ptr; Rti : Ghdl_Rti_Access; Index : Ghdl_Index_Type)
   is
      Enum_Rti : constant Ghdl_Rtin_Type_Enum_Acc :=
        To_Ghdl_Rtin_Type_Enum_Acc (Rti);
      Str : constant Ghdl_C_String := Enum_Rti.Names (Index);
   begin
      Return_String (Res, Str (1 .. strlen (Str)));
   end Return_Enum;

   procedure Ghdl_Image_B1
     (Res : Std_String_Ptr; Val : Ghdl_B1; Rti : Ghdl_Rti_Access)
   is
   begin
      Return_Enum (Res, Rti, Ghdl_B1'Pos (Val));
   end Ghdl_Image_B1;

   procedure Ghdl_Image_E8
     (Res : Std_String_Ptr; Val : Ghdl_E8; Rti : Ghdl_Rti_Access)
   is
   begin
      Return_Enum (Res, Rti, Ghdl_E8'Pos (Val));
   end Ghdl_Image_E8;

   procedure Ghdl_Image_E32
     (Res : Std_String_Ptr; Val : Ghdl_E32; Rti : Ghdl_Rti_Access)
   is
   begin
      Return_Enum (Res, Rti, Ghdl_E32'Pos (Val));
   end Ghdl_Image_E32;

   procedure Ghdl_Image_I32 (Res : Std_String_Ptr; Val : Ghdl_I32)
   is
      Str : String (1 .. 11);
      First : Natural;
   begin
      To_String (Str, First, Val);
      Return_String (Res, Str (First .. Str'Last));
   end Ghdl_Image_I32;

   procedure Ghdl_Image_I64 (Res : Std_String_Ptr; Val : Ghdl_I64)
   is
      --  biggest number is: 18446744073709551615 (20 digits)
      Str : String (1 .. 21);
      First : Natural;
   begin
      To_String (Str, First, Val);
      Return_String (Res, Str (First .. Str'Last));
   end Ghdl_Image_I64;

   procedure Ghdl_Image_P64
     (Res : Std_String_Ptr; Val : Ghdl_I64; Rti : Ghdl_Rti_Access)
   is
      Str : String (1 .. 21);
      First : Natural;
      Phys : constant Ghdl_Rtin_Type_Physical_Acc
        := To_Ghdl_Rtin_Type_Physical_Acc (Rti);
      Unit_Name : Ghdl_C_String;
      Unit_Len : Natural;
   begin
      To_String (Str, First, Val);
      Unit_Name := Get_Physical_Unit_Name (Phys.Units (0));
      Unit_Len := strlen (Unit_Name);
      declare
         L : constant Natural := Str'Last + 1 - First;
         Str2 : String (1 .. L + 1 + Unit_Len);
      begin
         Str2 (1 .. L) := Str (First .. Str'Last);
         Str2 (L + 1) := ' ';
         Str2 (L + 2 .. Str2'Last) := Unit_Name (1 .. Unit_Len);
         Return_String (Res, Str2);
      end;
   end Ghdl_Image_P64;

   procedure Ghdl_Image_P32
     (Res : Std_String_Ptr; Val : Ghdl_I32; Rti : Ghdl_Rti_Access)
   is
      Str : String (1 .. 11);
      First : Natural;
      Phys : constant Ghdl_Rtin_Type_Physical_Acc
        := To_Ghdl_Rtin_Type_Physical_Acc (Rti);
      Unit_Name : Ghdl_C_String;
      Unit_Len : Natural;
   begin
      To_String (Str, First, Val);
      Unit_Name := Get_Physical_Unit_Name (Phys.Units (0));
      Unit_Len := strlen (Unit_Name);
      declare
         L : constant Natural := Str'Last + 1 - First;
         Str2 : String (1 .. L + 1 + Unit_Len);
      begin
         Str2 (1 .. L) := Str (First .. Str'Last);
         Str2 (L + 1) := ' ';
         Str2 (L + 2 .. Str2'Last) := Unit_Name (1 .. Unit_Len);
         Return_String (Res, Str2);
      end;
   end Ghdl_Image_P32;

   procedure Ghdl_Image_F64 (Res : Std_String_Ptr; Val : Ghdl_F64)
   is
      Str : String (1 .. 24);
      P : Natural;
   begin
      To_String (Str, P, Val);
      Return_String (Res, Str (1 .. P));
   end Ghdl_Image_F64;

   procedure Ghdl_To_String_I32 (Res : Std_String_Ptr; Val : Ghdl_I32)
     renames Ghdl_Image_I32;
   procedure Ghdl_To_String_I64 (Res : Std_String_Ptr; Val : Ghdl_I64)
     renames Ghdl_Image_I64;
   procedure Ghdl_To_String_F64 (Res : Std_String_Ptr; Val : Ghdl_F64)
     renames Ghdl_Image_F64;

   procedure Ghdl_To_String_F64_Digits
     (Res : Std_String_Ptr; Val : Ghdl_F64; Nbr_Digits : Ghdl_I32)
   is
      Str : String (1 .. 128);
      P : Natural;
   begin
      To_String (Str, P, Val, Nbr_Digits);
      Return_String (Res, Str (1 .. P));
   end Ghdl_To_String_F64_Digits;

   procedure Ghdl_To_String_F64_Format
     (Res : Std_String_Ptr; Val : Ghdl_F64; Format : Std_String_Ptr)
   is
      C_Format : String (1 .. Positive (Format.Bounds.Dim_1.Length + 1));
      Str : String_Real_Format;
      P : Natural;
   begin
      for I in 1 .. C_Format'Last - 1 loop
         C_Format (I) := Format.Base (Ghdl_Index_Type (I - 1));
      end loop;
      C_Format (C_Format'Last) := NUL;

      To_String (Str, P, Val, To_Ghdl_C_String (C_Format'Address));
      Return_String (Res, Str (1 .. P));
   end Ghdl_To_String_F64_Format;

   subtype Log_Base_Type is Ghdl_Index_Type range 3 .. 4;
   Hex_Chars : constant array (Natural range 0 .. 15) of Character :=
     "0123456789ABCDEF";

   procedure Ghdl_BV_To_String (Res : Std_String_Ptr;
                                Val : Std_Bit_Vector_Basep;
                                Len : Ghdl_Index_Type;
                                Log_Base : Log_Base_Type)
   is
      Res_Len : constant Ghdl_Index_Type := (Len + Log_Base - 1) / Log_Base;
      Pos : Ghdl_Index_Type;
      V : Natural;
      Sh_Count : Natural range 0 .. 4;
      Sh : Natural range 1 .. 16;
   begin
      Res.Base := To_Std_String_Basep (Ghdl_Stack2_Allocate (Res_Len));
      V := 0;
      Sh_Count := 0;
      Sh := 1;
      Pos := Res_Len - 1;
      for I in reverse 1 .. Len loop
         V := V + Std_Bit'Pos (Val (I - 1)) * Sh;
         Sh_Count := Sh_Count + 1;
         Sh := Sh * 2;
         if Sh_Count = Natural (Log_Base) or else I = 1 then
            Res.Base (Pos) := Hex_Chars (V);
            Pos := Pos - 1;
            Sh_Count := 0;
            Sh := 1;
            V := 0;
         end if;
      end loop;
      Set_String_Bounds (Res, Res_Len);
   end Ghdl_BV_To_String;

   procedure Ghdl_BV_To_Ostring (Res : Std_String_Ptr;
                                 Base : Std_Bit_Vector_Basep;
                                 Len : Ghdl_Index_Type) is
   begin
      Ghdl_BV_To_String (Res, Base, Len, 3);
   end Ghdl_BV_To_Ostring;

   procedure Ghdl_BV_To_Hstring (Res : Std_String_Ptr;
                                 Base : Std_Bit_Vector_Basep;
                                 Len : Ghdl_Index_Type) is
   begin
      Ghdl_BV_To_String (Res, Base, Len, 4);
   end Ghdl_BV_To_Hstring;

   procedure To_String_Enum
     (Res : Std_String_Ptr; Rti : Ghdl_Rti_Access; Index : Ghdl_Index_Type)
   is
      Enum_Rti : Ghdl_Rtin_Type_Enum_Acc;
      Str : Ghdl_C_String;
      Len : Natural;
   begin
      Enum_Rti := To_Ghdl_Rtin_Type_Enum_Acc (Rti);
      Str := Enum_Rti.Names (Index);
      if Str (1) = ''' then
         Return_String (Res, Str (2 .. 2));
      else
         Len := strlen (Str);
         if Str (1) /= '\' then
            Return_String (Res, Str (1 .. Len));
         else
            --  Extended string.  Compute length.
            declare
               Skip : Boolean;
               Elen : Ghdl_Index_Type;
               Epos : Ghdl_Index_Type;
            begin
               Skip := False;
               Elen := 0;
               for I in 2 .. Len - 1 loop
                  if Skip then
                     Skip := False;
                  else
                     Elen := Elen + 1;
                     Skip := Str (I) = '\';
                  end if;
               end loop;
               Res.Base := To_Std_String_Basep (Ghdl_Stack2_Allocate (Elen));
               Epos := 0;
               for I in 2 .. Len - 1 loop
                  if Skip then
                     Skip := False;
                  else
                     Res.Base (Epos) := Str (I);
                     Epos := Epos + 1;
                     Skip := Str (I) = '\';
                  end if;
               end loop;
               Set_String_Bounds (Res, Elen);
            end;
         end if;
      end if;
   end To_String_Enum;

   procedure Ghdl_To_String_B1
     (Res : Std_String_Ptr; Val : Ghdl_B1; Rti : Ghdl_Rti_Access) is
   begin
      To_String_Enum (Res, Rti, Ghdl_B1'Pos (Val));
   end Ghdl_To_String_B1;

   procedure Ghdl_To_String_E8
     (Res : Std_String_Ptr; Val : Ghdl_E8; Rti : Ghdl_Rti_Access) is
   begin
      To_String_Enum (Res, Rti, Ghdl_E8'Pos (Val));
   end Ghdl_To_String_E8;

   procedure Ghdl_To_String_E32
     (Res : Std_String_Ptr; Val : Ghdl_E32; Rti : Ghdl_Rti_Access) is
   begin
      To_String_Enum (Res, Rti, Ghdl_E32'Pos (Val));
   end Ghdl_To_String_E32;

   procedure Ghdl_To_String_Char (Res : Std_String_Ptr; Val : Std_Character) is
   begin
      Return_String (Res, (1 => Val));
   end Ghdl_To_String_Char;

   procedure Ghdl_To_String_P32
     (Res : Std_String_Ptr; Val : Ghdl_I32; Rti : Ghdl_Rti_Access)
     renames Ghdl_Image_P32;

   procedure Ghdl_To_String_P64
     (Res : Std_String_Ptr; Val : Ghdl_I64; Rti : Ghdl_Rti_Access)
     renames Ghdl_Image_P64;

   procedure Ghdl_Time_To_String_Unit
     (Res : Std_String_Ptr;
      Val : Std_Time; Unit : Std_Time; Rti : Ghdl_Rti_Access)
   is
      Str : String_Time_Unit;
      First : Natural;
      Phys : constant Ghdl_Rtin_Type_Physical_Acc
        := To_Ghdl_Rtin_Type_Physical_Acc (Rti);
      Unit_Name : Ghdl_C_String;
      Unit_Len : Natural;
   begin
      Unit_Name := null;
      for I in 1 .. Phys.Nbr loop
         if Get_Physical_Unit_Value (Phys.Units (I - 1), Rti) = Ghdl_I64 (Unit)
         then
            Unit_Name := Get_Physical_Unit_Name (Phys.Units (I - 1));
            exit;
         end if;
      end loop;
      if Unit_Name = null then
         Error ("no unit for to_string");
      end if;
      To_String (Str, First, Ghdl_I64 (Val), Ghdl_I64 (Unit));
      Unit_Len := strlen (Unit_Name);
      declare
         L : constant Natural := Str'Last + 1 - First;
         Str2 : String (1 .. L + 1 + Unit_Len);
      begin
         Str2 (1 .. L) := Str (First .. Str'Last);
         Str2 (L + 1) := ' ';
         Str2 (L + 2 .. Str2'Last) := Unit_Name (1 .. Unit_Len);
         Return_String (Res, Str2);
      end;
   end Ghdl_Time_To_String_Unit;

   procedure Ghdl_Array_Char_To_String_B1
     (Res : Std_String_Ptr;
      Val : Ghdl_Ptr; Len : Ghdl_Index_Type; Rti : Ghdl_Rti_Access)
   is
      Enum_Rti : constant Ghdl_Rtin_Type_Enum_Acc :=
        To_Ghdl_Rtin_Type_Enum_Acc (Rti);
      Str : Ghdl_C_String;
      Arr : constant Ghdl_B1_Array_Base_Ptr := To_Ghdl_B1_Array_Base_Ptr (Val);
   begin
      Res.Base := To_Std_String_Basep (Ghdl_Stack2_Allocate (Len));
      for I in 1 .. Len loop
         Str := Enum_Rti.Names (Ghdl_B1'Pos (Arr (I - 1)));
         Res.Base (I - 1) := Str (2);
      end loop;
      Set_String_Bounds (Res, Len);
   end Ghdl_Array_Char_To_String_B1;

   procedure Ghdl_Array_Char_To_String_E8
     (Res : Std_String_Ptr;
      Val : Ghdl_Ptr; Len : Ghdl_Index_Type; Rti : Ghdl_Rti_Access)
   is
      Enum_Rti : constant Ghdl_Rtin_Type_Enum_Acc :=
        To_Ghdl_Rtin_Type_Enum_Acc (Rti);
      Str : Ghdl_C_String;
      Arr : constant Ghdl_E8_Array_Base_Ptr := To_Ghdl_E8_Array_Base_Ptr (Val);
   begin
      Res.Base := To_Std_String_Basep (Ghdl_Stack2_Allocate (Len));
      for I in 1 .. Len loop
         Str := Enum_Rti.Names (Ghdl_E8'Pos (Arr (I - 1)));
         Res.Base (I - 1) := Str (2);
      end loop;
      Set_String_Bounds (Res, Len);
   end Ghdl_Array_Char_To_String_E8;

   procedure Ghdl_Array_Char_To_String_E32
     (Res : Std_String_Ptr;
      Val : Ghdl_Ptr; Len : Ghdl_Index_Type; Rti : Ghdl_Rti_Access)
   is
      Enum_Rti : constant Ghdl_Rtin_Type_Enum_Acc :=
        To_Ghdl_Rtin_Type_Enum_Acc (Rti);
      Str : Ghdl_C_String;
      Arr : constant Ghdl_E32_Array_Base_Ptr :=
        To_Ghdl_E32_Array_Base_Ptr (Val);
   begin
      Res.Base := To_Std_String_Basep (Ghdl_Stack2_Allocate (Len));
      for I in 1 .. Len loop
         Str := Enum_Rti.Names (Ghdl_E32'Pos (Arr (I - 1)));
         Res.Base (I - 1) := Str (2);
      end loop;
      Set_String_Bounds (Res, Len);
   end Ghdl_Array_Char_To_String_E32;

--     procedure Ghdl_Image_F64 (Res : Std_String_Ptr; Val : Ghdl_F64)
--     is
--        --  Sign (1) + digit (1) + dot (1) + digits (15) + exp (1) + sign (1)
--        --  + exp_digits (4) -> 24.
--        Str : String (1 .. 25);

--        procedure Snprintf_G (Str : System.Address;
--                              Size : Integer;
--                              Arg : Ghdl_F64);
--        pragma Import (C, Snprintf_G, "__ghdl_snprintf_g");

--        function strlen (Str : System.Address) return Integer;
--        pragma Import (C, strlen);
--     begin
--        Snprintf_G (Str'Address, Str'Length, Val);
--        Return_String (Res, Str (1 .. strlen (Str'Address)));
--     end Ghdl_Image_F64;

end Grt.Images;
