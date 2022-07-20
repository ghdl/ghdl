--  GHDL Run Time (GRT) - common types.
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

with Grt.Types; use Grt.Types;

package Grt.Vhdl_Types is
   pragma Preelaborate (Grt.Vhdl_Types);

   type Ghdl_Dir_Type is (Dir_To, Dir_Downto);
   for Ghdl_Dir_Type use (Dir_To => 0, Dir_Downto => 1);
   for Ghdl_Dir_Type'Size use 8;

   subtype Std_Integer is Ghdl_I32;

   type Std_Integer_Acc is access Std_Integer;
   pragma Convention (C, Std_Integer_Acc);

   type Std_Time is new Ghdl_I64;
   Bad_Time : constant Std_Time := Std_Time'First;

   type Std_Integer_Trt is record
      Left : Std_Integer;
      Right : Std_Integer;
      Dir : Ghdl_Dir_Type;
      Length : Ghdl_Index_Type;
   end record;

   type Std_Integer_Range_Ptr is access Std_Integer_Trt;
   pragma Convention (C, Std_Integer_Range_Ptr);

   subtype Std_Character is Character;
   type Std_String_Uncons is array (Ghdl_Index_Type range <>) of Std_Character;
   subtype Std_String_Base is Std_String_Uncons (Ghdl_Index_Type);
   type Std_String_Basep is access all Std_String_Base;
   function To_Std_String_Basep is new Ada.Unchecked_Conversion
     (Source => Address, Target => Std_String_Basep);

   type Std_String_Bound is record
      Dim_1 : Std_Integer_Trt;
   end record;
   type Std_String_Boundp is access all Std_String_Bound;
   function To_Std_String_Boundp is new Ada.Unchecked_Conversion
     (Source => Address, Target => Std_String_Boundp);

   type Std_String is record
      Base : Std_String_Basep;
      Bounds : Std_String_Boundp;
   end record;
   type Std_String_Ptr is access all Std_String;
   function To_Std_String_Ptr is new Ada.Unchecked_Conversion
     (Source => Address, Target => Std_String_Ptr);

   type Std_Bit is ('0', '1');
   type Std_Bit_Vector_Uncons is array (Ghdl_Index_Type range <>) of Std_Bit;
   subtype Std_Bit_Vector_Base is Std_Bit_Vector_Uncons (Ghdl_Index_Type);
   type Std_Bit_Vector_Basep is access all Std_Bit_Vector_Base;

   --  An unconstrained array.
   --  It is in fact a fat pointer to the base and the bounds.
   type Ghdl_Uc_Array is record
      Base : Address;
      Bounds : Address;
   end record;
   type Ghdl_Uc_Array_Acc is access Ghdl_Uc_Array;
   function To_Ghdl_Uc_Array_Acc is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Uc_Array_Acc);

   --  Ranges.
   type Ghdl_Range_B1 is record
      Left : Ghdl_B1;
      Right : Ghdl_B1;
      Dir : Ghdl_Dir_Type;
      Len : Ghdl_Index_Type;
   end record;

   type Ghdl_Range_E8 is record
      Left : Ghdl_E8;
      Right : Ghdl_E8;
      Dir : Ghdl_Dir_Type;
      Len : Ghdl_Index_Type;
   end record;

   type Ghdl_Range_E32 is record
      Left : Ghdl_E32;
      Right : Ghdl_E32;
      Dir : Ghdl_Dir_Type;
      Len : Ghdl_Index_Type;
   end record;

   type Ghdl_Range_I32 is record
      Left : Ghdl_I32;
      Right : Ghdl_I32;
      Dir : Ghdl_Dir_Type;
      Len : Ghdl_Index_Type;
   end record;

   type Ghdl_Range_I64 is record
      Left : Ghdl_I64;
      Right : Ghdl_I64;
      Dir : Ghdl_Dir_Type;
      Len : Ghdl_Index_Type;
   end record;

   type Ghdl_Range_F64 is record
      Left : Ghdl_F64;
      Right : Ghdl_F64;
      Dir : Ghdl_Dir_Type;
   end record;

   type Ghdl_Range_Type (K : Mode_Type := Mode_B1) is record
      case K is
         when Mode_B1 =>
            B1 : Ghdl_Range_B1;
         when Mode_E8 =>
            E8 : Ghdl_Range_E8;
         when Mode_E32 =>
            E32 : Ghdl_Range_E32;
         when Mode_I32 =>
            I32 : Ghdl_Range_I32;
         when Mode_I64 =>
            P64 : Ghdl_Range_I64;
         when Mode_F64 =>
            F64 : Ghdl_Range_F64;
      end case;
   end record;
   pragma Unchecked_Union (Ghdl_Range_Type);

   type Ghdl_Range_Ptr is access all Ghdl_Range_Type;

   function To_Ghdl_Range_Ptr is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Range_Ptr);

   type Ghdl_Range_Array is array (Ghdl_Index_Type range <>) of Ghdl_Range_Ptr;

   --  The NOW value.
   Current_Time : Std_Time;
   --  The current delta cycle number.
   Current_Delta : Integer;

   --  For AMS
   Current_Time_AMS : Ghdl_F64;
private
   pragma Export (C, Current_Time, "__ghdl_now");
   pragma Export (C, Current_Time_AMS, "__ghdl_now_ams");
end Grt.Vhdl_Types;
