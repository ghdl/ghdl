--  GHDL Run Time (GRT) - common types.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with System; use System;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;

package Grt.Types is
   pragma Preelaborate (Grt.Types);

   type Ghdl_B1 is new Boolean;
   type Ghdl_E8 is new Unsigned_8;
   type Ghdl_U32 is new Unsigned_32;
   subtype Ghdl_E32 is Ghdl_U32;
   type Ghdl_I32 is new Integer_32;
   type Ghdl_I64 is new Integer_64;
   type Ghdl_U64 is new Unsigned_64;
   type Ghdl_F64 is new IEEE_Float_64;

   type Ghdl_Ptr is new Address;
   type Ghdl_Index_Type is mod 2 ** 32;
   subtype Ghdl_Real is Ghdl_F64;

   type Ghdl_Dir_Type is (Dir_To, Dir_Downto);
   for Ghdl_Dir_Type use (Dir_To => 0, Dir_Downto => 1);
   for Ghdl_Dir_Type'Size use 8;

   --  Access to an unconstrained string.
   type String_Access is access String;
   type String_Cst is access constant String;
   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Name => String_Access, Object => String);

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

   --  Verilog types.

   type Ghdl_Logic32 is record
      Val : Ghdl_U32;
      Xz : Ghdl_U32;
   end record;
   type Ghdl_Logic32_Ptr is access Ghdl_Logic32;
   type Ghdl_Logic32_Vec is array (Ghdl_U32) of Ghdl_Logic32;
   type Ghdl_Logic32_Vptr is access Ghdl_Logic32_Vec;

   function To_Ghdl_Logic32_Vptr is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Logic32_Vptr);

   function To_Ghdl_Logic32_Ptr is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Logic32_Ptr);

   --  Mimics C strings (NUL ended).
   --  Note: this is 1 based.
   type Ghdl_C_String is access String (Positive);
   NUL : constant Character := Character'Val (0);

   Nl : constant Character := Character'Val (10);  -- LF, nl or '\n'.

   function strlen (Str : Ghdl_C_String) return Natural;
   pragma Import (C, strlen);

   function Strcmp (L , R : Ghdl_C_String) return Integer;
   pragma Import (C, Strcmp);

   function To_Ghdl_C_String is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_C_String);
   function To_Address is new Ada.Unchecked_Conversion
     (Source => Ghdl_C_String, Target => Address);

   --  Str_len.
   type String_Ptr is access String (1 .. Natural'Last);
   type Ghdl_Str_Len_Type is record
      Len : Natural;
      Str : String_Ptr;
   end record;
   --  Same as previous one, but using 'address.
   type Ghdl_Str_Len_Address_Type is record
      Len : Natural;
      Str : Address;
   end record;
   type Ghdl_Str_Len_Ptr is access constant Ghdl_Str_Len_Type;
   type Ghdl_Str_Len_Array is array (Natural) of Ghdl_Str_Len_Type;
   type Ghdl_Str_Len_Array_Ptr is access all Ghdl_Str_Len_Array;

   --  Location is used for errors/messages.
   type Ghdl_Location is record
      Filename : Ghdl_C_String;
      Line : Integer;
      Col : Integer;
   end record;
   type Ghdl_Location_Ptr is access Ghdl_Location;
   function To_Ghdl_Location_Ptr is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Location_Ptr);

   --  Signal index.
   type Sig_Table_Index is new Integer;

   --  A range of signals.
   type Sig_Table_Range is record
      First, Last : Sig_Table_Index;
   end record;

   --  Simple values, used for signals.
   type Mode_Type is
     (Mode_B1, Mode_E8, Mode_E32, Mode_I32, Mode_I64, Mode_F64);

   type Ghdl_B1_Array is array (Ghdl_Index_Type range <>) of Ghdl_B1;
   subtype Ghdl_B1_Array_Base is Ghdl_B1_Array (Ghdl_Index_Type);
   type Ghdl_B1_Array_Base_Ptr is access Ghdl_B1_Array_Base;
   function To_Ghdl_B1_Array_Base_Ptr is new Ada.Unchecked_Conversion
     (Source => Ghdl_Ptr, Target => Ghdl_B1_Array_Base_Ptr);

   type Ghdl_E8_Array is array (Ghdl_Index_Type range <>) of Ghdl_E8;
   subtype Ghdl_E8_Array_Base is Ghdl_E8_Array (Ghdl_Index_Type);
   type Ghdl_E8_Array_Base_Ptr is access Ghdl_E8_Array_Base;
   function To_Ghdl_E8_Array_Base_Ptr is new Ada.Unchecked_Conversion
     (Source => Ghdl_Ptr, Target => Ghdl_E8_Array_Base_Ptr);

   type Ghdl_E32_Array is array (Ghdl_Index_Type range <>) of Ghdl_E32;
   subtype Ghdl_E32_Array_Base is Ghdl_E32_Array (Ghdl_Index_Type);
   type Ghdl_E32_Array_Base_Ptr is access Ghdl_E32_Array_Base;
   function To_Ghdl_E32_Array_Base_Ptr is new Ada.Unchecked_Conversion
     (Source => Ghdl_Ptr, Target => Ghdl_E32_Array_Base_Ptr);

   type Ghdl_I32_Array is array (Ghdl_Index_Type range <>) of Ghdl_I32;

   type Value_Union (Mode : Mode_Type := Mode_B1) is record
      case Mode is
         when Mode_B1 =>
            B1 : Ghdl_B1;
         when Mode_E8 =>
            E8 : Ghdl_E8;
         when Mode_E32 =>
            E32 : Ghdl_E32;
         when Mode_I32 =>
            I32 : Ghdl_I32;
         when Mode_I64 =>
            I64 : Ghdl_I64;
         when Mode_F64 =>
            F64 : Ghdl_F64;
      end case;
   end record;
   pragma Unchecked_Union (Value_Union);

   type Ghdl_Value_Ptr is access all Value_Union;
   function To_Ghdl_Value_Ptr is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Value_Ptr);

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

   type Ghdl_Indexes_Type is record
      Value : Ghdl_Index_Type;
      Signal : Ghdl_Index_Type;
   end record;

   type Ghdl_Indexes_Ptr is access all Ghdl_Indexes_Type;

   function To_Ghdl_Indexes_Ptr is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Indexes_Ptr);

   --  For PSL counters.
   type Ghdl_Index_Ptr is access all Ghdl_Index_Type;

   function To_Ghdl_Index_Ptr is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Index_Ptr);

   --  Mode of a signal.
   type Mode_Signal_Type is
     (Mode_Signal,
      Mode_Linkage, Mode_Buffer, Mode_Out, Mode_Inout, Mode_In,
      Mode_Stable, Mode_Quiet, Mode_Delayed, Mode_Transaction, Mode_Guard,
      Mode_Conv_In, Mode_Conv_Out,
      Mode_End);

   subtype Mode_Signal_Port is
     Mode_Signal_Type range Mode_Linkage .. Mode_In;

   --  Not implicit signals.
   subtype Mode_Signal_User is
     Mode_Signal_Type range Mode_Signal .. Mode_In;

   --  Implicit signals.
   subtype Mode_Signal_Implicit is
     Mode_Signal_Type range Mode_Stable .. Mode_Guard;

   subtype Mode_Signal_Forward is
     Mode_Signal_Type range Mode_Stable .. Mode_Delayed;

   --  Note: we could use system.storage_elements, but unfortunatly,
   --  this doesn't work with pragma no_run_time (gnat 3.15p).
   type Integer_Address is mod Memory_Size;

   function To_Address is new Ada.Unchecked_Conversion
     (Source => Integer_Address, Target => Address);

   function To_Integer is new Ada.Unchecked_Conversion
     (Source => Address, Target => Integer_Address);

   --  The NOW value.
   Current_Time : Std_Time;
   --  The current delta cycle number.
   Current_Delta : Integer;
private
   pragma Export (C, Current_Time, "__ghdl_now");
end Grt.Types;
