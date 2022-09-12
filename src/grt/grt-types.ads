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
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;

package Grt.Types is
   pragma Preelaborate (Grt.Types);

   type Ghdl_B1 is new Boolean;
   type Ghdl_U8 is new Unsigned_8;
   subtype Ghdl_E8 is Ghdl_U8;
   type Ghdl_U32 is new Unsigned_32;
   subtype Ghdl_E32 is Ghdl_U32;
   type Ghdl_I32 is new Integer_32;
   type Ghdl_I64 is new Integer_64;
   type Ghdl_U64 is new Unsigned_64;
   type Ghdl_F64 is new IEEE_Float_64;

   function To_Ghdl_I32 is new Ada.Unchecked_Conversion (Ghdl_U32, Ghdl_I32);
   function To_Ghdl_U64 is new Ada.Unchecked_Conversion (Ghdl_I64, Ghdl_U64);
   function To_Ghdl_I64 is new Ada.Unchecked_Conversion (Ghdl_U64, Ghdl_I64);

   type Ghdl_Ptr is new Address;
   type Ghdl_Index_Type is mod 2 ** 32;
   subtype Ghdl_Real is Ghdl_F64;

   --  Access to an unconstrained string.
   type String_Access is access String;
   type String_Cst is access constant String;
   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Name => String_Access, Object => String);

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

   function strcmp (L , R : Ghdl_C_String) return Integer;
   pragma Import (C, strcmp);

   procedure free (Buf : Ghdl_C_String);
   pragma Import (C, free);

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

   type C_Boolean is new Boolean;
   pragma Convention (C, C_Boolean);

   --  Signal index.
   type Sig_Table_Index is new Integer;

   --  A range of signals.
   type Sig_Table_Range is record
      First, Last : Sig_Table_Index;
   end record;

   --  Signal index in Waves.Dump_Table.
   type Dump_Table_Index is new Natural;

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
      Mode_Stable, Mode_Quiet, Mode_Delayed, Mode_Transaction,
      Mode_Above, Mode_Guard,
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
end Grt.Types;
