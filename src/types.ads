--  Common types.
--  Copyright (C) 2002 - 2015 Tristan Gingold
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
with Interfaces;
with System;
with Ada.Unchecked_Conversion;

package Types is
   pragma Preelaborate (Types);

   -- A tri state type.
   type Tri_State_Type is (Unknown, False, True);

   --  32 bits integer.
   type Int32 is range -2**31 .. 2**31 - 1;
   for Int32'Size use 32;

   --  64 bits integer.
   type Int64 is range -2**63 .. 2**63 - 1;
   for Int64'Size use 64;

   subtype Nat32 is Int32 range 0 .. Int32'Last;
   subtype Pos32 is Nat32 range 1 .. Nat32'Last;

   subtype Nat8 is Nat32 range 0 .. 255;

   type Uns32 is new Interfaces.Unsigned_32;
   type Uns64 is new Interfaces.Unsigned_64;

   type Fp64 is new Interfaces.IEEE_Float_64;
   type Fp32 is new Interfaces.IEEE_Float_32;

   --  The verilog logic type (when used in a vector).
   --  Coding of 01zx:
   --  For 0 and 1, ZX is 0, VAL is the bit value.
   --  For z: ZX is 1, VAL is 0.
   --  For x: ZX is 1, VAL is 1.
   type Logic_32 is record
      Val : Uns32;  --  AKA aval
      Zx  : Uns32;  --  AKA bval
   end record;

   --  Useful types.
   type String_Acc is access String;
   type String_Cst is access constant String;
   type String_Acc_Array is array (Natural range <>) of String_Acc;

   --  Fat strings, for compatibility with C.
   type Thin_String_Ptr is access String (Positive);
   pragma Convention (C, Thin_String_Ptr);
   function To_Thin_String_Ptr is new Ada.Unchecked_Conversion
     (System.Address, Thin_String_Ptr);

   --  The name table is defined in Name_Table package.  This is an hash table
   --  that associate a uniq Name_Id to a string.  Name_Id are allocated in
   --  increasing numbers, so it is possible to create a parallel table
   --  indexed on Name_Id to associate additional data to the names.
   type Name_Id is new Nat32;

   --  Null entry in the name table.
   --  It is sure that this entry is never allocated.
   No_Name_Id : constant Name_Id := 0;
   Null_Identifier: constant Name_Id := 0;

   --  A String8_Id represents a string stored in a dedicated table.  Contrary
   --  to Name_Id, String8 aren't uniq: two different String8_Id can correspond
   --  to a same String.  The purpose of an integer number for string is to
   --  have a 32 bit type to represent a string (contrary to pointers that
   --  could be 32 or 64 bit - in general - or to an access type which can be
   --  even wider in Ada).
   type String8_Id is new Uns32;
   for String8_Id'Size use 32;

   Null_String8 : constant String8_Id := 0;

   --  The length of a string is not stored in the string table.  Create a
   --  tuple that is meaningful.
   type String8_Len_Type is record
      Str : String8_Id;
      Len : Nat32;
   end record;

   --  Index type is the source file table.
   --  This table is defined in the files_map package.
   type Source_File_Entry is new Uns32;
   No_Source_File_Entry: constant Source_File_Entry := 0;

   --  Index into a file buffer.  Use a signed integers, so that empty string
   --  works correctly.
   type Source_Ptr is new Int32 range 0 .. Int32'Last;

   --  Valid bounds of any file buffer.
   Source_Ptr_Org : constant Source_Ptr := 0;
   Source_Ptr_Last : constant Source_Ptr := Source_Ptr'Last - 1;

   --  Bad file buffer index (used to mark no line).
   Source_Ptr_Bad : constant Source_Ptr := Source_Ptr'Last;

   --  Type of a file buffer.
   type File_Buffer is array (Source_Ptr range <>) of Character;
   type File_Buffer_Acc is access File_Buffer;
   type File_Buffer_Ptr is access File_Buffer (Source_Ptr);

   function To_File_Buffer_Ptr is new Ada.Unchecked_Conversion
     (System.Address, File_Buffer_Ptr);

   --  This type contains everything necessary to get a file name, a line
   --  number and a column number.
   type Location_Type is new Uns32;
   for Location_Type'Size use 32;
   Location_Nil : constant Location_Type := 0;
   No_Location : constant Location_Type := 0;

   --  Source coordinates.  An expanded form of location, almost ready to be
   --  printed.
   --  FILE is the reference to the source file.
   --  LINE_POS is the position in the source file of the first character of
   --   the line.  It usually comes for free but can be a little bit difficult
   --   to compute if the line table is being built.
   --  LINE is the line number; first line is 1 and 0 means unknown.
   --  OFFSET is the index in the line; first character is 0, any character
   --   (even tabulation) counts as 1 character.
   type Source_Coord_Type is record
      File : Source_File_Entry;
      Line_Pos : Source_Ptr;
      Line : Natural;
      Offset : Natural;
   end record;

   No_Source_Coord : constant Source_Coord_Type :=
     (No_Source_File_Entry, Source_Ptr_Bad, 0, 0);

   --  Indentation.
   --  This is used by all packages that display vhdl code or informations.
   Indentation : constant := 2;

   --  For array dimensions.  First dimension is 1.
   type Dim_Type is new Pos32;

   --  String representing a date/time (format is YYYYMMDDHHmmSS.sss).
   subtype Time_Stamp_String is String (1 .. 18);
   type Time_Stamp_Id is new String8_Id;
   Null_Time_Stamp : constant Time_Stamp_Id := 0;

   --  In order to detect file changes, a checksum of the content is computed.
   --  Currently SHA1 is used, but the cryptographic aspect is not a strong
   --  requirement.
   type File_Checksum_Id is new String8_Id;
   No_File_Checksum_Id : constant File_Checksum_Id := 0;

   --  String image of a File_Hash_Id.  SHA1 digests are 5 * 32 bytes long, so
   --  the hexadecimal image is 40 characters.
   subtype File_Checksum_String is String (1 .. 40);

   --  Self-explaining: raised when an internal error (such as consistency)
   --  is detected.
   Internal_Error : exception;

   --  Unrecoverable error.  Just exit() with an error status.
   Fatal_Error : exception;

   --  List of languages
   type Language_Type is
     (
      Language_Unknown,
      Language_Vhdl,
      Language_Psl,
      Language_Verilog
     );

   --  Result of a comparaison of two numeric values.
   type Order_Type is (Less, Equal, Greater);

   --  Direction for a range.  Used by many HDLs!
   type Direction_Type is (Dir_To, Dir_Downto);

   --  Modular type for the size.  We don't use Storage_Offset in order to
   --  make alignment computation efficient (knowing that alignment is a
   --  power of two).
   type Size_Type is mod System.Memory_Size;
end Types;
