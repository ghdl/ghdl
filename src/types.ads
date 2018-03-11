--  Common types.
--  Copyright (C) 2002 - 2015 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
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

   subtype Nat32 is Int32 range 0 .. Int32'Last;
   subtype Pos32 is Nat32 range 1 .. Nat32'Last;

   subtype Nat8 is Nat32 range 0 .. 255;

   type Uns32 is new Interfaces.Unsigned_32;
   type Uns64 is new Interfaces.Unsigned_64;

   type Fp64 is new Interfaces.IEEE_Float_64;

   -- iir_int32 is aimed at containing integer literal values.
   type Iir_Int32 is new Interfaces.Integer_32;

   -- iir_int64 is aimed at containing units values.
   type Iir_Int64 is new Interfaces.Integer_64;

   -- iir_fp64 is aimed at containing floating point values.
   subtype Iir_Fp64 is Fp64;

   --  iir_index32 is aimed at containing an array index.
   type Iir_Index32 is new Nat32;

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

   --  Index type is the source file table.
   --  This table is defined in the files_map package.
   type Source_File_Entry is new Uns32;
   No_Source_File_Entry: constant Source_File_Entry := 0;

   --  Index into a file buffer.
   type Source_Ptr is new Uns32;

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

   --  PSL Node.
   type PSL_Node is new Int32;

   --  PSL NFA
   type PSL_NFA is new Int32;

   --  Indentation.
   --  This is used by all packages that display vhdl code or informations.
   Indentation : constant := 2;

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
   Internal_Error: exception;

   --  In some case, a low level subprogram can't handle error
   --  (e.g eval_pos).  In this case it is easier to raise an exception and
   --  let upper level subprograms handle the case.
   Node_Error : exception;

   --  Result of a comparaison of two numeric values.
   type Order_Type is (Less, Equal, Greater);
end Types;
