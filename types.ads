--  Common types.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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

package Types is
   pragma Preelaborate (Types);

   -- A tri state type.
   type Tri_State_Type is (Unknown, False, True);

   --  32 bits integer.
   type Int32 is range -2**31 .. 2**31 - 1;
   for Int32'Size use 32;

   subtype Nat32 is Int32 range 0 .. Int32'Last;
   subtype Pos32 is Nat32 range 1 .. Nat32'Last;

   type Uns32 is new Interfaces.Unsigned_32;

   type Fp64 is new Interfaces.IEEE_Float_64;

   -- iir_int32 is aimed at containing integer literal values.
   type Iir_Int32 is new Interfaces.Integer_32;

   -- iir_int64 is aimed at containing units values.
   type Iir_Int64 is new Interfaces.Integer_64;

   -- iir_fp32 is aimed at containing floating point values.
   type Iir_Fp32 is new Interfaces.IEEE_Float_32;

   -- iir_fp64 is aimed at containing floating point values.
   subtype Iir_Fp64 is Fp64;

   --  iir_index32 is aimed at containing an array index.
   type Iir_Index32 is new Nat32;

   -- Useful type.
   type String_Acc is access String;
   type String_Cst is access constant String;
   type String_Acc_Array is array (Natural range <>) of String_Acc;

   type String_Fat is array (Pos32) of Character;
   type String_Fat_Acc is access String_Fat;

   -- Array of iir_int32.
   -- Used by recording feature of scan.
   type Iir_Int32_Array is array (Natural range <>) of Iir_Int32;
   type Iir_Int32_Array_Acc is access Iir_Int32_Array;

   -- Type of a name table element.
   -- The name table is defined in the name_table package.
   type Name_Id is new Nat32;

   -- null entry in the name table.
   -- It is sure that this entry is never allocated.
   Null_Identifier: constant Name_Id := 0;

   --  Type of a string stored into the string table.
   type String_Id is new Nat32;
   for String_Id'Size use 32;

   Null_String : constant String_Id := 0;

   -- Index type is the source file table.
   -- This table is defined in the files_map package.
   type Source_File_Entry is new Nat32;
   No_Source_File_Entry: constant Source_File_Entry := 0;

   --  FIXME: additional source file entries to create:
   --  *std.standard*: for those created in std.standard
   --  *error*: for erroneous one
   --  *command-line*: used for identifiers from command line
   --    (eg: unit to elab)

   -- Index into a file buffer.
   type Source_Ptr is new Int32;

   --  Lower boundary of any file buffer.
   Source_Ptr_Org : constant Source_Ptr := 0;

   --  Bad file buffer index (used to mark no line).
   Source_Ptr_Bad : constant Source_Ptr := -1;

   -- This type contains everything necessary to get a file name, a line
   -- number and a column number.
   type Location_Type is new Nat32;
   for Location_Type'Size use 32;
   Location_Nil : constant Location_Type := 0;

   -- Type of a file buffer.
   type File_Buffer is array (Source_Ptr range <>) of Character;
   type File_Buffer_Acc is access File_Buffer;

   --  PSL Node.
   type PSL_Node is new Int32;

   --  PSL NFA
   type PSL_NFA is new Int32;

   -- Indentation.
   -- This is used by all packages that display vhdl code or informations.
   Indentation : constant := 2;

   --  String representing a date/time (format is YYYYMMDDHHmmSS.sss).
   subtype Time_Stamp_String is String (1 .. 18);
   type Time_Stamp_Id is new String_Id;
   Null_Time_Stamp : constant Time_Stamp_Id := 0;

   --  Self-explaining: raised when an internal error (such as consistency)
   --  is detected.
   Internal_Error: exception;

   --  In some case, a low level subprogram can't handle error
   --  (e.g eval_pos).  In this case it is easier to raise an exception and
   --  let upper level subprograms handle the case.
   Node_Error : exception;
end Types;
