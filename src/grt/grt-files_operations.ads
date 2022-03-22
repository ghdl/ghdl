--  GHDL Run Time (GRT) -  VHDL files subprograms.
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

with Interfaces;

with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Stdio;

package Grt.Files_Operations is
   type Ghdl_File_Index is new Interfaces.Integer_32;

   --  File open mode.
   Read_Mode   : constant Ghdl_I32 := 0;
   Write_Mode  : constant Ghdl_I32 := 1;
   Append_Mode : constant Ghdl_I32 := 2;

   --  file_open_status.
   Open_Ok      : constant Ghdl_I32 := 0;
   Status_Error : constant Ghdl_I32 := 1;
   Name_Error   : constant Ghdl_I32 := 2;
   Mode_Error   : constant Ghdl_I32 := 3;

   type Op_Status is
     (
      Op_Ok,

      --  Correspond to file_open_status.
      Op_Status_Error,
      Op_Name_Error,
      Op_Mode_Error,

      --  For endfile: end of file reached (as if endfile returns True).
      Op_End_Of_File,

      --  Failed to call ungetc in endfile.
      Op_Ungetc_Error,

      --  Operation on a non-open file.
      Op_Not_Open,

      --  Try to read from a write-only file.
      Op_Read_Write_File,

      --  Try to write to a read-only file.
      Op_Write_Read_File,

      --  Internal error: incorrect file index.
      Op_Bad_Index,

      --  Internal error: binary operation on text file, or text operation
      --  on binary file.
      Op_Bad_Mode,

      --  Internal error: destroy a file that is still open.
      Op_Not_Closed,

      --  System error during write.
      Op_Write_Error,

      --  System error during read.
      Op_Read_Error,

      --  System error during close.
      Op_Close_Error,

      --  Incorrect file name (too long).
      Op_Filename_Error,

      --  Incorrect file type.
      Op_Signature_Error

     );

   --  General files.
   procedure Ghdl_File_Endfile
     (File : Ghdl_File_Index; Status : out Op_Status);

   --  Elaboration.
   function Ghdl_Text_File_Elaborate return Ghdl_File_Index;
   function Ghdl_File_Elaborate (Sig : Ghdl_C_String) return Ghdl_File_Index;

   --  Finalization.
   procedure Ghdl_Text_File_Finalize
     (File : Ghdl_File_Index; Status : out Op_Status);
   procedure Ghdl_File_Finalize
     (File : Ghdl_File_Index; Status : out Op_Status);

   --  Subprograms.
   procedure Ghdl_Text_File_Open (File : Ghdl_File_Index;
                                  Mode : Ghdl_I32;
                                  Name : Ghdl_C_String;
                                  Status : out Op_Status);
   procedure Ghdl_File_Open (File : Ghdl_File_Index;
                             Mode : Ghdl_I32;
                             Name : Ghdl_C_String;
                             Status : out Op_Status);

   procedure Ghdl_Text_Write (File : Ghdl_File_Index;
                              Str : Std_String_Ptr;
                              Status : out Op_Status);
   procedure Ghdl_Write_Scalar (File : Ghdl_File_Index;
                                Ptr : Ghdl_Ptr;
                                Length : Ghdl_Index_Type;
                                Status : out Op_Status);

   procedure Ghdl_Read_Scalar (File : Ghdl_File_Index;
                               Ptr : Ghdl_Ptr;
                               Length : Ghdl_Index_Type;
                               Status : out Op_Status);

   procedure Ghdl_Text_Read_Length (File : Ghdl_File_Index;
                                    Str : Std_String_Ptr;
                                    Status : out Op_Status;
                                    Length : out Std_Integer);

   procedure Ghdl_Untruncated_Text_Read (File : Ghdl_File_Index;
                                         Buf : Ghdl_C_String;
                                         Len : in out Std_Integer;
                                         Status : out Op_Status);

   procedure Ghdl_Text_File_Close (File : Ghdl_File_Index;
                                   Status : out Op_Status);
   procedure Ghdl_File_Close (File : Ghdl_File_Index;
                              Status : out Op_Status);

   procedure Ghdl_File_Flush (File : Ghdl_File_Index; Status : out Op_Status);

   type Open_Handler_Acc is access function
     (Name : Ghdl_C_String; Mode : Ghdl_C_String) return Grt.Stdio.FILEs;

   --  Like fopen(3)
   function Simple_Open (Name : Ghdl_C_String; Mode : Ghdl_C_String)
                        return Grt.Stdio.FILEs;

   --  Function called to open a file.  This hook can be used to search a file
   --  on a path.
   Open_Handler : Open_Handler_Acc := Simple_Open'Access;
end Grt.Files_Operations;
