--  GHDL Run Time (GRT) -  VHDL files subprograms.
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Grt.Types; use Grt.Types;
with Interfaces;

package Grt.Files is
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

   --  General files.
   function Ghdl_File_Endfile (File : Ghdl_File_Index) return Boolean;

   --  Elaboration.
   function Ghdl_Text_File_Elaborate return Ghdl_File_Index;
   function Ghdl_File_Elaborate (Sig : Ghdl_C_String) return Ghdl_File_Index;

   --  Finalization.
   procedure Ghdl_Text_File_Finalize (File : Ghdl_File_Index);
   procedure Ghdl_File_Finalize (File : Ghdl_File_Index);

   --  Subprograms.
   procedure Ghdl_Text_File_Open
     (File : Ghdl_File_Index; Mode : Ghdl_I32; Str : Std_String_Ptr);
   function Ghdl_Text_File_Open_Status
     (File : Ghdl_File_Index; Mode : Ghdl_I32; Str : Std_String_Ptr)
     return Ghdl_I32;

   procedure Ghdl_File_Open
     (File : Ghdl_File_Index; Mode : Ghdl_I32; Str : Std_String_Ptr);
   function Ghdl_File_Open_Status
     (File : Ghdl_File_Index; Mode : Ghdl_I32; Str : Std_String_Ptr)
     return Ghdl_I32;

   procedure Ghdl_Text_Write (File : Ghdl_File_Index; Str : Std_String_Ptr);
   procedure Ghdl_Write_Scalar (File : Ghdl_File_Index;
                                Ptr : Ghdl_Ptr;
                                Length : Ghdl_Index_Type);

   procedure Ghdl_Read_Scalar (File : Ghdl_File_Index;
                               Ptr : Ghdl_Ptr;
                               Length : Ghdl_Index_Type);

   function Ghdl_Text_Read_Length
     (File : Ghdl_File_Index; Str : Std_String_Ptr) return Std_Integer;

   type Ghdl_Untruncated_Text_Read_Result is record
      Len : Std_Integer;
   end record;

   type Ghdl_Untruncated_Text_Read_Result_Acc is
     access Ghdl_Untruncated_Text_Read_Result;

   procedure Ghdl_Untruncated_Text_Read
     (Res : Ghdl_Untruncated_Text_Read_Result_Acc;
      File : Ghdl_File_Index;
      Str : Std_String_Ptr);

   procedure Ghdl_Text_File_Close (File : Ghdl_File_Index);
   procedure Ghdl_File_Close (File : Ghdl_File_Index);
private
   pragma Export (Ada, Ghdl_File_Endfile, "__ghdl_file_endfile");

   pragma Export (C, Ghdl_Text_File_Elaborate, "__ghdl_text_file_elaborate");
   pragma Export (C, Ghdl_File_Elaborate, "__ghdl_file_elaborate");

   pragma Export (C, Ghdl_Text_File_Finalize, "__ghdl_text_file_finalize");
   pragma Export (C, Ghdl_File_Finalize, "__ghdl_file_finalize");

   pragma Export (C, Ghdl_Text_File_Open, "__ghdl_text_file_open");
   pragma Export (C, Ghdl_Text_File_Open_Status,
                  "__ghdl_text_file_open_status");

   pragma Export (C, Ghdl_File_Open, "__ghdl_file_open");
   pragma Export (C, Ghdl_File_Open_Status, "__ghdl_file_open_status");

   pragma Export (C, Ghdl_Text_Write, "__ghdl_text_write");
   pragma Export (C, Ghdl_Write_Scalar, "__ghdl_write_scalar");

   pragma Export (C, Ghdl_Read_Scalar, "__ghdl_read_scalar");

   pragma Export (C, Ghdl_Text_Read_Length, "__ghdl_text_read_length");
   pragma Export (C, Ghdl_Untruncated_Text_Read,
                  "std__textio__untruncated_text_read");

   pragma Export (C, Ghdl_Text_File_Close, "__ghdl_text_file_close");
   pragma Export (C, Ghdl_File_Close, "__ghdl_file_close");
end Grt.Files;
