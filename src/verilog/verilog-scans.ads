--  Verilog tokenizer
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.

with Types; use Types;
with Verilog.Types; use Verilog.Types;
with Verilog.Flags; use Verilog.Flags;
with Verilog.Tokens; use Verilog.Tokens;
with Verilog.Macros; use Verilog.Macros;

package Verilog.Scans is
   --  The scanner is used for both verilog and BSV.
   type Scan_Languages is (Language_Verilog, Language_BSV);
   Language : Scan_Languages := Language_Verilog;

   Keywords_Std : Standard_Type := Verilog_Sv2017;

   Current_Token : Token_Type;
   Current_Identifier : Name_Id;
   Current_Dollar_In_Id : Boolean;
   Current_Number_Lo : Logic_32;
   Current_Number_Hi : Logic_32;
   --  Number of bits for the current number (extracted from the number of
   --  digits).
   Current_Number_Len : Natural;
   Current_Bignum : Bn_Index;
   Current_Real : Fp64;
   Current_String : String8_Id;
   Current_String_Len : Natural;

   --  If set, returns Tok_EOL and preprocessor tokens
   Flag_Scan_All : Boolean := False;

   --  Handle translate on/off.
   Flag_Pragma_Comment : Boolean := False;

   --  Init the set of pathes.
   procedure Init_Pathes;

   --  Add DIR in the set of include directories.
   procedure Add_Incdir (Dir : String);

   --  Initialize the scanner with FILE.
   procedure Set_File (File : Source_File_Entry);

   procedure Close_File;

   --  Return the location of the token that has just been scaned.
   function Get_Token_Location return Location_Type;

   --  Call Report_Msg for each macro being expanded.
   procedure Scan_Report_Msg_Context;

   --  Return the width of the token (number of characters).
   function Get_Token_Width return Natural;

   --  Return the current source coordinates.  To be used only for error
   --  messages.
   function Get_Scan_Coord return Source_Coord_Type;

   --  Return the current macro (for read-only purposes).
   --  Defined only when the current_token is Tok_Pp_Define.
   function Get_Current_Macro return Macro_Acc;

   --  Scan the source file until the next token.
   procedure Scan;

   --  Scan an udp token.
   procedure Scan_Udp;

   --  For -E.
   function Image_Current_Token return String;
end Verilog.Scans;
