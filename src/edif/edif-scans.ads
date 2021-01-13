--  EDIF scanner.
--  Copyright (C) 2019 Tristan Gingold
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

with Types; use Types;
with Edif.Tokens; use Edif.Tokens;

package Edif.Scans is
   Current_Token : Token_Type;
   Current_Identifier : Name_Id;
   Current_Number : Int32;
   Current_String : String8_Id;
   Current_String_Len : Uns32;

   --  Initialize the scanner with FILE.
   procedure Set_File (File : Source_File_Entry);

   --  Return the location of the token that has just been scaned.
   function Get_Token_Location return Location_Type;

   --  Scan the source file until the next token.
   procedure Scan;
end Edif.Scans;
