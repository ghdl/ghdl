--  Back-end specialization
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
with Flags; use Flags;
with Iirs_Utils; use Iirs_Utils;

package body Back_End is
   -- Transform a library identifier into a file name.
   -- Very simple mechanism: just add '-simVV.cf' extension, where VV
   -- is the version.
   function Default_Library_To_File_Name (Library: Iir_Library_Declaration)
     return String
   is
   begin
      case Vhdl_Std is
         when Vhdl_87 =>
            return Image_Identifier (Library) & "-obj87.cf";
         when Vhdl_93c | Vhdl_93 | Vhdl_00 | Vhdl_02 =>
            return Image_Identifier (Library) & "-obj93.cf";
         when Vhdl_08 =>
            return Image_Identifier (Library) & "-obj08.cf";
      end case;
   end Default_Library_To_File_Name;
end Back_End;
