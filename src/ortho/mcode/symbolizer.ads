--  Dwarf symbolizer.
--  Copyright (C) 2015 Tristan Gingold
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

with System.Storage_Elements;
use System; use System.Storage_Elements;

package Symbolizer is
   --  Address (in memory) and size of a debug section.
   type Section_Content is record
      Vaddr : Address;
      Size : Storage_Offset;
   end record;

   --  Input sections.
   type Dwarf_Sections is record
      Debug_Line : Section_Content;
      Debug_Info : Section_Content;
      Debug_Abbrev : Section_Content;
   end record;

   --  The result, using C strings.
   type Symbolize_Result is record
      Filename : Address;
      Line : Natural;
      Subprg_Name : Address;
   end record;

   --  Translate PC to filename, line number and subprogram name using dwarf
   --  debug infos.
   procedure Symbolize_Address (Pc : Address;
                                Sections : Dwarf_Sections;
                                Res : out Symbolize_Result);
end Symbolizer;
