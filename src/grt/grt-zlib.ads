--  GHDL Run Time (GRT) - Zlib binding.
--  Copyright (C) 2005 - 2014 Tristan Gingold
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
with Grt.C; use Grt.C;

package Grt.Zlib is
   pragma Linker_Options ("-lz");

   type gzFile is new System.Address;

   NULL_gzFile : constant gzFile := gzFile (System'To_Address (0));

   function gzputc (File : gzFile; C : int) return int;
   pragma Import (C, gzputc);

   function gzwrite (File : gzFile; Buf : voids; Len : int) return int;
   pragma Import (C, gzwrite);

   function gzopen (Path : chars; Mode : chars) return gzFile;
   pragma Import (C, gzopen);

   procedure gzclose (File : gzFile);
   pragma Import (C, gzclose);
end Grt.Zlib;
