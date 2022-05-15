--  GHDL Run Time (GRT) - Stupid emulation of readline using stdio
--  Copyright (C) 2022 Tristan Gingold
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
with Grt.Stdio; use Grt.Stdio;

package body Grt.Readline_None is
   function getline (Linep : Address; Linecapp : Address; Stream : FILEs)
                    return ssize_t;
   pragma Import (C, getline);

   function Readline (Prompt : Ghdl_C_String) return Ghdl_C_String
   is
      Len : ssize_t;
      Linep : Ghdl_C_String;
      Linecapp : size_t;
      T : int;
   begin
      T := fputs (To_Address (Prompt), stdout);
      pragma Unreferenced (T);

      Linep := null;
      Linecapp := 0;
      Len := getline (Linep'Address, Linecapp'Address, stdin);
      if Len <= 0 then
         free (Linep);
         return null;
      end if;
      --  Remove end of line.
      if Linep (Natural (Len)) = ASCII.LF then
         Linep (Natural (Len)) := ASCII.NUL;
      end if;
      return Linep;
   end Readline;

   procedure Add_History (Line : Ghdl_C_String) is
   begin
      null;
   end Add_History;
end Grt.Readline_None;
