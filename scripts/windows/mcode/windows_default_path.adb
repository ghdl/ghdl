--  Find installation path (using executable).
--  Copyright (C) 2002 - 2016 Tristan Gingold
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

with Interfaces.C; use Interfaces.C;
with System; use System;

package body Windows_Default_Path is

   subtype DWORD is Interfaces.C.Unsigned_Long;
   subtype HINSTANCE is Address;
   function GetModuleFileName (Inst : HINSTANCE; Buf : Address; Size : DWORD)
                              return DWORD;
   pragma Import (Stdcall, GetModuleFileName, "GetModuleFileNameA");

   function Get_Windows_Exec_Path return String
   is
      File : String (1 .. 256);
      Size : DWORD;
      P : Natural;
   begin
      --  Get exe file path.
      Size := GetModuleFileName (Null_Address, File'Address, File'Length);
      if Size = 0 or Size = File'Length then
         return "{cannot find install path}\lib";
      end if;

      --  Remove Program file.
      P := Natural (Size);
      while P > 0 loop
         exit when File (P) = '\';
         exit when File (P) = ':' and P = 2;
         P := P - 1;
      end loop;
      if File (P) = '\' and P > 1 then
         --  Remove directory
         P := P - 1;
         while P > 0 loop
            exit when File (P) = '\';
            exit when File (P) = ':' and P = 2;
            P := P - 1;
         end loop;
      end if;

      return File (1 .. P);
   end Get_Windows_Exec_Path;
end Windows_Default_Path;
