--  Ada bindings for GCC internals.
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
with Ada.Unchecked_Conversion;
with System;

package body C is
   function C_String_Len (Str : C_String) return Natural is
   begin
      if Str = null then
         return 0;
      end if;
      for I in Str'Range loop
         if Str (I) = Character'Val (0) then
            return I - 1;
         end if;
      end loop;
      raise Program_Error;
   end C_String_Len;

   function Image (Str : C_Str_Len) return String is
   begin
      if Str.Str = null then
         return ''' & Character'Val (Str.Len) & ''';
      else
         return Str.Str (1 .. Str.Len);
      end if;
   end Image;

   function To_C_String (Acc : access String) return C_String
   is
      function Unchecked_Conversion is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => C_String);
   begin
      --  Check ACC is nul-terminated.
      if Acc (Acc.all'Last) /= Character'Val (0) then
         raise Program_Error;
      end if;
      return Unchecked_Conversion (Acc (Acc.all'First)'Address);
   end To_C_String;
end C;
