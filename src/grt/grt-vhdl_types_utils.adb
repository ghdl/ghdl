--  GHDL Run Time (GRT) - common types.
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

with Grt.Options; use Grt.Options;

package body Grt.Vhdl_Types_Utils is
   function Get_Std_String_Len (Str : Std_String_Any_Ptr)
                               return Ghdl_Index_Type is
   begin
      if Grt.Options.Flag_Integer_64 then
         declare
            Str64 : constant Std_String_64_Ptr := To_Std_String_64_Ptr (Str);
         begin
            return Str64.Bounds.Dim_1.Length;
         end;
      else
         declare
            Str32 : constant Std_String_32_Ptr := To_Std_String_32_Ptr (Str);
         begin
            return Str32.Bounds.Dim_1.Length;
         end;
      end if;
   end Get_Std_String_Len;

   function Get_Std_String_Base (Str : Std_String_Any_Ptr)
                                return Std_String_Basep is
   begin
      if Grt.Options.Flag_Integer_64 then
         return To_Std_String_64_Ptr (Str).Base;
      else
         return To_Std_String_32_Ptr (Str).Base;
      end if;
   end Get_Std_String_Base;
end Grt.Vhdl_Types_Utils;
