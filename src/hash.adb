--  Hash.
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

package body Hash is
   function String_Hash (Key : String) return Hash_Value_Type
   is
      Res : Hash_Value_Type;
   begin
      Res := 0;
      for I in Key'Range loop
         Res := Res * 5 + Character'Pos (Key (I));
      end loop;
      return Res;
   end String_Hash;
end Hash;
