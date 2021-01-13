--  Error message handling for PSL.
--  Copyright (C) 2002-2019 Tristan Gingold
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
with Files_Map;
with PSL.Types; use PSL.Types;

package PSL.Errors is
   function "+" (L : PSL.Types.PSL_Node) return Location_Type;

   function Image (Loc : Location_Type; Filename : Boolean := True)
                  return String renames Files_Map.Image;

   procedure Error_Kind (Msg : String; N : PSL_Node);
   pragma No_Return (Error_Kind);

   procedure Error_Msg_Sem (Msg: String; Loc: PSL_Node);
end PSL.Errors;
