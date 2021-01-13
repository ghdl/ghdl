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
with Logging; use Logging;
with Errorout; use Errorout;
with PSL.Nodes;

package body PSL.Errors is
   function "+" (L : PSL_Node) return Location_Type
   is
      use PSL.Nodes;
   begin
      if L = Null_Node then
         return No_Location;
      else
         return PSL.Nodes.Get_Location (L);
      end if;
   end "+";

   procedure Error_Kind (Msg : String; N : PSL_Node) is
   begin
      Log (Msg);
      Log (": cannot handle ");
      Log_Line (PSL.Nodes.Nkind'Image (PSL.Nodes.Get_Kind (N)));
      raise Internal_Error;
   end Error_Kind;

   procedure Error_Msg_Sem (Msg: String; Loc : PSL_Node) is
   begin
      Report_Msg (Msgid_Error, Semantic, +(+Loc), Msg, No_Eargs);
   end Error_Msg_Sem;
end PSL.Errors;
