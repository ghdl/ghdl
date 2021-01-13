--  Build ortho structures from iir_values
--  Copyright (C) 2016 Tristan Gingold
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

with Configuration; use Configuration;
with Iirs_Utils; use Iirs_Utils;
with Errorout; use Errorout;
with Elaboration;

package body Trans.Preelab is
   --  Primary unit + secondary unit (architecture name which may be null)
   --  to elaborate.
   procedure Pre_Elaborate (Primary : String;
                            Secondary : String;
                            Filelist : String;
                            Whole : Boolean)
   is
      pragma Unreferenced (Filelist, Whole);
      Config : Iir_Design_Unit;
      Config_Lib : Iir_Configuration_Declaration;
      Entity : Iir_Entity_Declaration;
      --  Arch : Iir_Architecture_Body;
   begin
      Config := Configure (Primary, Secondary);
      if Config = Null_Iir then
         return;
      end if;

      Config_Lib := Get_Library_Unit (Config);
      Entity := Get_Entity (Config_Lib);
      --  Arch := Strip_Denoting_Name
      --    (Get_Block_Specification (Get_Block_Configuration (Config_Lib)));

      --  Be sure the entity can be at the top of a design.
      Check_Entity_Declaration_Top (Entity);

      --  Return now in case of errors.
      if Nbr_Errors /= 0 then
         return;
      end if;

      Elaboration.Elaborate_Design (Config);
   end Pre_Elaborate;
end Trans.Preelab;
