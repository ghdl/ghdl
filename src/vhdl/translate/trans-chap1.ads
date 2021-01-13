--  Iir to ortho translator.
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

package Trans.Chap1  is
   --  Declare types for block BLK
   procedure Start_Block_Decl (Blk : Iir);

   procedure Translate_Entity_Declaration (Entity : Iir_Entity_Declaration);

   --  Generate code to initialize generics of instance INSTANCE of ENTITY
   --  using the default values.
   --  This is used when ENTITY is at the top of a design hierarchy.
   procedure Translate_Entity_Init_Generics (Entity : Iir);

   --  Likewise, but for ports.
   procedure Translate_Entity_Init_Ports (Entity : Iir);

   procedure Translate_Architecture_Body (Arch : Iir);

   --  CONFIG may be one of:
   --  * configuration_declaration
   --  * component_configuration
   procedure Translate_Configuration_Declaration_Decl (Config : Iir);
   procedure Translate_Configuration_Declaration_Body (Config : Iir);
end Trans.Chap1;
