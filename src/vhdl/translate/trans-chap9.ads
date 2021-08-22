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

package Trans.Chap9 is
   procedure Translate_Block_Declarations (Block : Iir; Origin : Iir);
   procedure Translate_Block_Subprograms (Block : Iir; Base_Block : Iir);

   procedure Elab_Block_Declarations (Block : Iir; Base_Block : Iir);
   procedure Elab_Block_Statements (Block : Iir; Base_Block : Iir);

   --  Generate code to instantiate an entity.
   --  ASPECT must be an entity_aspect.
   --  MAPPING must be a node with get_port/generic_map_aspect_list.
   --  PARENT is the block in which the instantiation is done.
   --  CONFIG_OVERRIDE, if set, is the configuration to use; if not set, the
   --    configuration to use is determined from ASPECT.
   procedure Translate_Entity_Instantiation
     (Aspect : Iir; Mapping : Iir; Parent : Iir; Config_Override : Iir);

   --  Elaborate an inertial association: register the anonymous process.
   procedure Elab_Inertial_Association (Assoc : Iir; Formal : Iir);

   --  Remove anonymous and implicit type definitions in a list of names.
   --  Such type definitions are created during slice translations, however
   --  variables created are defined in the translation scope.
   --  If the type is referenced again, the variables must be reachable.
   --  This is not the case for elaborator subprogram (which may references
   --  slices in the sensitivity or driver list) and the process subprg.
   procedure Destroy_Types (N : Iir);
   procedure Destroy_Types_In_List (L : Iir_List);

   --  Called by chap5 to initialize the driving value of a signal associated
   --  to a collapsed port.
   procedure Gen_Port_Init_Driving
     (Port : Mnode; Port_Type : Iir; Init : Mnode);

end Trans.Chap9;
