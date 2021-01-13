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

package Trans.Chap5 is
   --  Attribute specification.
   procedure Translate_Attribute_Specification
     (Spec : Iir_Attribute_Specification);
   procedure Elab_Attribute_Specification
     (Spec : Iir_Attribute_Specification);

   --  Disconnection specification.
   procedure Elab_Disconnection_Specification
     (Spec : Iir_Disconnection_Specification);

   --  Elab an unconstrained port.
   procedure Elab_Unconstrained_Port_Bounds (Port : Iir; Assoc : Iir);

   --  Describe how to set the environment to access to formal in a map.  This
   --  is really useful only for recursive instantiation where the formal is
   --  the same as the actual, but their environment differs.
   type Map_Env is record
      --  The scope that has to be modified.
      Scope_Ptr : Var_Scope_Acc;

      --  The value for the scope.
      Scope : Var_Scope_Type;
   end record;

   --  Save and restore the map environment defined by ENV.
   procedure Save_Map_Env (Env : out Map_Env; Scope_Ptr : Var_Scope_Acc);
   procedure Set_Map_Env (Env : Map_Env);

   procedure Elab_Generic_Map_Aspect
     (Header : Iir; Map : Iir; Formal_Env : Map_Env);

   --  There are 4 cases of generic/port map:
   --  1) component instantiation
   --  2) component configuration (association of a component with an entity
   --     / architecture)
   --  3) block header
   --  4) direct (entity + architecture or configuration) instantiation
   --
   --  HEADER is the node containing generics and ports declarations.
   --  MAPS is the node containing the generic/port map aspects.
   procedure Elab_Map_Aspect
     (Header : Iir; Maps : Iir; Block_Parent : Iir; Formal_Env : Map_Env);
end Trans.Chap5;
