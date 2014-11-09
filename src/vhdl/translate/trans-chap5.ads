--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
   procedure Elab_Unconstrained_Port (Port : Iir; Actual : Iir);

   procedure Elab_Generic_Map_Aspect (Mapping : Iir);

   --  There are 4 cases of generic/port map:
   --  1) component instantiation
   --  2) component configuration (association of a component with an entity
   --     / architecture)
   --  3) block header
   --  4) direct (entity + architecture or configuration) instantiation
   --
   --  MAPPING is the node containing the generic/port map aspects.
   procedure Elab_Map_Aspect (Mapping : Iir; Block_Parent : Iir);
end Trans.Chap5;
