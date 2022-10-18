--  Utils for elaboration.
--  Copyright (C) 2022 Tristan Gingold
--
--  This file is part of GHDL.
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

with Vhdl.Nodes; use Vhdl.Nodes;

package Elab.Vhdl_Utils is
   --  Iterator initializer over associations.
   type Association_Iterator_Init is private;

   --  Create the iterator, either for a subprogram call, or for an operator.
   function Association_Iterator_Build (Inter_Chain : Node; Assoc_Chain : Node)
                                       return Association_Iterator_Init;
   function Association_Iterator_Build
     (Inter_Chain : Node; Left : Node; Right : Node)
     return Association_Iterator_Init;

   --  Retrieve chains.
   function Get_Iterator_Inter_Chain (Init : Association_Iterator_Init)
                                     return Node;
   function Get_Iterator_Assoc_Chain (Init : Association_Iterator_Init)
                                     return Node;

   --  Iterator over associations.
   type Association_Iterator is limited private;

   --  Start iterator.
   procedure Association_Iterate_Init (Iterator : out Association_Iterator;
                                       Init : Association_Iterator_Init);

   --  Return the next association.
   --  ASSOC can be:
   --  * an Iir_Kind_Association_By_XXX node (normal case)
   --  * Null_Iir if INTER is not associated (and has a default value).
   --  * an expression (for operator association).
   --  Associations are returned in the order of interfaces.
   procedure Association_Iterate_Next (Iterator : in out Association_Iterator;
                                       Inter : out Node;
                                       Assoc : out Node);

private
   type Association_Iterator_Kind is
     (Association_Function,
      Association_Operator);

   type Association_Iterator_Init
     (Kind : Association_Iterator_Kind := Association_Function) is
   record
      Inter_Chain : Node;
      case Kind is
         when Association_Function =>
            Assoc_Chain : Node;
         when Association_Operator =>
            Left : Node;
            Right : Node;
      end case;
   end record;

   type Association_Iterator
     (Kind : Association_Iterator_Kind := Association_Function) is
   record
      Inter : Node;
      case Kind is
         when Association_Function =>
            First_Named_Assoc : Node;
            Assoc : Node;
         when Association_Operator =>
            Op1 : Node;
            Op2 : Node;
      end case;
   end record;
end Elab.Vhdl_Utils;
