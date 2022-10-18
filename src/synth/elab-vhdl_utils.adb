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

with Types; use Types;
with Vhdl.Utils; use Vhdl.Utils;

package body Elab.Vhdl_Utils is
   function Association_Iterator_Build (Inter_Chain : Node; Assoc_Chain : Node)
                                       return Association_Iterator_Init is
   begin
      return Association_Iterator_Init'(Kind => Association_Function,
                                        Inter_Chain => Inter_Chain,
                                        Assoc_Chain => Assoc_Chain);
   end Association_Iterator_Build;

   function Association_Iterator_Build
     (Inter_Chain : Node; Left : Node; Right : Node)
     return Association_Iterator_Init is
   begin
      return Association_Iterator_Init'(Kind => Association_Operator,
                                        Inter_Chain => Inter_Chain,
                                        Left => Left,
                                        Right => Right);
   end Association_Iterator_Build;

   function Get_Iterator_Inter_Chain (Init : Association_Iterator_Init)
                                     return Node is
   begin
      return Init.Inter_Chain;
   end Get_Iterator_Inter_Chain;

   function Get_Iterator_Assoc_Chain (Init : Association_Iterator_Init)
                                     return Node is
   begin
      return Init.Assoc_Chain;
   end Get_Iterator_Assoc_Chain;

   procedure Association_Iterate_Init (Iterator : out Association_Iterator;
                                       Init : Association_Iterator_Init) is
   begin
      case Init.Kind is
         when Association_Function =>
            Iterator := (Kind => Association_Function,
                         Inter => Init.Inter_Chain,
                         First_Named_Assoc => Null_Node,
                         Assoc => Init.Assoc_Chain);
         when Association_Operator =>
            Iterator := (Kind => Association_Operator,
                         Inter => Init.Inter_Chain,
                         Op1 => Init.Left,
                         Op2 => Init.Right);
      end case;
   end Association_Iterate_Init;

   --  Return the next association.
   --  ASSOC can be:
   --  * an Iir_Kind_Association_By_XXX node (normal case)
   --  * Null_Iir if INTER is not associated (and has a default value).
   --  * an expression (for operator association).
   --  Associations are returned in the order of interfaces.
   procedure Association_Iterate_Next (Iterator : in out Association_Iterator;
                                       Inter : out Node;
                                       Assoc : out Node) is
   begin
      --  Next interface.
      Inter := Iterator.Inter;

      if Inter = Null_Node then
         --  End of iterator.
         Assoc := Null_Node;
         return;
      end if;

      --  Advance to the next interface for the next call.
      Iterator.Inter := Get_Chain (Iterator.Inter);

      case Iterator.Kind is
         when Association_Function =>
            if Iterator.First_Named_Assoc = Null_Node then
               Assoc := Iterator.Assoc;
               --  Still using association by position.
               if Assoc = Null_Node then
                  --  No more associations, all open.
                  return;
               end if;
               if Get_Formal (Assoc) = Null_Node then
                  --  Still by position, update for the next call.
                  Iterator.Assoc := Get_Chain (Assoc);
                  return;
               end if;
               Iterator.First_Named_Assoc := Assoc;
            end if;

            --  Search by name.
            declare
               Formal : Node;
            begin
               Assoc := Iterator.First_Named_Assoc;
               while Assoc /= Null_Node loop
                  Formal := Get_Formal (Assoc);
                  if Formal = Null_Node then
                     pragma Assert (Get_Artificial_Flag (Assoc));
                     Assoc := Null_Node;
                     return;
                  end if;
                  Formal := Get_Interface_Of_Formal (Formal);

                  --  Compare by identifier, as INTER can be the generic
                  --  interface, while FORMAL is the instantiated one.
                  if Get_Identifier (Formal) = Get_Identifier (Inter) then
                     --  Found.
                     --  Optimize in case assocs are in order.
                     if Assoc = Iterator.First_Named_Assoc then
                        Iterator.First_Named_Assoc := Get_Chain (Assoc);
                     end if;
                     return;
                  end if;
                  Assoc := Get_Chain (Assoc);
               end loop;
            end;

            --  Not found: open association.
            return;

         when Association_Operator =>
            Assoc := Iterator.Op1;
            Iterator.Op1 := Iterator.Op2;
            Iterator.Op2 := Null_Node;
      end case;
   end Association_Iterate_Next;

end Elab.Vhdl_Utils;
