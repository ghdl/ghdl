--  Chain handling.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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

package body Vhdl.Nodes_Utils is
   function Get_Chain_Length (First : Iir) return Natural
   is
      Res : Natural := 0;
      El : Iir := First;
   begin
      while El /= Null_Iir loop
         Res := Res + 1;
         El := Get_Chain (El);
      end loop;
      return Res;
   end Get_Chain_Length;

   procedure Append_Chain
     (N : Iir; Field : Vhdl.Nodes_Meta.Fields_Enum; Chain : Iir)
   is
      use Vhdl.Nodes_Meta;
      N_Chain : Iir;
      Next_Chain : Iir;
   begin
      N_Chain := Get_Iir (N, Field);
      if Is_Null (N_Chain) then
         Set_Iir (N, Field, Chain);
      else
         loop
            Next_Chain := Get_Chain (N_Chain);
            if Is_Null (Next_Chain) then
               Set_Chain (N_Chain, Chain);
               exit;
            end if;
            N_Chain := Next_Chain;
         end loop;
      end if;
   end Append_Chain;

   procedure Chain_Init (First, Last : out Iir) is
   begin
      First := Null_Iir;
      Last := Null_Iir;
   end Chain_Init;

   procedure Chain_Append (First, Last : in out Iir; El : Iir) is
   begin
      pragma Assert (El /= Null_Iir);
      if First = Null_Iir then
         First := El;
      else
         Set_Chain (Last, El);
      end if;
      Last := El;
   end Chain_Append;

   procedure Chain_Append_Chain (First, Last : in out Iir;
                                     First_Sub, Last_Sub : Iir) is
   begin
      pragma Assert (First_Sub /= Null_Iir);
      if First = Null_Iir then
         First := First_Sub;
      else
         Set_Chain (Last, First_Sub);
      end if;
      Last := Last_Sub;
   end Chain_Append_Chain;

   procedure Chain_Append_Subchain (First, Last : in out Iir;
                                        Sub : Iir)
   is
      N : Iir;
   begin
      pragma Assert (Sub /= Null_Iir);
      if First = Null_Iir then
         First := Sub;
      else
         Set_Chain (Last, Sub);
      end if;

      --  Update last.
      N := Sub;
      while N /= Null_Iir loop
         Last := N;
         N := Get_Chain (N);
      end loop;
   end Chain_Append_Subchain;

   function Is_Chain_Length_One (Chain : Iir) return Boolean is
   begin
      return Chain /= Null_Iir and then Get_Chain (Chain) = Null_Iir;
   end Is_Chain_Length_One;

   procedure Insert_Incr (Last : in out Iir; El : Iir) is
   begin
      Set_Chain (El, Get_Chain (Last));
      Set_Chain (Last, El);
      Last := El;
   end Insert_Incr;
end Vhdl.Nodes_Utils;
