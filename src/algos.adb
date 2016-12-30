--  Generic algorithms
--  Copyright (C) 2016 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

package body Algos is
   procedure Heap_Sort (N : Natural) is
      --  An heap is an almost complete binary tree whose each edge is less
      --  than or equal as its decendent.

      --  Bubble down element I of a partially ordered heap of length N in
      --  array ARR.
      procedure Bubble_Down (I, N : Natural)
      is
         Child : Natural;
         Parent : Natural := I;
      begin
         loop
            Child := 2 * Parent;
            if Child < N and then Lt (Child, Child + 1) then
               Child := Child + 1;
            end if;
            exit when Child > N;
            exit when not Lt (Parent, Child);
            Swap (Parent, Child);
            Parent := Child;
         end loop;
      end Bubble_Down;

   begin
      --  Heapify
      for I in reverse 1 .. N / 2 loop
         Bubble_Down (I, N);
      end loop;

      --  Sort
      for I in reverse 2 .. N loop
         Swap (1, I);
         Bubble_Down (1, I - 1);
      end loop;
   end Heap_Sort;
end Algos;
