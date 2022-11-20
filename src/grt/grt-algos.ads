--  Generic algorithms
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

package Grt.Algos is
   --  Heap sort the N elements.  Indexes are from 1 to N.
   generic
      --  Compare two elements, return true iff L < R.
      with function Lt (L, R : Positive) return Boolean;

      --  Swap two elements.
      with procedure Swap (P1 : Positive; P2 : Positive);
   procedure Heap_Sort (N : Natural);
end Grt.Algos;
