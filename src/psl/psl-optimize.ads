--  PSL - Optimize NFA
--  Copyright (C) 2002-2016 Tristan Gingold
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

with PSL.NFAs; use PSL.NFAs;
with PSL.Nodes; use PSL.Nodes;

package PSL.Optimize is
   --  Remove unreachable states, ie
   --  *  states that can't be reach from the start state.
   --  *  states that can't reach the final state.
   --  O(N) algorithm.
   procedure Remove_Unreachable_States (N : NFA);

   --  Remove single prefix, ie edges to a state S that is also from start
   --  to S.
   --  O(M) algorithm.
   procedure Remove_Simple_Prefix (N : NFA);

   procedure Merge_Identical_States (N : NFA);

   procedure Merge_Edges (N : NFA);

   procedure Remove_Identical_Src_Edges (S : NFA_State);
   procedure Remove_Identical_Dest_Edges (S : NFA_State);

   --procedure Find_Partitions (N : NFA; Nbr_States : Natural);
end PSL.Optimize;
