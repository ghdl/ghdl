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
