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

with Types; use Types;
with PSL.Types; use PSL.Types;
with PSL.NFAs.Utils; use PSL.NFAs.Utils;
with PSL.CSE;

package body PSL.Optimize is
   procedure Push (Head : in out NFA_State; S : NFA_State) is
   begin
      Set_State_User_Link (S, Head);
      Head := S;
   end Push;

   procedure Pop (Head : in out NFA_State; S : out NFA_State) is
   begin
      S := Head;
      Head := Get_State_User_Link (S);
   end Pop;

   procedure Remove_Unreachable_States (N : NFA)
   is
      Start : constant NFA_State := Get_Start_State (N);
      Final : constant NFA_State := Get_Final_State (N);
      Active : constant NFA_State := Get_Active_State (N);
      Head : NFA_State;
      E : NFA_Edge;
      S, N_S : NFA_State;
   begin
      --  Remove unreachable states, ie states that can't be reached from
      --  start state.

      Head := No_State;

      --  The start state is reachable.
      Push (Head, Start);
      Set_State_Flag (Start, True);

      --  Follow edges and mark reachable states.
      while Head /= No_State loop
         Pop (Head, S);
         E := Get_First_Src_Edge (S);
         while E /= No_Edge loop
            S := Get_Edge_Dest (E);
            if not Get_State_Flag (S) then
               Push (Head, S);
               Set_State_Flag (S, True);
            end if;
            E := Get_Next_Src_Edge (E);
         end loop;
      end loop;

      --  Remove unreachable states.
      S := Get_First_State (N);
      while S /= No_State loop
         N_S := Get_Next_State (S);
         if Get_State_Flag (S) then
            --  Clean-up.
            Set_State_Flag (S, False);
         elsif S = Final then
            --  Do not remove final state!
            --  FIXME: deconnect state?
            null;
         elsif S = Active then
            --  Do not remove the active state, so that user can see that's
            --  vacuous.
            null;
         else
            Remove_State (N, S);
         end if;
         S := N_S;
      end loop;

      --  Remove no-where states, ie states that can't reach the final state.
      Head := No_State;

      --  The final state can reach the final state.
      Push (Head, Final);
      Set_State_Flag (Final, True);

      --  Follow edges and mark reachable states.
      while Head /= No_State loop
         Pop (Head, S);
         E := Get_First_Dest_Edge (S);
         while E /= No_Edge loop
            S := Get_Edge_Src (E);
            if not Get_State_Flag (S) then
               Push (Head, S);
               Set_State_Flag (S, True);
            end if;
            E := Get_Next_Dest_Edge (E);
         end loop;
      end loop;

      --  Remove unreachable states.
      S := Get_First_State (N);
      while S /= No_State loop
         N_S := Get_Next_State (S);
         if Get_State_Flag (S) then
            --  Clean-up.
            Set_State_Flag (S, False);
         elsif S = Start then
            --  Do not remove start state!
            --  FIXME: deconnect state?
            null;
         elsif S = Active then
            --  The active state is not expected to be reach the final state.
            null;
         else
            Remove_State (N, S);
         end if;
         S := N_S;
      end loop;
   end Remove_Unreachable_States;

   procedure Remove_Simple_Prefix (N : NFA)
   is
      Start : NFA_State;
      D : NFA_State;
      T_Start, T_D, Next_T_D : NFA_Edge;
      T_Expr : Node;
      Clean : Boolean := False;
   begin
      Start := Get_Start_State (N);

      --  Iterate on edges from the start state.
      T_Start := Get_First_Src_Edge (Start);
      while T_Start /= No_Edge loop
         --  Edge destination.
         D := Get_Edge_Dest (T_Start);
         T_Expr := Get_Edge_Expr (T_Start);

         --  Iterate on destination incoming edges.
         T_D := Get_First_Dest_Edge (D);
         while T_D /= No_Edge loop
            Next_T_D := Get_Next_Dest_Edge (T_D);
            --  Remove parallel edge.
            if T_D /= T_Start
              and then Get_Edge_Expr (T_D) = T_Expr
            then
               Remove_Edge (T_D);
               Clean := True;
            end if;
            T_D := Next_T_D;
         end loop;
         T_Start := Get_Next_Src_Edge (T_Start);
      end loop;
      if Clean then
         Remove_Unreachable_States (N);
      end if;
   end Remove_Simple_Prefix;

   --  Return TRUE iff the outgoing or incoming edges of L and R are the same.
   --  Outgoing edges must be sorted.
   generic
      with function Get_First_Edge (S : NFA_State) return NFA_Edge;
      with function Get_Next_Edge (E : NFA_Edge) return NFA_Edge;
      with function Get_Edge_State_Reverse (E : NFA_Edge) return NFA_State;
   function Are_States_Identical_Gen (L, R : NFA_State) return Boolean;

   function Are_States_Identical_Gen (L, R : NFA_State) return Boolean
   is
      L_E, R_E : NFA_Edge;
      L_S, R_S : NFA_State;
   begin
      L_E := Get_First_Edge (L);
      R_E := Get_First_Edge (R);
      loop
         if L_E = No_Edge and then R_E = No_Edge then
            --  End of chain for both L and R -> identical states.
            return True;
         elsif L_E = No_Edge or R_E = No_Edge then
            --  End of chain for either L or R -> non identical states.
            return False;
         elsif Get_Edge_Expr (L_E) /= Get_Edge_Expr (R_E) then
            --  Different edge (different expressions).
            return False;
         end if;
         L_S := Get_Edge_State_Reverse (L_E);
         R_S := Get_Edge_State_Reverse (R_E);
         if L_S /= R_S and then (L_S /= L or else R_S /= R) then
            --  Predecessors are differents and not loop.
            return False;
         end if;
         L_E := Get_Next_Edge (L_E);
         R_E := Get_Next_Edge (R_E);
      end loop;
   end Are_States_Identical_Gen;

   generic
      with procedure Sort_Edges (N : NFA);
      with procedure Sort_Edges_Reverse (S : NFA_State);
      with function Get_First_Edge (S : NFA_State) return NFA_Edge;
      with function Get_Next_Edge (E : NFA_Edge) return NFA_Edge;
      with function Get_First_Edge_Reverse (S : NFA_State) return NFA_Edge;
      with function Get_Next_Edge_Reverse (E : NFA_Edge) return NFA_Edge;
      with function Get_Edge_State (E : NFA_Edge) return NFA_State;
      with function Get_Edge_State_Reverse (E : NFA_Edge) return NFA_State;
      with procedure Merge_State_Reverse (N : NFA;
                                          S : NFA_State; S1 : NFA_State);
   procedure Merge_Identical_States_Gen (N : NFA);

   procedure Merge_Identical_States_Gen (N : NFA)
   is
      function Are_States_Identical is new Are_States_Identical_Gen
        (Get_First_Edge => Get_First_Edge,
         Get_Next_Edge => Get_Next_Edge,
         Get_Edge_State_Reverse => Get_Edge_State_Reverse);

      S : NFA_State;
      E : NFA_Edge;
      E_State, Next_E_State : NFA_State;
      Next_E, Next_Next_E : NFA_Edge;
   begin
      Sort_Edges (N);

      --  Iterate on states.
      S := Get_First_State (N);
      while S /= No_State loop
         Sort_Edges_Reverse (S);

         --  Iterate on incoming edges.
         E := Get_First_Edge_Reverse (S);
         while E /= No_Edge loop
            E_State := Get_Edge_State (E);

            --  Try to merge E with its successors.
            Next_E := Get_Next_Edge_Reverse (E);
            while Next_E /= No_Edge
              and then Get_Edge_Expr (E) = Get_Edge_Expr (Next_E)
            loop
               Next_E_State := Get_Edge_State (Next_E);
               Next_Next_E := Get_Next_Edge_Reverse (Next_E);
               if Next_E_State = E_State then
                  --  Identical edge: remove the duplicate.
                  Remove_Edge (Next_E);
               elsif Are_States_Identical (E_State, Next_E_State) then
                  Merge_State_Reverse (N, E_State, Next_E_State);
               end if;
               Next_E := Next_Next_E;
            end loop;

            E := Get_Next_Edge_Reverse (E);
         end loop;

         S := Get_Next_State (S);
      end loop;
   end Merge_Identical_States_Gen;

   procedure Merge_Identical_States_Src is new Merge_Identical_States_Gen
     (Sort_Edges => Sort_Src_Edges,
      Sort_Edges_Reverse => Sort_Dest_Edges,
      Get_First_Edge => Get_First_Src_Edge,
      Get_Next_Edge => Get_Next_Src_Edge,
      Get_First_Edge_Reverse => Get_First_Dest_Edge,
      Get_Next_Edge_Reverse => Get_Next_Dest_Edge,
      Get_Edge_State => Get_Edge_Src,
      Get_Edge_State_Reverse => Get_Edge_Dest,
      Merge_State_Reverse => Merge_State_Dest);

   procedure Merge_Identical_States_Dest is new Merge_Identical_States_Gen
     (Sort_Edges => Sort_Dest_Edges,
      Sort_Edges_Reverse => Sort_Src_Edges,
      Get_First_Edge => Get_First_Dest_Edge,
      Get_Next_Edge => Get_Next_Dest_Edge,
      Get_First_Edge_Reverse => Get_First_Src_Edge,
      Get_Next_Edge_Reverse => Get_Next_Src_Edge,
      Get_Edge_State => Get_Edge_Dest,
      Get_Edge_State_Reverse => Get_Edge_Src,
      Merge_State_Reverse => Merge_State_Src);

   procedure Merge_Identical_States (N : NFA) is
   begin
      Merge_Identical_States_Src (N);
      Merge_Identical_States_Dest (N);
   end Merge_Identical_States;

   procedure Merge_Edges (N : NFA)
   is
      use PSL.CSE;
      Nbr_States : Natural;
   begin
      Labelize_States (N, Nbr_States);
      declare
         Last_State : constant Int32 := Int32 (Nbr_States) - 1;
         type Edge_Array is array (0 .. Last_State) of NFA_Edge;
         Edges : Edge_Array;
         S, D : NFA_State;
         L_D : Int32;
         E, Next_E : NFA_Edge;
      begin
         --  Iterate on states.
         S := Get_First_State (N);
         while S /= No_State loop

            Edges := (others => No_Edge);
            E := Get_First_Src_Edge (S);
            while E /= No_Edge loop
               Next_E := Get_Next_Src_Edge (E);
               D := Get_Edge_Dest (E);
               L_D := Get_State_Label (D);
               if Edges (L_D) /= No_Edge then
                  Set_Edge_Expr
                    (Edges (L_D),
                     Build_Bool_Or (Get_Edge_Expr (Edges (L_D)),
                                    Get_Edge_Expr (E)));
                  --  FIXME: reduce expression.
                  Remove_Edge (E);
               else
                  Edges (L_D) := E;
               end if;
               E := Next_E;
            end loop;

            S := Get_Next_State (S);
         end loop;
      end;
   end Merge_Edges;

   procedure Remove_Identical_Src_Edges (S : NFA_State)
   is
      Next_E, E : NFA_Edge;
   begin
      Sort_Src_Edges (S);
      E := Get_First_Src_Edge (S);
      if E = No_Edge then
         return;
      end if;
      loop
         Next_E := Get_Next_Src_Edge (E);
         exit when Next_E = No_Edge;
         if Get_Edge_Dest (E) = Get_Edge_Dest (Next_E)
           and then Get_Edge_Expr (E) = Get_Edge_Expr (Next_E)
         then
            Remove_Edge (Next_E);
         else
            E := Next_E;
         end if;
      end loop;
   end Remove_Identical_Src_Edges;

   procedure Remove_Identical_Dest_Edges (S : NFA_State)
   is
      Next_E, E : NFA_Edge;
   begin
      Sort_Dest_Edges (S);
      E := Get_First_Dest_Edge (S);
      if E = No_Edge then
         return;
      end if;
      loop
         Next_E := Get_Next_Dest_Edge (E);
         exit when Next_E = No_Edge;
         if Get_Edge_Src (E) = Get_Edge_Src (Next_E)
           and then Get_Edge_Expr (E) = Get_Edge_Expr (Next_E)
         then
            Remove_Edge (Next_E);
         else
            E := Next_E;
         end if;
      end loop;
   end Remove_Identical_Dest_Edges;

   procedure Find_Partitions (N : NFA; Nbr_States : Natural)
   is
      Last_State : constant NFA_State := NFA_State (Nbr_States) - 1;
      type Part_Offset is new Int32 range -1 .. Nat32 (Nbr_States - 1);
      type Part_Id is new Part_Offset range 0 .. Part_Offset'Last;

      --  State to partition id.
      State_Part : array (0 .. Last_State) of Part_Id;
      pragma Unreferenced (State_Part);

      --  Last partition index.
      Last_Part : Part_Id;

      --  Partitions content.

      --  To get the states in a partition P, first get the offset OFF
      --  (from Offsets) of P.  States are in Parts (OFF ...).  The
      --  number of states is not known, but they all belong to P
      --  (check with STATE_PART).
      Parts : array (Part_Offset) of NFA_State;
      type Offset_Array is array (Part_Id) of Part_Offset;
      Start_Offsets : Offset_Array;
      Last_Offsets : Offset_Array;

      S, Final_State : NFA_State;
      First_S : NFA_State;
      Off, Last_Off : Part_Offset;

      Stable, Stable1 : Boolean;

      function Is_Equivalent (L, R : NFA_State) return Boolean is
      begin
         raise Program_Error;
         return False;
      end Is_Equivalent;
   begin
      --  Return now for trivial cases (0 or 1 state).
      if Nbr_States < 2 then
         return;
      end if;

      --  Partition 1 contains the final state.
      --  Partition 0 contains the other states.
      Final_State := Get_Final_State (N);
      Last_Part := 1;
      State_Part := (others => 0);
      State_Part (Final_State) := 1;
      S := Get_First_State (N);
      Off := -1;
      while S /= No_State loop
         if S /= Last_State then
            Off := Off + 1;
            Parts (Off) := S;
         end if;
         S := Get_Next_State (S);
      end loop;
      Start_Offsets (0) := 0;
      Last_Offsets (0) := Off;
      Start_Offsets (1) := Off + 1;
      Last_Offsets (1) := Off + 1;
      Parts (Off + 1) := Final_State;

      --  Now the hard work.
      loop
         Stable := True;
         --  For every partition
         for P in 0 .. Last_Part loop
            Off := Start_Offsets (P);
            First_S := Parts (Off);
            Off := Off + 1;

            --  For every S != First_S in P.
            Last_Off := Last_Offsets (P);
            Stable1 := True;
            while Off <= Last_Off loop
               S := Parts (Off);

               if not Is_Equivalent (First_S, S) then
                  --  Swap S with the last element of the partition.
                  Parts (Off) := Parts (Last_Off);
                  Parts (Last_Off) := S;
                  --  Reduce partition size.
                  Last_Off := Last_Off - 1;
                  Last_Offsets (P) := Last_Off;

                  if Stable1 then
                     --  Create a new partition.
                     Last_Part := Last_Part + 1;
                     Last_Offsets (Last_Part) := Last_Off + 1;
                     Stable1 := False;
                  end if;
                  --  Put S in the new partition.
                  Start_Offsets (Last_Part) := Last_Off + 1;
                  State_Part (S) := Last_Part;
                  Stable := False;

                  --  And continue with the swapped state.
               else
                  Off := Off + 1;
               end if;
            end loop;
         end loop;
         exit when Stable;
      end loop;
   end Find_Partitions;
   pragma Unreferenced (Find_Partitions);
end PSL.Optimize;
