--  PSL - NFA definition
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
with PSL.Nodes; use PSL.Nodes;

package PSL.NFAs is
   --  Represents NFAs for PSL.
   --  These NFAs have the following restrictions:
   --  * 1 start state
   --  * 1 final state (which can be the start state).
   --  * possible epsilon transition between start and final state with the
   --    meaning: A | eps

   type NFA_State is new Nat32;
   type NFA_Edge is new Nat32;

   No_NFA   : constant NFA := 0;
   No_State : constant NFA_State := 0;
   No_Edge  : constant NFA_Edge := 0;

   --  Create a new NFA.
   function Create_NFA return NFA;

   --  Add a new state to an NFA.
   function Add_State (N : NFA) return NFA_State;

   --  Add a transition.
   procedure Add_Edge (Src : NFA_State; Dest : NFA_State; Expr : Node);
   function Add_Edge (Src : NFA_State; Dest : NFA_State; Expr : Node)
                     return NFA_Edge;

   --  Disconnect and free edge E.
   procedure Remove_Edge (E : NFA_Edge);

   --  Return TRUE if there is an epsilon edge between start and final.
   function Get_Epsilon_NFA (N : NFA) return Boolean;
   procedure Set_Epsilon_NFA (N : NFA; Flag : Boolean);

   --  Each NFA has one start and one final state.
   function Get_Start_State (N : NFA) return NFA_State;
   procedure Set_Start_State (N : NFA; S : NFA_State);

   procedure Set_Final_State (N : NFA; S : NFA_State);
   function Get_Final_State (N : NFA) return NFA_State;

   --  Each NFA also can have an active state.
   procedure Set_Active_State (N : NFA; S : NFA_State);
   function Get_Active_State (N : NFA) return NFA_State;

   --  Iterate on all states.
   function Get_First_State (N : NFA) return NFA_State;
   function Get_Next_State (S : NFA_State) return NFA_State;

   --  Per state user flag.
   --  Initialized set to false.
   function Get_State_Flag (S : NFA_State) return Boolean;
   procedure Set_State_Flag (S : NFA_State; Val : Boolean);

   --  Per state user link.
   function Get_State_User_Link (S : NFA_State) return NFA_State;
   procedure Set_State_User_Link (S : NFA_State; Link : NFA_State);

   --  Edges of a state.
   --  A source edge is an edge whose source is the state.
   function Get_First_Src_Edge (N : NFA_State) return NFA_Edge;
   function Get_Next_Src_Edge (N : NFA_Edge) return NFA_Edge;

   --  A dest edge is an edge whose destination is the state.
   function Get_First_Dest_Edge (N : NFA_State) return NFA_Edge;
   function Get_Next_Dest_Edge (N : NFA_Edge) return NFA_Edge;

   function Get_State_Label (S : NFA_State) return Int32;
   procedure Set_State_Label (S : NFA_State; Label : Int32);

   function Get_Edge_Dest (E: NFA_Edge) return NFA_State;
   function Get_Edge_Src (E : NFA_Edge) return NFA_State;
   function Get_Edge_Expr (E : NFA_Edge) return Node;

   --  Move States and edges of R to L.
   procedure Merge_NFA (L, R : NFA);

   --  All edges to S are redirected to DEST.
   procedure Redest_Edges (S : NFA_State; Dest : NFA_State);

   --  All edges from S are redirected from SRC.
   procedure Resource_Edges (S : NFA_State; Src : NFA_State);

   --  Remove a state.  The state must be unconnected.
   procedure Remove_Unconnected_State (N : NFA; S : NFA_State);

   --  Deconnect and remove state S.
   procedure Remove_State (N : NFA; S : NFA_State);

   procedure Delete_Empty_NFA (N : NFA);

   --  Set a label on the states of the NFA N.
   --  Start state is has label 0.
   --  Return the number of states.
   procedure Labelize_States (N : NFA; Nbr_States : out Natural);

   --  Set state index as state label.
   --  Used to debug an NFA.
   procedure Labelize_States_Debug (N : NFA);

   procedure Set_Edge_Expr (E : NFA_Edge; N : Node);
private
   --  Low level procedures.  Shouldn't be used directly.
   procedure Set_First_Dest_Edge (N : NFA_State; T : NFA_Edge);
   procedure Set_Next_Dest_Edge (E : NFA_Edge; N_E : NFA_Edge);
   procedure Set_First_Src_Edge (N : NFA_State; T : NFA_Edge);
   procedure Set_Next_Src_Edge (E : NFA_Edge; N_E : NFA_Edge);
   procedure Set_Edge_Dest (E : NFA_Edge; D : NFA_State);
   procedure Set_Edge_Src (E : NFA_Edge; D : NFA_State);
end PSL.NFAs;
