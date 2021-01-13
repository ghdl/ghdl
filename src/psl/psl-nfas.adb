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

with Tables;
with PSL.Types; use PSL.Types;

package body PSL.NFAs is
   --  Record that describes an NFA.
   type NFA_Node is record
      --  Chain of States.
      First_State : NFA_State;
      Last_State : NFA_State;

      --  Start and final state.
      Start : NFA_State;
      Final : NFA_State;

      Active : NFA_State;

      --  If true there is an epsilon transition between the start and
      --  the final state.
      Epsilon : Boolean;
   end record;

   --  Record that describe a node.
   type NFA_State_Node is record
      --  States may be numbered.
      Label : Int32;

      --  Edges.
      First_Src : NFA_Edge;
      First_Dst : NFA_Edge;

      --  State links.
      Next_State : NFA_State;
      Prev_State : NFA_State;

      --  User fields.
      User_Link : NFA_State;
      User_Flag : Boolean;
   end record;

   --  Record that describe an edge between SRC and DEST.
   type NFA_Edge_Node is record
      Dest : NFA_State;
      Src : NFA_State;
      Expr : Node;

      --  Links.
      Next_Src : NFA_Edge;
      Next_Dst : NFA_Edge;
   end record;

   --  Table of NFA.
   package Nfat is new Tables
     (Table_Component_Type => NFA_Node,
      Table_Index_Type => NFA,
      Table_Low_Bound => 1,
      Table_Initial => 128);

   --  List of free nodes.
   Free_Nfas : NFA := No_NFA;

   --  Table of States.
   package Statet is new Tables
     (Table_Component_Type => NFA_State_Node,
      Table_Index_Type => NFA_State,
      Table_Low_Bound => 1,
      Table_Initial => 128);

   --  List of free states.
   Free_States : NFA_State := No_State;

   --  Table of edges.
   package Transt is new Tables
     (Table_Component_Type => NFA_Edge_Node,
      Table_Index_Type => NFA_Edge,
      Table_Low_Bound => 1,
      Table_Initial => 128);

   --  List of free edges.
   Free_Edges : NFA_Edge := No_Edge;

   function Get_First_State (N : NFA) return NFA_State is
   begin
      return Nfat.Table (N).First_State;
   end Get_First_State;

   function Get_Last_State (N : NFA) return NFA_State is
   begin
      return Nfat.Table (N).Last_State;
   end Get_Last_State;

   procedure Set_First_State (N : NFA; S : NFA_State) is
   begin
      Nfat.Table (N).First_State := S;
   end Set_First_State;

   procedure Set_Last_State (N : NFA; S : NFA_State) is
   begin
      Nfat.Table (N).Last_State := S;
   end Set_Last_State;

   function Get_Next_State (S : NFA_State) return NFA_State is
   begin
      return Statet.Table (S).Next_State;
   end Get_Next_State;

   procedure Set_Next_State (S : NFA_State; N : NFA_State) is
   begin
      Statet.Table (S).Next_State := N;
   end Set_Next_State;

   function Get_Prev_State (S : NFA_State) return NFA_State is
   begin
      return Statet.Table (S).Prev_State;
   end Get_Prev_State;

   procedure Set_Prev_State (S : NFA_State; N : NFA_State) is
   begin
      Statet.Table (S).Prev_State := N;
   end Set_Prev_State;

   function Get_State_Label (S : NFA_State) return Int32 is
   begin
      return Statet.Table (S).Label;
   end Get_State_Label;

   procedure Set_State_Label (S : NFA_State; Label : Int32) is
   begin
      Statet.Table (S).Label := Label;
   end Set_State_Label;

   function Get_Epsilon_NFA (N : NFA) return Boolean is
   begin
      return Nfat.Table (N).Epsilon;
   end Get_Epsilon_NFA;

   procedure Set_Epsilon_NFA (N : NFA; Flag : Boolean) is
   begin
      Nfat.Table (N).Epsilon := Flag;
   end Set_Epsilon_NFA;

   function Add_State (N : NFA) return NFA_State is
      Res : NFA_State;
      Last : NFA_State;
   begin
      --  Get a new state.
      if Free_States = No_State then
         Statet.Increment_Last;
         Res := Statet.Last;
      else
         Res := Free_States;
         Free_States := Get_Next_State (Res);
      end if;

      --  Put it in N.
      Last := Get_Last_State (N);
      Statet.Table (Res) := (Label => 0,
                             First_Src => No_Edge,
                             First_Dst => No_Edge,
                             Next_State => No_State,
                             Prev_State => Last,
                             User_Link => No_State,
                             User_Flag => False);
      if Last = No_State then
         Nfat.Table (N).First_State := Res;
      else
         Statet.Table (Last).Next_State := Res;
      end if;
      Nfat.Table (N).Last_State := Res;
      return Res;
   end Add_State;

   procedure Delete_Detached_State (S : NFA_State) is
   begin
      --  Put it in front of the free_states list.
      Set_Next_State (S, Free_States);
      Free_States := S;
   end Delete_Detached_State;

   function Create_NFA return NFA
   is
      Res : NFA;
   begin
      --  Allocate a node.
      if Free_Nfas = No_NFA then
         Nfat.Increment_Last;
         Res := Nfat.Last;
      else
         Res := Free_Nfas;
         Free_Nfas := NFA (Get_First_State (Res));
      end if;

      --  Fill it.
      Nfat.Table (Res) := (First_State => No_State,
                           Last_State => No_State,
                           Start | Final | Active => No_State,
                           Epsilon => False);
      return Res;
   end Create_NFA;

   procedure Set_First_Src_Edge (N : NFA_State; T : NFA_Edge) is
   begin
      Statet.Table (N).First_Src := T;
   end Set_First_Src_Edge;

   function Get_First_Src_Edge (N : NFA_State) return NFA_Edge is
   begin
      return Statet.Table (N).First_Src;
   end Get_First_Src_Edge;

   procedure Set_First_Dest_Edge (N : NFA_State; T : NFA_Edge) is
   begin
      Statet.Table (N).First_Dst := T;
   end Set_First_Dest_Edge;

   function Get_First_Dest_Edge (N : NFA_State) return NFA_Edge is
   begin
      return Statet.Table (N).First_Dst;
   end Get_First_Dest_Edge;

   function Get_State_Flag (S : NFA_State) return Boolean is
   begin
      return Statet.Table (S).User_Flag;
   end Get_State_Flag;

   procedure Set_State_Flag (S : NFA_State; Val : Boolean) is
   begin
      Statet.Table (S).User_Flag := Val;
   end Set_State_Flag;

   function Get_State_User_Link (S : NFA_State) return NFA_State is
   begin
      return Statet.Table (S).User_Link;
   end Get_State_User_Link;

   procedure Set_State_User_Link (S : NFA_State; Link : NFA_State) is
   begin
      Statet.Table (S).User_Link := Link;
   end Set_State_User_Link;

   function Add_Edge (Src : NFA_State; Dest : NFA_State; Expr : Node)
                     return NFA_Edge
   is
      Res : NFA_Edge;
   begin
      --  Allocate a note.
      if Free_Edges /= No_Edge then
         Res := Free_Edges;
         Free_Edges := Get_Next_Dest_Edge (Res);
      else
         Transt.Increment_Last;
         Res := Transt.Last;
      end if;

      --  Initialize it.
      Transt.Table (Res) := (Dest => Dest,
                             Src => Src,
                             Expr => Expr,
                             Next_Src => Get_First_Src_Edge (Src),
                             Next_Dst => Get_First_Dest_Edge (Dest));
      Set_First_Src_Edge (Src, Res);
      Set_First_Dest_Edge (Dest, Res);
      return Res;
   end Add_Edge;

   procedure Add_Edge (Src : NFA_State; Dest : NFA_State; Expr : Node) is
      Res : NFA_Edge;
      pragma Unreferenced (Res);
   begin
      Res := Add_Edge (Src, Dest, Expr);
   end Add_Edge;

   procedure Delete_Empty_NFA (N : NFA) is
   begin
      pragma Assert (Get_First_State (N) = No_State);
      pragma Assert (Get_Last_State (N) = No_State);

      --  Put it in front of the free_nfas list.
      Set_First_State (N, NFA_State (Free_Nfas));
      Free_Nfas := N;
   end Delete_Empty_NFA;

   function Get_Start_State (N : NFA) return NFA_State is
   begin
      return Nfat.Table (N).Start;
   end Get_Start_State;

   procedure Set_Start_State (N : NFA; S : NFA_State) is
   begin
      Nfat.Table (N).Start := S;
   end Set_Start_State;

   function Get_Final_State (N : NFA) return NFA_State is
   begin
      return Nfat.Table (N).Final;
   end Get_Final_State;

   procedure Set_Final_State (N : NFA; S : NFA_State) is
   begin
      Nfat.Table (N).Final := S;
   end Set_Final_State;

   function Get_Active_State (N : NFA) return NFA_State is
   begin
      return Nfat.Table (N).Active;
   end Get_Active_State;

   procedure Set_Active_State (N : NFA; S : NFA_State) is
   begin
      Nfat.Table (N).Active := S;
   end Set_Active_State;

   function Get_Next_Src_Edge (N : NFA_Edge) return NFA_Edge is
   begin
      return Transt.Table (N).Next_Src;
   end Get_Next_Src_Edge;

   procedure Set_Next_Src_Edge (E : NFA_Edge; N_E : NFA_Edge) is
   begin
      Transt.Table (E).Next_Src := N_E;
   end Set_Next_Src_Edge;

   function Get_Next_Dest_Edge (N : NFA_Edge) return NFA_Edge is
   begin
      return Transt.Table (N).Next_Dst;
   end Get_Next_Dest_Edge;

   procedure Set_Next_Dest_Edge (E : NFA_Edge; N_E : NFA_Edge) is
   begin
      Transt.Table (E).Next_Dst := N_E;
   end Set_Next_Dest_Edge;

   function Get_Edge_Dest (E : NFA_Edge) return NFA_State is
   begin
      return Transt.Table (E).Dest;
   end Get_Edge_Dest;

   procedure Set_Edge_Dest (E : NFA_Edge; D : NFA_State) is
   begin
      Transt.Table (E).Dest := D;
   end Set_Edge_Dest;

   function Get_Edge_Src (E : NFA_Edge) return NFA_State is
   begin
      return Transt.Table (E).Src;
   end Get_Edge_Src;

   procedure Set_Edge_Src (E : NFA_Edge; D : NFA_State) is
   begin
      Transt.Table (E).Src := D;
   end Set_Edge_Src;

   function Get_Edge_Expr (E : NFA_Edge) return Node is
   begin
      return Transt.Table (E).Expr;
   end Get_Edge_Expr;

   procedure Set_Edge_Expr (E : NFA_Edge; N : Node) is
   begin
      Transt.Table (E).Expr := N;
   end Set_Edge_Expr;

   procedure Remove_Unconnected_State (N : NFA; S : NFA_State) is
      N_S : constant NFA_State := Get_Next_State (S);
      P_S : constant NFA_State := Get_Prev_State (S);
   begin
      pragma Assert (Get_First_Src_Edge (S) = No_Edge);
      pragma Assert (Get_First_Dest_Edge (S) = No_Edge);

      if P_S = No_State then
         Set_First_State (N, N_S);
      else
         Set_Next_State (P_S, N_S);
      end if;
      if N_S = No_State then
         Set_Last_State (N, P_S);
      else
         Set_Prev_State (N_S, P_S);
      end if;
      Delete_Detached_State (S);
   end Remove_Unconnected_State;

   procedure Merge_NFA (L, R : NFA) is
      Last_L  : constant NFA_State := Get_Last_State (L);
      First_R : constant NFA_State := Get_First_State (R);
      Last_R  : constant NFA_State := Get_Last_State (R);
   begin
      if First_R = No_State then
         return;
      end if;
      if Last_L = No_State then
         Set_First_State (L, First_R);
      else
         Set_Next_State (Last_L, First_R);
         Set_Prev_State (First_R, Last_L);
      end if;
      Set_Last_State (L, Last_R);
      Set_First_State (R, No_State);
      Set_Last_State (R, No_State);
      Delete_Empty_NFA (R);
   end Merge_NFA;

   procedure Redest_Edges (S : NFA_State; Dest : NFA_State) is
      E, N_E : NFA_Edge;
      Head : NFA_Edge;
   begin
      E := Get_First_Dest_Edge (S);
      if E = No_Edge then
         return;
      end if;
      Set_First_Dest_Edge (S, No_Edge);
      Head := Get_First_Dest_Edge (Dest);
      Set_First_Dest_Edge (Dest, E);
      loop
         N_E := Get_Next_Dest_Edge (E);
         Set_Edge_Dest (E, Dest);
         exit when N_E = No_Edge;
         E := N_E;
      end loop;
      Set_Next_Dest_Edge (E, Head);
   end Redest_Edges;

   procedure Resource_Edges (S : NFA_State; Src : NFA_State) is
      E, N_E : NFA_Edge;
      Head : NFA_Edge;
   begin
      E := Get_First_Src_Edge (S);
      if E = No_Edge then
         return;
      end if;
      Set_First_Src_Edge (S, No_Edge);
      Head := Get_First_Src_Edge (Src);
      Set_First_Src_Edge (Src, E);
      loop
         N_E := Get_Next_Src_Edge (E);
         Set_Edge_Src (E, Src);
         exit when N_E = No_Edge;
         E := N_E;
      end loop;
      Set_Next_Src_Edge (E, Head);
   end Resource_Edges;

   procedure Disconnect_Edge_Src (N : NFA_State; E : NFA_Edge) is
      N_E : constant NFA_Edge := Get_Next_Src_Edge (E);
      Prev, Cur : NFA_Edge;
   begin
      Cur := Get_First_Src_Edge (N);
      if Cur = E then
         Set_First_Src_Edge (N, N_E);
      else
         while Cur /= E loop
            Prev := Cur;
            Cur := Get_Next_Src_Edge (Prev);
            pragma Assert (Cur /= No_Edge);
         end loop;
         Set_Next_Src_Edge (Prev, N_E);
      end if;
   end Disconnect_Edge_Src;

   procedure Disconnect_Edge_Dest (N : NFA_State; E : NFA_Edge) is
      N_E : constant NFA_Edge := Get_Next_Dest_Edge (E);
      Prev, Cur : NFA_Edge;
   begin
      Cur := Get_First_Dest_Edge (N);
      if Cur = E then
         Set_First_Dest_Edge (N, N_E);
      else
         while Cur /= E loop
            Prev := Cur;
            Cur := Get_Next_Dest_Edge (Prev);
            pragma Assert (Cur /= No_Edge);
         end loop;
         Set_Next_Dest_Edge (Prev, N_E);
      end if;
   end Disconnect_Edge_Dest;

   procedure Remove_Edge (E : NFA_Edge) is
   begin
      Disconnect_Edge_Src (Get_Edge_Src (E), E);
      Disconnect_Edge_Dest (Get_Edge_Dest (E), E);

      -- Put it on the free list.
      Set_Next_Dest_Edge (E, Free_Edges);
      Free_Edges := E;
   end Remove_Edge;

   procedure Remove_State (N : NFA; S : NFA_State) is
      E, N_E : NFA_Edge;
   begin
      E := Get_First_Dest_Edge (S);
      while E /= No_Edge loop
         N_E := Get_Next_Dest_Edge (E);
         Remove_Edge (E);
         E := N_E;
      end loop;

      E := Get_First_Src_Edge (S);
      while E /= No_Edge loop
         N_E := Get_Next_Src_Edge (E);
         Remove_Edge (E);
         E := N_E;
      end loop;

      Remove_Unconnected_State (N, S);
   end Remove_State;

   procedure Labelize_States (N : NFA; Nbr_States : out Natural)
   is
      S, Start, Final : NFA_State;
   begin
      S := Get_First_State (N);
      Start := Get_Start_State (N);
      Final := Get_Final_State (N);
      pragma Assert (Start /= No_State);
      Set_State_Label (Start, 0);
      Nbr_States := 1;
      while S /= No_State loop
         if S /= Start and then S /= Final then
            Set_State_Label (S, Int32 (Nbr_States));
            Nbr_States := Nbr_States + 1;
         end if;
         S := Get_Next_State (S);
      end loop;
      pragma Assert (Final /= No_State);
      Set_State_Label (Final, Int32 (Nbr_States));
      Nbr_States := Nbr_States + 1;
   end Labelize_States;

   procedure Labelize_States_Debug (N : NFA)
   is
      S : NFA_State;
   begin
      S := Get_First_State (N);
      while S /= No_State loop
         Set_State_Label (S, Int32 (S));
         S := Get_Next_State (S);
      end loop;
   end Labelize_States_Debug;

end PSL.NFAs;
