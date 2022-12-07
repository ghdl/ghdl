--  PSL - NFA builder.
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
with Ada.Text_IO; use Ada.Text_IO;
with Types; use Types;
with PSL.Types; use PSL.Types;
with PSL.Errors; use PSL.Errors;
with PSL.CSE; use PSL.CSE;
with PSL.QM;
with PSL.Disp_NFAs; use PSL.Disp_NFAs;
with PSL.Optimize; use PSL.Optimize;
with PSL.NFAs.Utils;
with PSL.Prints;
with PSL.NFAs; use PSL.NFAs;

package body PSL.Build is
   package Intersection is
      function Build_Inter (L, R : NFA; Match_Len : Boolean) return NFA;
   end Intersection;

   package body Intersection is

      type Stack_Entry_Id is new Natural;
      No_Stack_Entry : constant Stack_Entry_Id := 0;
      type Stack_Entry is record
         L, R : NFA_State;
         Res : NFA_State;
         Next_Unhandled : Stack_Entry_Id;
      end record;

      package Stackt is new Tables
        (Table_Component_Type => Stack_Entry,
         Table_Index_Type => Stack_Entry_Id,
         Table_Low_Bound => 1,
         Table_Initial => 128);

      First_Unhandled : Stack_Entry_Id;

      procedure Init_Stack is
      begin
         Stackt.Init;
         First_Unhandled := No_Stack_Entry;
      end Init_Stack;

      function Not_Empty return Boolean is
      begin
         return First_Unhandled /= No_Stack_Entry;
      end Not_Empty;

      procedure Pop_State (L, R : out NFA_State) is
      begin
         L := Stackt.Table (First_Unhandled).L;
         R := Stackt.Table (First_Unhandled).R;
         First_Unhandled := Stackt.Table (First_Unhandled).Next_Unhandled;
      end Pop_State;

      function Get_State (N : NFA; L, R : NFA_State) return NFA_State
      is
         Res : NFA_State;
      begin
         for I in Stackt.First .. Stackt.Last loop
            if Stackt.Table (I).L = L
              and then Stackt.Table (I).R = R
            then
               return Stackt.Table (I).Res;
            end if;
         end loop;
         Res := Add_State (N);
         Stackt.Append ((L => L, R => R, Res => Res,
                         Next_Unhandled => First_Unhandled));
         First_Unhandled := Stackt.Last;
         return Res;
      end Get_State;

      function Build_Inter (L, R : NFA; Match_Len : Boolean) return NFA
      is
         Start_L, Start_R : NFA_State;
         Final_L, Final_R : NFA_State;
         S_L, S_R : NFA_State;
         E_L, E_R : NFA_Edge;
         Res : NFA;
         Start : NFA_State;
         Extra_L, Extra_R : NFA_Edge;
         T : Node;
      begin
         Start_L := Get_Start_State (L);
         Start_R := Get_Start_State (R);
         Final_R := Get_Final_State (R);
         Final_L := Get_Final_State (L);

         if False then
            Disp_Body (L);
            Disp_Body (R);
            Put ("//start state: ");
            Disp_State (Start_L);
            Put (",");
            Disp_State (Start_R);
            New_Line;
         end if;

         if Match_Len then
            Extra_L := No_Edge;
            Extra_R := No_Edge;
         else
            Extra_L := Add_Edge (Final_L, Final_L, True_Node);
            Extra_R := Add_Edge (Final_R, Final_R, True_Node);
         end if;

         Res := Create_NFA;
         Init_Stack;
         Start := Get_State (Res, Start_L, Start_R);
         Set_Start_State (Res, Start);

         while Not_Empty loop
            Pop_State (S_L, S_R);

            if False then
               Put ("//poped state: ");
               Disp_State (S_L);
               Put (",");
               Disp_State (S_R);
               New_Line;
            end if;

            E_L := Get_First_Src_Edge (S_L);
            while E_L /= No_Edge loop
               E_R := Get_First_Src_Edge (S_R);
               while E_R /= No_Edge loop
                  if not (E_L = Extra_L and E_R = Extra_R) then
                     T := Build_Bool_And (Get_Edge_Expr (E_L),
                                          Get_Edge_Expr (E_R));
                     Add_Edge (Get_State (Res, S_L, S_R),
                               Get_State (Res,
                                          Get_Edge_Dest (E_L),
                                          Get_Edge_Dest (E_R)),
                               T);
                  end if;
                  E_R := Get_Next_Src_Edge (E_R);
               end loop;
               E_L := Get_Next_Src_Edge (E_L);
            end loop;
         end loop;
         Set_Final_State (Res, Get_State (Res, Final_L, Final_R));
         Remove_Unreachable_States (Res);

         if not Match_Len then
            Remove_Edge (Extra_L);
            Remove_Edge (Extra_R);
         end if;

         --  FIXME: free L and R.
         return Res;
      end Build_Inter;
   end Intersection;

   --  All edges from A are duplicated using B as a source.
   --  Handle epsilon-edges.
   procedure Duplicate_Src_Edges (N : NFA; A, B : NFA_State)
   is
      pragma Unreferenced (N);
      E : NFA_Edge;
      Expr : Node;
      Dest : NFA_State;
   begin
      pragma Assert (A /= B);
      E := Get_First_Src_Edge (A);
      while E /= No_Edge loop
         Expr := Get_Edge_Expr (E);
         Dest := Get_Edge_Dest (E);
         if Expr /= Null_Node then
            Add_Edge (B, Dest, Expr);
         end if;
         E := Get_Next_Src_Edge (E);
      end loop;
   end Duplicate_Src_Edges;

   --  All edges to A are duplicated using B as a destination.
   --  Handle epsilon-edges.
   procedure Duplicate_Dest_Edges (N : NFA; A, B : NFA_State)
   is
      pragma Unreferenced (N);
      E : NFA_Edge;
      Expr : Node;
      Src : NFA_State;
   begin
      pragma Assert (A /= B);
      E := Get_First_Dest_Edge (A);
      while E /= No_Edge loop
         Expr := Get_Edge_Expr (E);
         Src := Get_Edge_Src (E);
         if Expr /= Null_Node then
            Add_Edge (Src, B, Expr);
         end if;
         E := Get_Next_Dest_Edge (E);
      end loop;
   end Duplicate_Dest_Edges;

   procedure Remove_Epsilon_Edge (N : NFA; S, D : NFA_State) is
   begin
      if Get_First_Src_Edge (S) = No_Edge then
         --  No edge from S.
         --  Move edges to S to D.
         Redest_Edges (S, D);
         Remove_Unconnected_State (N, S);
         if Get_Start_State (N) = S then
            Set_Start_State (N, D);
         end if;
      elsif Get_First_Dest_Edge (D) = No_Edge then
         --  No edge to D.
         --  Move edges from D to S.
         Resource_Edges (D, S);
         Remove_Unconnected_State (N, D);
         if Get_Final_State (N) = D then
            Set_Final_State (N, S);
         end if;
      else
         Duplicate_Dest_Edges (N, S, D);
         Duplicate_Src_Edges (N, D, S);
         Remove_Identical_Src_Edges (S);
      end if;
   end Remove_Epsilon_Edge;

   procedure Remove_Epsilon (N : NFA;
                             E : NFA_Edge) is
      S : constant NFA_State := Get_Edge_Src (E);
      D : constant NFA_State := Get_Edge_Dest (E);
   begin
      Remove_Edge (E);

      Remove_Epsilon_Edge (N, S, D);
   end Remove_Epsilon;

   function Build_Concat (L, R : NFA) return NFA
   is
      Start_L, Start_R : NFA_State;
      Final_L, Final_R : NFA_State;
      Eps_L, Eps_R : Boolean;
      E_L, E_R : NFA_Edge;
   begin
      Start_L := Get_Start_State (L);
      Start_R := Get_Start_State (R);
      Final_R := Get_Final_State (R);
      Final_L := Get_Final_State (L);
      Eps_L := Get_Epsilon_NFA (L);
      Eps_R := Get_Epsilon_NFA (R);

      Merge_NFA (L, R);

      Set_Start_State (L, Start_L);
      Set_Final_State (L, Final_R);
      Set_Epsilon_NFA (L, False);

      if Eps_L then
         E_L := Add_Edge (Start_L, Final_L, Null_Node);
      end if;

      if Eps_R then
         E_R := Add_Edge (Start_R, Final_R, Null_Node);
      end if;

      Remove_Epsilon_Edge (L, Final_L, Start_R);

      if Eps_L then
         Remove_Epsilon (L, E_L);
      end if;
      if Eps_R then
         Remove_Epsilon (L, E_R);
      end if;

      if (Start_L = Final_L or else Eps_L)
        and then (Start_R = Final_R or else Eps_R)
      then
         Set_Epsilon_NFA (L, True);
      end if;

      Remove_Identical_Src_Edges (Final_L);
      Remove_Identical_Dest_Edges (Start_R);

      return L;
   end Build_Concat;

   function Build_Or (L, R : NFA) return NFA
   is
      Start_L, Start_R : NFA_State;
      Final_L, Final_R : NFA_State;
      Eps : Boolean;
      Start, Final : NFA_State;
      E_S_L, E_S_R, E_L_F, E_R_F : NFA_Edge;
   begin
      Start_L := Get_Start_State (L);
      Start_R := Get_Start_State (R);
      Final_R := Get_Final_State (R);
      Final_L := Get_Final_State (L);
      Eps := Get_Epsilon_NFA (L) or Get_Epsilon_NFA (R);

      --  Optimize [*0] | R.
      --  TODO: this was not valid if there is an edge from Start(R)
      --  to Start(R), like: {[*0] | {d[*]; e}}
      if False
        and then Start_L = Final_L
        and then Get_First_Src_Edge (Start_L) = No_Edge
      then
         if Start_R /= Final_R then
            Set_Epsilon_NFA (R, True);
         end if;
         --  FIXME
         --  delete_NFA (L);
         return R;
      end if;

      Merge_NFA (L, R);

      --  Use Thompson construction.
      Start := Add_State (L);
      Set_Start_State (L, Start);
      E_S_L := Add_Edge (Start, Start_L, Null_Node);
      E_S_R := Add_Edge (Start, Start_R, Null_Node);

      Final := Add_State (L);
      Set_Final_State (L, Final);
      E_L_F := Add_Edge (Final_L, Final, Null_Node);
      E_R_F := Add_Edge (Final_R, Final, Null_Node);

      Set_Epsilon_NFA (L, Eps);

      Remove_Epsilon (L, E_S_L);
      Remove_Epsilon (L, E_S_R);
      Remove_Epsilon (L, E_L_F);
      Remove_Epsilon (L, E_R_F);

      return L;
   end Build_Or;

   function Build_Fusion (L, R : NFA) return NFA
   is
      Start_R : NFA_State;
      Final_L, Final_R, S_L : NFA_State;
      E_L : NFA_Edge;
      E_R : NFA_Edge;
      N_L, Expr : Node;
   begin
      Start_R := Get_Start_State (R);
      Final_R := Get_Final_State (R);
      Final_L := Get_Final_State (L);

      Merge_NFA (L, R);

      E_L := Get_First_Dest_Edge (Final_L);
      while E_L /= No_Edge loop
         S_L := Get_Edge_Src (E_L);
         N_L := Get_Edge_Expr (E_L);

         E_R := Get_First_Src_Edge (Start_R);
         while E_R /= No_Edge loop
            Expr := Build_Bool_And (N_L, Get_Edge_Expr (E_R));
            Expr := PSL.QM.Reduce (Expr);
            if Expr /= False_Node then
               Add_Edge (S_L, Get_Edge_Dest (E_R), Expr);
            end if;
            E_R := Get_Next_Src_Edge (E_R);
         end loop;
         Remove_Identical_Src_Edges (S_L);
         E_L := Get_Next_Dest_Edge (E_L);
      end loop;

      Set_Final_State (L, Final_R);

      Set_Epsilon_NFA (L, False);

      if Get_First_Src_Edge (Final_L) = No_Edge
        and then Final_L /= Get_Active_State (L)
      then
         Remove_State (L, Final_L);
      end if;
      if Get_First_Dest_Edge (Start_R) = No_Edge then
         Remove_State (L, Start_R);
      end if;

      return L;
   end Build_Fusion;

   function Build_Star_Repeat (N : Node) return NFA is
      Res : NFA;
      Start, Final, S : NFA_State;
      Seq : Node;
   begin
      Seq := Get_Sequence (N);
      if Seq = Null_Node then
         --  Epsilon.
         Res := Create_NFA;
         S := Add_State (Res);
         Set_Start_State (Res, S);
         Set_Final_State (Res, S);
         return Res;
      end if;
      Res := Build_SERE_FA (Seq);
      Start := Get_Start_State (Res);
      Final := Get_Final_State (Res);
      Redest_Edges (Final, Start);
      Set_Final_State (Res, Start);
      Remove_Unconnected_State (Res, Final);
      Set_Epsilon_NFA (Res, False);
      return Res;
   end Build_Star_Repeat;

   function Build_Plus_Repeat (N : Node) return NFA is
      Res : NFA;
      Start, Final : NFA_State;
      T : NFA_Edge;
   begin
      Res := Build_SERE_FA (Get_Sequence (N));
      Start := Get_Start_State (Res);
      Final := Get_Final_State (Res);
      T := Get_First_Dest_Edge (Final);
      while T /= No_Edge loop
         Add_Edge (Get_Edge_Src (T), Start, Get_Edge_Expr (T));
         T := Get_Next_Src_Edge (T);
      end loop;
      return Res;
   end Build_Plus_Repeat;

   --  Association actual to formals, so that when a formal is referenced, the
   --  actual can be used instead.
   procedure Assoc_Instance (Decl : Node; Instance : Node)
   is
      Formal : Node;
      Actual : Node;
   begin
      --  Temporary associates actuals to formals.
      Formal := Get_Parameter_List (Decl);
      Actual := Get_Association_Chain (Instance);
      while Formal /= Null_Node loop
         if Actual = Null_Node then
            --  Not enough actual.
            raise Internal_Error;
         end if;
         if Get_Actual (Formal) /= Null_Node then
            --  Recursion
            raise Internal_Error;
         end if;
         Set_Actual (Formal, Get_Actual (Actual));
         Formal := Get_Chain (Formal);
         Actual := Get_Chain (Actual);
      end loop;
      if Actual /= Null_Node then
         --  Too many actual.
         raise Internal_Error;
      end if;
   end Assoc_Instance;

   procedure Unassoc_Instance (Decl : Node)
   is
      Formal : Node;
   begin
      --  Remove temporary association.
      Formal := Get_Parameter_List (Decl);
      while Formal /= Null_Node loop
         Set_Actual (Formal, Null_Node);
         Formal := Get_Chain (Formal);
      end loop;
   end Unassoc_Instance;

   function Build_SERE_FA (N : Node) return NFA
   is
      Res : NFA;
      S1, S2 : NFA_State;
   begin
      case Get_Kind (N) is
         when N_Booleans =>
            Res := Create_NFA;
            S1 := Add_State (Res);
            S2 := Add_State (Res);
            Set_Start_State (Res, S1);
            Set_Final_State (Res, S2);
            if N /= False_Node then
               Add_Edge (S1, S2, N);
            end if;
            return Res;
         when N_Braced_SERE =>
            return Build_SERE_FA (Get_SERE (N));
         when N_Concat_SERE =>
            return Build_Concat (Build_SERE_FA (Get_Left (N)),
                                 Build_SERE_FA (Get_Right (N)));
         when N_Fusion_SERE =>
            return Build_Fusion (Build_SERE_FA (Get_Left (N)),
                                 Build_SERE_FA (Get_Right (N)));
         when N_Match_And_Seq =>
            return Intersection.Build_Inter (Build_SERE_FA (Get_Left (N)),
                                             Build_SERE_FA (Get_Right (N)),
                                             True);
         when N_And_Seq =>
            return Intersection.Build_Inter (Build_SERE_FA (Get_Left (N)),
                                             Build_SERE_FA (Get_Right (N)),
                                             False);
         when N_Or_Prop
           | N_Or_Seq =>
            return Build_Or (Build_SERE_FA (Get_Left (N)),
                             Build_SERE_FA (Get_Right (N)));
         when N_Star_Repeat_Seq =>
            return Build_Star_Repeat (N);
         when N_Plus_Repeat_Seq =>
            return Build_Plus_Repeat (N);
         when N_Sequence_Instance
           | N_Endpoint_Instance =>
            declare
               Decl : Node;
            begin
               Decl := Get_Declaration (N);
               Assoc_Instance (Decl, N);
               Res := Build_SERE_FA (Get_Sequence (Decl));
               Unassoc_Instance (Decl);
               return Res;
            end;
         when N_Boolean_Parameter
           | N_Sequence_Parameter =>
            declare
               Actual : constant Node := Get_Actual (N);
            begin
               if Actual = Null_Node then
                  raise Internal_Error;
               end if;
               return Build_SERE_FA (Actual);
            end;
         when others =>
            Error_Kind ("build_sere_fa", N);
      end case;
   end Build_SERE_FA;

   function Count_Edges (S : NFA_State) return Natural
   is
      Res : Natural;
      E : NFA_Edge;
   begin
      Res := 0;
      E := Get_First_Src_Edge (S);
      while E /= No_Edge loop
         Res := Res + 1;
         E := Get_Next_Src_Edge (E);
      end loop;
      return Res;
   end Count_Edges;

   type Count_Vector is array (Natural range <>) of Natural;

   procedure Count_All_Edges (N : NFA; Res : out Count_Vector)
   is
      S : NFA_State;
   begin
      S := Get_First_State (N);
      while S /= No_State loop
         Res (Natural (Get_State_Label (S))) := Count_Edges (S);
         S := Get_Next_State (S);
      end loop;
   end Count_All_Edges;

   pragma Unreferenced (Count_All_Edges);

   package Determinize is
      --  Create a new NFA that reaches its final state only when N fails
      --  (ie when the final state is not reached).
      function Determinize (N : NFA) return NFA;
   end Determinize;

   package body Determinize is
      --  In all the comments N stands for the initial NFA (ie the NFA to
      --  determinize).

      use Prints;

      Flag_Trace : constant Boolean := False;
      Last_Label : Int32 := 0;

      --  The tree associates a set of states in N to *an* uniq set in the
      --  result NFA.
      --
      --  As the NFA is labelized, each node represent a state in N, and has
      --  two branches: one for state is present and one for state is absent.
      --
      --  The leaves contain the state in the result NFA.
      --
      --  The leaves are chained to create a stack of state to handle.
      --
      --  The root of the tree is node Start_Tree_Id and represent the start
      --  state of N.
      type Deter_Tree_Id is new Natural;
      No_Tree_Id : constant Deter_Tree_Id := 0;
      Start_Tree_Id : constant Deter_Tree_Id := 1;

      --  List of unhanded leaves.
      Deter_Head : Deter_Tree_Id;

      type Deter_Tree_Id_Bool_Array is array (Boolean) of Deter_Tree_Id;

      --  Node in the tree.
      type Deter_Tree_Entry is record
         Parent : Deter_Tree_Id;

         --  For non-leaf:
         Child : Deter_Tree_Id_Bool_Array;

         --  For leaf:
         Link : Deter_Tree_Id;
         State : NFA_State;
         --  + value ?
      end record;

      package Detert is new Tables
        (Table_Component_Type => Deter_Tree_Entry,
         Table_Index_Type => Deter_Tree_Id,
         Table_Low_Bound => 1,
         Table_Initial => 128);

      type Bool_Vector is array (Natural range <>) of Boolean;
      pragma Pack (Bool_Vector);

      --  Convert a set of states in N to a state in the result NFA.
      --  The set is represented by a vector of boolean.  An element of the
      --  vector is true iff the corresponding state is present.
      function Add_Vector (V : Bool_Vector; N : NFA) return NFA_State
      is
         E : Deter_Tree_Id;
         Added : Boolean;
         Res : NFA_State;
      begin
         E := Start_Tree_Id;
         Added := False;
         for I in V'Range loop
            if Detert.Table (E).Child (V (I)) = No_Tree_Id then
               Detert.Append ((Child => (No_Tree_Id, No_Tree_Id),
                               Parent => E,
                               Link => No_Tree_Id,
                               State => No_State));
               Detert.Table (E).Child (V (I)) := Detert.Last;
               E := Detert.Last;
               Added := True;
            else
               E := Detert.Table (E).Child (V (I));
               Added := False;
            end if;
         end loop;
         if Added then
            --  Create the new state.
            Res := Add_State (N);
            Detert.Table (E).State := Res;

            if Flag_Trace then
               Set_State_Label (Res, Last_Label);
               Put ("Result state" & Int32'Image (Last_Label) & " for");
               for I in V'Range loop
                  if V (I) then
                     Put (Natural'Image (I));
                  end if;
               end loop;
               New_Line;
               Last_Label := Last_Label + 1;
            end if;

            --  Put it to the list of states to be handled.
            Detert.Table (E).Link := Deter_Head;
            Deter_Head := E;

            return Res;
         else
            return Detert.Table (E).State;
         end if;
      end Add_Vector;

      --  Return true iff the stack is empty (ie all the states have been
      --  handled).
      function Stack_Empty return Boolean is
      begin
         return Deter_Head = No_Tree_Id;
      end Stack_Empty;

      --  Get an element from the stack.
      --  Extract the state in the result NFA.
      --  Rebuild the set of states in N (ie rebuild the vector of states).
      procedure Stack_Pop (V : out Bool_Vector; S : out NFA_State)
      is
         L, P : Deter_Tree_Id;
      begin
         L := Deter_Head;
         pragma Assert (L /= No_Tree_Id);
         S := Detert.Table (L).State;
         Deter_Head := Detert.Table (L).Link;

         for I in reverse V'Range loop
            pragma Assert (L /= Start_Tree_Id);
            P := Detert.Table (L).Parent;
            if L = Detert.Table (P).Child (True) then
               V (I) := True;
            elsif L = Detert.Table (P).Child (False) then
               V (I) := False;
            else
               raise Program_Error;
            end if;
            L := P;
         end loop;
         pragma Assert (L = Start_Tree_Id);
      end Stack_Pop;

      type State_Vector is array (Natural range <>) of Natural;
      type Expr_Vector is array (Natural range <>) of Node;

      procedure Build_Arcs (N : NFA;
                            State : NFA_State;
                            States : State_Vector;
                            Exprs : Expr_Vector;
                            Expr : Node;
                            V : Bool_Vector)
      is
         T : Node;
      begin
         if Expr = False_Node then
            return;
         end if;

         if States'Length = 0 then
            declare
               Reduced_Expr : constant Node := PSL.QM.Reduce (Expr);
               --Reduced_Expr : constant Node := Expr;
               S : NFA_State;
            begin
               if Reduced_Expr = False_Node then
                  return;
               end if;
               S := Add_Vector (V, N);
               Add_Edge (State, S, Reduced_Expr);
               if Flag_Trace then
                  Put (" Add edge");
                  Put (Int32'Image (Get_State_Label (State)));
                  Put (" to");
                  Put (Int32'Image (Get_State_Label (S)));
                  Put (", expr=");
                  Dump_Expr (Expr);
                  Put (", reduced=");
                  Dump_Expr (Reduced_Expr);
                  New_Line;
               end if;
            end;
         else
            declare
               N_States : State_Vector renames
                 States (States'First + 1 .. States'Last);
               N_V : Bool_Vector (V'Range) := V;
               S : constant Natural := States (States'First);
               E : constant Node := Exprs (S);
            begin
               N_V (S) := True;
               if Expr = Null_Node then
                  Build_Arcs (N, State, N_States, Exprs, E, N_V);
                  T := Build_Bool_Not (E);
                  Build_Arcs (N, State, N_States, Exprs, T, V);
               else
                  T := Build_Bool_And (E, Expr);

                  Build_Arcs (N, State, N_States, Exprs, T, N_V);
                  T := Build_Bool_Not (E);
                  T := Build_Bool_And (T, Expr);
                  Build_Arcs (N, State, N_States, Exprs, T, V);
               end if;
            end;
         end if;
      end Build_Arcs;

      function Determinize_1 (N : NFA; Nbr_States : Natural) return NFA
      is
         Final : Natural;
         V : Bool_Vector (0 .. Nbr_States - 1);
         Exprs : Expr_Vector (0 .. Nbr_States - 1);
         S : NFA_State;
         E : NFA_Edge;
         D : Natural;
         Edge_Expr : Node;
         Expr : Node;
         Nbr_Dest : Natural;
         States : State_Vector (0 .. Nbr_States - 1);
         Res : NFA;
         State : NFA_State;
         R : Node;
      begin
         Final := Natural (Get_State_Label (Get_Final_State (N)));

         -- FIXME: handle epsilon or final = start -> create an empty NFA.

         --  Initialize the tree.
         Res := Create_NFA;
         Detert.Init;
         Detert.Append ((Child => (No_Tree_Id, No_Tree_Id),
                         Parent => No_Tree_Id,
                         Link => No_Tree_Id,
                         State => No_State));
         pragma Assert (Detert.Last = Start_Tree_Id);
         Deter_Head := No_Tree_Id;

         --  Put the initial state in the tree and in the stack.
         --  FIXME: ok, we know that its label is 0.
         V := (0 => True, others => False);
         State := Add_Vector (V, Res);
         Set_Start_State (Res, State);

         --  The failure state.  As there is nothing to do with this
         --  state, remove it from the stack.
         V := (others => False);
         State := Add_Vector (V, Res);
         Set_Final_State (Res, State);
         Stack_Pop (V, State);

         --  Iterate on states in the result NFA that haven't yet been handled.
         while not Stack_Empty loop
            Stack_Pop (V, State);

            if Flag_Trace then
               Put_Line ("Handle result state"
                           & Int32'Image (Get_State_Label (State)));
            end if;

            --  Build edges vector.
            Exprs := (others => Null_Node);
            Expr := Null_Node;

            S := Get_First_State (N);
            Nbr_Dest := 0;
            while S /= No_State loop
               if V (Natural (Get_State_Label (S))) then
                  E := Get_First_Src_Edge (S);
                  while E /= No_Edge loop
                     D := Natural (Get_State_Label (Get_Edge_Dest (E)));
                     Edge_Expr := Get_Edge_Expr (E);

                     if False and Flag_Trace then
                        Put_Line ("  edge" & Int32'Image (Get_State_Label (S))
                                    & " to" & Natural'Image (D));
                     end if;

                     if D = Final then
                        R := Build_Bool_Not (Edge_Expr);
                        if Expr = Null_Node then
                           Expr := R;
                        else
                           Expr := Build_Bool_And (Expr, R);
                        end if;
                     else
                        if Exprs (D) = Null_Node then
                           Exprs (D) := Edge_Expr;
                           States (Nbr_Dest) := D;
                           Nbr_Dest := Nbr_Dest + 1;
                        else
                           Exprs (D) := Build_Bool_Or (Exprs (D), Edge_Expr);
                        end if;
                     end if;
                     E := Get_Next_Src_Edge (E);
                  end loop;
               end if;
               S := Get_Next_State (S);
            end loop;

            if Flag_Trace then
               Put (" Final: expr=");
               Print_Expr (Expr);
               New_Line;
               for I in 0 .. Nbr_Dest - 1 loop
                  Put ("   Dest");
                  Put (Natural'Image (States (I)));
                  Put (" expr=");
                  Print_Expr (Exprs (States (I)));
                  New_Line;
               end loop;
            end if;

            --  Build arcs.
            if not (Nbr_Dest = 0 and Expr = Null_Node) then
               Build_Arcs (Res, State,
                           States (0 .. Nbr_Dest - 1), Exprs, Expr,
                           Bool_Vector'(0 .. Nbr_States - 1 => False));
            end if;
         end loop;

         --Remove_Unreachable_States (Res);
         return Res;
      end Determinize_1;

      function Determinize (N : NFA) return NFA
      is
         Nbr_States : Natural;
      begin
         Labelize_States (N, Nbr_States);

         if Flag_Trace then
            Put_Line ("NFA to determinize:");
            Disp_NFA (N);
            Last_Label := 0;
         end if;

         return Determinize_1 (N, Nbr_States);
      end Determinize;
   end Determinize;

   function Build_Initial_Rep (N : NFA) return NFA
   is
      S : constant NFA_State := Get_Start_State (N);
   begin
      Add_Edge (S, S, True_Node);
      return N;
   end Build_Initial_Rep;

   procedure Build_Strong (N : NFA)
   is
      S : NFA_State;
      Final : constant NFA_State := Get_Final_State (N);
   begin
      S := Get_First_State (N);
      while S /= No_State loop
         --  FIXME.
         if S /= Final then
            Add_Edge (S, Final, EOS_Node);
         end if;
         S := Get_Next_State (S);
      end loop;
   end Build_Strong;

   procedure Build_Abort (N : NFA; Expr : Node)
   is
      S : NFA_State;
      E : NFA_Edge;
      Not_Expr : Node;
   begin
      Not_Expr := Build_Bool_Not (Expr);
      S := Get_First_State (N);
      while S /= No_State loop
         E := Get_First_Src_Edge (S);
         while E /= No_Edge loop
            Set_Edge_Expr (E, Build_Bool_And (Not_Expr, Get_Edge_Expr (E)));
            E := Get_Next_Src_Edge (E);
         end loop;
         S := Get_Next_State (S);
      end loop;
   end Build_Abort;

   function Build_Property_FA (N : Node; With_Active : Boolean) return NFA;

   function Build_Overlap_Imp
     (Left, Right : Node; With_Active : Boolean) return NFA
   is
      L, R : NFA;
      Res : NFA;
   begin
      L := Build_SERE_FA (Left);
      R := Build_Property_FA (Right, False);
      if With_Active then
         Set_Active_State (L, Get_Final_State (L));
      end if;
      Res := Build_Fusion (L, R);
      --  Ensure the active state is kept.
      pragma Assert (Res = L);
      return Res;
   end Build_Overlap_Imp;

   function Build_Property_FA (N : Node; With_Active : Boolean) return NFA
   is
      L, R : NFA;
   begin
      case Get_Kind (N) is
         when N_Sequences
           | N_Booleans
           | N_Sequence_Instance =>
            --  Build A(S) or A(B)
            R := Build_SERE_FA (N);
            return Determinize.Determinize (R);
         when N_Strong =>
            R := Build_Property_FA (Get_Property (N), False);
            Build_Strong (R);
            return R;
         when N_Imp_Seq =>
            --  R |=> P  -->  {R; TRUE} |-> P
            L := Build_SERE_FA (Get_Sequence (N));
            R := Build_Property_FA (Get_Property (N), False);
            if With_Active then
               declare
                  A : NFA_State;
               begin
                  A := Add_State (L);
                  Duplicate_Dest_Edges (L, Get_Final_State (L), A);
                  Set_Active_State (L, A);
               end;
            end if;
            return Build_Concat (L, R);
         when N_Overlap_Imp_Seq =>
            --  S |-> P  is defined as Ac(S) : A(P)
            return Build_Overlap_Imp
              (Get_Sequence (N), Get_Property (N), With_Active);
         when N_Log_Imp_Prop =>
            --  B -> P  -->  {B} |-> P  -->  Ac(B) : A(P)
            return Build_Overlap_Imp
              (Get_Left (N), Get_Right (N), With_Active);
         when N_And_Prop =>
            --  P1 && P2  -->  A(P1) | A(P2)
            L := Build_Property_FA (Get_Left (N), False);
            R := Build_Property_FA (Get_Right (N), False);
            return Build_Or (L, R);
         when N_Never =>
            R := Build_SERE_FA (Get_Property (N));
            return Build_Initial_Rep (R);
         when N_Always =>
            R := Build_Property_FA (Get_Property (N), With_Active);
            return Build_Initial_Rep (R);
         when N_Abort
            | N_Sync_Abort =>
            R := Build_Property_FA (Get_Property (N), With_Active);
            Build_Abort (R, Get_Boolean (N));
            return R;
         when N_Property_Instance =>
            declare
               Decl : Node;
            begin
               Decl := Get_Declaration (N);
               Assoc_Instance (Decl, N);
               R := Build_Property_FA (Get_Property (Decl), With_Active);
               Unassoc_Instance (Decl);
               return R;
            end;
         when others =>
            Error_Kind ("build_property_fa", N);
      end case;
   end Build_Property_FA;

   function Build_FA (N : Node) return NFA
   is
      use PSL.NFAs.Utils;
      Res : NFA;
   begin
      Res := Build_Property_FA (N, True);
      if Optimize_Final then
         pragma Debug (Check_NFA (Res));

         Remove_Unreachable_States (Res);
         Remove_Simple_Prefix (Res);
         Merge_Identical_States (Res);
         Merge_Edges (Res);
      end if;
      --  Clear the QM table.
      PSL.QM.Reset;
      return Res;
   end Build_FA;
end PSL.Build;
