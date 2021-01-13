--  PSL - Utils
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

with PSL.Types; use PSL.Types;
with PSL.Nodes_Priv;
with PSL.Errors; use PSL.Errors;

package body PSL.NFAs.Utils is
   generic
      with function Get_First_Edge (S : NFA_State) return NFA_Edge;
      with function Get_Next_Edge (E : NFA_Edge) return NFA_Edge;
      with procedure Set_First_Edge (S : NFA_State; E : NFA_Edge);
      with procedure Set_Next_Edge (E : NFA_Edge; N_E : NFA_Edge);
      with function Get_Edge_State (E : NFA_Edge) return NFA_State;
   package Sort_Edges is
      procedure Sort_Edges (S : NFA_State);
      procedure Sort_Edges (N : NFA);
   end Sort_Edges;

   package body Sort_Edges is
      --  Use merge sort to sort a list of edges.
      --  The first edge is START and the list has LEN edges.
      --  RES is the head of the sorted list.
      --  NEXT_EDGE is the LEN + 1 edge (not sorted).
      procedure Edges_Merge_Sort (Start : NFA_Edge;
                                  Len : Natural;
                                  Res : out NFA_Edge;
                                  Next_Edge : out NFA_Edge)
      is
         function Lt (L, R : NFA_Edge) return Boolean
         is
            L_Expr : constant Node := Get_Edge_Expr (L);
            R_Expr : constant Node := Get_Edge_Expr (R);
         begin
            return PSL.Nodes_Priv."<" (L_Expr, R_Expr)
              or else (L_Expr = R_Expr
                         and then Get_Edge_State (L) < Get_Edge_State (R));
         end Lt;

         pragma Inline (Lt);

         Half : constant Natural := Len / 2;
         Left_Start, Right_Start : NFA_Edge;
         Left_Next, Right_Next : NFA_Edge;
         L, R : NFA_Edge;
         Last, E : NFA_Edge;
      begin
         --  With less than 2 elements, the sort is trivial.
         if Len < 2 then
            if Len = 0 then
               Next_Edge := Start;
            else
               Next_Edge := Get_Next_Edge (Start);
            end if;
            Res := Start;
            return;
         end if;

         --  Sort each half.
         Edges_Merge_Sort (Start, Half, Left_Start, Left_Next);
         Edges_Merge_Sort (Left_Next, Len - Half, Right_Start, Right_Next);

         --  Merge.
         L := Left_Start;
         R := Right_Start;
         Last := No_Edge;
         loop
            --  Take from left iff:
            --  * it is not empty
            --  * right is empty or else (left < right)
            if L /= Left_Next and then (R = Right_Next or else Lt (L, R)) then
               E := L;
               L := Get_Next_Edge (L);

            --  Take from right if right is not empty.
            elsif R /= Right_Next then
               E := R;
               R := Get_Next_Edge (R);

            --  Both left are right are empty.
            else
               exit;
            end if;

            if Last = No_Edge then
               Res := E;
            else
               Set_Next_Edge (Last, E);
            end if;
            Last := E;
         end loop;
         --  Let the link clean.
         Next_Edge := Right_Next;
         Set_Next_Edge (Last, Next_Edge);
      end Edges_Merge_Sort;

      procedure Sort_Edges (S : NFA_State)
      is
         Nbr_Edges : Natural;
         First_E, E, Res : NFA_Edge;
      begin
         --  Count number of edges.
         Nbr_Edges := 0;
         First_E := Get_First_Edge (S);
         E := First_E;
         while E /= No_Edge loop
            Nbr_Edges := Nbr_Edges + 1;
            E := Get_Next_Edge (E);
         end loop;

         --  Sort edges by expression.
         Edges_Merge_Sort (First_E, Nbr_Edges, Res, E);
         pragma Assert (E = No_Edge);
         Set_First_Edge (S, Res);

      end Sort_Edges;

      procedure Sort_Edges (N : NFA)
      is
         S : NFA_State;
      begin
         --  Iterate on states.
         S := Get_First_State (N);
         while S /= No_State loop
            Sort_Edges (S);
            S := Get_Next_State (S);
         end loop;
      end Sort_Edges;
   end Sort_Edges;

   package Sort_Src_Edges_Pkg is new
     Sort_Edges (Get_First_Edge => Get_First_Src_Edge,
                 Get_Next_Edge => Get_Next_Src_Edge,
                 Set_First_Edge => Set_First_Src_Edge,
                 Set_Next_Edge => Set_Next_Src_Edge,
                 Get_Edge_State => Get_Edge_Dest);

   procedure Sort_Src_Edges (S : NFA_State) renames
     Sort_Src_Edges_Pkg.Sort_Edges;
   procedure Sort_Src_Edges (N : NFA) renames
     Sort_Src_Edges_Pkg.Sort_Edges;

   package Sort_Dest_Edges_Pkg is new
     Sort_Edges (Get_First_Edge => Get_First_Dest_Edge,
                 Get_Next_Edge => Get_Next_Dest_Edge,
                 Set_First_Edge => Set_First_Dest_Edge,
                 Set_Next_Edge => Set_Next_Dest_Edge,
                 Get_Edge_State => Get_Edge_Src);

   procedure Sort_Dest_Edges (S : NFA_State) renames
     Sort_Dest_Edges_Pkg.Sort_Edges;
   procedure Sort_Dest_Edges (N : NFA) renames
     Sort_Dest_Edges_Pkg.Sort_Edges;

   generic
      with function Get_First_Edge_Reverse (S : NFA_State) return NFA_Edge;
      with function Get_First_Edge (S : NFA_State) return NFA_Edge;
      with procedure Set_First_Edge (S : NFA_State; E : NFA_Edge);
      with function Get_Next_Edge (E : NFA_Edge) return NFA_Edge;
      with procedure Set_Next_Edge (E : NFA_Edge; E1 : NFA_Edge);
      with procedure Set_Edge_State (E : NFA_Edge; S : NFA_State);
   procedure Merge_State (N : NFA; S : NFA_State; S1 : NFA_State);

   procedure Merge_State (N : NFA; S : NFA_State; S1 : NFA_State)
   is
      E, First_E, Next_E : NFA_Edge;
   begin
      pragma Assert (S /= S1);

      --  Discard outgoing edges of S1.
      loop
         E := Get_First_Edge_Reverse (S1);
         exit when E = No_Edge;
         Remove_Edge (E);
      end loop;

      --  Prepend incoming edges of S1 to S.
      First_E := Get_First_Edge (S);
      E := Get_First_Edge (S1);
      while E /= No_Edge loop
         Next_E := Get_Next_Edge (E);
         Set_Next_Edge (E, First_E);
         Set_Edge_State (E, S);
         First_E := E;
         E := Next_E;
      end loop;
      Set_First_Edge (S, First_E);
      Set_First_Edge (S1, No_Edge);

      --  Move the active state if it is deleted.
      if Get_Active_State (N) = S1 then
         Set_Active_State (N, S);
      end if;

      Remove_State (N, S1);
   end Merge_State;

   procedure Merge_State_Dest_1 is new Merge_State
     (Get_First_Edge_Reverse => Get_First_Src_Edge,
      Get_First_Edge => Get_First_Dest_Edge,
      Set_First_Edge => Set_First_Dest_Edge,
      Get_Next_Edge => Get_Next_Dest_Edge,
      Set_Next_Edge => Set_Next_Dest_Edge,
      Set_Edge_State => Set_Edge_Dest);

   procedure Merge_State_Dest (N : NFA; S : NFA_State; S1 : NFA_State) renames
     Merge_State_Dest_1;

   procedure Merge_State_Src_1 is new Merge_State
     (Get_First_Edge_Reverse => Get_First_Dest_Edge,
      Get_First_Edge => Get_First_Src_Edge,
      Set_First_Edge => Set_First_Src_Edge,
      Get_Next_Edge => Get_Next_Src_Edge,
      Set_Next_Edge => Set_Next_Src_Edge,
      Set_Edge_State => Set_Edge_Src);

   procedure Merge_State_Src (N : NFA; S : NFA_State; S1 : NFA_State) renames
     Merge_State_Src_1;

   procedure Sort_Outgoing_Edges (N : NFA; Nbr_States : Natural)
   is
      Last_State : constant NFA_State := NFA_State (Nbr_States) - 1;
      type Edge_Array is array (0 .. Last_State) of NFA_Edge;
      Edges : Edge_Array := (others => No_Edge);
      S, D : NFA_State;
      E, Next_E : NFA_Edge;
      First_Edge, Last_Edge : NFA_Edge;
   begin
      --  Iterate on states.
      S := Get_First_State (N);
      while S /= No_State loop

         --  Create an array of edges
         E := Get_First_Dest_Edge (S);
         while E /= No_Edge loop
            Next_E := Get_Next_Dest_Edge (E);
            D := Get_Edge_Dest (E);
            if Edges (D) /= No_Edge then
               --  TODO: merge edges.
               raise Program_Error;
            end if;
            Edges (D) := E;
            E := Next_E;
         end loop;

         --  Rebuild the edge list (sorted by destination).
         Last_Edge := No_Edge;
         First_Edge := No_Edge;
         for I in Edge_Array'Range loop
            E := Edges (I);
            if E /= No_Edge then
               Edges (I) := No_Edge;
               if First_Edge = No_Edge then
                  First_Edge := E;
               else
                  Set_Next_Dest_Edge (Last_Edge, E);
               end if;
               Last_Edge := E;
            end if;
         end loop;
         Set_First_Dest_Edge (S, First_Edge);
         S := Get_Next_State (S);
      end loop;
   end Sort_Outgoing_Edges;
   pragma Unreferenced (Sort_Outgoing_Edges);

   generic
      with function Get_First_Edge (S : NFA_State) return NFA_Edge;
      with function Get_Next_Edge (E : NFA_Edge) return NFA_Edge;
      with function Get_State_Reverse (E : NFA_Edge) return NFA_State;
      with function Get_First_Edge_Reverse (S : NFA_State) return NFA_Edge;
      with function Get_Next_Edge_Reverse (E : NFA_Edge) return NFA_Edge;
   procedure Check_Edges_Gen (N : NFA);

   procedure Check_Edges_Gen (N : NFA)
   is
      S : NFA_State;
      E : NFA_Edge;
      R_S : NFA_State;
      R_E : NFA_Edge;
   begin
      S := Get_First_State (N);
      while S /= No_State loop
         E := Get_First_Edge (S);
         while E /= No_Edge loop
            R_S := Get_State_Reverse (E);
            R_E := Get_First_Edge_Reverse (R_S);
            while R_E /= No_Edge and then R_E /= E loop
               R_E := Get_Next_Edge_Reverse (R_E);
            end loop;
            if R_E /= E then
               raise Program_Error;
            end if;
            E := Get_Next_Edge (E);
         end loop;
         S := Get_Next_State (S);
      end loop;
   end Check_Edges_Gen;

   procedure Check_Edges_Src is new Check_Edges_Gen
     (Get_First_Edge => Get_First_Src_Edge,
      Get_Next_Edge => Get_Next_Src_Edge,
      Get_State_Reverse => Get_Edge_Dest,
      Get_First_Edge_Reverse => Get_First_Dest_Edge,
      Get_Next_Edge_Reverse => Get_Next_Dest_Edge);

   procedure Check_Edges_Dest is new Check_Edges_Gen
     (Get_First_Edge => Get_First_Dest_Edge,
      Get_Next_Edge => Get_Next_Dest_Edge,
      Get_State_Reverse => Get_Edge_Src,
      Get_First_Edge_Reverse => Get_First_Src_Edge,
      Get_Next_Edge_Reverse => Get_Next_Src_Edge);

   procedure Check_NFA (N : NFA) is
   begin
      Check_Edges_Src (N);
      Check_Edges_Dest (N);
   end Check_NFA;

   function Has_EOS (N : Node) return Boolean is
   begin
      case Get_Kind (N) is
         when N_EOS =>
            return True;
         when N_False
           | N_True
           | N_HDL_Bool =>
            return False;
         when N_Not_Bool =>
            return Has_EOS (Get_Boolean (N));
         when N_And_Bool
           | N_Or_Bool
           | N_Imp_Bool =>
            return Has_EOS (Get_Left (N)) or else Has_EOS (Get_Right (N));
         when others =>
            Error_Kind ("Has_EOS", N);
      end case;
   end Has_EOS;

   procedure Set_Init_Loop (N : NFA)
   is
      Start : constant NFA_State := Get_Start_State (N);
      E : NFA_Edge;
      Expr : Node;
   begin
      --  Look for existing edge.
      E := Get_First_Src_Edge (Start);
      while E /= No_Edge loop
         if Get_Edge_Dest (E) = Start then
            Expr := Get_Edge_Expr (E);
            if Get_Kind (Expr) = N_True then
               return;
            end if;
            Set_Edge_Expr (E, True_Node);
            return;
         end if;
         E := Get_Next_Src_Edge (E);
      end loop;

      --  No existing edge.  Create one.
      Add_Edge (Start, Start, True_Node);
   end Set_Init_Loop;

end PSL.NFAs.Utils;
