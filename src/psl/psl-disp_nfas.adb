--  PSL - Disp nodes
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

with Ada.Text_IO; use Ada.Text_IO;
with Types; use Types;
with PSL.Prints; use PSL.Prints;

package body PSL.Disp_NFAs is
   procedure Disp_State (S : NFA_State) is
      Str : constant String := Int32'Image (Get_State_Label (S));
   begin
      Put (Str (2 .. Str'Last));
   end Disp_State;

   procedure Disp_Head (Name : String) is
   begin
      Put ("digraph ");
      Put (Name);
      Put_Line (" {");
      Put_Line ("  rankdir=LR;");
   end Disp_Head;

   procedure Disp_Tail is
   begin
      Put_Line ("}");
   end Disp_Tail;

   procedure Disp_Body (N : NFA) is
      S, F : NFA_State;
      T : NFA_Edge;
   begin
      S := Get_Start_State (N);
      F := Get_Final_State (N);
      if S /= No_State then
         if S = F then
            Put ("  node [shape = doublecircle, style = bold];");
         else
            Put ("  node [shape = circle, style = bold];");
         end if;
         Put (" /* Start: */ ");
         Disp_State (S);
         Put_Line (";");
      end if;
      if F /= No_State and then F /= S then
         Put ("  node [shape = doublecircle, style = solid];");
         Put (" /* Final: */ ");
         Disp_State (F);
         Put_Line (";");
      end if;
      Put_Line ("  node [shape = circle, style = solid];");

      if Get_Epsilon_NFA (N) then
         Put ("  ");
         Disp_State (Get_Start_State (N));
         Put (" -> ");
         Disp_State (Get_Final_State (N));
         Put_Line (" [ label = ""*""]");
      end if;

      S := Get_First_State (N);
      while S /= No_State loop
         T := Get_First_Src_Edge (S);
         if T = No_Edge then
            if Get_First_Dest_Edge (S) = No_Edge then
               Put ("  ");
               Disp_State (S);
               Put_Line (";");
            end if;
         else
            loop
               Put ("  ");
               Disp_State (S);
               Put (" -> ");
               Disp_State (Get_Edge_Dest (T));
               Put (" [ label = """);
               Print_Expr (Get_Edge_Expr (T));
               Put ('"');
               if True then
                  Put (" /* Node =");
                  Put (Node'Image (Get_Edge_Expr (T)));
                  Put (" */");
               end if;
               if True then
                  Put (" /* Edge =");
                  Put (NFA_Edge'Image (T));
                  Put (" */");
               end if;
               Put_Line (" ];");

               T := Get_Next_Src_Edge (T);
               exit when T = No_Edge;
            end loop;
         end if;
         S := Get_Next_State (S);
      end loop;
   end Disp_Body;

   procedure Disp_NFA (N : NFA; Name : String := "nfa") is
   begin
      Disp_Head (Name);
      Disp_Body (N);
      Disp_Tail;
   end Disp_NFA;

   procedure Debug_NFA (N : NFA) is
   begin
      Labelize_States_Debug (N);
      Disp_Head ("nfa");
      Disp_Body (N);
      Disp_Tail;
   end Debug_NFA;

   pragma Unreferenced (Debug_NFA);
end PSL.Disp_NFAs;
