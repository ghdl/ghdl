--  GHDL Run Time (GRT) - binary balanced tree.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with Grt.Errors; use Grt.Errors;

package body Grt.Avls is
   function Get_Height (Tree: AVL_Tree; N : AVL_Nid) return Ghdl_I32 is
   begin
      if N = AVL_Nil then
         return 0;
      else
         return Tree (N).Height;
      end if;
   end Get_Height;
   pragma Inline (Get_Height);

   procedure Check_AVL (Tree : AVL_Tree; N : AVL_Nid)
   is
      L, R : AVL_Nid;
      Lh, Rh : Ghdl_I32;
      H : Ghdl_I32;
   begin
      if N = AVL_Nil then
         return;
      end if;
      L := Tree (N).Left;
      R := Tree (N).Right;
      H := Tree (N).Height;
      if L = AVL_Nil and R = AVL_Nil then
         if H /= 1 then
            Internal_Error ("check_AVL(1)");
         end if;
      elsif L = AVL_Nil then
         Check_AVL (Tree, R);
         if H /= Get_Height (Tree, R) + 1 or H > 2 then
            Internal_Error ("check_AVL(2)");
         end if;
      elsif R = AVL_Nil then
         Check_AVL (Tree, L);
         if H /= Get_Height (Tree, L) + 1 or H > 2 then
            Internal_Error ("check_AVL(3)");
         end if;
      else
         Check_AVL (Tree, L);
         Check_AVL (Tree, R);
         Lh := Get_Height (Tree, L);
         Rh := Get_Height (Tree, R);
         if Ghdl_I32'Max (Lh, Rh) + 1 /= H then
            Internal_Error ("check_AVL(4)");
         end if;
         if Rh - Lh > 1 or Rh - Lh < -1 then
            Internal_Error ("check_AVL(5)");
         end if;
      end if;
   end Check_AVL;

   procedure Compute_Height (Tree : in out AVL_Tree; N : AVL_Nid)
   is
   begin
      Tree (N).Height :=
        Ghdl_I32'Max (Get_Height (Tree, Tree (N).Left),
                      Get_Height (Tree, Tree (N).Right)) + 1;
   end Compute_Height;

   procedure Simple_Rotate_Right (Tree : in out AVL_Tree; N : AVL_Nid)
   is
      R : AVL_Nid;
      V : AVL_Value;
   begin
      --  Rotate nodes.
      R := Tree (N).Right;
      Tree (N).Right := Tree (R).Right;
      Tree (R).Right := Tree (R).Left;
      Tree (R).Left := Tree (N).Left;
      Tree (N).Left := R;
      --  Swap vals.
      V := Tree (N).Val;
      Tree (N).Val := Tree (R).Val;
      Tree (R).Val := V;
      --  Adjust bal.
      Compute_Height (Tree, R);
      Compute_Height (Tree, N);
   end Simple_Rotate_Right;

   procedure Simple_Rotate_Left (Tree : in out AVL_Tree; N : AVL_Nid)
   is
      L : AVL_Nid;
      V : AVL_Value;
   begin
      L := Tree (N).Left;
      Tree (N).Left := Tree (L).Left;
      Tree (L).Left := Tree (L).Right;
      Tree (L).Right := Tree (N).Right;
      Tree (N).Right := L;
      V := Tree (N).Val;
      Tree (N).Val := Tree (L).Val;
      Tree (L).Val := V;
      Compute_Height (Tree, L);
      Compute_Height (Tree, N);
   end Simple_Rotate_Left;

   procedure Double_Rotate_Right (Tree : in out AVL_Tree; N : AVL_Nid)
   is
      R : AVL_Nid;
   begin
      R := Tree (N).Right;
      Simple_Rotate_Left (Tree, R);
      Simple_Rotate_Right (Tree, N);
   end Double_Rotate_Right;

   procedure Double_Rotate_Left (Tree : in out AVL_Tree; N : AVL_Nid)
   is
      L : AVL_Nid;
   begin
      L := Tree (N).Left;
      Simple_Rotate_Right (Tree, L);
      Simple_Rotate_Left (Tree, N);
   end Double_Rotate_Left;

   procedure Insert (Tree : in out AVL_Tree;
                    Cmp : AVL_Compare_Func;
                    Val : AVL_Nid;
                    N : AVL_Nid;
                    Res : out AVL_Nid)
   is
      Diff : Integer;
      Op_Ch, Ch : AVL_Nid;
   begin
      Diff := Cmp.all (Tree (Val).Val, Tree (N).Val);
      if Diff = 0 then
         Res := N;
         return;
      end if;
      if Diff < 0 then
         if Tree (N).Left = AVL_Nil then
            Tree (N).Left := Val;
            Compute_Height (Tree, N);
            --  N is balanced.
            Res := Val;
         else
            Ch := Tree (N).Left;
            Op_Ch := Tree (N).Right;
            Insert (Tree, Cmp, Val, Ch, Res);
            if Res /= Val then
               return;
            end if;
            if Get_Height (Tree, Ch) - Get_Height (Tree, Op_Ch) = 2 then
               --  Rotate
               if Get_Height (Tree, Tree (Ch).Left)
                 > Get_Height (Tree, Tree (Ch).Right)
               then
                  Simple_Rotate_Left (Tree, N);
               else
                  Double_Rotate_Left (Tree, N);
               end if;
            else
               Compute_Height (Tree, N);
            end if;
         end if;
      else
         if Tree (N).Right = AVL_Nil then
            Tree (N).Right := Val;
            Compute_Height (Tree, N);
            --  N is balanced.
            Res := Val;
         else
            Ch := Tree (N).Right;
            Op_Ch := Tree (N).Left;
            Insert (Tree, Cmp, Val, Ch, Res);
            if Res /= Val then
               return;
            end if;
            if Get_Height (Tree, Ch) - Get_Height (Tree, Op_Ch) = 2 then
               --  Rotate
               if Get_Height (Tree, Tree (Ch).Right)
                 > Get_Height (Tree, Tree (Ch).Left)
               then
                  Simple_Rotate_Right (Tree, N);
               else
                  Double_Rotate_Right (Tree, N);
               end if;
            else
               Compute_Height (Tree, N);
            end if;
         end if;
      end if;
   end Insert;

   procedure Get_Node (Tree : in out AVL_Tree;
                       Cmp : AVL_Compare_Func;
                       N : AVL_Nid;
                       Res : out AVL_Nid)
   is
   begin
      if Tree'First /= AVL_Root or N /= Tree'Last then
         Internal_Error ("avls.get_node");
      end if;
      Insert (Tree, Cmp, N, AVL_Root, Res);
      pragma Debug (Check_AVL (Tree, AVL_Root));
   end Get_Node;

   function Find_Node (Tree : AVL_Tree;
                       Cmp : AVL_Compare_Func;
                       Val : AVL_Value) return AVL_Nid
   is
      N : AVL_Nid;
      Diff : Integer;
   begin
      N := AVL_Root;
      if Tree'Last < AVL_Root then
         return AVL_Nil;
      end if;
      loop
         Diff := Cmp.all (Val, Tree (N).Val);
         if Diff = 0 then
            return N;
         end if;
         if Diff < 0 then
            N := Tree (N).Left;
         else
            N := Tree (N).Right;
         end if;
         if N = AVL_Nil then
            return AVL_Nil;
         end if;
      end loop;
   end Find_Node;
end Grt.Avls;
