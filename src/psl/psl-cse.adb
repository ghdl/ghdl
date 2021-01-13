--  PSL - Simplify expressions
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

with Ada.Text_IO;
with PSL.Types; use PSL.Types;
with PSL.Prints;
with Types; use Types;

package body PSL.CSE is
   function Is_X_And_Not_X (A, B : Node) return Boolean is
   begin
      return (Get_Kind (A) = N_Not_Bool
                and then Get_Boolean (A) = B)
        or else (Get_Kind (B) = N_Not_Bool
                   and then Get_Boolean (B) = A);
   end Is_X_And_Not_X;

   type Hash_Table_Type is array (Uns32 range 0 .. 128) of Node;
   Hash_Table : Hash_Table_Type := (others => Null_Node);

   function Compute_Hash (L, R : Node; Op : Uns32) return Uns32
   is
   begin
      return Shift_Left (Get_Hash (L), 12)
        xor Shift_Left (Get_Hash (R), 2)
        xor Op;
   end Compute_Hash;

   function Compute_Hash (L: Node; Op : Uns32) return Uns32
   is
   begin
      return Shift_Left (Get_Hash (L), 2) xor Op;
   end Compute_Hash;

   procedure Dump_Hash_Table (Level : Natural := 0)
   is
      use Ada.Text_IO;
      Cnt : Natural;
      Total : Natural;
      N : Node;
   begin
      Total := 0;
      for I in Hash_Table_Type'Range loop
         Cnt := 0;
         N := Hash_Table (I);
         while N /= Null_Node loop
            Cnt := Cnt + 1;
            N := Get_Hash_Link (N);
         end loop;
         Put_Line ("Hash_table(" & Uns32'Image (I)
                     & "):" & Natural'Image (Cnt));
         Total := Total + Cnt;
         if Level > 0 then
            Cnt := 0;
            N := Hash_Table (I);
            while N /= Null_Node loop
               Put (Uns32'Image (Get_Hash (N)));
               if Level > 1 then
                  Put (": ");
                  PSL.Prints.Dump_Expr (N);
                  New_Line;
               end if;
               Cnt := Cnt + 1;
               N := Get_Hash_Link (N);
            end loop;
            if Level = 1 and then Cnt > 0 then
               New_Line;
            end if;
         end if;
      end loop;
      Put_Line ("Total:" & Natural'Image (Total));
   end Dump_Hash_Table;

   function Build_Bool_And (L, R : Node) return Node
   is
      R1 : Node;
      Res : Node;
      Hash : Uns32;
      Head, H : Node;
   begin
      if L = True_Node then
         return R;
      elsif R = True_Node then
         return L;
      elsif L = False_Node or else R = False_Node then
         return False_Node;
      elsif L = R then
         return L;
      elsif Is_X_And_Not_X (L, R) then
         return False_Node;
      end if;

      --  More simple optimizations.
      if Get_Kind (R) = N_And_Bool then
         R1 := Get_Left (R);
         if L = R1 then
            return R;
         elsif Is_X_And_Not_X (L, R1) then
            return False_Node;
         end if;
      end if;

      Hash := Compute_Hash (L, R, 2);
      Head := Hash_Table (Hash mod Hash_Table'Length);
      H := Head;
      while H /= Null_Node loop
         if Get_Hash (H) = Hash
           and then Get_Kind (H) = N_And_Bool
           and then Get_Left (H) = L
           and then Get_Right (H) = R
         then
            return H;
         end if;
         H := Get_Hash_Link (H);
      end loop;

      Res := Create_Node (N_And_Bool);
      Set_Left (Res, L);
      Set_Right (Res, R);
      Copy_Location (Res, L);
      Set_Hash_Link (Res, Head);
      Set_Hash (Res, Hash);
      Hash_Table (Hash mod Hash_Table'Length) := Res;
      return Res;
   end Build_Bool_And;

   function Build_Bool_Or (L, R : Node) return Node
   is
      Res : Node;
      Hash : Uns32;
      Head, H : Node;
   begin
      if L = True_Node then
         return L;
      elsif R = True_Node then
         return R;
      elsif L = False_Node then
         return R;
      elsif R = False_Node then
         return L;
      elsif L = R then
         return L;
      elsif Is_X_And_Not_X (L, R) then
         return True_Node;
      end if;

      Hash := Compute_Hash (L, R, 3);
      Head := Hash_Table (Hash mod Hash_Table'Length);
      H := Head;
      while H /= Null_Node loop
         if Get_Hash (H) = Hash
           and then Get_Kind (H) = N_Or_Bool
           and then Get_Left (H) = L
           and then Get_Right (H) = R
         then
            return H;
         end if;
         H := Get_Hash_Link (H);
      end loop;

      Res := Create_Node (N_Or_Bool);
      Set_Left (Res, L);
      Set_Right (Res, R);
      Copy_Location (Res, L);
      Set_Hash_Link (Res, Head);
      Set_Hash (Res, Hash);
      Hash_Table (Hash mod Hash_Table'Length) := Res;
      return Res;
   end Build_Bool_Or;

   function Build_Bool_Not (N : Node) return Node is
      Res : Node;
      Hash : Uns32;
      Head : Node;
      H : Node;
   begin
      if N = True_Node then
         return False_Node;
      elsif N = False_Node then
         return True_Node;
      elsif Get_Kind (N) = N_Not_Bool then
         return Get_Boolean (N);
      end if;

      --  Find in hash table.
      Hash := Compute_Hash (N, 1);
      Head := Hash_Table (Hash mod Hash_Table'Length);
      H := Head;
      while H /= Null_Node loop
         if Get_Hash (H) = Hash
           and then Get_Kind (H) = N_Not_Bool
           and then Get_Boolean (H) = N
         then
            return H;
         end if;
         H := Get_Hash_Link (H);
      end loop;

      Res := Create_Node (N_Not_Bool);
      Set_Boolean (Res, N);
      Copy_Location (Res, N);
      Set_Hash_Link (Res, Head);
      Set_Hash (Res, Hash);
      Hash_Table (Hash mod Hash_Table'Length) := Res;

      return Res;
   end Build_Bool_Not;
end PSL.CSE;
