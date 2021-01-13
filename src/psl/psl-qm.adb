--  PSL - Small QM reduction
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
with Types; use Types;
with PSL.Types; use PSL.Types;
with PSL.Errors; use PSL.Errors;
with PSL.Prints;
with PSL.CSE;

package body PSL.QM is
   procedure Reset is
   begin
      for I in 1 .. Nbr_Terms loop
         Set_HDL_Index (Term_Assoc (I), 0);
      end loop;
      Nbr_Terms := 0;
      Term_Assoc := (others => Null_Node);
   end Reset;

   function Term (P : Natural) return Vector_Type is
   begin
      return Shift_Left (1, P - 1);
   end Term;

   procedure Disp_Primes_Set (Ps : Primes_Set)
   is
      use Ada.Text_IO;
      use PSL.Prints;
      Prime : Prime_Type;
      T : Vector_Type;
      First_Term : Boolean;
   begin
      if Ps.Nbr = 0 then
         Put ("FALSE");
         return;
      end if;
      for I in 1 .. Ps.Nbr loop
         Prime := Ps.Set (I);
         if I /= 1 then
            Put (" | ");
         end if;
         if Prime.Set = 0 then
            Put ("TRUE");
         else
            First_Term := True;
            for J in 1 .. Max_Terms loop
               T := Term (J);
               if (Prime.Set and T) /= 0 then
                  if First_Term then
                     First_Term := False;
                  else
                     Put ('.');
                  end if;
                  if (Prime.Val and T) = 0 then
                     Put ('!');
                  end if;
                  Print_Expr (Term_Assoc (J));
               end if;
            end loop;
         end if;
      end loop;
   end Disp_Primes_Set;

   --  Return TRUE iff L includes R, ie
   --  for all x, x in L => x in R.
   function Included (L, R : Prime_Type) return Boolean is
   begin
      return ((L.Set or R.Set) = L.Set)
        and then ((L.Val and R.Set) = (R.Val and R.Set));
   end Included;

   --  Return TRUE iff L and R have the same don't care set
   --  and L and R can be merged into a new prime with a new don't care.
   function Is_One_Change_Same (L, R : Prime_Type) return Boolean
   is
      V : Vector_Type;
   begin
      if L.Set /= R.Set then
         return False;
      end if;
      V := L.Val xor R.Val;
      return (V and -V) = V;
   end Is_One_Change_Same;

   --  Return true iff L can add a new DC in R.
   function Is_One_Change (L, R : Prime_Type) return Boolean
   is
      V : Vector_Type;
   begin
      if (L.Set or R.Set) /= R.Set then
         return False;
      end if;
      V := (L.Val xor R.Val) and L.Set;
      return (V and -V) = V;
   end Is_One_Change;

   procedure Merge (Ps : in out Primes_Set; P : Prime_Type)
   is
      Do_Append : Boolean := True;
      T : Prime_Type;
   begin
      for I in 1 .. Ps.Nbr loop
         T := Ps.Set (I);
         if Included (P, T) then
            --  Already in the set.
            return;
         end if;
         if Included (T, P) then
            Ps.Set (I) := P;
            Do_Append := False;
         else
            if Is_One_Change_Same (P, T) then
               declare
                  V : constant Vector_Type := T.Val xor P.Val;
               begin
                  Ps.Set (I).Set := T.Set and not V;
                  Ps.Set (I).Val := T.Val and not V;
               end;
               Do_Append := False;
            end if;
            if Is_One_Change (P, T) then
               declare
                  V : constant Vector_Type := (T.Val xor P.Val) and P.Set;
               begin
                  Ps.Set (I).Set := T.Set and not V;
                  Ps.Set (I).Val := T.Val and not V;
               end;
               --  continue.
            end if;
         end if;
      end loop;
      if Do_Append then
         Ps.Nbr := Ps.Nbr + 1;
         Ps.Set (Ps.Nbr) := P;
      end if;
   end Merge;

   function Build_Primes_And (L, R : Primes_Set) return Primes_Set
   is
      Res : Primes_Set (L.Nbr * R.Nbr);
      L_P, R_P : Prime_Type;
      P : Prime_Type;
   begin
      for I in 1 .. L.Nbr loop
         L_P := L.Set (I);
         for J in 1 .. R.Nbr loop
            R_P := R.Set (J);
            --  In case of conflict, discard.
            if ((L_P.Val xor R_P.Val) and (L_P.Set and R_P.Set)) /= 0 then
               null;
            else
               P.Set := L_P.Set or R_P.Set;
               P.Val := (R_P.Set and R_P.Val)
                 or ((L_P.Set and not R_P.Set) and L_P.Val);
               Merge (Res, P);
            end if;
         end loop;
      end loop;

      return Res;
   end Build_Primes_And;


   function Build_Primes_Or (L, R : Primes_Set) return Primes_Set
   is
      Res : Primes_Set (L.Nbr + R.Nbr);
      L_P, R_P : Prime_Type;
   begin
      for I in 1 .. L.Nbr loop
         L_P := L.Set (I);
         Merge (Res, L_P);
      end loop;
      for J in 1 .. R.Nbr loop
         R_P := R.Set (J);
         Merge (Res, R_P);
      end loop;

      return Res;
   end Build_Primes_Or;

   function Build_Primes (N : Node; Negate : Boolean) return Primes_Set is
   begin
      case Get_Kind (N) is
         when N_HDL_Bool
           | N_EOS =>
            declare
               Res : Primes_Set (1);
               Index : Int32;
               T : Vector_Type;
            begin
               Index := Get_HDL_Index (N);
               if Index = 0 then
                  Nbr_Terms := Nbr_Terms + 1;
                  if Nbr_Terms > Max_Terms then
                     raise Program_Error;
                  end if;
                  Term_Assoc (Nbr_Terms) := N;
                  Index := Int32 (Nbr_Terms);
                  Set_HDL_Index (N, Index);
               else
                  if Index not in 1 .. Int32 (Nbr_Terms)
                    or else Term_Assoc (Natural (Index)) /= N
                  then
                     raise Internal_Error;
                  end if;
               end if;
               T := Term (Natural (Index));
               Res.Nbr := 1;
               Res.Set (1).Set := T;
               if Negate then
                  Res.Set (1).Val := 0;
               else
                  Res.Set (1).Val := T;
               end if;
               return Res;
            end;
         when N_False =>
            declare
               Res : Primes_Set (0);
            begin
               return Res;
            end;
         when N_True =>
            declare
               Res : Primes_Set (1);
            begin
               Res.Nbr := 1;
               Res.Set (1).Set := 0;
               Res.Set (1).Val := 0;
               return Res;
            end;
         when N_Not_Bool =>
            return Build_Primes (Get_Boolean (N), not Negate);
         when N_And_Bool =>
            if Negate then
               --  !(a & b) <-> !a || !b
               return Build_Primes_Or (Build_Primes (Get_Left (N), True),
                                       Build_Primes (Get_Right (N), True));
            else
               return Build_Primes_And (Build_Primes (Get_Left (N), False),
                                        Build_Primes (Get_Right (N), False));
            end if;
         when N_Or_Bool =>
            if Negate then
               --  !(a || b) <-> !a && !b
               return Build_Primes_And (Build_Primes (Get_Left (N), True),
                                        Build_Primes (Get_Right (N), True));
            else
               return Build_Primes_Or (Build_Primes (Get_Left (N), False),
                                       Build_Primes (Get_Right (N), False));
            end if;
         when N_Imp_Bool =>
            if not Negate then
               --  a -> b  <->  !a || b
               return Build_Primes_Or (Build_Primes (Get_Left (N), True),
                                       Build_Primes (Get_Right (N), False));
            else
               -- !(a -> b)  <->  a && !b
               return Build_Primes_And (Build_Primes (Get_Left (N), False),
                                        Build_Primes (Get_Right (N), True));
            end if;
         when N_Equiv_Bool =>
            if not Negate then
               --  a <-> b  <->  (a && b) || (!a && !b)
               return Build_Primes_Or
                 (Build_Primes_And (Build_Primes (Get_Left (N), False),
                                    Build_Primes (Get_Right (N), False)),
                  Build_Primes_And (Build_Primes (Get_Left (N), True),
                                    Build_Primes (Get_Right (N), True)));
            else
               -- !(a <-> b)  <->  (!a && b) || (a && !b)
               return Build_Primes_Or
                 (Build_Primes_And (Build_Primes (Get_Left (N), True),
                                    Build_Primes (Get_Right (N), False)),
                  Build_Primes_And (Build_Primes (Get_Left (N), False),
                                    Build_Primes (Get_Right (N), True)));
            end if;
         when others =>
            Error_Kind ("build_primes", N);
      end case;
   end Build_Primes;

   function Build_Primes (N : Node) return Primes_Set is
   begin
      return Build_Primes (N, False);
   end Build_Primes;

   function Build_Node (P : Prime_Type) return Node
   is
      Res : Node := Null_Node;
      N : Node;
      S : Vector_Type := P.Set;
      T : Vector_Type;
   begin
      if S = 0 then
         return True_Node;
      end if;
      for I in Natural range 1 .. Vector_Type'Modulus loop
         T := Term (I);
         if (S and T) /= 0 then
            N := Term_Assoc (I);
            if (P.Val and T) = 0 then
               N := PSL.CSE.Build_Bool_Not (N);
            end if;
            if Res = Null_Node then
               Res := N;
            else
               Res := PSL.CSE.Build_Bool_And (Res, N);
            end if;
            S := S and not T;
            exit when S = 0;
         end if;
      end loop;
      return Res;
   end Build_Node;

   function Build_Node (Ps : Primes_Set) return Node
   is
      Res : Node;
   begin
      if Ps.Nbr = 0 then
         return False_Node;
      else
         Res := Build_Node (Ps.Set (1));
         for I in 2 .. Ps.Nbr loop
            Res := PSL.CSE.Build_Bool_Or (Res, Build_Node (Ps.Set (I)));
         end loop;
         return Res;
      end if;
   end Build_Node;

   --  FIXME: finish the work: do a real Quine-McKluskey minimization.
   function Reduce (N : Node) return Node is
   begin
      return Build_Node (Build_Primes (N));
   end Reduce;
end PSL.QM;
