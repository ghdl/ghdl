--  Nodes recognizer for ieee.std_logic_arith.
--  Copyright (C) 2019 Tristan Gingold
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
with Vhdl.Std_Package;
with Std_Names; use Std_Names;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Ieee.Std_Logic_1164;

package body Vhdl.Ieee.Std_Logic_Arith is
   --  Unsigned and signed type definition.
   Unsigned_Type : Iir := Null_Iir;
   Signed_Type : Iir := Null_Iir;

   Error : exception;

   type Arg_Kind is (Type_Slv, Type_Signed, Type_Unsigned, Type_Int, Type_Log);

   subtype Conv_Arg_Kind is Arg_Kind range Type_Signed .. Type_Log;
   type Conv_Pattern_Type is
     array (Conv_Arg_Kind) of Iir_Predefined_Functions;

   subtype Res_Arg_Kind is Arg_Kind range Type_Slv .. Type_Unsigned;

   type Bin_Pattern_Type is
     array (Res_Arg_Kind, Conv_Arg_Kind, Conv_Arg_Kind)
     of Iir_Predefined_Functions;

   subtype Cmp_Arg_Kind is Arg_Kind range Type_Signed .. Type_Int;

   type Cmp_Pattern_Type is array (Cmp_Arg_Kind, Cmp_Arg_Kind)
     of Iir_Predefined_Functions;

   Conv_Uns_Patterns : constant Conv_Pattern_Type :=
     (Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Log);

   Conv_Int_Patterns : constant Conv_Pattern_Type :=
     (Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Log);

   Conv_Vec_Patterns : constant Conv_Pattern_Type :=
     (Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Vector_Sgn,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Vector_Uns,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Vector_Int,
      Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Vector_Log);

   Mul_Patterns : constant Bin_Pattern_Type :=
     (Type_Slv =>
        (Type_Unsigned =>
           (Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Uns_Slv,
            Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Slv,
            others => Iir_Predefined_None),
         Type_Signed =>
           (Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Slv,
            Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Slv,
            others => Iir_Predefined_None),
         others =>
           (others => Iir_Predefined_None)),
      Type_Signed =>
        (Type_Signed =>
           (Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Sgn,
            Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Sgn,
            others => Iir_Predefined_None),
         Type_Unsigned =>
           (Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Sgn,
            others => Iir_Predefined_None),
         others =>
           (others => Iir_Predefined_None)),
      Type_Unsigned =>
        (Type_Unsigned =>
           (Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Uns_Uns,
            others => Iir_Predefined_None),
         others =>
           (others => Iir_Predefined_None)));

   Add_Patterns : constant Bin_Pattern_Type :=
     (Type_Slv =>
        (Type_Unsigned =>
           (Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Uns_Slv,
            Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Sgn_Slv,
            Type_Int =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Int_Slv,
            Type_Log =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Log_Slv),
         Type_Signed =>
           (Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Uns_Slv,
            Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Slv,
            Type_Int =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Int_Slv,
            Type_Log =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Log_Slv),
         Type_Int =>
           (Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Uns_Slv,
            Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Sgn_Slv,
            others => Iir_Predefined_None),
         Type_Log =>
           (Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Uns_Slv,
            Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Sgn_Slv,
            others => Iir_Predefined_None)),
      Type_Signed =>
        (Type_Signed =>
           (Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Sgn,
            Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Uns_Sgn,
            Type_Int =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Int_Sgn,
            Type_Log =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Log_Sgn),
         Type_Unsigned =>
           (Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Sgn_Sgn,
            others => Iir_Predefined_None),
         Type_Int =>
           (Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Sgn_Sgn,
            others => Iir_Predefined_None),
         Type_Log =>
           (Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Sgn_Sgn,
            others => Iir_Predefined_None)),
      Type_Unsigned =>
        (Type_Unsigned =>
           (Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Uns_Uns,
            Type_Int =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Int_Uns,
            Type_Log =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Log_Uns,
            others => Iir_Predefined_None),
         Type_Int =>
            (Type_Unsigned =>
               Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Uns_Uns,
             others => Iir_Predefined_None),
         Type_Log =>
            (Type_Unsigned =>
               Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Uns_Uns,
             others => Iir_Predefined_None),
         others =>
           (others => Iir_Predefined_None)));

   Sub_Patterns : constant Bin_Pattern_Type :=
     (Type_Slv =>
        (Type_Unsigned =>
           (Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Uns_Slv,
            Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Slv,
            Type_Int =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Int_Slv,
            Type_Log =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Log_Slv),
         Type_Signed =>
           (Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Slv,
            Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Slv,
            Type_Int =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Int_Slv,
            Type_Log =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Log_Slv),
         Type_Int =>
           (Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Uns_Slv,
            Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Sgn_Slv,
            others => Iir_Predefined_None),
         Type_Log =>
           (Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Uns_Slv,
            Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Sgn_Slv,
            others => Iir_Predefined_None)),
      Type_Signed =>
        (Type_Signed =>
           (Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Sgn,
            Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Sgn,
            Type_Int =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Int_Sgn,
            Type_Log =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Log_Sgn),
         Type_Unsigned =>
           (Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Sgn,
            others => Iir_Predefined_None),
         Type_Int =>
           (Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Sgn_Sgn,
            others => Iir_Predefined_None),
         Type_Log =>
           (Type_Signed =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Sgn_Sgn,
            others => Iir_Predefined_None)),
      Type_Unsigned =>
        (Type_Unsigned =>
           (Type_Unsigned =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Uns_Uns,
            Type_Int =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Int_Uns,
            Type_Log =>
              Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Log_Uns,
            others => Iir_Predefined_None),
         Type_Int =>
            (Type_Unsigned =>
               Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Uns_Uns,
             others => Iir_Predefined_None),
         Type_Log =>
            (Type_Unsigned =>
               Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Uns_Uns,
             others => Iir_Predefined_None),
         others =>
           (others => Iir_Predefined_None)));

   Lt_Patterns : constant Cmp_Pattern_Type :=
     (Type_Unsigned =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Uns_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Uns_Sgn,
         Type_Int      => Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Uns_Int),
      Type_Signed =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Sgn_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Sgn_Sgn,
         Type_Int      => Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Sgn_Int),
      Type_Int =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Int_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Int_Sgn,
         others => Iir_Predefined_None));

   Le_Patterns : constant Cmp_Pattern_Type :=
     (Type_Unsigned =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Le_Uns_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Le_Uns_Sgn,
         Type_Int      => Iir_Predefined_Ieee_Std_Logic_Arith_Le_Uns_Int),
      Type_Signed =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Le_Sgn_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Le_Sgn_Sgn,
         Type_Int      => Iir_Predefined_Ieee_Std_Logic_Arith_Le_Sgn_Int),
      Type_Int =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Le_Int_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Le_Int_Sgn,
         others => Iir_Predefined_None));

   Gt_Patterns : constant Cmp_Pattern_Type :=
     (Type_Unsigned =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Uns_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Uns_Sgn,
         Type_Int      => Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Uns_Int),
      Type_Signed =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Sgn_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Sgn_Sgn,
         Type_Int      => Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Sgn_Int),
      Type_Int =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Int_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Int_Sgn,
         others => Iir_Predefined_None));

   Ge_Patterns : constant Cmp_Pattern_Type :=
     (Type_Unsigned =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Uns_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Uns_Sgn,
         Type_Int      => Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Uns_Int),
      Type_Signed =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Sgn_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Sgn_Sgn,
         Type_Int      => Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Sgn_Int),
      Type_Int =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Int_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Int_Sgn,
         others => Iir_Predefined_None));

   Eq_Patterns : constant Cmp_Pattern_Type :=
     (Type_Unsigned =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Uns_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Uns_Sgn,
         Type_Int      => Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Uns_Int),
      Type_Signed =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Sgn_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Sgn_Sgn,
         Type_Int      => Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Sgn_Int),
      Type_Int =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Int_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Int_Sgn,
         others => Iir_Predefined_None));

   Ne_Patterns : constant Cmp_Pattern_Type :=
     (Type_Unsigned =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Uns_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Uns_Sgn,
         Type_Int      => Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Uns_Int),
      Type_Signed =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Sgn_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Sgn_Sgn,
         Type_Int      => Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Sgn_Int),
      Type_Int =>
        (Type_Unsigned => Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Int_Uns,
         Type_Signed   => Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Int_Sgn,
         others => Iir_Predefined_None));

   procedure Classify_Arg (Arg : Iir; Kind : out Arg_Kind)
   is
      Arg_Type : constant Iir := Get_Type (Arg);
   begin
      if Arg_Type = Signed_Type then
         Kind := Type_Signed;
      elsif Arg_Type = Unsigned_Type then
         Kind := Type_Unsigned;
      elsif Arg_Type = Vhdl.Std_Package.Integer_Subtype_Definition then
         Kind := Type_Int;
      elsif Arg_Type = Ieee.Std_Logic_1164.Std_Ulogic_Type then
         Kind := Type_Log;
      elsif Arg_Type = Ieee.Std_Logic_1164.Std_Logic_Vector_Type then
         Kind := Type_Slv;
      else
         raise Error;
      end if;
   end Classify_Arg;

   function Handle_Unary (Decl : Iir; Arg : Arg_Kind)
                         return Iir_Predefined_Functions
   is
      Res_Kind : Arg_Kind;
   begin
      case Get_Identifier (Decl) is
         when Name_Conv_Integer =>
            return Conv_Int_Patterns (Arg);
         when Name_Op_Plus =>
            Classify_Arg (Decl, Res_Kind);
            case Arg is
               when Type_Unsigned =>
                  case Res_Kind is
                     when Type_Unsigned =>
                        return Iir_Predefined_Ieee_Std_Logic_Arith_Id_Uns_Uns;
                     when Type_Slv =>
                        return Iir_Predefined_Ieee_Std_Logic_Arith_Id_Uns_Slv;
                     when others =>
                        null;
                  end case;
               when Type_Signed =>
                  case Res_Kind is
                     when Type_Signed =>
                        return Iir_Predefined_Ieee_Std_Logic_Arith_Id_Sgn_Sgn;
                     when Type_Slv =>
                        return Iir_Predefined_Ieee_Std_Logic_Arith_Id_Sgn_Slv;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
         when Name_Op_Minus =>
            Classify_Arg (Decl, Res_Kind);
            if Arg = Type_Signed then
               case Res_Kind is
                  when Type_Signed =>
                     return Iir_Predefined_Ieee_Std_Logic_Arith_Neg_Sgn_Sgn;
                  when Type_Slv =>
                     return Iir_Predefined_Ieee_Std_Logic_Arith_Neg_Sgn_Slv;
                  when others =>
                     null;
               end case;
            end if;
         when Name_Abs =>
            Classify_Arg (Decl, Res_Kind);
            if Arg = Type_Signed then
               case Res_Kind is
                  when Type_Signed =>
                     return Iir_Predefined_Ieee_Std_Logic_Arith_Abs_Sgn_Sgn;
                  when Type_Slv =>
                     return Iir_Predefined_Ieee_Std_Logic_Arith_Abs_Sgn_Slv;
                  when others =>
                     null;
               end case;
            end if;
         when others =>
            null;
      end case;
      return Iir_Predefined_None;
   end Handle_Unary;

   procedure Extract_Declarations (Pkg : Iir_Package_Declaration)
   is

      Decl : Iir;
      Type_Def : Iir;

      Arg1, Arg2 : Iir;
      Arg1_Kind, Arg2_Kind : Arg_Kind;
      Res_Kind : Arg_Kind;

      function Handle_Conv (Pats : Conv_Pattern_Type)
                           return Iir_Predefined_Functions is
      begin
         if Arg2_Kind /= Type_Int then
            raise Error;
         end if;
         return Pats (Arg1_Kind);
      end Handle_Conv;

      function Handle_Bin (Pats : Bin_Pattern_Type)
                          return Iir_Predefined_Functions is
      begin
         return Pats (Res_Kind, Arg1_Kind, Arg2_Kind);
      end Handle_Bin;

      function Handle_Cmp (Pats : Cmp_Pattern_Type)
                          return Iir_Predefined_Functions is
      begin
         return Pats (Arg1_Kind, Arg2_Kind);
      end Handle_Cmp;

      Def : Iir_Predefined_Functions;
   begin
      Decl := Get_Declaration_Chain (Pkg);

      if Decl /= Null_Iir
        and then Get_Kind (Decl) = Iir_Kind_Use_Clause
      then
         --  Mentor version.  Don't extract and don't crash.
         return;
      end if;

      --  The first declaration should be type Unsigned.
      if not (Decl /= Null_Iir
                and then Get_Kind (Decl) = Iir_Kind_Type_Declaration
                and then Get_Identifier (Decl) = Name_Unsigned)
      then
         raise Error;
      end if;

      Type_Def := Get_Type_Definition (Decl);
      if Get_Kind (Type_Def) /= Iir_Kind_Array_Type_Definition then
         raise Error;
      end if;
      Unsigned_Type := Type_Def;

      --  The second declaration should be type Signed.
      Decl := Get_Chain (Decl);
      Decl := Skip_Implicit (Decl);
      if not (Decl /= Null_Iir
                and then Get_Kind (Decl) = Iir_Kind_Type_Declaration
                and then Get_Identifier (Decl) = Name_Signed)
      then
         raise Error;
      end if;

      Type_Def := Get_Type_Definition (Decl);
      if Get_Kind (Type_Def) /= Iir_Kind_Array_Type_Definition then
         raise Error;
      end if;
      Signed_Type := Type_Def;

      --  Skip subtypes
      Decl := Get_Chain (Decl);
      Decl := Skip_Implicit (Decl);
      while Is_Valid (Decl) loop
         exit when Get_Kind (Decl) /= Iir_Kind_Subtype_Declaration;
         Decl := Get_Chain (Decl);
      end loop;

      --  Handle functions.
      while Is_Valid (Decl) loop
         Def := Iir_Predefined_None;

         case Get_Kind (Decl) is
            when Iir_Kind_Function_Declaration =>
               Arg1 := Get_Interface_Declaration_Chain (Decl);
               if Is_Null (Arg1) then
                  raise Error;
               end if;

               Classify_Arg (Arg1, Arg1_Kind);
               Arg2 := Get_Chain (Arg1);
               if Is_Valid (Arg2) then
                  --  Dyadic function.
                  Classify_Arg (Arg2, Arg2_Kind);

                  case Get_Identifier (Decl) is
                     when Name_Op_Plus =>
                        Classify_Arg (Decl, Res_Kind);
                        Def := Handle_Bin (Add_Patterns);
                     when Name_Op_Minus =>
                        Classify_Arg (Decl, Res_Kind);
                        Def := Handle_Bin (Sub_Patterns);
                     when Name_Op_Mul =>
                        Classify_Arg (Decl, Res_Kind);
                        Def := Handle_Bin (Mul_Patterns);
                     when Name_Conv_Unsigned =>
                        Def := Handle_Conv (Conv_Uns_Patterns);
                     when Name_Conv_Std_Logic_Vector =>
                        Def := Handle_Conv (Conv_Vec_Patterns);
                     when Name_Op_Less =>
                        Def := Handle_Cmp (Lt_Patterns);
                     when Name_Op_Less_Equal =>
                        Def := Handle_Cmp (Le_Patterns);
                     when Name_Op_Greater =>
                        Def := Handle_Cmp (Gt_Patterns);
                     when Name_Op_Greater_Equal =>
                        Def := Handle_Cmp (Ge_Patterns);
                     when Name_Op_Equality =>
                        Def := Handle_Cmp (Eq_Patterns);
                     when Name_Op_Inequality =>
                        Def := Handle_Cmp (Ne_Patterns);
                     when Name_Ext =>
                        if Arg1_Kind /= Type_Slv or Arg2_Kind /= Type_Int then
                           raise Error;
                        end if;
                        Def := Iir_Predefined_Ieee_Std_Logic_Arith_Ext;
                     when Name_Sxt =>
                        if Arg1_Kind /= Type_Slv or Arg2_Kind /= Type_Int then
                           raise Error;
                        end if;
                        Def := Iir_Predefined_Ieee_Std_Logic_Arith_Sxt;
                     when Name_Shl =>
                        if Arg2_Kind /= Type_Unsigned then
                           raise Error;
                        end if;
                        if Arg1_Kind = Type_Unsigned then
                           Def := Iir_Predefined_Ieee_Std_Logic_Arith_Shl_Uns;
                        elsif Arg1_Kind = Type_Signed then
                           Def := Iir_Predefined_Ieee_Std_Logic_Arith_Shl_Sgn;
                        end if;
                     when Name_Shr =>
                        if Arg2_Kind /= Type_Unsigned then
                           raise Error;
                        end if;
                        if Arg1_Kind = Type_Unsigned then
                           Def := Iir_Predefined_Ieee_Std_Logic_Arith_Shr_Uns;
                        elsif Arg1_Kind = Type_Signed then
                           Def := Iir_Predefined_Ieee_Std_Logic_Arith_Shr_Sgn;
                        end if;
                     when others =>
                        null;
                  end case;
               else
                  --  Monadic function.
                  Def := Handle_Unary (Decl, Arg1_Kind);
               end if;

            when Iir_Kind_Non_Object_Alias_Declaration
              | Iir_Kind_Procedure_Declaration =>
               null;

            when others =>
               raise Error;
         end case;
         Set_Implicit_Definition (Decl, Def);

         Decl := Get_Chain (Decl);
      end loop;
   exception
      when Error =>
         Error_Msg_Sem (+Pkg, "package ieee.std_logic_arith is ill-formed");
   end Extract_Declarations;
end Vhdl.Ieee.Std_Logic_Arith;
