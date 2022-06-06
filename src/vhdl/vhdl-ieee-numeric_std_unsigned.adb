--  Nodes recognizer for ieee.std_logic_unsigned and ieee.std_logic_signed
--  Copyright (C) 2021 Tristan Gingold
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

with Vhdl.Std_Package;
with Std_Names; use Std_Names;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Errors;

package body Vhdl.Ieee.Numeric_Std_Unsigned is
   type Arg_Kind is (Arg_Slv, Arg_Int, Arg_Log);

   Error : exception;

   procedure Classify_Arg (Arg : Iir; Kind : out Arg_Kind)
   is
      Arg_Type : constant Iir := Get_Type (Arg);
   begin
      if Arg_Type = Vhdl.Std_Package.Integer_Subtype_Definition
        or else Arg_Type = Vhdl.Std_Package.Natural_Subtype_Definition
      then
         Kind := Arg_Int;
      elsif Arg_Type = Ieee.Std_Logic_1164.Std_Logic_Type
        or else Arg_Type = Ieee.Std_Logic_1164.Std_Ulogic_Type
      then
         Kind := Arg_Log;
      elsif Arg_Type = Ieee.Std_Logic_1164.Std_Logic_Vector_Type
        or else Arg_Type = Ieee.Std_Logic_1164.Std_Ulogic_Vector_Type
      then
         Kind := Arg_Slv;
      else
         raise Error;
      end if;
   end Classify_Arg;

   function Extract_Dyadic_Declaration (Decl : Iir; Arg1: Iir; Arg2: Iir)
                                       return Iir_Predefined_Functions
   is
      Arg1_Kind, Arg2_Kind  : Arg_Kind;
      Res : Iir_Predefined_Functions;
   begin
      Res := Iir_Predefined_None;
      Classify_Arg (Arg1, Arg1_Kind);
      Classify_Arg (Arg2, Arg2_Kind);
      case Get_Identifier (Decl) is
         when Name_Op_Plus =>
            if Arg1_Kind = Arg_Slv and Arg2_Kind = Arg_Slv then
               Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Add_Slv_Slv;
            elsif Arg1_Kind = Arg_Slv and Arg2_Kind = Arg_Int then
               Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Add_Slv_Nat;
            elsif Arg1_Kind = Arg_Int and Arg2_Kind = Arg_Slv then
               Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Add_Nat_Slv;
            end if;
         when Name_Op_Minus =>
            if Arg1_Kind = Arg_Slv and Arg2_Kind = Arg_Slv then
               Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Sub_Slv_Slv;
            elsif Arg1_Kind = Arg_Slv and Arg2_Kind = Arg_Int then
               Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Sub_Slv_Nat;
            elsif Arg1_Kind = Arg_Int and Arg2_Kind = Arg_Slv then
               Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Sub_Nat_Slv;
            end if;
         when Name_To_Stdlogicvector =>
            if Arg1_Kind = Arg_Int and Arg2_Kind = Arg_Int then
               Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Slv_Nat_Nat;
            elsif Arg1_Kind = Arg_Int and Arg2_Kind = Arg_Slv then
               Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Slv_Nat_Slv;
            end if;
         when Name_To_Stdulogicvector =>
            if Arg1_Kind = Arg_Int and Arg2_Kind = Arg_Int then
               Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Suv_Nat_Nat;
            elsif Arg1_Kind = Arg_Int and Arg2_Kind = Arg_Slv then
               Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Suv_Nat_Suv;
            end if;
         when Name_Resize =>
            if Arg2_Kind = Arg_Int then
               Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Resize_Slv_Nat;
            elsif Arg2_Kind = Arg_Slv then
               Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Resize_Slv_Slv;
            end if;
         when Name_Find_Leftmost =>
            pragma Assert (Arg1_Kind = Arg_Slv and Arg2_Kind = Arg_Log);
            Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Find_Leftmost;
         when Name_Find_Rightmost =>
            pragma Assert (Arg1_Kind = Arg_Slv and Arg2_Kind = Arg_Log);
            Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Find_Rightmost;
         when Name_Shift_Left =>
            pragma Assert (Arg1_Kind = Arg_Slv and Arg2_Kind = Arg_Int);
            Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Shift_Left;
         when Name_Shift_Right =>
            pragma Assert (Arg1_Kind = Arg_Slv and Arg2_Kind = Arg_Int);
            Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Shift_Right;
         when Name_Rotate_Left =>
            pragma Assert (Arg1_Kind = Arg_Slv and Arg2_Kind = Arg_Int);
            Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Rotate_Left;
         when Name_Rotate_Right =>
            pragma Assert (Arg1_Kind = Arg_Slv and Arg2_Kind = Arg_Int);
            Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Rotate_Right;
         when Name_Maximum =>
            if Arg1_Kind = Arg_Slv and Arg2_Kind = Arg_Slv then
               Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Maximum_Slv_Slv;
            end if;
         when Name_Minimum =>
            if Arg1_Kind = Arg_Slv and Arg2_Kind = Arg_Slv then
               Res := Iir_Predefined_Ieee_Numeric_Std_Unsigned_Minimum_Slv_Slv;
            end if;
         when others =>
            null;
      end case;
      return Res;
   end Extract_Dyadic_Declaration;

   procedure Extract_Declaration (Decl : Iir)
   is
      Arg1, Arg2 : Iir;
      Arg1_Kind : Arg_Kind;
      Res : Iir_Predefined_Functions;
   begin
      Arg1 := Get_Interface_Declaration_Chain (Decl);
      if Is_Null (Arg1) then
         raise Error;
      end if;

      Arg2 := Get_Chain (Arg1);
      if Is_Valid (Arg2) then
         Res := Extract_Dyadic_Declaration (Decl, Arg1, Arg2);
      else
         --  Monadic function.
         Res := Iir_Predefined_None;
         Classify_Arg (Arg1, Arg1_Kind);
         case Get_Identifier (Decl) is
            when Name_To_Integer =>
               pragma Assert (Arg1_Kind = Arg_Slv);
               Res :=
                 Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Integer_Slv_Nat;
            when others =>
               null;
         end case;
      end if;
      Set_Implicit_Definition (Decl, Res);
   end Extract_Declaration;

   procedure Extract_Declarations (Pkg : Iir_Package_Declaration)
   is
      Decl : Iir;
   begin
      Decl := Get_Declaration_Chain (Pkg);

      Decl := Skip_Copyright_Notice (Decl);

      --  Handle functions.
      while Is_Valid (Decl) loop
         case Get_Kind (Decl) is
            when Iir_Kind_Function_Declaration =>
               Extract_Declaration (Decl);
            when Iir_Kind_Non_Object_Alias_Declaration =>
               null;
            when others =>
               Vhdl.Errors.Error_Kind ("extract_declarations", Decl);
               raise Error;
         end case;

         Decl := Get_Chain (Decl);
      end loop;
   end Extract_Declarations;
end Vhdl.Ieee.Numeric_Std_Unsigned;
