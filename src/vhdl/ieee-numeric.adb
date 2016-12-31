--  Nodes recognizer for ieee.numeric_std and ieee.numeric_bit.
--  Copyright (C) 2016 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Types; use Types;
with Std_Package;
with Std_Names; use Std_Names;
with Errorout; use Errorout;
with Ieee.Std_Logic_1164;

package body Ieee.Numeric is
   type Pkg_Kind is (Pkg_Std, Pkg_Bit);
   type Sign_Kind is (Type_Signed, Type_Unsigned,
                      Type_Log, Type_Slv, Type_Suv);
   subtype Sign_Num_Kind is Sign_Kind range Type_Signed .. Type_Unsigned;
   type Arg_Kind is (Arg_Vect, Arg_Scal);
   type Args_Kind is (Arg_Vect_Vect, Arg_Vect_Scal, Arg_Scal_Vect,
                      Arg_Vect_Log, Arg_Log_Vect);

   type Binary_Pattern_Type is array (Pkg_Kind, Sign_Num_Kind, Args_Kind)
     of Iir_Predefined_Functions;

   Add_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Add_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Add_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Eq_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Eq_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Eq_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Eq_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Eq_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Error : exception;

   procedure Extract_Declarations (Pkg_Decl : Iir_Package_Declaration;
                                   Pkg : Pkg_Kind;
                                   Unsigned_Type : out Iir;
                                   Signed_Type : out Iir)
   is
      procedure Classify_Arg
        (Arg : Iir; Sign : out Sign_Kind; Kind : out Arg_Kind)
      is
         Arg_Type : constant Iir := Get_Type (Arg);
      begin
         if Arg_Type = Signed_Type then
            Sign := Type_Signed;
            Kind := Arg_Vect;
         elsif Arg_Type = Unsigned_Type then
            Sign := Type_Unsigned;
            Kind := Arg_Vect;
         elsif Arg_Type = Std_Package.Integer_Subtype_Definition then
            Sign := Type_Signed;
            Kind := Arg_Scal;
         elsif Arg_Type = Std_Package.Natural_Subtype_Definition then
            Sign := Type_Unsigned;
            Kind := Arg_Scal;
         elsif Arg_Type = Ieee.Std_Logic_1164.Std_Ulogic_Type then
            Sign := Type_Log;
            Kind := Arg_Scal;
         elsif Arg_Type = Ieee.Std_Logic_1164.Std_Ulogic_Vector_Type then
            Sign := Type_Suv;
            Kind := Arg_Vect;
         elsif Arg_Type = Ieee.Std_Logic_1164.Std_Logic_Type then
            Sign := Type_Log;
            Kind := Arg_Scal;
         elsif Arg_Type = Ieee.Std_Logic_1164.Std_Logic_Vector_Type then
            Sign := Type_Slv;
            Kind := Arg_Vect;
         else
            raise Error;
         end if;
      end Classify_Arg;

      Decl : Iir;
      Def : Iir;

      Arg1, Arg2 : Iir;
      Arg1_Sign, Arg2_Sign : Sign_Kind;
      Arg1_Kind, Arg2_Kind : Arg_Kind;

      procedure Handle_Binary (Pats : Binary_Pattern_Type)
      is
         Kind : Args_Kind;
         Sign : Sign_Kind;
      begin
         if Arg1_Sign = Arg2_Sign then
            Sign := Arg1_Sign;
            case Arg1_Kind is
               when Arg_Vect =>
                  case Arg2_Kind is
                     when Arg_Vect => Kind := Arg_Vect_Vect;
                     when Arg_Scal => Kind := Arg_Vect_Scal;
                  end case;
               when Arg_Scal =>
                  case Arg2_Kind is
                     when Arg_Vect => Kind := Arg_Scal_Vect;
                     when Arg_Scal => raise Error;
                  end case;
            end case;
         elsif Arg1_Kind = Arg_Vect and Arg2_Sign = Type_Log then
            Sign := Arg1_Sign;
            Kind := Arg_Vect_Log;
         elsif Arg1_Sign = Type_Log and Arg2_Kind = Arg_Vect then
            Sign := Arg2_Sign;
            Kind := Arg_Log_Vect;
         else
            raise Error;
         end if;

         Set_Implicit_Definition (Decl, Pats (Pkg, Sign, Kind));
      end Handle_Binary;

   begin
      Decl := Get_Declaration_Chain (Pkg_Decl);

      --  Skip a potential copyright constant.
      if Decl /= Null_Iir
        and then Get_Kind (Decl) = Iir_Kind_Constant_Declaration
        and then (Get_Base_Type (Get_Type (Decl))
                  = Std_Package.String_Type_Definition)
      then
         Decl := Get_Chain (Decl);
      end if;

      --  The first declaration should be type Unsigned or Unresolved_Unsigned
      if not (Decl /= Null_Iir
                and then Get_Kind (Decl) = Iir_Kind_Type_Declaration
                and then (Get_Identifier (Decl) = Name_Unsigned
                            or else
                            Get_Identifier (Decl) = Name_Unresolved_Unsigned))
      then
         raise Error;
      end if;

      Def := Get_Type_Definition (Decl);
      if Get_Kind (Def) /= Iir_Kind_Array_Type_Definition then
         raise Error;
      end if;
      Unsigned_Type := Def;

      --  The second declaration should be type Signed.
      Decl := Get_Chain (Decl);
      Decl := Skip_Implicit (Decl);
      if not (Decl /= Null_Iir
                and then Get_Kind (Decl) = Iir_Kind_Type_Declaration
                and then (Get_Identifier (Decl) = Name_Signed
                            or else
                            Get_Identifier (Decl) = Name_Unresolved_Signed))
      then
         raise Error;
      end if;

      Def := Get_Type_Definition (Decl);
      if Get_Kind (Def) /= Iir_Kind_Array_Type_Definition then
         raise Error;
      end if;
      Signed_Type := Def;

      --  For vhdl 2008, skip subtypes
      Decl := Get_Chain (Decl);
      Decl := Skip_Implicit (Decl);
      while Is_Valid (Decl) loop
         exit when Get_Kind (Decl) /= Iir_Kind_Subtype_Declaration;
         Decl := Get_Chain (Decl);
      end loop;

      --  Handle functions.
      while Is_Valid (Decl) loop
         case Get_Kind (Decl) is
            when Iir_Kind_Function_Declaration =>
               Arg1 := Get_Interface_Declaration_Chain (Decl);
               if Is_Null (Arg1) then
                  raise Error;
               end if;

               Classify_Arg (Arg1, Arg1_Sign, Arg1_Kind);
               Arg2 := Get_Chain (Arg1);
               if Is_Valid (Arg2) then
                  Classify_Arg (Arg2, Arg2_Sign, Arg2_Kind);

                  case Get_Identifier (Decl) is
                     when Name_Op_Plus =>
                        Handle_Binary (Add_Patterns);
                     when Name_Op_Equality =>
                        Handle_Binary (Eq_Patterns);
                     when Name_To_Bstring
                       | Name_To_Ostring
                       | Name_To_Hstring =>
                        null;
                     when others =>
                        null;
                  end case;
               end if;

            when Iir_Kind_Non_Object_Alias_Declaration
              | Iir_Kind_Procedure_Declaration =>
               null;

            when others =>
               raise Error;
         end case;
         Decl := Get_Chain (Decl);
      end loop;
   end Extract_Declarations;

   procedure Extract_Std_Declarations (Pkg : Iir_Package_Declaration) is
   begin
      Numeric_Std_Pkg := Pkg;

      Extract_Declarations
        (Pkg, Pkg_Std, Numeric_Std_Unsigned_Type, Numeric_Std_Signed_Type);
   exception
      when Error =>
         Error_Msg_Sem (+Pkg, "package ieee.numeric_std is ill-formed");
         Numeric_Std_Pkg := Null_Iir;
         Numeric_Std_Unsigned_Type := Null_Iir;
         Numeric_Std_Signed_Type := Null_Iir;
   end Extract_Std_Declarations;
end Ieee.Numeric;
