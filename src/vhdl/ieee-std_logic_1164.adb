--  Nodes recognizer for ieee.std_logic_1164.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Std_Names; use Std_Names;
with Iirs_Utils; use Iirs_Utils;
with Errorout; use Errorout;
with Std_Package;

package body Ieee.Std_Logic_1164 is
   function Skip_Implicit (Decl : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Decl;
      loop
         exit when Res = Null_Iir;
         exit when not (Get_Kind (Res) = Iir_Kind_Function_Declaration
                          and then Is_Implicit_Subprogram (Res));
         Res := Get_Chain (Res);
      end loop;
      return Res;
   end Skip_Implicit;

   function Is_Scalar_Parameter (Inter : Iir) return Boolean is
   begin
      return Get_Base_Type (Get_Type (Inter)) = Std_Ulogic_Type;
   end Is_Scalar_Parameter;

   function Is_Vector_Parameter (Inter : Iir) return Boolean
   is
      Base_Type : constant Iir := Get_Base_Type (Get_Type (Inter));
   begin
      return Base_Type = Std_Ulogic_Vector_Type
        or Base_Type = Std_Logic_Vector_Type;
   end Is_Vector_Parameter;

   --  Return True iff the profile of FUNC is: (l, r : std_ulogic)
   function Is_Scalar_Scalar_Function (Func : Iir) return Boolean
   is
      Inter : constant Iir := Get_Interface_Declaration_Chain (Func);
      Inter2 : Iir;
   begin
      if Get_Implicit_Definition (Func) /= Iir_Predefined_None then
         return False;
      end if;
      if Inter = Null_Iir or else not Is_Scalar_Parameter (Inter) then
         return False;
      end if;
      Inter2 := Get_Chain (Inter);
      if Inter2 =  Null_Iir or else not Is_Scalar_Parameter (Inter2) then
         return False;
      end if;
      if Get_Chain (Inter2) /= Null_Iir then
         return False;
      end if;

      return True;
   end Is_Scalar_Scalar_Function;

   --  Return True iff the profile of FUNC is: (l : std_ulogic)
   function Is_Scalar_Function (Func : Iir) return Boolean
   is
      Inter : constant Iir := Get_Interface_Declaration_Chain (Func);
   begin
      if Get_Implicit_Definition (Func) /= Iir_Predefined_None then
         return False;
      end if;
      if Inter = Null_Iir or else not Is_Scalar_Parameter (Inter) then
         return False;
      end if;
      if Get_Chain (Inter) /= Null_Iir then
         return False;
      end if;

      return True;
   end Is_Scalar_Function;

   --  Return True iff the profile of FUNC is: (l, r : std_[u]logic_vector)
   function Is_Vector_Vector_Function (Func : Iir) return Boolean
   is
      Inter : constant Iir := Get_Interface_Declaration_Chain (Func);
      Inter2 : Iir;
   begin
      if Get_Implicit_Definition (Func) /= Iir_Predefined_None then
         return False;
      end if;
      if Inter = Null_Iir or else not Is_Vector_Parameter (Inter) then
         return False;
      end if;
      Inter2 := Get_Chain (Inter);
      if Inter2 =  Null_Iir or else not Is_Vector_Parameter (Inter2) then
         return False;
      end if;
      if Get_Chain (Inter2) /= Null_Iir then
         return False;
      end if;

      return True;
   end Is_Vector_Vector_Function;

   --  Return True iff the profile of FUNC is: (l : std_[u]logic_vector)
   function Is_Vector_Function (Func : Iir) return Boolean
   is
      Inter : constant Iir := Get_Interface_Declaration_Chain (Func);
   begin
      if Get_Implicit_Definition (Func) /= Iir_Predefined_None then
         return False;
      end if;
      if Inter = Null_Iir or else not Is_Vector_Parameter (Inter) then
         return False;
      end if;
      if Get_Chain (Inter) /= Null_Iir then
         return False;
      end if;

      return True;
   end Is_Vector_Function;

   procedure Extract_Declarations (Pkg : Iir_Package_Declaration)
   is
      Error : exception;

      Decl : Iir;
      Def : Iir;
   begin
      Std_Logic_1164_Pkg := Pkg;

      Decl := Get_Declaration_Chain (Pkg);

      --  Skip a potential copyright constant.
      if Decl /= Null_Iir
        and then Get_Kind (Decl) = Iir_Kind_Constant_Declaration
        and then (Get_Base_Type (Get_Type (Decl))
                  = Std_Package.String_Type_Definition)
      then
         Decl := Get_Chain (Decl);
      end if;

      --  The first declaration should be type std_ulogic.
      if Decl = Null_Iir
        or else Get_Kind (Decl) /= Iir_Kind_Type_Declaration
        or else Get_Identifier (Decl) /= Name_Std_Ulogic
      then
         raise Error;
      end if;

      Def := Get_Type_Definition (Decl);
      if Get_Kind (Def) /= Iir_Kind_Enumeration_Type_Definition then
         raise Error;
      end if;
      Std_Ulogic_Type := Def;

      --  The second declaration should be std_ulogic_vector.
      Decl := Get_Chain (Decl);
      Decl := Skip_Implicit (Decl);
      if Decl = Null_Iir
        or else Get_Kind (Decl) /= Iir_Kind_Type_Declaration
        or else Get_Identifier (Decl) /= Name_Std_Ulogic_Vector
      then
         raise Error;
      end if;
      Def := Get_Type_Definition (Decl);
      if Get_Kind (Def) /= Iir_Kind_Array_Type_Definition then
         raise Error;
      end if;
      Std_Ulogic_Vector_Type := Def;

      --  The third declaration should be resolved.
      Decl := Get_Chain (Decl);
      Decl := Skip_Implicit (Decl);
      if Decl = Null_Iir
        or else Get_Kind (Decl) /= Iir_Kind_Function_Declaration
      then
         --  FIXME: check name ?
         raise Error;
      end if;
      Resolved := Decl;

      --  The fourth declaration should be std_logic.
      Decl := Get_Chain (Decl);
      Decl := Skip_Implicit (Decl);
      if Decl = Null_Iir
        or else Get_Kind (Decl) /= Iir_Kind_Subtype_Declaration
        or else Get_Identifier (Decl) /= Name_Std_Logic
      then
         raise Error;
      end if;
      Def := Get_Type (Decl);
      if Get_Kind (Def) /= Iir_Kind_Enumeration_Subtype_Definition then
         raise Error;
      end if;
      Std_Logic_Type := Def;

      --  The fifth declaration should be std_logic_vector.
      Decl := Get_Chain (Decl);
      Decl := Skip_Implicit (Decl);
      if Decl = Null_Iir
        or else (Get_Kind (Decl) /= Iir_Kind_Type_Declaration
                   and then Get_Kind (Decl) /= Iir_Kind_Subtype_Declaration)
        or else Get_Identifier (Decl) /= Name_Std_Logic_Vector
      then
         raise Error;
      end if;
      Def := Get_Type (Decl);
--      if Get_Kind (Def) /= Iir_Kind_Array_Type_Definition then
--         raise Error;
--      end if;
      Std_Logic_Vector_Type := Def;

      --  Skip any declarations but functions.
      loop
         Decl := Get_Chain (Decl);
         exit when Decl = Null_Iir;

         if Get_Kind (Decl) = Iir_Kind_Function_Declaration then
            if Get_Identifier (Decl) = Name_Rising_Edge then
               Rising_Edge := Decl;
            elsif Get_Identifier (Decl) = Name_Falling_Edge then
               Falling_Edge := Decl;
            elsif Is_Scalar_Scalar_Function (Decl) then
               declare
                  Predefined : Iir_Predefined_Functions;
               begin
                  case Get_Identifier (Decl) is
                     when Name_And =>
                        Predefined := Iir_Predefined_Ieee_1164_Scalar_And;
                     when Name_Nand =>
                        Predefined := Iir_Predefined_Ieee_1164_Scalar_Nand;
                     when Name_Or =>
                        Predefined := Iir_Predefined_Ieee_1164_Scalar_Or;
                     when Name_Nor =>
                        Predefined := Iir_Predefined_Ieee_1164_Scalar_Nor;
                     when Name_Xor =>
                        Predefined := Iir_Predefined_Ieee_1164_Scalar_Xor;
                     when Name_Xnor =>
                        Predefined := Iir_Predefined_Ieee_1164_Scalar_Xnor;
                     when others =>
                        Predefined := Iir_Predefined_None;
                  end case;
                  Set_Implicit_Definition (Decl, Predefined);
               end;
            elsif Is_Scalar_Function (Decl)
              and then Get_Identifier (Decl) = Name_Not
            then
               Set_Implicit_Definition
                 (Decl, Iir_Predefined_Ieee_1164_Scalar_Not);
            elsif Is_Vector_Vector_Function (Decl) then
               declare
                  Predefined : Iir_Predefined_Functions;
               begin
                  case Get_Identifier (Decl) is
                     when Name_And =>
                        Predefined := Iir_Predefined_Ieee_1164_Vector_And;
                     when Name_Nand =>
                        Predefined := Iir_Predefined_Ieee_1164_Vector_Nand;
                     when Name_Or =>
                        Predefined := Iir_Predefined_Ieee_1164_Vector_Or;
                     when Name_Nor =>
                        Predefined := Iir_Predefined_Ieee_1164_Vector_Nor;
                     when Name_Xor =>
                        Predefined := Iir_Predefined_Ieee_1164_Vector_Xor;
                     when Name_Xnor =>
                        Predefined := Iir_Predefined_Ieee_1164_Vector_Xnor;
                     when others =>
                        Predefined := Iir_Predefined_None;
                  end case;
                  Set_Implicit_Definition (Decl, Predefined);
               end;
            elsif Is_Vector_Function (Decl)
              and then Get_Identifier (Decl) = Name_Not
            then
               Set_Implicit_Definition
                 (Decl, Iir_Predefined_Ieee_1164_Vector_Not);
            end if;
         end if;
      end loop;

      --  Since rising_edge and falling_edge do not read activity of its
      --  parameter, clear the flag to allow more optimizations.
      if Rising_Edge /= Null_Iir then
         Set_Has_Active_Flag
           (Get_Interface_Declaration_Chain (Rising_Edge), False);
      else
         raise Error;
      end if;
      if Falling_Edge /= Null_Iir then
         Set_Has_Active_Flag
           (Get_Interface_Declaration_Chain (Falling_Edge), False);
      else
         raise Error;
      end if;

   exception
      when Error =>
         Error_Msg_Sem (+Pkg, "package ieee.std_logic_1164 is ill-formed");

         --  Clear all definitions.
         Std_Logic_1164_Pkg := Null_Iir;
         Std_Ulogic_Type := Null_Iir;
         Std_Ulogic_Vector_Type := Null_Iir;
         Std_Logic_Type := Null_Iir;
         Std_Logic_Vector_Type := Null_Iir;
         Rising_Edge := Null_Iir;
         Falling_Edge := Null_Iir;
   end Extract_Declarations;
end Ieee.Std_Logic_1164;
