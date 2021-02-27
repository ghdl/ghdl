--  Nodes recognizer for ieee.math_real.
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

with Std_Names; use Std_Names;

package body Vhdl.Ieee.Math_Real is
   procedure Extract_Declarations (Pkg : Iir_Package_Declaration)
   is
      Decl : Iir;
      Predef : Iir_Predefined_Functions;
   begin
      Math_Real_Pkg := Pkg;

      Decl := Get_Declaration_Chain (Pkg);

      --  Skip a potential copyright constant.
      Decl := Skip_Copyright_Notice (Decl);

      --  Skip any declarations but functions.
      loop
         Decl := Get_Chain (Decl);
         exit when Decl = Null_Iir;

         case Get_Kind (Decl) is
            when Iir_Kind_Function_Declaration =>
               Predef := Iir_Predefined_None;
               case Get_Identifier (Decl) is
                  when Name_Ceil =>
                     Predef := Iir_Predefined_Ieee_Math_Real_Ceil;
                  when Name_Floor =>
                     Predef := Iir_Predefined_Ieee_Math_Real_Floor;
                  when Name_Round =>
                     Predef := Iir_Predefined_Ieee_Math_Real_Round;
                  when Name_Log2 =>
                     Predef := Iir_Predefined_Ieee_Math_Real_Log2;
                  when Name_Sin =>
                     Predef := Iir_Predefined_Ieee_Math_Real_Sin;
                  when Name_Cos =>
                     Predef := Iir_Predefined_Ieee_Math_Real_Cos;
                  when Name_Arctan =>
                     Predef := Iir_Predefined_Ieee_Math_Real_Arctan;
                  when Name_Op_Exp =>
                     Predef := Iir_Predefined_Ieee_Math_Real_Pow;
                  when others =>
                     null;
               end case;
               Set_Implicit_Definition (Decl, Predef);
            when Iir_Kind_Constant_Declaration =>
               null;
            when others =>
               null;
         end case;
      end loop;
   end Extract_Declarations;
end Vhdl.Ieee.Math_Real;
