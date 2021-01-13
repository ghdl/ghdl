--  Nodes recognizer for ieee.std_logic_misc.
--  Copyright (C) 2020 Tristan Gingold
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
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Ieee.Std_Logic_1164;

package body Vhdl.Ieee.Std_Logic_Misc is
   Error : exception;

   procedure Extract_Declarations (Pkg : Iir_Package_Declaration)
   is
      Decl : Iir;

      function Handle_Reduce (Res_Slv : Iir_Predefined_Functions;
                              Res_Suv : Iir_Predefined_Functions)
                             return Iir_Predefined_Functions
      is
         Arg : Iir;
         Arg_Type : Iir;
      begin
         Arg := Get_Interface_Declaration_Chain (Decl);
         if Is_Null (Arg) then
            raise Error;
         end if;
         if Get_Chain (Arg) /= Null_Iir then
            raise Error;
         end if;
         Arg_Type := Get_Type (Arg);
         if Arg_Type = Ieee.Std_Logic_1164.Std_Logic_Vector_Type then
            return Res_Slv;
         elsif Arg_Type = Ieee.Std_Logic_1164.Std_Ulogic_Vector_Type then
            return Res_Suv;
         else
            raise Error;
         end if;
      end Handle_Reduce;

      Def : Iir_Predefined_Functions;
   begin
      Decl := Get_Declaration_Chain (Pkg);

      --  Handle functions.
      while Is_Valid (Decl) loop
         Def := Iir_Predefined_None;

         if Get_Kind (Decl) = Iir_Kind_Function_Declaration
           and then Get_Implicit_Definition (Decl) = Iir_Predefined_None
         then
            case Get_Identifier (Decl) is
               when Name_And_Reduce =>
                  Def := Handle_Reduce
                    (Iir_Predefined_Ieee_Std_Logic_Misc_And_Reduce_Slv,
                     Iir_Predefined_Ieee_Std_Logic_Misc_And_Reduce_Suv);
               when Name_Nand_Reduce =>
                  Def := Handle_Reduce
                    (Iir_Predefined_Ieee_Std_Logic_Misc_Nand_Reduce_Slv,
                     Iir_Predefined_Ieee_Std_Logic_Misc_Nand_Reduce_Suv);
               when Name_Or_Reduce =>
                  Def := Handle_Reduce
                    (Iir_Predefined_Ieee_Std_Logic_Misc_Or_Reduce_Slv,
                     Iir_Predefined_Ieee_Std_Logic_Misc_Or_Reduce_Suv);
               when Name_Nor_Reduce =>
                  Def := Handle_Reduce
                    (Iir_Predefined_Ieee_Std_Logic_Misc_Nor_Reduce_Slv,
                     Iir_Predefined_Ieee_Std_Logic_Misc_Nor_Reduce_Suv);
               when Name_Xor_Reduce =>
                  Def := Handle_Reduce
                    (Iir_Predefined_Ieee_Std_Logic_Misc_Xor_Reduce_Slv,
                     Iir_Predefined_Ieee_Std_Logic_Misc_Xor_Reduce_Suv);
               when Name_Xnor_Reduce =>
                  Def := Handle_Reduce
                    (Iir_Predefined_Ieee_Std_Logic_Misc_Xnor_Reduce_Slv,
                     Iir_Predefined_Ieee_Std_Logic_Misc_Xnor_Reduce_Suv);
               when others =>
                  Def := Iir_Predefined_None;
            end case;
            Set_Implicit_Definition (Decl, Def);
         end if;

         Decl := Get_Chain (Decl);
      end loop;
   exception
      when Error =>
         Error_Msg_Sem (+Pkg, "package ieee.std_logic_misc is ill-formed");
   end Extract_Declarations;
end Vhdl.Ieee.Std_Logic_Misc;
