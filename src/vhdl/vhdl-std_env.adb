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

with Types; use Types;
with Std_Names; use Std_Names;

package body Vhdl.Std_Env is
   procedure Extract_Declarations (Pkg : Iir_Package_Declaration)
   is
      Decl : Iir;
      Predef : Iir_Predefined_Functions;
      Inter : Iir;
   begin
      Std_Env_Pkg := Pkg;

      Decl := Get_Declaration_Chain (Pkg);

      while Decl /= Null_Iir loop
         pragma Assert (Get_Kind (Decl) in Iir_Kinds_Subprogram_Declaration);
         Inter := Get_Interface_Declaration_Chain (Decl);
         case Get_Identifier (Decl) is
            when Name_Stop =>
               if Inter = Null_Iir then
                  Predef := Iir_Predefined_Std_Env_Stop;
               else
                  Predef := Iir_Predefined_Std_Env_Stop_Status;
                  pragma Assert (Get_Chain (Inter) = Null_Iir);
               end if;
            when Name_Finish =>
               if Inter = Null_Iir then
                  Predef := Iir_Predefined_Std_Env_Finish;
               else
                  Predef := Iir_Predefined_Std_Env_Finish_Status;
                  pragma Assert (Get_Chain (Inter) = Null_Iir);
               end if;
            when Name_Resolution_Limit =>
               pragma Assert (Inter = Null_Iir);
               Predef := Iir_Predefined_Std_Env_Resolution_Limit;
            when others =>
               raise Internal_Error;
         end case;
         Set_Implicit_Definition (Decl, Predef);
         Decl := Get_Chain (Decl);
      end loop;
   end Extract_Declarations;
end Vhdl.Std_Env;
