--  Interpreter AMS simulation
--  Copyright (C) 2014 Tristan Gingold
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

with Simple_IO; use Simple_IO;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Prints;

package body Simul.Debugger.AMS is
   procedure Disp_Quantity_Name (Quantity : Quantity_Index_Type)
   is
      Obj : Scalar_Quantity renames Scalar_Quantities.Table (Quantity);
   begin
      Disp_Instance_Name (Obj.Instance, True);
      Put ('.');
      Put (Image_Identifier (Obj.Decl));
      if Obj.Kind = Quantity_Reference then
         Put ("'Ref");
      end if;
   end Disp_Quantity_Name;

   procedure Disp_Term (Term : Ams_Term_Acc) is
   begin
      case Term.Sign is
         when Op_Plus =>
            Put (" + ");
         when Op_Minus =>
            Put (" - ");
      end case;

      case Term.Op is
         when Op_Quantity =>
            Disp_Quantity_Name (Term.Quantity);
         when Op_Vhdl_Expr =>
            Vhdl.Prints.Disp_Expression (Term.Vhdl_Expr);
      end case;
   end Disp_Term;

   procedure Disp_Characteristic_Expression
     (Ce : Characteristic_Expressions_Index)
   is
      Obj : Characteristic_Expr renames
        Characteristic_Expressions.Table (Ce);
      Expr : Ams_Term_Acc := Obj.Expr;
   begin
      case Obj.Kind is
         when Explicit =>
            Put ("Explic:");
         when Contribution =>
            Put ("Contri:");
         when Structural =>
            Put ("Struct:");
      end case;

      while Expr /= null loop
         Disp_Term (Expr);
         Expr := Expr.Next;
      end loop;
      New_Line;
   end Disp_Characteristic_Expression;

   procedure Disp_Characteristic_Expressions is
   begin
      Put_Line ("Characteristic expressions:");
      for I in Characteristic_Expressions.First
        .. Characteristic_Expressions.Last
      loop
         Disp_Characteristic_Expression (I);
      end loop;
   end Disp_Characteristic_Expressions;
end Simul.Debugger.AMS;
