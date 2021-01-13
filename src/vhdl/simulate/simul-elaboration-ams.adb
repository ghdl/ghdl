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

with Vhdl.Errors; use Vhdl.Errors;
with Simul.Execution;

package body Simul.Elaboration.AMS is
   function Create_Characteristic_Expression
     (Kind : Characteristic_Expr_Kind) return Characteristic_Expressions_Index
   is
   begin
      case Kind is
         when Contribution =>
            Characteristic_Expressions.Append
              ((Kind => Contribution,
                Expr => null,
                Tolerance => 0,
                Dependencies => null));
         when others =>
            raise Program_Error;
      end case;
      return Characteristic_Expressions.Last;
   end Create_Characteristic_Expression;

   function Create_Scalar_Quantity (Kind : Quantity_Kind;
                                    Decl : Iir;
                                    Instance : Block_Instance_Acc)
                                   return Quantity_Index_Type
   is
   begin
      case Kind is
         when Quantity_Reference =>
            Scalar_Quantities.Append
              ((Kind => Quantity_Reference,
                Value => 0.0,
                Decl => Decl,
                Instance => Instance,
                Contribution =>
                  Create_Characteristic_Expression (Contribution)));
         when Quantity_Across =>
            Scalar_Quantities.Append
              ((Kind => Quantity_Across,
                Value => 0.0,
                Decl => Decl,
                Instance => Instance));
         when Quantity_Through =>
            Scalar_Quantities.Append
              ((Kind => Quantity_Through,
                Value => 0.0,
                Decl => Decl,
                Instance => Instance));
         when others =>
            raise Program_Error;
      end case;
      return Scalar_Quantities.Last;
   end Create_Scalar_Quantity;

   function Create_Scalar_Terminal (Decl : Iir;
                                    Instance : Block_Instance_Acc)
                                   return Terminal_Index_Type
   is
   begin
      --  Simply create the reference quantity for a terminal
      return Terminal_Index_Type
        (Create_Scalar_Quantity (Quantity_Reference, Decl, Instance));
   end Create_Scalar_Terminal;

   function Get_Terminal_Reference (Terminal : Terminal_Index_Type)
                                   return Quantity_Index_Type is
   begin
      return Quantity_Index_Type (Terminal);
   end Get_Terminal_Reference;

   procedure Add_Characteristic_Expression
     (Kind : Characteristic_Expr_Kind; Expr : Ams_Term_Acc)
   is
   begin
      Characteristic_Expressions.Append
        ((Kind => Kind,
          Expr => Expr,
          Tolerance => Default_Tolerance_Index,
          Dependencies => null));
   end Add_Characteristic_Expression;

   procedure Compute_Dependencies (Idx : Characteristic_Expressions_Index)
   is
      package Quantity_Table is new Tables
        (Table_Component_Type => Quantity_Index_Type,
         Table_Index_Type => Natural,
         Table_Low_Bound => 1,
         Table_Initial => 16);

      El : Characteristic_Expr renames Characteristic_Expressions.Table (Idx);
      Res : Quantity_Dependency_Acc := null;

      procedure Add_Dependency (Block : Block_Instance_Acc; N : Iir)
      is
         Q : Iir_Value_Literal_Acc;
      begin
         case Get_Kind (N) is
            when Iir_Kinds_Branch_Quantity_Declaration =>
               Q := Execution.Execute_Name (Block, N, True);
               Quantity_Table.Append (Q.Quantity);
            when Iir_Kind_Simple_Name =>
               Add_Dependency (Block, Get_Named_Entity (N));
            when Iir_Kinds_Dyadic_Operator =>
               Add_Dependency (Block, Get_Left (N));
               Add_Dependency (Block, Get_Right (N));
            when Iir_Kinds_Literal =>
               null;
            when others =>
               Error_Kind ("compute_dependencies", N);
         end case;
      end Add_Dependency;

      Term : Ams_Term_Acc := El.Expr;
   begin
      pragma Assert (El.Dependencies = null);

      while Term /= null loop
         case Term.Op is
            when Op_Quantity =>
               Quantity_Table.Append (Term.Quantity);
            when Op_Vhdl_Expr =>
               Add_Dependency (Term.Vhdl_Instance, Term.Vhdl_Expr);
         end case;
         Term := Term.Next;
      end loop;
      Res := new Quantity_Dependency_Type (Nbr => Quantity_Table.Last);
      for I in Quantity_Table.First .. Quantity_Table.Last loop
         Res.Quantities (I) := Quantity_Table.Table (I);
      end loop;
      Quantity_Table.Free;
      El.Dependencies := Res;
   end Compute_Dependencies;

   function Build (Op : Ams_Sign;
                   Val : Quantity_Index_Type;
                   Right : Ams_Term_Acc := null)
                  return Ams_Term_Acc
   is
   begin
      return new Ams_Term'(Op => Op_Quantity,
                           Sign => Op,
                           Next => Right,
                           Quantity => Val);
   end Build;

   function Build (Op : Ams_Sign;
                   Instance : Block_Instance_Acc;
                   Expr : Iir;
                   Right : Ams_Term_Acc := null)
                  return Ams_Term_Acc
   is
   begin
      return new Ams_Term'
        (Op => Op_Vhdl_Expr,
         Sign => Op,
         Vhdl_Expr => Expr,
         Vhdl_Instance => Instance,
         Next => Right);
   end Build;

   procedure Append_Characteristic_Expression
     (Terminal : Terminal_Index_Type; Expr : Ams_Term_Acc)
   is
      Ref : constant Quantity_Index_Type := Get_Terminal_Reference (Terminal);
      Ce : constant Characteristic_Expressions_Index :=
        Scalar_Quantities.Table (Ref).Contribution;
   begin
      pragma Assert (Expr.Next = null);
      Expr.Next := Characteristic_Expressions.Table (Ce).Expr;
      Characteristic_Expressions.Table (Ce).Expr := Expr;
   end Append_Characteristic_Expression;

   procedure Create_Tables is
   begin
      for I in Characteristic_Expressions.First
        .. Characteristic_Expressions.Last
      loop
         Compute_Dependencies (I);
      end loop;
   end Create_Tables;
end Simul.Elaboration.AMS;
