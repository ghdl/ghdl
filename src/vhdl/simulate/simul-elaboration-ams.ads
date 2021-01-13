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

with Tables;

package Simul.Elaboration.AMS is
   --  AMS expressions
   --
   --  At many places during elaboration, the LRM defines characteristic
   --  expressions that aren't present in source code:
   --  * contribution expression (12.3.1.4)
   --  * characteristic expression for an across quantity declaration
   --    (12.3.1.4)
   --  * characteristic expression for simple simultaneous statement (the
   --    expression is in the source in that case) (15.1)
   --
   --  They are represented using a list of Ams_Expression elements.  The value
   --  is the sum of each element, using the + or - sign.

   type Ams_Sign is (Op_Plus, Op_Minus);
   --  Sign for the operand

   type Ams_Operand is (Op_Quantity, Op_Vhdl_Expr);
   --  The operand is one of:
   --  Op_Quantity: a quantity
   --  Op_Vhdl_Expr: an expression from the design. This expression may contain
   --   quantities

   type Ams_Term;
   type Ams_Term_Acc is access Ams_Term;
   --  A term of a characteristic expression

   type Characteristic_Expr_Kind is
     (Explicit,
      Contribution,
      Structural);

   type Tolerance_Index_Type is new Natural;
   Default_Tolerance_Index : constant Tolerance_Index_Type := 0;
   --  Tolerance

   type Characteristic_Expressions_Index is new Natural;

   type Quantity_Kind is
     (Quantity_Reference,
      --  The potential of a terminal. This is an across quantity between the
      --  terminal and the reference terminal of the nature.

      Quantity_Across,
      Quantity_Through,
      Quantity_Free
      --  Explicitly declared quantities
     );

   function Create_Scalar_Quantity (Kind : Quantity_Kind;
                                    Decl : Iir;
                                    Instance : Block_Instance_Acc)
                                   return Quantity_Index_Type;
   --  Create a new scalar quantity

   function Create_Scalar_Terminal (Decl : Iir;
                                    Instance : Block_Instance_Acc)
                                   return Terminal_Index_Type;
   --  Create a new scalar terminal

   function Get_Terminal_Reference (Terminal : Terminal_Index_Type)
                                   return Quantity_Index_Type;
   --  Get the reference quantity of a terminal

   procedure Add_Characteristic_Expression
     (Kind : Characteristic_Expr_Kind; Expr : Ams_Term_Acc);
   --  Add a new characteristic expression

   function Build (Op : Ams_Sign;
                   Val : Quantity_Index_Type;
                   Right : Ams_Term_Acc := null)
                  return Ams_Term_Acc;
   function Build (Op : Ams_Sign;
                   Instance : Block_Instance_Acc;
                   Expr : Iir;
                   Right : Ams_Term_Acc := null)
                  return Ams_Term_Acc;
   --  Build a term of a characteristic expression

   procedure Append_Characteristic_Expression
     (Terminal : Terminal_Index_Type; Expr : Ams_Term_Acc);
   --  Append an expression to the contribution of a terminal

   procedure Create_Tables;

   type Quantity_Index_Array is array (Positive range <>)
     of Quantity_Index_Type;

   type Quantity_Dependency_Type (Nbr : Natural);
   type Quantity_Dependency_Acc is access Quantity_Dependency_Type;

   type Quantity_Dependency_Type (Nbr : Natural) is record
      Quantities : Quantity_Index_Array (1 .. Nbr);
   end record;

   type Ams_Term (Op : Ams_Operand) is record
      Sign : Ams_Sign;
      Next : Ams_Term_Acc;

      case Op is
         when Op_Quantity =>
            Quantity : Quantity_Index_Type;
         when Op_Vhdl_Expr =>
            Vhdl_Expr : Iir;
            Vhdl_Instance : Block_Instance_Acc;
      end case;
   end record;

   type Characteristic_Expr is record
      Kind : Characteristic_Expr_Kind;
      Expr : Ams_Term_Acc;
      Tolerance : Tolerance_Index_Type;
      Dependencies : Quantity_Dependency_Acc;
   end record;

   package Characteristic_Expressions is new Tables
     (Table_Index_Type => Characteristic_Expressions_Index,
      Table_Component_Type => Characteristic_Expr,
      Table_Low_Bound => 1,
      Table_Initial => 128);

   type Scalar_Quantity (Kind : Quantity_Kind := Quantity_Reference) is record
      Value : Ghdl_F64;
      --  The value of the quantity

      Decl : Iir;
      Instance : Block_Instance_Acc;
      --  Declaration for the quantity

      case Kind is
         when Quantity_Reference =>
            Contribution : Characteristic_Expressions_Index;
         when others =>
            null;
      end case;
   end record;

   package Scalar_Quantities is new Tables
     (Table_Index_Type => Quantity_Index_Type,
      Table_Component_Type => Scalar_Quantity,
      Table_Low_Bound => 1,
      Table_Initial => 128);
end Simul.Elaboration.AMS;
