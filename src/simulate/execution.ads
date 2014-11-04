--  Interpreted simulation
--  Copyright (C) 2014 Tristan Gingold
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
with Iirs; use Iirs;
with Iir_Values; use Iir_Values;
with Elaboration; use Elaboration;
with Areapools; use Areapools;

package Execution is
   Trace_Statements : Boolean := False;

   -- If true, disp current time in assert message.
   Disp_Time_Before_Values: Boolean := False;

   Current_Component : Block_Instance_Acc := null;

   -- State associed with each process.
   type Process_State_Type is record
      --  The process instance.
      Top_Instance: Block_Instance_Acc := null;
      Proc: Iir := Null_Iir;

      --  Memory pool to allocate objects from.
      Pool : aliased Areapool;

      -- The stack of the process.
      Instance : Block_Instance_Acc := null;
   end record;
   type Process_State_Acc is access all Process_State_Type;

   Simulation_Finished : exception;

   --  Current process being executed.  This is only for the debugger.
   Current_Process : Process_State_Acc;

   --  Pseudo process used for resolution functions, ...
   No_Process : Process_State_Acc := new Process_State_Type;
   -- Execute a list of sequential statements.
   -- Return when there is no more statements to execute.
   procedure Execute_Sequential_Statements (Proc : Process_State_Acc);

   --  Evaluate an expression.
   function Execute_Expression (Block: Block_Instance_Acc; Expr: Iir)
                               return Iir_Value_Literal_Acc;

   --  Evaluate boolean condition COND.  If COND is Null_Iir, returns true.
   function Execute_Condition (Instance : Block_Instance_Acc;
                               Cond : Iir) return Boolean;

   --  Execute a name.  Return the value if Ref is False, or the reference
   --  (for a signal, a quantity or a terminal) if Ref is True.
   function Execute_Name (Block: Block_Instance_Acc;
                          Expr: Iir;
                          Ref : Boolean := False)
                         return Iir_Value_Literal_Acc;

   procedure Execute_Name_With_Base (Block: Block_Instance_Acc;
                                     Expr: Iir;
                                     Base : Iir_Value_Literal_Acc;
                                     Res : out Iir_Value_Literal_Acc;
                                     Is_Sig : out Boolean);

   --  Return the initial value (default value) of signal name EXPR.  To be
   --  used only during (non-dynamic) elaboration.
   function Execute_Signal_Init_Value (Block : Block_Instance_Acc; Expr : Iir)
                                      return Iir_Value_Literal_Acc;

   function Execute_Expression_With_Type
     (Block: Block_Instance_Acc;
      Expr: Iir;
      Expr_Type : Iir)
     return Iir_Value_Literal_Acc;

   function Execute_Resolution_Function
     (Block: Block_Instance_Acc; Imp : Iir; Arr : Iir_Value_Literal_Acc)
      return Iir_Value_Literal_Acc;

   function Execute_Assoc_Conversion
     (Block : Block_Instance_Acc; Conv : Iir; Val : Iir_Value_Literal_Acc)
     return Iir_Value_Literal_Acc;

   -- Sub function common for left/right/length/low/high attributes.
   -- Return bounds of PREFIX.
   function Execute_Bounds (Block: Block_Instance_Acc; Prefix: Iir)
                            return Iir_Value_Literal_Acc;

   -- Compute the offset for INDEX into a range BOUNDS.
   -- EXPR is only used in case of error.
   function Get_Index_Offset
     (Index: Iir_Value_Literal_Acc;
      Bounds: Iir_Value_Literal_Acc;
      Expr: Iir)
     return Iir_Index32;

   function Execute_Low_Limit (Bounds : Iir_Value_Literal_Acc)
                              return Iir_Value_Literal_Acc;

   function Get_Instance_For_Slot (Instance: Block_Instance_Acc; Decl: Iir)
                                   return Block_Instance_Acc;

   --  Store VALUE to TARGET.
   --  Note: VALUE is not freed.
   procedure Assign_Value_To_Object
     (Instance: Block_Instance_Acc;
      Target: Iir_Value_Literal_Acc;
      Target_Type: Iir;
      Value: Iir_Value_Literal_Acc;
      Stmt: Iir);

   -- Check VALUE follows the constraints of DEF.
   -- INSTANCE,DEF is the definition of a subtype.
   -- EXPR is just used in case of error to display the location
   -- If there is no location, EXPR can be null.
   -- Implicitly convert VALUE (array cases).
   -- Return in case of success.
   -- Raise errorout.execution_constraint_error in case of failure.
   procedure Check_Constraints
     (Instance: Block_Instance_Acc;
      Value: Iir_Value_Literal_Acc;
      Def: Iir; Expr: Iir);

   --  If VALUE is not an array, then this is a no-op.
   --  If VALUE is an array, then bounds are checked and converted.  INSTANCE
   --  is the instance corresponding to REF_TYPE.
   --  EXPR is used in case of error.
   procedure Implicit_Array_Conversion (Value : in out Iir_Value_Literal_Acc;
                                        Ref_Value : Iir_Value_Literal_Acc;
                                        Expr : Iir);
   procedure Implicit_Array_Conversion (Instance : Block_Instance_Acc;
                                        Value : in out Iir_Value_Literal_Acc;
                                        Ref_Type : Iir;
                                        Expr : Iir);

   --  Create an iir_value_literal of kind iir_value_array and of life LIFE.
   --  Allocate the array of bounds, and fill it from A_TYPE.
   --  Allocate the array of values.
   function Create_Array_Bounds_From_Type
     (Block : Block_Instance_Acc;
      A_Type : Iir;
      Create_Val_Array : Boolean)
     return Iir_Value_Literal_Acc;

   --  Create a range from LEN for scalar type ATYPE.
   function Create_Bounds_From_Length (Block : Block_Instance_Acc;
                                       Atype : Iir;
                                       Len : Iir_Index32)
                                      return Iir_Value_Literal_Acc;

   --  Return TRUE iff VAL is in the range defined by BOUNDS.
   function Is_In_Range (Val : Iir_Value_Literal_Acc;
                         Bounds : Iir_Value_Literal_Acc)
     return Boolean;

   --  Increment or decrement VAL according to BOUNDS.DIR.
   procedure Update_Loop_Index (Val : Iir_Value_Literal_Acc;
                                Bounds : Iir_Value_Literal_Acc);

   --  Create a block instance for subprogram IMP.
   function Create_Subprogram_Instance (Instance : Block_Instance_Acc;
                                        Imp : Iir)
                                       return Block_Instance_Acc;

   function Execute_Function_Body (Instance : Block_Instance_Acc; Func : Iir)
                                  return Iir_Value_Literal_Acc;

   function Execute_Image_Attribute (Val : Iir_Value_Literal_Acc;
                                     Expr_Type : Iir)
                                    return String;
end Execution;
