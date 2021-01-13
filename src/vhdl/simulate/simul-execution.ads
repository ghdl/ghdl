--  Interpreted simulation
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

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Annotations; use Vhdl.Annotations;
with Simul.Environments; use Simul.Environments;
with Simul.Elaboration; use Simul.Elaboration;
with Areapools; use Areapools;

package Simul.Execution is
   Trace_Statements : Boolean := False;

   -- If true, disp current time in assert message.
   Disp_Time_Before_Values: Boolean := False;

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

   type Process_State_Array is
      array (Process_Index_Type range <>) of aliased Process_State_Type;
   type Process_State_Array_Acc is access Process_State_Array;

   --  Array containing all processes.
   Processes_State: Process_State_Array_Acc;

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

   function Execute_Association_Expression
     (Actual_Instance : Block_Instance_Acc;
      Actual : Iir;
      Formal_Instance : Block_Instance_Acc)
     return Iir_Value_Literal_Acc;

   --  There are up to three slots per instance for signals:
   --  Signal_Sig: the signal (as handled by grt), with all its attribute
   --  Signal_Val: the value of the signal (as assigned by grt).
   --  Signal_Init: the initial value of drivers, only defined for ports.
   type Signal_Slot is (Signal_Sig, Signal_Val, Signal_Init);

   function Execute_Signal_Name
     (Block : Block_Instance_Acc; Expr : Iir; Kind : Signal_Slot)
     return Iir_Value_Literal_Acc;

   function Execute_Expression_With_Type
     (Block: Block_Instance_Acc;
      Expr: Iir;
      Expr_Type : Iir)
     return Iir_Value_Literal_Acc;

   procedure Execute_Failed_Assertion
     (Instance: Block_Instance_Acc;
      Label : String;
      Stmt : Iir;
      Default_Msg : String;
      Default_Severity : Natural);

   function Execute_Resolution_Function
     (Block: Block_Instance_Acc; Imp :  Iir; Arr : Iir_Value_Literal_Acc)
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

   --  Return True iff EXPR is covered by CHOICE.
   function Is_In_Choice (Instance : Block_Instance_Acc;
                          Choice : Iir;
                          Expr : Iir_Value_Literal_Acc)
                         return Boolean;

   function Get_Instance_By_Scope
     (Instance: Block_Instance_Acc; Scope: Sim_Info_Acc)
     return Block_Instance_Acc;

   --  Check that bounds of RNG belong to RNG_TYPE (unless this is a null
   --  range).
   procedure Check_Range_Constraints (Instance : Block_Instance_Acc;
                                      Rng : Iir_Value_Literal_Acc;
                                      Rng_Type : Iir;
                                      Loc : Iir);

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
                                        Prot_Obj : Block_Instance_Acc;
                                        Imp : Iir)
                                       return Block_Instance_Acc;

   function Execute_Image_Attribute (Val : Iir_Value_Literal_Acc;
                                     Expr_Type : Iir)
                                    return String;

   --  Like Get_Protected_Type_Body, but also works for instances, where
   --  instantiated nodes have no bodies.
   --  FIXME: maybe fix the issue directly in Sem_Inst ?
   function Get_Protected_Type_Body_Origin (Spec : Iir) return Iir;

end Simul.Execution;
