--  Elaboration for interpretation
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
with Types; use Types;
with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Annotations; use Vhdl.Annotations;
with Simul.Environments; use Simul.Environments;
with Grt.Types; use Grt.Types;

--  This package elaborates design hierarchy.

package Simul.Elaboration is
   Trace_Elaboration : Boolean := False;
   Trace_Drivers : Boolean := False;

   --  Number of block instances and also Id of the last one.
   Nbr_Block_Instances : Block_Instance_Id := 0;

   -- A block instance with its architecture/entity declaration is an
   -- instancied entity.

   procedure Elaborate_Design (Design: Iir_Design_Unit);

   procedure Elaborate_Declarative_Part
     (Instance : Block_Instance_Acc; Decl_Chain : Iir);

   --  Reverse operation of Elaborate_Declarative_Part.
   --  At least, finalize files.
   procedure Finalize_Declarative_Part
     (Instance : Block_Instance_Acc; Decl_Chain : Iir);

   procedure Elaborate_Declaration (Instance : Block_Instance_Acc; Decl : Iir);

   procedure Destroy_Iterator_Declaration
     (Instance : Block_Instance_Acc; Decl : Iir);

   --  How are created scalar values for Create_Value_For_Type.
   type Init_Value_Kind is
     (--  Use the default value for the type (lefmost value).
      Init_Value_Default,

      --  Undefined.  The caller doesn't care as it will overwrite the value.
      Init_Value_Any,

      --  Create signal placeholder.  Only for individual associations.
      Init_Value_Signal);

   --  Create a value for type DECL.
   function Create_Value_For_Type
     (Block: Block_Instance_Acc; Decl: Iir; Init : Init_Value_Kind)
     return Iir_Value_Literal_Acc;

   --  LRM93 12.3.1.3  Subtype Declarations
   --  The elaboration of a subtype indication creates a subtype.
   --  Used for allocator.
   procedure Elaborate_Subtype_Indication
     (Instance : Block_Instance_Acc; Ind : Iir);

   --  Create object DECL.
   --  This does nothing except marking DECL as elaborated.
   --  Used by simulation to dynamically create subprograms interfaces.
   procedure Create_Object (Instance : Block_Instance_Acc; Decl : Iir);
   procedure Create_Signal (Instance : Block_Instance_Acc; Decl : Iir);

   Global_Instances : Block_Instance_Acc;
   Top_Instance: Block_Instance_Acc;

   type Block_Instance_Acc_Array is array (Instance_Slot_Type range <>) of
     Block_Instance_Acc;

   --  Disconnections.  For each disconnection specification, the elaborator
   --  adds an entry in the table.
   type Disconnection_Entry is record
      Sig : Iir_Value_Literal_Acc;
      Time : Iir_Value_Time;
   end record;

   package Disconnection_Table is new Tables
     (Table_Component_Type => Disconnection_Entry,
      Table_Index_Type => Integer,
      Table_Low_Bound => 0,
      Table_Initial => 16);

   --  Connections.  For each associations (block/component/entry), the
   --  elaborator adds an entry in that table.
   type Connect_Entry is record
      Formal : Iir_Value_Literal_Acc;
      Formal_Instance : Block_Instance_Acc;
      Actual : Iir_Value_Literal_Acc;
      Actual_Instance : Block_Instance_Acc;
      Inter : Iir;
      Assoc : Iir;
   end record;

   package Connect_Table is new Tables
     (Table_Component_Type => Connect_Entry,
      Table_Index_Type => Integer,
      Table_Low_Bound => 0,
      Table_Initial => 32);

   --  Signals.

   type Signal_Entry (Kind : Mode_Signal_Type := Mode_Signal) is record
      Decl : Iir;
      Sig : Iir_Value_Literal_Acc;
      Val : Iir_Value_Literal_Acc;
      Instance : Block_Instance_Acc;
      case Kind is
         when Mode_Signal_User =>
            null;
         when Mode_Quiet | Mode_Stable | Mode_Delayed
           | Mode_Transaction =>
            Time : Std_Time;
            Prefix : Iir_Value_Literal_Acc;
         when Mode_Guard =>
            null;
         when Mode_Conv_In | Mode_Conv_Out | Mode_End =>
            --  Unused.
            null;
      end case;
   end record;

   package Signals_Table is new Tables
     (Table_Component_Type => Signal_Entry,
      Table_Index_Type => Integer,
      Table_Low_Bound => 0,
      Table_Initial => 128);

   type Process_Index_Type is new Natural;

   package Processes_Table is new Tables
     (Table_Component_Type => Block_Instance_Acc,
      Table_Index_Type => Process_Index_Type,
      Table_Low_Bound => 1,
      Table_Initial => 128);

   package Protected_Table is new Tables
     (Table_Component_Type => Block_Instance_Acc,
      Table_Index_Type => Protected_Index_Type,
      Table_Low_Bound => 1,
      Table_Initial => 2);

   type Boolean_Vector is array (Nat32 range <>) of Boolean;
   type Boolean_Vector_Acc is access Boolean_Vector;

   type PSL_Entry is record
      Instance : Block_Instance_Acc;
      Stmt : Iir;
      States : Boolean_Vector_Acc;
      Done : Boolean;
   end record;

   type PSL_Index_Type is new Natural;

   package PSL_Table is new Tables
     (Table_Component_Type => PSL_Entry,
      Table_Index_Type => PSL_Index_Type,
      Table_Low_Bound => 1,
      Table_Initial => 2);
end Simul.Elaboration;
