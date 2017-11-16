--  Elaboration for interpretation
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

with Ada.Unchecked_Deallocation;
with Tables;
with Types; use Types;
with Iirs; use Iirs;
with Iir_Values; use Iir_Values;
with Grt.Types; use Grt.Types;
with Annotations; use Annotations;
with Areapools;

--  This package elaborates design hierarchy.

package Elaboration is
   Trace_Elaboration : Boolean := False;
   Trace_Drivers : Boolean := False;

   -- A block instance with its architecture/entity declaration is an
   -- instancied entity.
   type Block_Instance_Type;
   type Block_Instance_Acc is access Block_Instance_Type;

   type Objects_Array is array (Object_Slot_Type range <>) of
     Iir_Value_Literal_Acc;

   type Block_Instance_Id is new Natural;
   No_Block_Instance_Id : constant Block_Instance_Id := 0;

   --  Number of block instances and also Id of the last one.
   Nbr_Block_Instances : Block_Instance_Id := 0;

   -- A block instance with its architecture/entity declaration is an
   -- instancied entity.

   type Block_Instance_Type (Max_Objs : Object_Slot_Type) is record
      --  Flag for wait statement: true if not yet executed.
      In_Wait_Flag : Boolean;

      --  Uniq number for a block instance.
      Id : Block_Instance_Id;

      -- Useful informations for a dynamic block (ie, a frame).
      -- The scope level and an access to the block of upper scope level.
      Block_Scope : Scope_Type;
      Up_Block : Block_Instance_Acc;

      --  Block, architecture, package, process, component instantiation for
      --  this instance.
      Label : Iir;

      --  For blocks: corresponding block (different from label for direct
      --  component instantiation statement and generate iterator).
      --  For packages: Null_Iir
      --  For subprograms and processes: statement being executed.
      Stmt : Iir;

      --  Instanciation tree.

      --  Parent is always set (but null for top-level block and packages)
      Parent: Block_Instance_Acc;

      --  Chain of children.  They are in declaration order after elaboration.
      --  (in reverse order during elaboration).
      --  Not null only for blocks and processes.
      Children: Block_Instance_Acc;
      Brother: Block_Instance_Acc;

      --  Port association map for this block, if any.
      Ports_Map : Iir;

      --  Pool marker for the child (only for subprograms and processes).
      Marker : Areapools.Mark_Type;

      --  Reference to the actuals, for copy-out when returning from a
      --  procedure.
      Actuals_Ref : Value_Array_Acc;

      -- Only for function frame; contains the result.
      Result: Iir_Value_Literal_Acc;

      --  Last object elaborated (or number of objects elaborated).
      --  Note: this is generally the slot index of the next object to be
      --  elaborated (this may be wrong for dynamic objects due to execution
      --  branches).
      Elab_Objects : Object_Slot_Type := 0;

      --  Values of the objects in that frame.
      Objects : Objects_Array (1 .. Max_Objs);
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Block_Instance_Type, Name => Block_Instance_Acc);

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

   --  LRM93 §12.3.1.3  Subtype Declarations
   --  The elaboration of a subtype indication creates a subtype.
   --  Used for allocator.
   procedure Elaborate_Subtype_Indication
     (Instance : Block_Instance_Acc; Ind : Iir);

   --  Create object DECL.
   --  This does nothing except marking DECL as elaborated.
   --  Used by simulation to dynamically create subprograms interfaces.
   procedure Create_Object (Instance : Block_Instance_Acc; Decl : Iir);
   procedure Create_Signal (Instance : Block_Instance_Acc; Decl : Iir);

   Top_Instance: Block_Instance_Acc;

   type Block_Instance_Acc_Array is array (Instance_Slot_Type range <>) of
     Block_Instance_Acc;

   type Package_Instances_Array is array (Pkg_Index_Type range <>) of
     Block_Instance_Acc;
   type Package_Instances_Array_Acc is access Package_Instances_Array;

   Package_Instances : Package_Instances_Array_Acc;

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

   package Environment_Table is new Tables
     (Table_Component_Type => Block_Instance_Acc,
      Table_Index_Type => Environment_Index_Type,
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
end Elaboration;
