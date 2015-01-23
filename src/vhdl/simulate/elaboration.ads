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
with GNAT.Table;
with Iirs; use Iirs;
with Iir_Values; use Iir_Values;
with Grt.Types;
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

   -- A block instance with its architecture/entity declaration is an
   -- instancied entity.

   type Block_Instance_Type (Max_Objs : Object_Slot_Type) is record
      --  Flag for wait statement: true if not yet executed.
      In_Wait_Flag : Boolean;

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
      --  Not null only for blocks and processes.
      Children: Block_Instance_Acc;
      Brother: Block_Instance_Acc;

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

   --  Create a value for type DECL.  Initialize it if DEFAULT is true.
   function Create_Value_For_Type
     (Block: Block_Instance_Acc; Decl: Iir; Default : Boolean)
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
   type Block_Instance_Acc_Array_Acc is access Block_Instance_Acc_Array;

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

   package Disconnection_Table is new GNAT.Table
     (Table_Component_Type => Disconnection_Entry,
      Table_Index_Type => Integer,
      Table_Low_Bound => 0,
      Table_Initial => 16,
      Table_Increment => 100);

   --  Connections.  For each associations (block/component/entry), the
   --  elaborator adds an entry in that table.
   type Connect_Entry is record
      Formal : Iir_Value_Literal_Acc;
      Formal_Instance : Block_Instance_Acc;
      Actual : Iir_Value_Literal_Acc;
      Actual_Instance : Block_Instance_Acc;
      Assoc : Iir;
   end record;

   package Connect_Table is new GNAT.Table
     (Table_Component_Type => Connect_Entry,
      Table_Index_Type => Integer,
      Table_Low_Bound => 0,
      Table_Initial => 32,
      Table_Increment => 100);

   --  Signals.
   type Signal_Type_Kind is
     (User_Signal,
      Implicit_Quiet, Implicit_Stable, Implicit_Delayed,
      Implicit_Transaction,
      Guard_Signal);

   type Signal_Entry (Kind : Signal_Type_Kind := User_Signal) is record
      Decl : Iir;
      Sig : Iir_Value_Literal_Acc;
      Instance : Block_Instance_Acc;
      case Kind is
         when User_Signal =>
            Init : Iir_Value_Literal_Acc;
         when Implicit_Quiet | Implicit_Stable | Implicit_Delayed
           | Implicit_Transaction =>
            Time : Grt.Types.Ghdl_I64;
            Prefix : Iir_Value_Literal_Acc;
         when Guard_Signal =>
            null;
      end case;
   end record;

   package Signals_Table is new GNAT.Table
     (Table_Component_Type => Signal_Entry,
      Table_Index_Type => Integer,
      Table_Low_Bound => 0,
      Table_Initial => 128,
      Table_Increment => 100);

   type Process_Index_Type is new Natural;

   package Processes_Table is new GNAT.Table
     (Table_Component_Type => Block_Instance_Acc,
      Table_Index_Type => Process_Index_Type,
      Table_Low_Bound => 1,
      Table_Initial => 128,
      Table_Increment => 100);

   package Protected_Table is new GNAT.Table
     (Table_Component_Type => Block_Instance_Acc,
      Table_Index_Type => Protected_Index_Type,
      Table_Low_Bound => 1,
      Table_Initial => 2,
      Table_Increment => 100);
end Elaboration;
