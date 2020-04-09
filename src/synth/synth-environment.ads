--  Environment definition for synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Types; use Types;
with Tables;

with Netlists; use Netlists;
with Netlists.Builders;

with Synth.Source;
with Synth.Objtypes; use Synth.Objtypes;

package Synth.Environment is
   --  This package declares the type Wire_Id and its methods.
   --
   --  A wire_id represents an HDL signal or variable and keeps the current
   --  value of it accross control statements.
   --  This is not a memory storage, because:
   --  * the current value may not be static.
   --    e.g.:  a <= b + 1;  --  If B is a port, the value of A is defined but
   --                        --  not known
   --  * the current value depends on the control statements.
   --    e.g.:  a := data;  -- a0
   --           if cond then
   --              a := a + 4;  --  Reads a0, but writes to a1
   --              b := a + 1;  --  Reads a1, writes b1
   --           else
   --              b <= a + 2;  --  Reads a0, writes b2
   --           end if;
   --           c <= b * 2;     --  b = phi(cond, b1, b2)
   --
   --  This is very similar to SSA (static single assignments)

   type Wire_Id is private;
   No_Wire_Id : constant Wire_Id;

   --  Wire_Id can be ordered, so that merges can be efficient.
   function Is_Lt (L, R : Wire_Id) return Boolean;

   --  A Wire is either a signal, a variable or a port.  We need to know the
   --  nature of a wire as the assignment semantic is not the same (a variable
   --  assignment overwrite the old value, while a signal assignment is
   --  effective at the next cycle).
   type Wire_Kind is
     (
      Wire_None,
      Wire_Signal, Wire_Variable,
      Wire_Input, Wire_Output, Wire_Inout
     );

   --  Create a wire.
   function Alloc_Wire (Kind : Wire_Kind; Obj : Source.Syn_Src) return Wire_Id;

   --  Mark the wire as free.
   procedure Free_Wire (Wid : Wire_Id);

   --  Read and write the mark flag.
   function Get_Wire_Mark (Wid : Wire_Id) return Boolean;
   procedure Set_Wire_Mark (Wid : Wire_Id; Mark : Boolean := True);

   --  Simple mark & release.  This is a very simple mechanism (will free
   --  all wires allocated after the mark), but efficient and working well
   --  for the stack based allocation.
   --  Not related to the mark flag.
   procedure Mark (M : out Wire_Id);
   procedure Release (M : in out Wire_Id);

   --  Check that all the wires have been released.
   procedure All_Released;

   --  Remove wires WID1 and WID2 from current phi.
   --  Used for internal wires (exit/quit) when exiting their scope.
   procedure Phi_Discard_Wires (Wid1 : Wire_Id; Wid2 : Wire_Id);

   --  For signals, only the future value can be assigned.  But the current
   --  value can be read.  A gate is needed to represent the current value
   --  (as only a gate can provide a net).  In most cases, this is a virtual
   --  gate whose output is equal to the input and this virtual gate would be
   --  later removed during cleanup.
   --
   --  Set the gate for a wire.
   procedure Set_Wire_Gate (Wid : Wire_Id; Gate : Net);
   function Get_Wire_Gate (Wid : Wire_Id) return Net;

   --  The current value of WID.  For variables, this is the last assigned
   --  value.  For signals, this is the gate.
   --  A builder is needed in case of concatenation.
   function Get_Current_Value (Ctxt : Builders.Context_Acc; Wid : Wire_Id)
                              return Net;

   --  Get the currently assigned value of WID at OFF/WD.
   --  Used when assigning as a memory.
   function Get_Current_Assign_Value
     (Ctxt : Builders.Context_Acc; Wid : Wire_Id; Off : Uns32; Wd : Width)
     return Net;

   --  In the current phi context, assign VAL to DEST.
   procedure Phi_Assign_Net
     (Ctxt : Builders.Context_Acc; Dest : Wire_Id; Val : Net; Offset : Uns32);

   --  Assign a static value to DEST.  VAL is copied.
   procedure Phi_Assign_Static (Dest : Wire_Id; Val : Memtyp);

   --  A Phi represent a split in the control flow (two or more branches).
   type Phi_Type is private;

   --  Create a new phi context.
   procedure Push_Phi;

   procedure Pop_Phi (Phi : out Phi_Type);

   --  Destroy the current phi context and merge it.  Can apply only for the
   --  first non-top level phi context.
   procedure Pop_And_Merge_Phi (Ctxt : Builders.Context_Acc;
                                Stmt : Source.Syn_Src);

   --  All assignments in PHI to wires below MARK are propagated to the
   --  current phi.  Used to propagate assignments to wires defined out of
   --  a subprogram when leaving a subprogram.
   procedure Propagate_Phi_Until_Mark (Ctxt : Builders.Context_Acc;
                                       Phi : Phi_Type;
                                       Mark : Wire_Id);

   --  Handle if statement.  According to SEL, the value of the wires are
   --  those from T or from F.
   procedure Merge_Phis (Ctxt : Builders.Context_Acc;
                         Sel : Net;
                         T, F : Phi_Type;
                         Stmt : Source.Syn_Src);

   --  Lower level part.
   --  Currently public to handle case statements.

   --  Within a Phi, assignments are represented as a linked list of
   --  sequential assignments.
   type Seq_Assign is private;
   No_Seq_Assign : constant Seq_Assign;

   --  Sort all seq assign of P by wire id.  Used to more easily merge them.
   function Sort_Phi (P : Phi_Type) return Seq_Assign;

   --  A sequential assignment represent an assignment to a wire.
   function Get_Wire_Id (W : Seq_Assign) return Wire_Id;
   function Get_Assign_Chain (Asgn : Seq_Assign) return Seq_Assign;
   function Get_Assign_Value (Ctxt : Builders.Context_Acc; Asgn : Seq_Assign)
                             return Net;

   --  For low-level phi merge.
   --  A sequential assignment is a linked list of partial assignment.
   type Partial_Assign is private;
   No_Partial_Assign : constant Partial_Assign;

   function Get_Assign_Partial (Asgn : Seq_Assign) return Partial_Assign;

   --  Force the value of a Seq_Assign to be a net if needed, return it.
   function Get_Assign_Partial_Force (Asgn : Seq_Assign) return Partial_Assign;

   function New_Partial_Assign (Val : Net; Offset : Uns32)
                               return Partial_Assign;

   type Partial_Assign_Array is array (Int32 range <>) of Partial_Assign;

   type Partial_Assign_List is limited private;

   procedure Partial_Assign_Init (List : out Partial_Assign_List);
   procedure Partial_Assign_Append (List : in out Partial_Assign_List;
                                    Pasgn : Partial_Assign);
   procedure Merge_Partial_Assigns (Ctxt : Builders.Context_Acc;
                                    Wid : Wire_Id;
                                    List : in out Partial_Assign_List);

   --  P is an array of Partial_Assign.  Each element is a list
   --  of partial assign from a different basic block.
   --  Extract the value to nets N of the maximal partial assignment starting
   --  at offset OFF for all partial assignments.  Fully handled partial
   --  assignments are poped.  Set the offset and width to OFF and WD of the
   --  result.
   procedure Extract_Merge_Partial_Assigns (Ctxt : Builders.Context_Acc;
                                            P : in out Partial_Assign_Array;
                                            N : out Net_Array;
                                            Off : in out Uns32;
                                            Wd : out Width);

   --  Concurrent assignments.

   type Conc_Assign is private;
   No_Conc_Assign : constant Conc_Assign;

   procedure Add_Conc_Assign
     (Wid : Wire_Id; Val : Net; Off : Uns32; Stmt : Source.Syn_Src);

   procedure Finalize_Assignments (Ctxt : Builders.Context_Acc);

   --  A static wire is a wire_signal which has one whole (same width as the
   --  wire) assignment and whose assignment value is a const net.
   --  That's rather restrictive but still efficient.
   function Is_Static_Wire (Wid : Wire_Id) return Boolean;

   --  Return the corresponding net for a static wire.
   function Get_Static_Wire (Wid : Wire_Id) return Memtyp;
private
   type Wire_Id is new Uns32;
   No_Wire_Id : constant Wire_Id := 0;

   function Is_Lt (L, R : Wire_Id) return Boolean renames "<";

   type Seq_Assign is new Uns32;
   No_Seq_Assign : constant Seq_Assign := 0;

   type Partial_Assign is new Uns32;
   No_Partial_Assign : constant Partial_Assign := 0;

   type Partial_Assign_List is record
      First, Last : Partial_Assign;
   end record;

   type Conc_Assign is new Uns32;
   No_Conc_Assign : constant Conc_Assign := 0;

   type Phi_Id is new Uns32;
   No_Phi_Id : constant Phi_Id := 0;

   --  Get current phi context.
   function Current_Phi return Phi_Id;
   pragma Inline (Current_Phi);

   type Wire_Id_Record is record
      --  Kind of wire: signal, variable...
      --  Set at initialization and cannot be changed.
      --  Used to know what is the current value of the wire (could be either
      --  Gate when it is a signal or Cur_Assign when it is a variable).
      Kind : Wire_Kind;

      --  Used in various algorithms: a flag on a wire.  This flag must be
      --  cleared after usage.
      Mark_Flag : Boolean;

      --  Source node that created the wire.
      Decl : Source.Syn_Src;

      --  The initial net for the wire.
      --  This is a pseudo gate that is needed because the value of the wire
      --  can be read before anything was assigned to it.
      Gate : Net;

      --  Current assignment (if there is one).
      --  This is needed so that the current value (for variable) can be read.
      Cur_Assign : Seq_Assign;

      --  Chain of concurrent assigns for this wire.
      --  This is used to detect multiple collision and to handle partial
      --  assignments.
      Final_Assign : Conc_Assign;
      Nbr_Final_Assign : Natural;
   end record;

   type Seq_Assign_Value (Is_Static : Boolean := True) is record
      case Is_Static is
         when True =>
            Val : Memtyp;
         when False =>
            --  Values assigned.
            Asgns : Partial_Assign;
      end case;
   end record;

   type Seq_Assign_Record is record
      --  Target of the assignment.
      Id : Wire_Id;

      --  Assignment in the previous phi context.
      --  Used to restore Cur_Assign of the wire when the phi context is poped.
      Prev : Seq_Assign;

      --  Corresponding phi context for this wire.
      Phi : Phi_Id;

      --  Next wire in the phi context.
      Chain : Seq_Assign;

      --  Current value.
      Val : Seq_Assign_Value;
   end record;

   type Partial_Assign_Record is record
      Next : Partial_Assign;

      --  Assignment at OFFSET.  The width is set by the width of the value.
      Value : Net;
      Offset : Uns32;
   end record;

   type Conc_Assign_Record is record
      Next : Conc_Assign;

      --  Concurrent assignment at OFFSET.  The width is set by value width.
      Value : Net;
      Offset : Uns32;

      --  Source of the assignment.  Useful to report errors.
      Stmt : Source.Syn_Src;
   end record;

   type Phi_Type is record
      --  Chain of sequential assignments in the current phi context (BB).
      First : Seq_Assign;
      Last : Seq_Assign;
      --  Number of assignments.
      Nbr : Uns32;
   end record;

   package Phis_Table is new Tables
     (Table_Component_Type => Phi_Type,
      Table_Index_Type => Phi_Id,
      Table_Low_Bound => No_Phi_Id,
      Table_Initial => 16);

   package Wire_Id_Table is new Tables
     (Table_Component_Type => Wire_Id_Record,
      Table_Index_Type => Wire_Id,
      Table_Low_Bound => No_Wire_Id,
      Table_Initial => 1024);

   package Assign_Table is new Tables
     (Table_Component_Type => Seq_Assign_Record,
      Table_Index_Type => Seq_Assign,
      Table_Low_Bound => No_Seq_Assign,
      Table_Initial => 1024);

   package Partial_Assign_Table is new Tables
     (Table_Component_Type => Partial_Assign_Record,
      Table_Index_Type => Partial_Assign,
      Table_Low_Bound => No_Partial_Assign,
      Table_Initial => 1024);

   package Conc_Assign_Table is new Tables
     (Table_Component_Type => Conc_Assign_Record,
      Table_Index_Type => Conc_Assign,
      Table_Low_Bound => No_Conc_Assign,
      Table_Initial => 1024);
end Synth.Environment;
