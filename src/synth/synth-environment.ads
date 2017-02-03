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

package Synth.Environment is
   --  A simple signal/variable is either a bit or a std_ulogic
   --  signal/variable, or a bus (bit_vector, std_ulogic_vector, signed,
   --  unsigned...).
   --
   --  Complex signals/variables (records, arrays) are decomposed to simple
   --  signals/variables.
   --
   --  Each simple signal/variable is represented by a Wire_Id.  Synthesis
   --  deals only with these wires or group of them.
   type Wire_Id is new Uns32;
   No_Wire_Id : constant Wire_Id := 0;

   --  A Wire is either a signal, a variable or a port.  We need to know the
   --  nature of a wire as the assignment semantic is not the same (a variable
   --  assignment overwrite the old value, while a signal assignment is
   --  effective at the next cycle).
   type Wire_Kind is (Wire_None,
                      Wire_Signal, Wire_Variable,
                      Wire_Input, Wire_Output, Wire_Inout);

   type Assign is new Uns32;
   No_Assign : constant Assign := 0;

   --  A Wire_Id represents a bit or a vector.
   type Wire_Id_Record is record
      --  Kind of wire: signal, variable...
      --  Set at initialization and cannot be changed.
      Kind : Wire_Kind;

      --  Used in various algorithms: a flag on a wire.  This flag must be
      --  cleared after usage.
      Mark_Flag : Boolean;

      --  Source node that created the wire.
      Decl : Source.Syn_Src;

      --  The initial net for the wire.
      Gate : Net;

      Cur_Assign : Assign;
   end record;

   --  The current value of WID.  For variables, this is the last assigned
   --  value.  For signals, this is the initial value.
   function Get_Current_Value (Wid : Wire_Id) return Net;

   --  The last assigned value to WID.
   function Get_Last_Assigned_Value (Wid : Wire_Id) return Net;

   --

   type Phi_Id is new Uns32;
   No_Phi_Id : constant Phi_Id := 0;

   type Assign_Record is record
      --  Target of the assignment.
      Id : Wire_Id;

      --  Assignment is the previous phi context.
      Prev : Assign;

      --  Corresponding phi context for this wire.
      Phi : Phi_Id;

      --  Next wire in the phi context.
      Chain : Assign;

      --  Value assigned.
      Value : Net;
   end record;

   function Get_Wire_Id (W : Assign) return Wire_Id;
   function Get_Assign_Chain (Asgn : Assign) return Assign;
   function Get_Assign_Value (Asgn : Assign) return Net;

   type Phi_Type is private;

   --  Create a new phi context.
   procedure Push_Phi;

   procedure Pop_Phi (Phi : out Phi_Type);

   --  Destroy the current phi context and merge it.  Can apply only for the
   --  first non-top level phi context.
   procedure Pop_And_Merge_Phi (Ctxt : Builders.Context_Acc);

   procedure Merge_Phis (Ctxt : Builders.Context_Acc;
                         Sel : Net;
                         T, F : Phi_Type);

   function Sort_Phi (P : Phi_Type) return Assign;

   --  Add a new wire in the phi context.
   procedure Phi_Insert_Assign (Asgn : Assign);

   --  In the current phi context, assign VAL to DEST.
   procedure Phi_Assign (Dest : Wire_Id; Val : Net);

   --  Get current phi context.
   function Current_Phi return Phi_Id;
   pragma Inline (Current_Phi);

   package Wire_Id_Table is new Tables
     (Table_Component_Type => Wire_Id_Record,
      Table_Index_Type => Wire_Id,
      Table_Low_Bound => No_Wire_Id,
      Table_Initial => 1024);

   package Assign_Table is new Tables
     (Table_Component_Type => Assign_Record,
      Table_Index_Type => Assign,
      Table_Low_Bound => No_Assign,
      Table_Initial => 1024);

private
   type Phi_Type is record
      First : Assign;
      Nbr : Uns32;
   end record;

   package Phis_Table is new Tables
     (Table_Component_Type => Phi_Type,
      Table_Index_Type => Phi_Id,
      Table_Low_Bound => No_Phi_Id,
      Table_Initial => 16);
end Synth.Environment;
