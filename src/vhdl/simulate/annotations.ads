--  Annotations for interpreted simulation
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

with Iirs; use Iirs;
with Iir_Values; use Iir_Values;
with Types; use Types;

package Annotations is
   Trace_Annotation : Boolean := False;

   -- Decorate the tree in order to be usable with the internal simulator.
   procedure Annotate (Tree: Iir_Design_Unit);

   -- Disp annotations for an iir node.
   procedure Disp_Vhdl_Info (Node: Iir);
   procedure Disp_Tree_Info (Node: Iir);

   type Object_Slot_Type is new Natural;
   subtype Parameter_Slot_Type is Object_Slot_Type range 0 .. 2**15;

   type Pkg_Index_Type is new Natural;
   Nbr_Packages : Pkg_Index_Type := 0;

   -- Annotations are used to collect informations for elaboration and to
   -- locate iir_value_literal for signals, variables or constants.

   -- Scope corresponding to an object.
   -- Scope_level_global is for objects that can be instancied only one
   -- time, ie shared signals or constants declared in a package.
   --
   -- Scope_Level_Process is for objects declared in an entity, architecture,
   -- process, bloc (but not generated bloc).  These are static objects, that
   -- can be instancied several times.
   --
   -- Scope_Level_First_Function and above are for dynamic objects declared
   -- in a subprogram.  The level is also the nest level.
   --
   --  Scope_Level_Component is set to a maximum, since there is at
   --  most one scope after it (the next one is an entity).
   type Scope_Level_Kind is
     (
      --  For a package, the depth is
      Scope_Kind_Package,
      Scope_Kind_Component,
      Scope_Kind_Frame,
      Scope_Kind_Pkg_Inst,
      Scope_Kind_None
     );
   type Scope_Depth_Type is range 0 .. 2**15;
   type Scope_Level_Type (Kind : Scope_Level_Kind := Scope_Kind_None) is
      record
         case Kind is
            when Scope_Kind_Package =>
               Pkg_Index : Pkg_Index_Type;
            when Scope_Kind_Component =>
               null;
            when Scope_Kind_Frame =>
               Depth : Scope_Depth_Type;
            when Scope_Kind_Pkg_Inst =>
               Pkg_Inst : Parameter_Slot_Type;
            when Scope_Kind_None =>
               null;
         end case;
      end record;

   type Instance_Slot_Type is new Integer;
   Invalid_Instance_Slot : constant Instance_Slot_Type := -1;

   -- The annotation depends on the kind of the node.
   type Sim_Info_Kind is
     (Kind_Block, Kind_Process, Kind_Frame,
      Kind_Scalar_Type, Kind_File_Type,
      Kind_Object, Kind_Signal, Kind_Range,
      Kind_File,
      Kind_Terminal, Kind_Quantity);

   type Sim_Info_Type (Kind: Sim_Info_Kind);
   type Sim_Info_Acc is access all Sim_Info_Type;

   -- Annotation for an iir node in order to be able to simulate it.
   type Sim_Info_Type (Kind: Sim_Info_Kind) is record
      case Kind is
         when Kind_Block
           | Kind_Frame
           | Kind_Process =>
            --  Slot number.
            Inst_Slot : Instance_Slot_Type;

            -- scope level for this frame.
            Frame_Scope_Level: Scope_Level_Type;

            -- Number of objects/signals.
            Nbr_Objects : Object_Slot_Type;

            --  Number of children (blocks, generate, instantiation).
            Nbr_Instances : Instance_Slot_Type;

         when Kind_Object
           | Kind_Signal
           | Kind_Range
           | Kind_File
           | Kind_Terminal
           | Kind_Quantity =>
            -- block considered (hierarchy).
            Scope_Level: Scope_Level_Type;

            -- Variable index.
            Slot: Object_Slot_Type;

         when Kind_Scalar_Type =>
            Scalar_Mode : Iir_Value_Kind;

         when Kind_File_Type =>
            File_Signature : String_Acc;
      end case;
   end record;

   -- Get/Set annotation fied from/to an iir.
   procedure Set_Info (Target: Iir; Info: Sim_Info_Acc);
   pragma Inline (Set_Info);
   function Get_Info (Target: Iir) return Sim_Info_Acc;
   pragma Inline (Get_Info);

   --  Expand the annotation table.  This is automatically done by Annotate,
   --  to be used only by debugger.
   procedure Annotate_Expand_Table;

   --  For debugging.
   function Image (Scope : Scope_Level_Type) return String;
end Annotations;
