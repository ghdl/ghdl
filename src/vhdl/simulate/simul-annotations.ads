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
with Simul.Environments; use Simul.Environments;
with Types; use Types;

package Simul.Annotations is
   --  Decorate the tree in order to be usable with the internal simulator.
   procedure Annotate (Unit : Iir_Design_Unit);

   --  Disp annotations for an iir node.
   procedure Disp_Vhdl_Info (Node : Iir);
   procedure Disp_Tree_Info (Node : Iir);

   --  For Kind_Extra: a number.  Kind_Extra is not used by annotations, and
   --  is free for another pass like preelab.
   type Extra_Slot_Type is new Natural;

   Nbr_Packages : Pkg_Index_Type := 0;

   -- Annotations are used to collect informations for elaboration and to
   -- locate iir_value_literal for signals, variables or constants.

   -- The annotation depends on the kind of the node.
   type Sim_Info_Kind is
     (Kind_Block, Kind_Process, Kind_Frame,
      Kind_Scalar_Type, Kind_File_Type,
      Kind_Object, Kind_Signal,
      Kind_File,
      Kind_Terminal, Kind_Quantity,
      Kind_Environment,
      Kind_PSL,
      Kind_Extra);

   type Sim_Info_Type (Kind : Sim_Info_Kind);
   type Sim_Info_Acc is access all Sim_Info_Type;

   type Instance_Slot_Type is new Integer;
   Invalid_Instance_Slot : constant Instance_Slot_Type := -1;

   -- Annotation for an iir node in order to be able to simulate it.
   type Sim_Info_Type (Kind: Sim_Info_Kind) is record
      case Kind is
         when Kind_Block
           | Kind_Frame
           | Kind_Process
           | Kind_Environment =>
            --  Scope level for this frame.
            Frame_Scope : Scope_Type;

            --  Number of objects/signals.
            Nbr_Objects : Object_Slot_Type;

            case Kind is
               when Kind_Block =>
                  --  Slot number in the parent (for blocks).
                  Inst_Slot : Instance_Slot_Type;

                  --  Number of children (blocks, generate, instantiation).
                  Nbr_Instances : Instance_Slot_Type;

               when Kind_Environment =>
                  Env_Slot : Object_Slot_Type;

               when others =>
                  null;
            end case;

         when Kind_Object
           | Kind_Signal
           | Kind_File
           | Kind_Terminal
           | Kind_Quantity
           | Kind_PSL =>
            --  Block in which this object is declared in.
            Obj_Scope : Scope_Type;

            --  Variable index in the block.
            Slot: Object_Slot_Type;

         when Kind_Scalar_Type =>
            Scalar_Mode : Iir_Value_Kind;

         when Kind_File_Type =>
            File_Signature : String_Acc;

         when Kind_Extra =>
            Extra_Slot : Extra_Slot_Type;
      end case;
   end record;

   -- Get/Set annotation fied from/to an iir.
   procedure Set_Info (Target : Iir; Info : Sim_Info_Acc);
   pragma Inline (Set_Info);
   function Get_Info (Target : Iir) return Sim_Info_Acc;
   pragma Inline (Get_Info);

   --  Expand the annotation table.  This is automatically done by Annotate,
   --  to be used only by debugger.
   procedure Annotate_Expand_Table;

   --  For debugging.
   function Image (Scope : Scope_Type) return String;
end Simul.Annotations;
