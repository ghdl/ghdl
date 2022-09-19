--  Annotations for interpreted simulation
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

package Elab.Vhdl_Annotations is
   type Object_Slot_Type is new Natural;

   --  This slot is not used.
   Invalid_Object_Slot : constant Object_Slot_Type := 0;

   type Block_Instance_Id is new Natural;
   No_Block_Instance_Id : constant Block_Instance_Id := 0;

   --  For Kind_Extra: a number.  Kind_Extra is not used by annotations, and
   --  is free for another pass like preelab.
   type Extra_Slot_Type is new Natural;

   -- The annotation depends on the kind of the node.
   type Sim_Info_Kind is
     (
      Kind_Block, Kind_Process, Kind_Frame, Kind_Protected, Kind_Package,
      Kind_Type,
      Kind_Object, Kind_Signal,
      Kind_File,
      Kind_Terminal, Kind_Quantity,
      Kind_PSL,
      Kind_Extra
     );

   type Instance_Slot_Type is new Integer;
   Invalid_Instance_Slot : constant Instance_Slot_Type := -1;

   type Sim_Info_Type (Kind : Sim_Info_Kind);
   type Sim_Info_Acc is access all Sim_Info_Type;

   -- Annotation for an iir node in order to be able to simulate it.
   type Sim_Info_Type (Kind: Sim_Info_Kind) is record
      --  Redundant, to be used only for debugging.
      Ref : Iir;

      case Kind is
         when Kind_Block
           | Kind_Frame
           | Kind_Protected
           | Kind_Process
           | Kind_Package =>
            --  Number of objects/signals.
            Nbr_Objects : Object_Slot_Type;

            case Kind is
               when Kind_Block =>
                  --  Slot number in the parent (for blocks).
                  Inst_Slot : Object_Slot_Type;

                  --  Number of children (blocks, generate, instantiation).
                  Nbr_Instances : Instance_Slot_Type;

               when Kind_Package =>
                  Pkg_Slot : Object_Slot_Type;
                  Pkg_Parent : Sim_Info_Acc;

               when others =>
                  null;
            end case;

         when Kind_Object
           | Kind_Signal
           | Kind_File
           | Kind_Terminal
           | Kind_Quantity
           | Kind_PSL
           | Kind_Type =>
            --  Block in which this object is declared in.
            Obj_Scope : Sim_Info_Acc;

            --  Variable index in the block.
            Slot: Object_Slot_Type;

         when Kind_Extra =>
            Extra_Slot : Extra_Slot_Type;
      end case;
   end record;

   --  The first initialization is done automatically, but must be done again
   --  after finalization.
   procedure Initialize_Annotate;
   procedure Finalize_Annotate;

   --  Decorate the tree in order to be usable with the internal simulator.
   procedure Annotate (Unit : Iir_Design_Unit);

   --  Disp annotations for an iir node.
   procedure Disp_Vhdl_Info (Node : Iir);
   procedure Disp_Tree_Info (Node : Iir);

   Global_Info : Sim_Info_Acc;

   -- Annotations are used to collect informations for elaboration and to
   -- locate iir_value_literal for signals, variables or constants.

   -- Get/Set annotation fied from/to an iir.
   procedure Set_Info (Target : Iir; Info : Sim_Info_Acc);
   pragma Inline (Set_Info);
   function Get_Info (Target : Iir) return Sim_Info_Acc;
   pragma Inline (Get_Info);

   --  Expand the annotation table.  This is automatically done by Annotate,
   --  to be used only by debugger.
   procedure Annotate_Expand_Table;
end Elab.Vhdl_Annotations;
