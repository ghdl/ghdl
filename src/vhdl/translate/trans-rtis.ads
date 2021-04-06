--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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

package Trans.Rtis is
   --  Run-Time Information (RTI) Kind.
   Ghdl_Rtik                             : O_Tnode;
   Ghdl_Rtik_Top                         : O_Cnode;
   Ghdl_Rtik_Library                     : O_Cnode;
   Ghdl_Rtik_Package                     : O_Cnode;
   Ghdl_Rtik_Package_Body                : O_Cnode;
   Ghdl_Rtik_Entity                      : O_Cnode;
   Ghdl_Rtik_Architecture                : O_Cnode;
   Ghdl_Rtik_Process                     : O_Cnode;
   Ghdl_Rtik_Block                       : O_Cnode;
   Ghdl_Rtik_If_Generate                 : O_Cnode;
   Ghdl_Rtik_Case_Generate               : O_Cnode;
   Ghdl_Rtik_For_Generate                : O_Cnode;
   Ghdl_Rtik_Generate_Body               : O_Cnode;
   Ghdl_Rtik_Instance                    : O_Cnode;
   Ghdl_Rtik_Constant                    : O_Cnode;
   Ghdl_Rtik_Iterator                    : O_Cnode;
   Ghdl_Rtik_Variable                    : O_Cnode;
   Ghdl_Rtik_Signal                      : O_Cnode;
   Ghdl_Rtik_File                        : O_Cnode;
   Ghdl_Rtik_Port                        : O_Cnode;
   Ghdl_Rtik_Generic                     : O_Cnode;
   Ghdl_Rtik_Alias                       : O_Cnode;
   Ghdl_Rtik_Guard                       : O_Cnode;
   Ghdl_Rtik_Component                   : O_Cnode;
   Ghdl_Rtik_Attribute                   : O_Cnode;
   Ghdl_Rtik_Type_B1                     : O_Cnode;
   Ghdl_Rtik_Type_E8                     : O_Cnode;
   Ghdl_Rtik_Type_E32                    : O_Cnode;
   Ghdl_Rtik_Type_I32                    : O_Cnode;
   Ghdl_Rtik_Type_I64                    : O_Cnode;
   Ghdl_Rtik_Type_F64                    : O_Cnode;
   Ghdl_Rtik_Type_P32                    : O_Cnode;
   Ghdl_Rtik_Type_P64                    : O_Cnode;
   Ghdl_Rtik_Type_Access                 : O_Cnode;
   Ghdl_Rtik_Type_Array                  : O_Cnode;
   Ghdl_Rtik_Type_Record                 : O_Cnode;
   Ghdl_Rtik_Type_Unbounded_Record       : O_Cnode;
   Ghdl_Rtik_Type_File                   : O_Cnode;
   Ghdl_Rtik_Subtype_Scalar              : O_Cnode;
   Ghdl_Rtik_Subtype_Array               : O_Cnode;
   Ghdl_Rtik_Subtype_Unbounded_Array     : O_Cnode;
   Ghdl_Rtik_Subtype_Record              : O_Cnode;
   Ghdl_Rtik_Subtype_Unbounded_Record    : O_Cnode;
   Ghdl_Rtik_Subtype_Access              : O_Cnode;
   Ghdl_Rtik_Type_Protected              : O_Cnode;
   Ghdl_Rtik_Element                     : O_Cnode;
   Ghdl_Rtik_Unit64                      : O_Cnode;
   Ghdl_Rtik_Unitptr                     : O_Cnode;
   Ghdl_Rtik_Attribute_Transaction       : O_Cnode;
   Ghdl_Rtik_Attribute_Quiet             : O_Cnode;
   Ghdl_Rtik_Attribute_Stable            : O_Cnode;
   Ghdl_Rtik_Psl_Assert                  : O_Cnode;
   Ghdl_Rtik_Psl_Assume                  : O_Cnode;
   Ghdl_Rtik_Psl_Cover                   : O_Cnode;
   Ghdl_Rtik_Psl_Endpoint                : O_Cnode;
   Ghdl_Rtik_Error                       : O_Cnode;

   -- PSL State types
   Ghdl_Rti_Psl_State                    : O_Tnode;
   Ghdl_Rti_Psl_State_Inactive           : O_Cnode;
   Ghdl_Rti_Psl_State_Running            : O_Cnode;
   Ghdl_Rti_Psl_State_Failed             : O_Cnode;
   Ghdl_Rti_Psl_State_Covered            : O_Cnode;

   --  RTI types.
   Ghdl_Rti_Depth : O_Tnode;
   Ghdl_Rti_U8    : O_Tnode;

   --  Common node.
   Ghdl_Rti_Common           : O_Tnode;
   Ghdl_Rti_Common_Kind      : O_Fnode;
   Ghdl_Rti_Common_Depth     : O_Fnode;
   Ghdl_Rti_Common_Mode      : O_Fnode;
   Ghdl_Rti_Common_Max_Depth : O_Fnode;

   --  Node accesses and arrays.
   Ghdl_Rti_Access  : O_Tnode;
   Ghdl_Rti_Array   : O_Tnode;
   Ghdl_Rti_Arr_Acc : O_Tnode;

   --  Instance link.
   --  This is a structure at the beginning of each entity/architecture
   --  instance.  This allow the run-time to find the parent of an instance.
   Ghdl_Entity_Link_Type   : O_Tnode;
   --  RTI for this instance.
   Ghdl_Entity_Link_Rti    : O_Fnode;
   --  RTI of the parent, which has instancied the instance.
   Ghdl_Entity_Link_Parent : O_Fnode;

   Ghdl_Component_Link_Type     : O_Tnode;
   --  Pointer to a Ghdl_Entity_Link_Type, which is the entity instantiated.
   Ghdl_Component_Link_Instance : O_Fnode;
   --  RTI for the component instantiation statement.
   Ghdl_Component_Link_Stmt     : O_Fnode;

   --  Access to Ghdl_Entity_Link_Type.
   Ghdl_Entity_Link_Acc    : O_Tnode;
   --  Access to a Ghdl_Component_Link_Type.
   Ghdl_Component_Link_Acc : O_Tnode;

   --  Generate initial rti declarations.
   procedure Rti_Initialize;

   --  Get address (as Ghdl_Rti_Access) of constant RTI.
   function New_Rti_Address (Rti : O_Dnode) return O_Enode;

   --  Generate rtis for a library unit.
   procedure Generate_Unit (Lib_Unit : Iir);

   --  Generate a constant declaration for SIG; but do not set its value.
   procedure Generate_Signal_Rti (Sig : Iir);

   --  Generate RTIs for subprogram body BOD.
   procedure Generate_Subprogram_Body (Bod : Iir);

   --  Generate RTI for LIB.  If PUBLIC is FALSE, only generate the
   --  declaration as external.
   procedure Generate_Library (Lib    : Iir_Library_Declaration;
                               Public : Boolean);

   --  Generate RTI for the top of the hierarchy.  Return the maximum number
   --  of packages.
   procedure Generate_Top (Nbr_Pkgs : out Natural);

   --  Add two associations to ASSOC to add an rti_context for NODE.
   procedure Associate_Rti_Context
     (Assoc : in out O_Assoc_List; Node : Iir);
   procedure Associate_Null_Rti_Context (Assoc : in out O_Assoc_List);

   function Get_Context_Rti (Node : Iir) return O_Enode;
   function Get_Context_Addr (Node : Iir) return O_Enode;
end Trans.Rtis;
