--  GHDL Run Time (GRT) -  RTI address handling.
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with System; use System;
with Ada.Unchecked_Conversion;
with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Rtis; use Grt.Rtis;

--  Addresses handling.
package Grt.Rtis_Addr is
   function "+" (L : Address; R : Ghdl_Rti_Loc) return Address;
   function "+" (L : Address; R : Ghdl_Index_Type) return Address;

   function "-" (L : Address; R : Ghdl_Rti_Loc) return Address;

   function Align (L : Address; R : Ghdl_Rti_Loc) return Address;

   --  An RTI context contains a pointer (BASE) to or into an instance.
   --  BLOCK describes data being pointed.  If a reference is made to a field
   --  described by a parent of BLOCK, BASE must be modified.
   type Rti_Context is record
      Base : Address;
      Block : Ghdl_Rti_Access;
   end record;

   Null_Context : constant Rti_Context;

   --  Access to an address.
   type Addr_Acc is access Address;
   function To_Addr_Acc is new Ada.Unchecked_Conversion
     (Source => Address, Target => Addr_Acc);

   --  Get the parent context of CTXT.
   --  The parent of an architecture is its entity.
   function Get_Parent_Context (Ctxt : Rti_Context) return Rti_Context;

   --  From an entity link, extract context and instantiation statement.
   procedure Get_Instance_Link (Link : Ghdl_Entity_Link_Acc;
                                Ctxt : out Rti_Context;
                                Stmt : out Ghdl_Rti_Access);

   --  Get the child context of if-generate statement GEN.  Return Null_Context
   --  if there is no child.
   function Get_If_Case_Generate_Child
     (Ctxt : Rti_Context; Gen : Ghdl_Rti_Access) return Rti_Context;

   --  Convert a location to an address.
   --  `Depth` is the position in the design hierarchy of the object
   --  we are finding the address of.
   --  `Loc` is the location of that object, relative to the address
   --  of it's parent.
   --  `Context` is the RTI node and address of an object deeper in
   --  the hierarchy that is referencing this object.
   function Loc_To_Addr (Depth : Ghdl_Rti_Depth;
                         Loc : Ghdl_Rti_Loc;
                         Ctxt : Rti_Context)
                        return Address;

   --  Get the length of for_generate GEN.
   function Get_For_Generate_Length (Gen : Ghdl_Rtin_Generate_Acc;
                                     Ctxt : Rti_Context)
                                    return Ghdl_Index_Type;

   --  Get the context of instance INST.
   procedure Get_Instance_Context (Inst : Ghdl_Rtin_Instance_Acc;
                                   Ctxt : Rti_Context;
                                   Sub_Ctxt : out Rti_Context);

   --  Extract range RNG of type DEF from BOUNDS.  BOUNDS is updated to the
   --  next range.  DEF must be a base type.
   procedure Extract_Range (Bounds : in out Address;
                            Def : Ghdl_Rti_Access;
                            Rng : out Ghdl_Range_Ptr);

   function Array_Layout_To_Bounds (Layout : Address) return Address;

   --  Return bounds (for arrays) or layout (for records) of array
   --  layout LAYOUT according to element type EL_RTI.
   function Array_Layout_To_Element
     (Layout : Address; El_Rti : Ghdl_Rti_Access) return Address;

   --  Extract range of every dimension from bounds.
   procedure Bound_To_Range (Bounds_Addr : Address;
                             Def : Ghdl_Rtin_Type_Array_Acc;
                             Res : out Ghdl_Range_Array);

   function Range_To_Length (Rng : Ghdl_Range_Ptr; Base_Type : Ghdl_Rti_Access)
                            return Ghdl_Index_Type;

   --  Get the base type of ATYPE.
   function Get_Base_Type (Atype : Ghdl_Rti_Access) return Ghdl_Rti_Access;

   --  Likewise, but for an array type.
   function Get_Base_Array_Type (Atype : Ghdl_Rti_Access)
                                return Ghdl_Rtin_Type_Array_Acc;

   --  Return true iff ATYPE is anonymous.
   --  Valid only on type and subtype definitions.
   function Rti_Anonymous_Type (Atype : Ghdl_Rti_Access) return Boolean;
   pragma Inline (Rti_Anonymous_Type);

   --  Return true iff ATYPE is complex.
   --  Valid only on type and subtype definitions.
   function Rti_Complex_Type (Atype : Ghdl_Rti_Access) return Boolean;
   pragma Inline (Rti_Complex_Type);

   --  Get the top context.
   function Get_Top_Context return Rti_Context;

private
   Null_Context : constant Rti_Context := (Base => Null_Address,
                                           Block => null);
end Grt.Rtis_Addr;
