--  GHDL Run Time (GRT) -  Run Time Information Instances.
--  Copyright (C) 2020 Tristan Gingold
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with System; use System;
with Grt.Types; use Grt.Types;
with Grt.Rtis; use Grt.Rtis;
with Grt.Rtis_Addr; use Grt.Rtis_Addr;

package Grt.Rtiis is

   type Ghdl_Type_Rtii is record
      --  The RTI of the base type (this type or the unbounded version of it)
      Rti : Ghdl_Rti_Access;
      --  The context is used to find the layouts and bound associated with
      --  types.
      Ctxt : Rti_Context;
      --  This is the address of the layout information. It can be determined
      --  from the Rti and the Ctxt, but we also store it here so we don't have
      --  to determine it too often.
      Layout_Addr : Address;
      --  If the Base_Rti is unbound then this is the address of the
      --  layout since the type's layout was unknown when the type was defined.
      Binding_Layout_Addr : Address;
   end record;

   Rtii_Name_Length : constant Natural := 256;
   type Ghdl_Object_Rtii_Name is record
      -- We keep the value null-terminated so that we can easily
      -- cast it to a Ghdl_C_String.
      Value : String(1 .. Rtii_Name_Length);
      Length : Natural range 1 .. Rtii_Name_Length;
   end record;
   function New_Name (S : String) return Ghdl_Object_Rtii_Name;
   function New_Name (S : Ghdl_C_String) return Ghdl_Object_Rtii_Name;
   function Append_To_Name (Name : Ghdl_Object_Rtii_Name; S : String)
                            return Ghdl_Object_Rtii_Name;
   function To_Ghdl_C_String(Name : Ghdl_Object_Rtii_Name)
                            return Ghdl_C_String;

   --  A RTII (Run time instance information) represents a node in the
   --  design tree. Where an RTI node depends on the context and could
   --  be instatiated many times in the design, an RTII node
   --  represents one specfic instance of that node.
   type Ghdl_Object_Rtii is record
      --  Whether it is a signal/port or a generic/constant.
      Is_Sig : Boolean;
      --  Information we need about the type of this node.
      Typ : Ghdl_Type_Rtii;
      --  The address of the data for this component.
      Addr : Address;
      -- The name of the signal/port/constant/generic of subcomponent of it.
      Name : Ghdl_Object_Rtii_Name;
      -- This is a pointer to the RTI name.  It is only defined for the top
      -- level component.  Necessary because grt-waves expects to get a
      -- a pointer to a string that outlasts the avhpi handle.
      Name_Ptr: Ghdl_C_String;
      --  The Rti of the base signal/port/constant/generic.
      --  Useful for accessing the mode and location in source code.
      Base_Rti : Ghdl_Rtin_Object_Acc;
   end record;

   function To_Ghdl_Object_Rtii (Rti : Ghdl_Rtin_Object_Acc;
                                 Ctxt : Rti_Context)
                                return Ghdl_Object_Rtii;

   --  Get a indexed child of an Rtii.
   function Get_Rtii_Child (Rtii : Ghdl_Object_Rtii; Index : Ghdl_Index_Type)
   return Ghdl_Object_Rtii;

   --  Get number of children in a Rtii.
   function Get_Rtii_Nbr_Children (Rtii : Ghdl_Object_Rtii)
                                  return Ghdl_Index_Type;

   --  Checks all of the components signals to see if any of them has
   --  Event == True.
   --  FIXME: Unimplemented
   --  function Rtii_Event (Rtii : Ghdl_Object_Rtii) return Boolean;

   --  Returns number of elements in an array or record.
   --  Returns number of bits in a simple type.
   --  Should correspond to vpiSize, (I think also to vhpiSizeP)
   function Get_Size (Rtii : Ghdl_Object_Rtii) return Ghdl_Index_Type;

   --  Returns the number of bits to represent the data.
   --  I.e. for an array this will be
   --  the size of the array * bit_size of the element.
   function Get_Bit_Size (Rtii : Ghdl_Object_Rtii) return Ghdl_Index_Type;

   --  Whether this is array-like.
   function Is_Array (Rtii : Ghdl_Object_Rtii) return Boolean;

   --  Whether this is record-like.
   function Is_Record (Rtii : Ghdl_Object_Rtii) return Boolean;

end Grt.Rtiis;
