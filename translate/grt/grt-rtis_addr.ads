--  GHDL Run Time (GRT) -  RTI address handling.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with System; use System;
with Ada.Unchecked_Conversion;
with Grt.Types; use Grt.Types;
with Grt.Rtis; use Grt.Rtis;

--  Addresses handling.
package Grt.Rtis_Addr is
   function "+" (L : Address; R : Ghdl_Index_Type) return Address;

   function "-" (L : Address; R : Ghdl_Index_Type) return Address;

   function Align (L : Address; R : Ghdl_Index_Type) return Address;

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

   type Ghdl_Index_Acc is access Ghdl_Index_Type;
   function To_Ghdl_Index_Acc is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Index_Acc);

   --  Get the parent context of CTXT.
   --  The parent of an architecture is its entity.
   function Get_Parent_Context (Ctxt : Rti_Context) return Rti_Context;

   --  From an entity link, extract context and instantiation statement.
   procedure Get_Instance_Link (Link : Ghdl_Entity_Link_Acc;
                                Ctxt : out Rti_Context;
                                Stmt : out Ghdl_Rti_Access);

   --  Convert a location to an address.
   function Loc_To_Addr (Depth : Ghdl_Rti_Depth;
                         Loc : Ghdl_Rti_Loc;
                         Ctxt : Rti_Context)
                        return Address;

   --  Get the length of for_generate BLK.
   function Get_For_Generate_Length (Blk : Ghdl_Rtin_Block_Acc;
                                     Ctxt : Rti_Context)
                                    return Ghdl_Index_Type;

   --  Get the context of instance INST.
   procedure Get_Instance_Context (Inst : Ghdl_Rtin_Instance_Acc;
                                   Ctxt : Rti_Context;
                                   Sub_Ctxt : out Rti_Context);

   --  Extract range of every dimension from bounds.
   procedure Bound_To_Range (Bounds_Addr : Address;
                             Def : Ghdl_Rtin_Type_Array_Acc;
                             Res : out Ghdl_Range_Array);

   function Range_To_Length (Rng : Ghdl_Range_Ptr; Base_Type : Ghdl_Rti_Access)
                            return Ghdl_Index_Type;

   --  Get the base type of ATYPE.
   function Get_Base_Type (Atype : Ghdl_Rti_Access) return Ghdl_Rti_Access;

   --  Get the top context.
   function Get_Top_Context return Rti_Context;

private
   Null_Context : constant Rti_Context := (Base => Null_Address,
                                           Block => null);
end Grt.Rtis_Addr;
