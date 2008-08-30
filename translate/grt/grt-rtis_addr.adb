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
with Grt.Errors; use Grt.Errors;

package body Grt.Rtis_Addr is
   function "+" (L : Address; R : Ghdl_Index_Type) return Address
   is
   begin
      return To_Address (To_Integer (L) + Integer_Address (R));
   end "+";

   function "-" (L : Address; R : Ghdl_Index_Type) return Address
   is
   begin
      return To_Address (To_Integer (L) - Integer_Address (R));
   end "-";

   function Align (L : Address; R : Ghdl_Index_Type) return Address
   is
      Nad : Integer_Address;
   begin
      Nad := To_Integer (L + (R - 1));
      return To_Address (Nad - (Nad mod Integer_Address (R)));
   end Align;

   function Get_Parent_Context (Ctxt : Rti_Context) return Rti_Context
   is
      Blk : Ghdl_Rtin_Block_Acc;
   begin
      Blk := To_Ghdl_Rtin_Block_Acc (Ctxt.Block);
      case Ctxt.Block.Kind is
         when Ghdl_Rtik_Process
           | Ghdl_Rtik_Block =>
            return (Base => Ctxt.Base - Blk.Loc.Off,
                    Block => Blk.Parent);
         when Ghdl_Rtik_Architecture =>
            if Blk.Loc.Off /= 0 then
               Internal_Error ("get_parent_context(3)");
            end if;
            return (Base => Ctxt.Base + Blk.Loc.Off,
                    Block => Blk.Parent);
         when Ghdl_Rtik_For_Generate
           | Ghdl_Rtik_If_Generate =>
            declare
               Nbase : Address;
               Parent : Ghdl_Rti_Access;
               Blk1 : Ghdl_Rtin_Block_Acc;
            begin
               --  Read the pointer to the parent.
               --  This is the first field.
               Nbase := To_Addr_Acc (Ctxt.Base).all;
               --  Since the parent may be a grant-parent, adjust
               --  the base.
               Parent := Blk.Parent;
               loop
                  case Parent.Kind is
                     when Ghdl_Rtik_Architecture
                       | Ghdl_Rtik_For_Generate
                       | Ghdl_Rtik_If_Generate =>
                        exit;
                     when Ghdl_Rtik_Block =>
                        Blk1 := To_Ghdl_Rtin_Block_Acc (Parent);
                        Nbase := Nbase + Blk1.Loc.Off;
                        Parent := Blk1.Parent;
                     when others =>
                        Internal_Error ("get_parent_context(2)");
                  end case;
               end loop;
               return (Base => Nbase,
                       Block => Blk.Parent);
            end;
         when others =>
            Internal_Error ("get_parent_context(1)");
      end case;
   end Get_Parent_Context;

   procedure Get_Instance_Link (Link : Ghdl_Entity_Link_Acc;
                                Ctxt : out Rti_Context;
                                Stmt : out Ghdl_Rti_Access)
   is
      Obj : Ghdl_Rtin_Instance_Acc;
   begin
      if Link.Parent = null then
         --  Top entity.
         Stmt := null;
         Ctxt := (Base => Null_Address, Block => null);
      else
         Stmt := Link.Parent.Stmt;
         Obj := To_Ghdl_Rtin_Instance_Acc (Stmt);
         Ctxt := (Base => Link.Parent.all'Address - Obj.Loc.Off,
                  Block => Obj.Parent);
      end if;
   end Get_Instance_Link;

   function Loc_To_Addr (Depth : Ghdl_Rti_Depth;
                         Loc : Ghdl_Rti_Loc;
                         Ctxt : Rti_Context)
                        return Address
   is
      Cur_Ctxt : Rti_Context;
      Nctxt : Rti_Context;
   begin
      if Depth = 0 then
         return Loc.Addr;
      elsif Ctxt.Block.Depth = Depth then
         --Addr := Base + Storage_Offset (Obj.Loc.Off);
         return Ctxt.Base + Loc.Off;
      else
         if Ctxt.Block.Depth < Depth then
            Internal_Error ("loc_to_addr");
         end if;
         Cur_Ctxt := Ctxt;
         loop
            Nctxt := Get_Parent_Context (Cur_Ctxt);
            if Nctxt.Block.Depth = Depth then
               return Nctxt.Base + Loc.Off;
            end if;
            Cur_Ctxt := Nctxt;
         end loop;
      end if;
   end Loc_To_Addr;

   function Range_To_Length (Rng : Ghdl_Range_Ptr; Base_Type : Ghdl_Rti_Access)
                            return Ghdl_Index_Type
   is
   begin
      case Base_Type.Kind is
         when Ghdl_Rtik_Type_B2 =>
            return Rng.B2.Len;
         when Ghdl_Rtik_Type_E8 =>
            return Rng.E8.Len;
         when Ghdl_Rtik_Type_E32 =>
            return Rng.E32.Len;
         when Ghdl_Rtik_Type_I32 =>
            return Rng.I32.Len;
         when others =>
            Internal_Error ("range_to_length");
      end case;
   end Range_To_Length;

   function Get_For_Generate_Length (Blk : Ghdl_Rtin_Block_Acc;
                                     Ctxt : Rti_Context)
                                    return Ghdl_Index_Type
   is
      Iter_Type : Ghdl_Rtin_Subtype_Scalar_Acc;
      Rng : Ghdl_Range_Ptr;
   begin
      Iter_Type := To_Ghdl_Rtin_Subtype_Scalar_Acc
        (To_Ghdl_Rtin_Object_Acc (Blk.Children (0)).Obj_Type);
      if Iter_Type.Common.Kind /= Ghdl_Rtik_Subtype_Scalar then
         Internal_Error ("get_for_generate_length(1)");
      end if;
      Rng := To_Ghdl_Range_Ptr
        (Loc_To_Addr (Iter_Type.Common.Depth, Iter_Type.Range_Loc, Ctxt));
      return Range_To_Length (Rng, Iter_Type.Basetype);
   end Get_For_Generate_Length;

   procedure Get_Instance_Context (Inst : Ghdl_Rtin_Instance_Acc;
                                   Ctxt : Rti_Context;
                                   Sub_Ctxt : out Rti_Context)
   is
      Inst_Addr : Address;
      Inst_Base : Address;
   begin
      --  Address of the field containing the address of the instance.
      Inst_Addr := Ctxt.Base + Inst.Loc.Off;
      --  Read sub instance address.
      Inst_Base := To_Addr_Acc (Inst_Addr).all;
      --  Read instance RTI.
      if Inst_Base = Null_Address then
         Sub_Ctxt := (Base => Null_Address, Block => null);
      else
         Sub_Ctxt := (Base => Inst_Base,
                      Block => To_Ghdl_Rti_Acc_Acc (Inst_Base).all);
      end if;
   end Get_Instance_Context;

   procedure Bound_To_Range (Bounds_Addr : Address;
                             Def : Ghdl_Rtin_Type_Array_Acc;
                             Res : out Ghdl_Range_Array)
   is
      Bounds : Address;

      procedure Align (A : Ghdl_Index_Type) is
      begin
         Bounds := Align (Bounds, A);
      end Align;

      procedure Update (S : Ghdl_Index_Type) is
      begin
         Bounds := Bounds + (S / Storage_Unit);
      end Update;

      Idx_Def : Ghdl_Rti_Access;
   begin
      if Res'Length /= Def.Nbr_Dim or else Res'First /= 0 then
         Internal_Error ("disp_rti.bound_to_range");
      end if;

      Bounds := Bounds_Addr;

      for I in 0 .. Def.Nbr_Dim - 1 loop
         Idx_Def := Def.Indexes (I);

         if Bounds = Null_Address then
            Res (I) := null;
         else
            Idx_Def := Get_Base_Type (Idx_Def);
            case Idx_Def.Kind is
               when Ghdl_Rtik_Type_I32 =>
                  Align (Ghdl_Range_I32'Alignment);
                  Res (I) := To_Ghdl_Range_Ptr (Bounds);
                  Update (Ghdl_Range_I32'Size);
               when Ghdl_Rtik_Type_E8 =>
                  Align (Ghdl_Range_E8'Alignment);
                  Res (I) := To_Ghdl_Range_Ptr (Bounds);
                  Update (Ghdl_Range_E8'Size);
               when Ghdl_Rtik_Type_E32 =>
                  Align (Ghdl_Range_E32'Alignment);
                  Res (I) := To_Ghdl_Range_Ptr (Bounds);
                  Update (Ghdl_Range_E32'Size);
               when others =>
                  --  Bounds are not known anymore.
                  Bounds := Null_Address;
            end case;
         end if;
      end loop;
   end Bound_To_Range;

   function Get_Base_Type (Atype : Ghdl_Rti_Access) return Ghdl_Rti_Access
   is
   begin
      case Atype.Kind is
         when Ghdl_Rtik_Subtype_Scalar =>
            return To_Ghdl_Rtin_Subtype_Scalar_Acc (Atype).Basetype;
         when Ghdl_Rtik_Subtype_Array =>
            return To_Ghdl_Rti_Access
              (To_Ghdl_Rtin_Subtype_Array_Acc (Atype).Basetype);
         when Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32
           | Ghdl_Rtik_Type_B2 =>
            return Atype;
         when others =>
            Internal_Error ("rtis_addr.get_base_type");
      end case;
   end Get_Base_Type;

   function Get_Top_Context return Rti_Context
   is
      Ctxt : Rti_Context;
   begin
      Ctxt := (Base => To_Address (Ghdl_Rti_Top_Instance),
               Block => Ghdl_Rti_Top_Ptr.Parent);
      return Ctxt;
   end Get_Top_Context;

end Grt.Rtis_Addr;
