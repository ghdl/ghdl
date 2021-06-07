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
with Grt.Errors; use Grt.Errors;

package body Grt.Rtis_Addr is
   function "+" (L : Address; R : Ghdl_Rti_Loc) return Address
   is
   begin
      return To_Address (To_Integer (L) + R);
   end "+";

   function "+" (L : Address; R : Ghdl_Index_Type) return Address
   is
   begin
      return To_Address (To_Integer (L) + Integer_Address (R));
   end "+";

   function "-" (L : Address; R : Ghdl_Rti_Loc) return Address
   is
   begin
      return To_Address (To_Integer (L) - R);
   end "-";

   function Align (L : Address; R : Ghdl_Rti_Loc) return Address
   is
      Nad : Integer_Address;
   begin
      Nad := To_Integer (L + (R - 1));
      return To_Address (Nad - (Nad mod R));
   end Align;

   function Get_Parent_Context (Ctxt : Rti_Context) return Rti_Context
   is
      Blk : constant Ghdl_Rtin_Block_Acc :=
        To_Ghdl_Rtin_Block_Acc (Ctxt.Block);
   begin
      case Ctxt.Block.Kind is
         when Ghdl_Rtik_Process
           | Ghdl_Rtik_Block
           | Ghdl_Rtiks_Psl =>
            return (Base => Ctxt.Base - Blk.Loc,
                    Block => Blk.Parent);
         when Ghdl_Rtik_Architecture =>
            if Blk.Loc /= Null_Rti_Loc then
               Internal_Error ("get_parent_context(3)");
            end if;
            return (Base => Ctxt.Base + Blk.Loc,
                    Block => Blk.Parent);
         when Ghdl_Rtik_Generate_Body =>
            declare
               Nbase : Address;
               Nblk : Ghdl_Rti_Access;
               Parent : Ghdl_Rti_Access;
            begin
               --  Read the pointer to the parent.
               --  This is the first field.
               Nbase := To_Addr_Acc (Ctxt.Base).all;
               --  Parent (by default).
               Nblk := Blk.Parent;
               --  Since the parent may be a grant-parent, adjust
               --  the base (so that the substraction above will work).
               Parent := Blk.Parent;
               loop
                  case Parent.Kind is
                     when Ghdl_Rtik_Architecture
                       | Ghdl_Rtik_Generate_Body =>
                        exit;
                     when Ghdl_Rtik_Block =>
                        declare
                           Blk1 : constant Ghdl_Rtin_Block_Acc :=
                             To_Ghdl_Rtin_Block_Acc (Parent);
                        begin
                           Nbase := Nbase + Blk1.Loc;
                           Parent := Blk1.Parent;
                        end;
                     when Ghdl_Rtik_For_Generate
                       | Ghdl_Rtik_If_Generate =>
                        declare
                           Gen : constant Ghdl_Rtin_Generate_Acc :=
                             To_Ghdl_Rtin_Generate_Acc (Parent);
                        begin
                           Parent := Gen.Parent;
                           --  For/If generate statement are not blocks.  Skip
                           --  them.
                           Nblk := Gen.Parent;
                        end;
                     when others =>
                        Internal_Error ("get_parent_context(2)");
                  end case;
               end loop;
               return (Base => Nbase,
                       Block => Nblk);
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
         Ctxt := (Base => Link.Parent.all'Address - Obj.Loc,
                  Block => Obj.Parent);
      end if;
   end Get_Instance_Link;

   function Get_If_Case_Generate_Child
     (Ctxt : Rti_Context; Gen : Ghdl_Rti_Access) return Rti_Context
   is
      pragma Assert (Gen.Kind = Ghdl_Rtik_If_Generate
                       or Gen.Kind = Ghdl_Rtik_Case_Generate);
      Blk : constant Ghdl_Rtin_Block_Acc := To_Ghdl_Rtin_Block_Acc (Gen);
      Base_Addr : constant Address := Ctxt.Base + Blk.Loc;

      --  Address of the block_id field.  It is just after the instance field.
      --  Assume alignment is ok (it is on 32 and 64 bit platforms).
      Id_Addr : constant Address :=
        Base_Addr + Ghdl_Index_Type'(Address'Size / Storage_Unit);
      Id : Ghdl_Index_Type;
      pragma Import (Ada, Id);
      for Id'Address use Id_Addr;
   begin
      return (Base => To_Addr_Acc (Base_Addr).all,
              Block => Blk.Children (Id));
   end Get_If_Case_Generate_Child;

   function Loc_To_Addr (Depth : Ghdl_Rti_Depth;
                         Loc : Ghdl_Rti_Loc;
                         Ctxt : Rti_Context)
                        return Address
   is
      Cur_Ctxt : Rti_Context;
      Nctxt : Rti_Context;
   begin
      if Depth = 0 then
         return To_Address (Loc);
      elsif Ctxt.Block.Depth = Depth then
         --Addr := Base + Storage_Offset (Obj.Loc.Off);
         return Ctxt.Base + Loc;
      else
         if Ctxt.Block.Depth < Depth then
            Internal_Error ("loc_to_addr");
         end if;
         Cur_Ctxt := Ctxt;
         loop
            Nctxt := Get_Parent_Context (Cur_Ctxt);
            if Nctxt.Block.Depth = Depth then
               return Nctxt.Base + Loc;
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
         when Ghdl_Rtik_Type_B1 =>
            return Rng.B1.Len;
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

   function Get_For_Generate_Length (Gen : Ghdl_Rtin_Generate_Acc;
                                     Ctxt : Rti_Context)
                                    return Ghdl_Index_Type
   is
      Bod : constant Ghdl_Rtin_Block_Acc :=
        To_Ghdl_Rtin_Block_Acc (Gen.Child);
      Iter_Type : Ghdl_Rtin_Subtype_Scalar_Acc;
      Rng : Ghdl_Range_Ptr;
   begin
      Iter_Type := To_Ghdl_Rtin_Subtype_Scalar_Acc
        (To_Ghdl_Rtin_Object_Acc (Bod.Children (0)).Obj_Type);
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
      --  Address of the field containing the address of the instance.
      Inst_Addr : constant Address := Ctxt.Base + Inst.Loc;
      --  Read sub instance address.
      Inst_Base : constant Address := To_Addr_Acc (Inst_Addr).all;
   begin
      --  Read instance RTI.
      if Inst_Base = Null_Address then
         --  No instance.
         Sub_Ctxt := (Base => Null_Address, Block => null);
      else
         Sub_Ctxt := (Base => Inst_Base,
                      Block => To_Ghdl_Rti_Acc_Acc (Inst_Base).all);
      end if;
   end Get_Instance_Context;

   procedure Extract_Range (Bounds : in out Address;
                            Def : Ghdl_Rti_Access;
                            Rng : out Ghdl_Range_Ptr)
   is
      procedure Align (A : Ghdl_Index_Type) is
      begin
         Bounds := Align (Bounds, Ghdl_Rti_Loc (A));
      end Align;

      procedure Update (S : Ghdl_Index_Type) is
      begin
         Bounds := Bounds + (S / Storage_Unit);
      end Update;
   begin
      if Bounds = Null_Address then
         --  Propagate failure.
         Rng := null;
         return;
      end if;

      case Def.Kind is
         when Ghdl_Rtik_Type_I32 =>
            Align (Ghdl_Range_I32'Alignment);
            Rng := To_Ghdl_Range_Ptr (Bounds);
            Update (Ghdl_Range_I32'Size);
         when Ghdl_Rtik_Type_B1 =>
            Align (Ghdl_Range_B1'Alignment);
            Rng := To_Ghdl_Range_Ptr (Bounds);
            Update (Ghdl_Range_B1'Size);
         when Ghdl_Rtik_Type_E8 =>
            Align (Ghdl_Range_E8'Alignment);
            Rng := To_Ghdl_Range_Ptr (Bounds);
            Update (Ghdl_Range_E8'Size);
         when others =>
            --  Bounds are not known anymore.
            Rng := null;
      end case;
   end Extract_Range;

   function Array_Layout_To_Bounds (Layout : Address) return Address is
   begin
      --  Skip the 2 size fields (1 for objects size, 1 for signals size).
      return Layout + Ghdl_Index_Type'(Ghdl_Indexes_Type'Size / 8);
   end Array_Layout_To_Bounds;

   function Array_Layout_To_Element
     (Layout : Address; El_Rti : Ghdl_Rti_Access) return Address is
   begin
      case El_Rti.Kind is
         when Ghdl_Rtik_Type_Array
           | Ghdl_Rtik_Subtype_Array
           | Ghdl_Rtik_Subtype_Unbounded_Array =>
            --  Trim size to pass the bounds
            return Array_Layout_To_Bounds (Layout);
         when Ghdl_Rtik_Type_Unbounded_Record
           | Ghdl_Rtik_Subtype_Unbounded_Record =>
            --  Keep full layout.
            return Layout;
         when Ghdl_Rtik_Type_Record =>
            return Null_Address;
         when others =>
            return Null_Address;
      end case;
   end Array_Layout_To_Element;

   procedure Bound_To_Range (Bounds_Addr : Address;
                             Def : Ghdl_Rtin_Type_Array_Acc;
                             Res : out Ghdl_Range_Array)
   is
      Bounds : Address;
      Idx_Def : Ghdl_Rti_Access;
   begin
      if Res'Length /= Def.Nbr_Dim or else Res'First /= 0 then
         Internal_Error ("rtis_addr.bound_to_range");
      end if;

      Bounds := Bounds_Addr;

      for I in 0 .. Def.Nbr_Dim - 1 loop
         Idx_Def := Def.Indexes (I);
         Idx_Def := Get_Base_Type (Idx_Def);
         Extract_Range (Bounds, Idx_Def, Res (I));
      end loop;
   end Bound_To_Range;

   function Get_Base_Type (Atype : Ghdl_Rti_Access) return Ghdl_Rti_Access
   is
      Res : Ghdl_Rti_Access;
   begin
      Res := Atype;
      loop
         case Res.Kind is
            when Ghdl_Rtik_Type_E8
               | Ghdl_Rtik_Type_E32
               | Ghdl_Rtik_Type_B1
               | Ghdl_Rtik_Type_I32
               | Ghdl_Rtik_Type_I64
               | Ghdl_Rtik_Type_P32
               | Ghdl_Rtik_Type_P64
               | Ghdl_Rtik_Type_F64 =>
               return Res;
            when Ghdl_Rtik_Subtype_Scalar =>
               Res := To_Ghdl_Rtin_Subtype_Scalar_Acc (Res).Basetype;
            when Ghdl_Rtik_Type_Array
               | Ghdl_Rtik_Type_Record
               | Ghdl_Rtik_Type_Unbounded_Record =>
               return Res;
            when Ghdl_Rtik_Subtype_Array
               | Ghdl_Rtik_Subtype_Unbounded_Array
               | Ghdl_Rtik_Subtype_Record
               | Ghdl_Rtik_Subtype_Unbounded_Record =>
               Res := To_Ghdl_Rtin_Subtype_Composite_Acc (Res).Basetype;
            when others =>
               Internal_Error ("rtis_addr.get_base_type");
         end case;
      end loop;
   end Get_Base_Type;

   function Get_Base_Array_Type (Atype : Ghdl_Rti_Access)
                                return Ghdl_Rtin_Type_Array_Acc
   is
      Res : Ghdl_Rti_Access;
   begin
      Res := Atype;
      loop
         case Res.Kind is
            when Ghdl_Rtik_Type_Array =>
               return To_Ghdl_Rtin_Type_Array_Acc (Res);
            when Ghdl_Rtik_Subtype_Array
               | Ghdl_Rtik_Subtype_Unbounded_Array =>
               Res := To_Ghdl_Rtin_Subtype_Composite_Acc (Res).Basetype;
            when others =>
               Internal_Error ("rtis_addr.get_base_array_type");
         end case;
      end loop;
   end Get_Base_Array_Type;

   function Rti_Complex_Type (Atype : Ghdl_Rti_Access) return Boolean is
   begin
      return (Atype.Mode and Ghdl_Rti_Type_Complex_Mask)
        = Ghdl_Rti_Type_Complex;
   end Rti_Complex_Type;

   function Rti_Anonymous_Type (Atype : Ghdl_Rti_Access) return Boolean is
   begin
      return (Atype.Mode and Ghdl_Rti_Type_Anonymous_Mask)
        = Ghdl_Rti_Type_Anonymous;
   end Rti_Anonymous_Type;

   function Get_Top_Context return Rti_Context
   is
      Ctxt : Rti_Context;
   begin
      Ctxt := (Base => Ghdl_Rti_Top_Instance,
               Block => Ghdl_Rti_Top.Parent);
      return Ctxt;
   end Get_Top_Context;

end Grt.Rtis_Addr;
