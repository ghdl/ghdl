--  GHDL Run Time (GRT) - RTI utilities.
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
--with Grt.Disp; use Grt.Disp;
with Grt.Errors; use Grt.Errors;

package body Grt.Rtis_Utils is

   function Traverse_Blocks (Ctxt : Rti_Context) return Traverse_Result
   is
      function Traverse_Instance (Ctxt : Rti_Context) return Traverse_Result;

      function Traverse_Blocks_1 (Ctxt : Rti_Context) return Traverse_Result
      is
         Blk : Ghdl_Rtin_Block_Acc;

         Res : Traverse_Result;
         Nctxt : Rti_Context;
         Index : Ghdl_Index_Type;
         Child : Ghdl_Rti_Access;
      begin
         Res := Process (Ctxt, Ctxt.Block);
         if Res /= Traverse_Ok then
            return Res;
         end if;

         Blk := To_Ghdl_Rtin_Block_Acc (Ctxt.Block);
         Index := 0;
         while Index < Blk.Nbr_Child loop
            Child := Blk.Children (Index);
            Index := Index + 1;
            case Child.Kind is
               when Ghdl_Rtik_Process
                 | Ghdl_Rtik_Block =>
                  declare
                     Nblk : Ghdl_Rtin_Block_Acc;
                  begin
                     Nblk := To_Ghdl_Rtin_Block_Acc (Child);
                     Nctxt := (Base => Ctxt.Base + Nblk.Loc.Off,
                               Block => Child);
                     Res := Traverse_Blocks_1 (Nctxt);
                  end;
               when Ghdl_Rtik_For_Generate =>
                  declare
                     Nblk : Ghdl_Rtin_Block_Acc;
                     Length : Ghdl_Index_Type;
                  begin
                     Nblk := To_Ghdl_Rtin_Block_Acc (Child);
                     Nctxt :=
                       (Base => To_Addr_Acc (Ctxt.Base + Nblk.Loc.Off).all,
                        Block => Child);
                     Length := Get_For_Generate_Length (Nblk, Ctxt);
                     for I in 1 .. Length loop
                        Res := Traverse_Blocks_1 (Nctxt);
                        exit when Res = Traverse_Stop;
                        Nctxt.Base := Nctxt.Base + Nblk.Size;
                     end loop;
                  end;
               when Ghdl_Rtik_If_Generate =>
                  declare
                     Nblk : Ghdl_Rtin_Block_Acc;
                  begin
                     Nblk := To_Ghdl_Rtin_Block_Acc (Child);
                     Nctxt :=
                       (Base => To_Addr_Acc (Ctxt.Base + Nblk.Loc.Off).all,
                        Block => Child);
                     if Nctxt.Base /= Null_Address then
                        Res := Traverse_Blocks_1 (Nctxt);
                     end if;
                  end;
               when Ghdl_Rtik_Instance =>
                  Res := Process (Ctxt, Child);
                  if Res = Traverse_Ok then
                     declare
                        Obj : Ghdl_Rtin_Instance_Acc;
                     begin
                        Obj := To_Ghdl_Rtin_Instance_Acc (Child);

                        Get_Instance_Context (Obj, Ctxt, Nctxt);
                        if Nctxt /= Null_Context then
                           Res := Traverse_Instance (Nctxt);
                        end if;
                     end;
                  end if;
               when Ghdl_Rtik_Package
                 | Ghdl_Rtik_Entity
                 | Ghdl_Rtik_Architecture =>
                  Internal_Error ("traverse_blocks");
               when Ghdl_Rtik_Port
                 | Ghdl_Rtik_Signal
                 | Ghdl_Rtik_Guard
                 | Ghdl_Rtik_Attribute_Quiet
                 | Ghdl_Rtik_Attribute_Stable
                 | Ghdl_Rtik_Attribute_Transaction =>
                  Res := Process (Ctxt, Child);
               when others =>
                  null;
            end case;
            exit when Res = Traverse_Stop;
         end loop;

         return Res;
      end Traverse_Blocks_1;

      function Traverse_Instance (Ctxt : Rti_Context) return Traverse_Result
      is
         Blk : Ghdl_Rtin_Block_Acc;

         Res : Traverse_Result;
         Nctxt : Rti_Context;

      begin
         Blk := To_Ghdl_Rtin_Block_Acc (Ctxt.Block);
         case Blk.Common.Kind is
            when Ghdl_Rtik_Architecture =>
               Nctxt := (Base => Ctxt.Base,
                         Block => Blk.Parent);
               --  The entity.
               Res := Traverse_Blocks_1 (Nctxt);
               if Res /= Traverse_Stop then
                  --  The architecture.
                  Res := Traverse_Blocks_1 (Ctxt);
               end if;
            when Ghdl_Rtik_Package_Body =>
               Nctxt := (Base => Ctxt.Base,
                         Block => Blk.Parent);
               Res := Traverse_Blocks_1 (Nctxt);
            when others =>
               Internal_Error ("traverse_blocks");
         end case;
         return Res;
      end Traverse_Instance;
   begin
      return Traverse_Instance (Ctxt);
   end Traverse_Blocks;

   function Rti_Complex_Type (Atype : Ghdl_Rti_Access) return Boolean
   is
   begin
      return (Atype.Mode and Ghdl_Rti_Type_Complex_Mask)
        = Ghdl_Rti_Type_Complex;
   end Rti_Complex_Type;

   --  Disp value stored at ADDR and whose type is described by RTI.
   procedure Get_Enum_Value
     (Vstr : in out Vstring; Rti : Ghdl_Rti_Access; Val : Ghdl_Index_Type)
   is
      Enum_Rti : Ghdl_Rtin_Type_Enum_Acc;
   begin
      Enum_Rti := To_Ghdl_Rtin_Type_Enum_Acc (Rti);
      Append (Vstr, Enum_Rti.Names (Val));
   end Get_Enum_Value;


   procedure Foreach_Scalar (Ctxt : Rti_Context;
                             Obj_Type : Ghdl_Rti_Access;
                             Obj_Addr : Address;
                             Is_Sig : Boolean;
                             Param : Param_Type)
   is
      --  Current address.
      Addr : Address;

      Name : Vstring;

      procedure Handle_Any (Rti : Ghdl_Rti_Access);

      procedure Handle_Scalar (Rti : Ghdl_Rti_Access)
      is
         procedure Update (S : Ghdl_Index_Type) is
         begin
            Addr := Addr + (S / Storage_Unit);
         end Update;
      begin
         Process (Addr, Name, Rti, Param);

         if Is_Sig then
            Update (Address'Size);
         else
            case Rti.Kind is
               when Ghdl_Rtik_Type_I32 =>
                  Update (32);
               when Ghdl_Rtik_Type_E8 =>
                  Update (8);
               when Ghdl_Rtik_Type_E32 =>
                  Update (32);
               when Ghdl_Rtik_Type_B2 =>
                  Update (8);
               when Ghdl_Rtik_Type_F64 =>
                  Update (64);
               when Ghdl_Rtik_Type_P64 =>
                  Update (64);
               when others =>
                  Internal_Error ("handle_scalar");
            end case;
         end if;
      end Handle_Scalar;

      procedure Range_Pos_To_Val (Rti : Ghdl_Rti_Access;
                                  Rng : Ghdl_Range_Ptr;
                                  Pos : Ghdl_Index_Type;
                                  Val : out Value_Union)
      is
      begin
         case Rti.Kind is
            when Ghdl_Rtik_Type_I32 =>
               case Rng.I32.Dir is
                  when Dir_To =>
                     Val.I32 := Rng.I32.Left + Ghdl_I32 (Pos);
                  when Dir_Downto =>
                     Val.I32 := Rng.I32.Left - Ghdl_I32 (Pos);
               end case;
            when Ghdl_Rtik_Type_E8 =>
               case Rng.E8.Dir is
                  when Dir_To =>
                     Val.E8 := Rng.E8.Left + Ghdl_E8 (Pos);
                  when Dir_Downto =>
                     Val.E8 := Rng.E8.Left - Ghdl_E8 (Pos);
               end case;
            when Ghdl_Rtik_Type_E32 =>
               case Rng.E32.Dir is
                  when Dir_To =>
                     Val.E32 := Rng.E32.Left + Ghdl_E32 (Pos);
                  when Dir_Downto =>
                     Val.E32 := Rng.E32.Left - Ghdl_E32 (Pos);
               end case;
            when Ghdl_Rtik_Type_B2 =>
               case Pos is
                  when 0 =>
                     Val.B2 := Rng.B2.Left;
                  when 1 =>
                     Val.B2 := Rng.B2.Right;
                  when others =>
                     Val.B2 := False;
               end case;
            when others =>
               Internal_Error ("grt.rtis_utils.range_pos_to_val");
         end case;
      end Range_Pos_To_Val;

      procedure Pos_To_Vstring
        (Vstr : in out Vstring;
         Rti : Ghdl_Rti_Access;
         Rng : Ghdl_Range_Ptr;
         Pos : Ghdl_Index_Type)
      is
         V : Value_Union;
      begin
         Range_Pos_To_Val (Rti, Rng, Pos, V);
         case Rti.Kind is
            when Ghdl_Rtik_Type_I32 =>
               declare
                  S : String (1 .. 12);
                  F : Natural;
               begin
                  To_String (S, F, V.I32);
                  Append (Vstr, S (F .. S'Last));
               end;
            when Ghdl_Rtik_Type_E8 =>
               Get_Enum_Value (Vstr, Rti, Ghdl_Index_Type (V.E8));
            when Ghdl_Rtik_Type_E32 =>
               Get_Enum_Value (Vstr, Rti, Ghdl_Index_Type (V.E32));
            when Ghdl_Rtik_Type_B2 =>
               Get_Enum_Value (Vstr, Rti, Ghdl_B2'Pos (V.B2));
            when others =>
               Append (Vstr, '?');
         end case;
      end Pos_To_Vstring;

      procedure Handle_Array_1 (El_Rti : Ghdl_Rti_Access;
                                Rngs : Ghdl_Range_Array;
                                Rtis : Ghdl_Rti_Arr_Acc;
                                Index : Ghdl_Index_Type)
      is
         Len : Ghdl_Index_Type;
         P : Natural;
         Base_Type : Ghdl_Rti_Access;
      begin
         P := Length (Name);
         if Index = 0 then
            Append (Name, '(');
         else
            Append (Name, ',');
         end if;

         Base_Type := Get_Base_Type (Rtis (Index));
         Len := Range_To_Length (Rngs (Index),  Base_Type);

         for I in 1 .. Len loop
            Pos_To_Vstring (Name, Base_Type, Rngs (Index), I - 1);
            if Index = Rngs'Last then
               Append (Name, ')');
               Handle_Any (El_Rti);
            else
               Handle_Array_1 (El_Rti, Rngs, Rtis, Index + 1);
            end if;
            Truncate (Name, P + 1);
         end loop;
         Truncate (Name, P);
      end Handle_Array_1;

      procedure Handle_Array (Rti : Ghdl_Rtin_Type_Array_Acc;
                              Vals : Ghdl_Uc_Array_Acc)
      is
         Nbr_Dim : constant Ghdl_Index_Type := Rti.Nbr_Dim;
         Rngs : Ghdl_Range_Array (0 .. Nbr_Dim - 1);
      begin
         Bound_To_Range (Vals.Bounds, Rti, Rngs);
         Addr := Vals.Base;
         Handle_Array_1 (Rti.Element, Rngs, Rti.Indexes, 0);
      end Handle_Array;

      procedure Handle_Record (Rti : Ghdl_Rtin_Type_Record_Acc)
      is
         El : Ghdl_Rtin_Element_Acc;
         Obj_Addr : Address;
         P : Natural;
      begin
         P := Length (Name);
         Obj_Addr := Addr;
         for I in 1 .. Rti.Nbrel loop
            El := To_Ghdl_Rtin_Element_Acc (Rti.Elements (I - 1));
            if Is_Sig then
               Addr := Obj_Addr + El.Sig_Off;
            else
               Addr := Obj_Addr + El.Val_Off;
            end if;
            Append (Name, '.');
            Append (Name, El.Name);
            Handle_Any (El.Eltype);
            Truncate (Name, P);
         end loop;
         -- FIXME
         --Addr := Obj_Addr + Rti.Xx;
      end Handle_Record;

      procedure Handle_Any (Rti : Ghdl_Rti_Access)
      is
         Save_Addr : Address;
      begin
         case Rti.Kind is
            when Ghdl_Rtik_Subtype_Scalar =>
               Handle_Scalar (To_Ghdl_Rtin_Subtype_Scalar_Acc (Rti).Basetype);
            when Ghdl_Rtik_Type_I32
              | Ghdl_Rtik_Type_E8
              | Ghdl_Rtik_Type_E32
              | Ghdl_Rtik_Type_B2 =>
               Handle_Scalar (Rti);
            when Ghdl_Rtik_Type_Array =>
               Handle_Array (To_Ghdl_Rtin_Type_Array_Acc (Rti),
                             To_Ghdl_Uc_Array_Acc (Addr));
            when Ghdl_Rtik_Subtype_Array =>
               declare
                  St : constant Ghdl_Rtin_Subtype_Array_Acc :=
                    To_Ghdl_Rtin_Subtype_Array_Acc (Rti);
                  Bt : constant Ghdl_Rtin_Type_Array_Acc := St.Basetype;
                  Rngs : Ghdl_Range_Array (0 .. Bt.Nbr_Dim - 1);
               begin
                  Bound_To_Range
                    (Loc_To_Addr (St.Common.Depth, St.Bounds, Ctxt), Bt, Rngs);
                  if Rti_Complex_Type (Rti) then
                     Save_Addr := Addr;
                     Addr := To_Addr_Acc (Addr).all;
                  end if;
                  Handle_Array_1 (Bt.Element, Rngs, Bt.Indexes, 0);
                  if Rti_Complex_Type (Rti) then
                     Addr := Save_Addr + (Address'Size / Storage_Unit);
                  end if;
               end;
            when Ghdl_Rtik_Subtype_Array_Ptr =>
               declare
                  St : constant Ghdl_Rtin_Subtype_Array_Acc :=
                    To_Ghdl_Rtin_Subtype_Array_Acc (Rti);
                  Bt : constant Ghdl_Rtin_Type_Array_Acc := St.Basetype;
                  Rngs : Ghdl_Range_Array (0 .. Bt.Nbr_Dim - 1);
               begin
                  Bound_To_Range
                    (Loc_To_Addr (St.Common.Depth, St.Bounds, Ctxt), Bt, Rngs);
                  Save_Addr := Addr;
                  Addr := To_Addr_Acc (Addr).all;
                  Handle_Array_1 (Bt.Element, Rngs, Bt.Indexes, 0);
                  Addr := Save_Addr + (Address'Size / Storage_Unit);
               end;
--          when Ghdl_Rtik_Type_File =>
--             declare
--                Vptr : Ghdl_Value_Ptr;
--             begin
--                Vptr := To_Ghdl_Value_Ptr (Obj);
--                Put (Stream, "File#");
--                Put_I32 (Stream, Vptr.I32);
--                --  FIXME: update OBJ (not very useful since never in a
--                --   composite type).
--             end;
            when Ghdl_Rtik_Type_Record =>
               if Rti_Complex_Type (Rti) then
                  Save_Addr := Addr;
                  Addr := To_Addr_Acc (Addr).all;
               end if;
               Handle_Record (To_Ghdl_Rtin_Type_Record_Acc (Rti));
               if Rti_Complex_Type (Rti) then
                  Addr := Save_Addr + (Address'Size / Storage_Unit);
               end if;
            when others =>
               Internal_Error ("grt.rtis_utils.foreach_scalar.handle_any");
         end case;
      end Handle_Any;
   begin
      Addr := Obj_Addr;
      Handle_Any (Obj_Type);
      Free (Name);
   end Foreach_Scalar;

   procedure Get_Value (Str : in out Vstring;
                        Value : Value_Union;
                        Type_Rti : Ghdl_Rti_Access)
   is
   begin
      case Type_Rti.Kind is
         when Ghdl_Rtik_Type_I32 =>
            declare
               S : String (1 .. 12);
               F : Natural;
            begin
               To_String (S, F, Value.I32);
               Append (Str, S (F .. S'Last));
            end;
         when Ghdl_Rtik_Type_E8 =>
            Get_Enum_Value (Str, Type_Rti, Ghdl_Index_Type (Value.E8));
         when Ghdl_Rtik_Type_E32 =>
            Get_Enum_Value (Str, Type_Rti, Ghdl_Index_Type (Value.E32));
         when Ghdl_Rtik_Type_B2 =>
            Get_Enum_Value
              (Str, Type_Rti, Ghdl_Index_Type (Ghdl_B2'Pos (Value.B2)));
         when Ghdl_Rtik_Type_F64 =>
            declare
               S : String (1 .. 32);
               L : Integer;

               function Snprintf_G (Cstr : Address;
                                    Size : Natural;
                                    Arg : Ghdl_F64)
                 return Integer;
               pragma Import (C, Snprintf_G, "__ghdl_snprintf_g");

            begin
               L := Snprintf_G (S'Address, S'Length, Value.F64);
               if L < 0 then
                  --  FIXME.
                  Append (Str, "?");
               else
                  Append (Str, S (1 .. L));
               end if;
            end;
         when Ghdl_Rtik_Type_P32 =>
            declare
               S : String (1 .. 12);
               F : Natural;
            begin
               To_String (S, F, Value.I32);
               Append (Str, S (F .. S'Last));
               Append (Str,
                       To_Ghdl_Rtin_Unit_Acc (To_Ghdl_Rtin_Type_Physical_Acc
                                              (Type_Rti).Units (0)).Name);
            end;
         when Ghdl_Rtik_Type_P64 =>
            declare
               S : String (1 .. 21);
               F : Natural;
            begin
               To_String (S, F, Value.I64);
               Append (Str, S (F .. S'Last));
               Append (Str,
                       To_Ghdl_Rtin_Unit_Acc (To_Ghdl_Rtin_Type_Physical_Acc
                                              (Type_Rti).Units (0)).Name);
            end;
         when others =>
            Internal_Error ("grt.rtis_utils.get_value");
      end case;
   end Get_Value;

   procedure Disp_Value (Stream : FILEs;
                         Value : Value_Union;
                         Type_Rti : Ghdl_Rti_Access)
   is
      Name : Vstring;
   begin
      Rtis_Utils.Get_Value (Name, Value, Type_Rti);
      Put (Stream, Name);
      Free (Name);
   end Disp_Value;

   procedure Get_Enum_Value
     (Rstr : in out Rstring; Rti : Ghdl_Rti_Access; Val : Ghdl_Index_Type)
   is
      Enum_Rti : Ghdl_Rtin_Type_Enum_Acc;
   begin
      Enum_Rti := To_Ghdl_Rtin_Type_Enum_Acc (Rti);
      Prepend (Rstr, Enum_Rti.Names (Val));
   end Get_Enum_Value;


   procedure Get_Value (Rstr : in out Rstring;
                        Addr : Address;
                        Type_Rti : Ghdl_Rti_Access)
   is
      Value : constant Ghdl_Value_Ptr := To_Ghdl_Value_Ptr (Addr);
   begin
      case Type_Rti.Kind is
         when Ghdl_Rtik_Type_I32 =>
            declare
               S : String (1 .. 12);
               F : Natural;
            begin
               To_String (S, F, Value.I32);
               Prepend (Rstr, S (F .. S'Last));
            end;
         when Ghdl_Rtik_Type_E8 =>
            Get_Enum_Value (Rstr, Type_Rti, Ghdl_Index_Type (Value.E8));
         when Ghdl_Rtik_Type_E32 =>
            Get_Enum_Value (Rstr, Type_Rti, Ghdl_Index_Type (Value.E32));
         when Ghdl_Rtik_Type_B2 =>
            Get_Enum_Value
              (Rstr, Type_Rti, Ghdl_Index_Type (Ghdl_B2'Pos (Value.B2)));
         when others =>
            Internal_Error ("grt.rtis_utils.get_value(rstr)");
      end case;
   end Get_Value;

   procedure Get_Path_Name (Rstr : in out Rstring;
                            Last_Ctxt : Rti_Context;
                            Sep : Character;
                            Is_Instance : Boolean := True)
   is
      Blk : Ghdl_Rtin_Block_Acc;
      Ctxt : Rti_Context;
   begin
      Ctxt := Last_Ctxt;
      loop
         Blk := To_Ghdl_Rtin_Block_Acc (Ctxt.Block);
         case Ctxt.Block.Kind is
            when Ghdl_Rtik_Process
              | Ghdl_Rtik_Block
              | Ghdl_Rtik_If_Generate =>
               Prepend (Rstr, Blk.Name);
               Prepend (Rstr, Sep);
               Ctxt := Get_Parent_Context (Ctxt);
            when Ghdl_Rtik_Entity =>
               declare
                  Link : Ghdl_Entity_Link_Acc;
               begin
                  Link := To_Ghdl_Entity_Link_Acc (Ctxt.Base);
                  Ctxt := (Base => Ctxt.Base,
                           Block => Link.Rti);
                  if Ctxt.Block = null then
                     --  Process in an entity.
                     --  FIXME: check.
                     Prepend (Rstr, Blk.Name);
                     return;
                  end if;
               end;
            when Ghdl_Rtik_Architecture =>
               declare
                  Entity_Ctxt: Rti_Context;
                  Link : Ghdl_Entity_Link_Acc;
                  Parent_Inst : Ghdl_Rti_Access;
               begin
                  --  Architecture name.
                  if Is_Instance then
                     Prepend (Rstr, ')');
                     Prepend (Rstr, Blk.Name);
                     Prepend (Rstr, '(');
                  end if;

                  Entity_Ctxt := Get_Parent_Context (Ctxt);

                  --  Instance parent.
                  Link := To_Ghdl_Entity_Link_Acc (Entity_Ctxt.Base);
                  Get_Instance_Link (Link, Ctxt, Parent_Inst);

                  --  Add entity name.
                  if Is_Instance or Parent_Inst = null then
                     Prepend (Rstr,
                              To_Ghdl_Rtin_Block_Acc (Entity_Ctxt.Block).Name);
                  end if;

                  if Parent_Inst = null then
                     --  Top reached.
                     Prepend (Rstr, Sep);
                     return;
                  else
                     --  Instantiation statement label.
                     if Is_Instance then
                        Prepend (Rstr, '@');
                     end if;
                     Prepend (Rstr,
                              To_Ghdl_Rtin_Object_Acc (Parent_Inst).Name);
                     Prepend (Rstr, Sep);
                  end if;
               end;
            when Ghdl_Rtik_For_Generate =>
               declare
                  Iter : Ghdl_Rtin_Object_Acc;
                  Addr : Address;
               begin
                  Prepend (Rstr, ')');
                  Iter := To_Ghdl_Rtin_Object_Acc (Blk.Children (0));
                  Addr := Loc_To_Addr (Iter.Common.Depth, Iter.Loc, Ctxt);
                  Get_Value (Rstr, Addr, Get_Base_Type (Iter.Obj_Type));
                  Prepend (Rstr, '(');
                  Prepend (Rstr, Blk.Name);
                  Prepend (Rstr, Sep);
                  Ctxt := Get_Parent_Context (Ctxt);
               end;
            when others =>
               Internal_Error ("grt.rtis_utils.get_path_name");
         end case;
      end loop;
   end Get_Path_Name;

   procedure Put (Stream : FILEs; Ctxt : Rti_Context)
   is
      Rstr : Rstring;
   begin
      Get_Path_Name (Rstr, Ctxt, '.');
      Put (Stream, Rstr);
      Free (Rstr);
   end Put;

end Grt.Rtis_Utils;
