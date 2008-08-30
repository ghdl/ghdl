--  GHDL Run Time (GRT) - RTI dumper.
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
with Grt.Astdio; use Grt.Astdio;
with Grt.Errors; use Grt.Errors;
with Grt.Hooks; use Grt.Hooks;

package body Grt.Disp_Rti is
   procedure Disp_Kind (Kind : Ghdl_Rtik);

   procedure Disp_Name (Name : Ghdl_C_String) is
   begin
      if Name = null then
         Put (stdout, "<anonymous>");
      else
         Put (stdout, Name);
      end if;
   end Disp_Name;

   --  Disp value stored at ADDR and whose type is described by RTI.
   procedure Disp_Enum_Value
     (Stream : FILEs; Rti : Ghdl_Rti_Access; Val : Ghdl_Index_Type)
   is
      Enum_Rti : Ghdl_Rtin_Type_Enum_Acc;
   begin
      Enum_Rti := To_Ghdl_Rtin_Type_Enum_Acc (Rti);
      Put (Stream, Enum_Rti.Names (Val));
   end Disp_Enum_Value;

   procedure Disp_Scalar_Value
     (Stream : FILEs;
      Rti : Ghdl_Rti_Access;
      Addr : in out Address;
      Is_Sig : Boolean)
   is
      procedure Update (S : Ghdl_Index_Type) is
      begin
         Addr := Addr + (S / Storage_Unit);
      end Update;

      Vptr : Ghdl_Value_Ptr;
   begin
      if Is_Sig then
         Vptr := To_Ghdl_Value_Ptr (To_Addr_Acc (Addr).all);
         Update (Address'Size);
      else
         Vptr := To_Ghdl_Value_Ptr (Addr);
      end if;

      case Rti.Kind is
         when Ghdl_Rtik_Type_I32 =>
            Put_I32 (Stream, Vptr.I32);
            if not Is_Sig then
               Update (32);
            end if;
         when Ghdl_Rtik_Type_E8 =>
            Disp_Enum_Value (Stream, Rti, Ghdl_Index_Type (Vptr.E8));
            if not Is_Sig then
               Update (8);
            end if;
         when Ghdl_Rtik_Type_E32 =>
            Disp_Enum_Value (Stream, Rti, Ghdl_Index_Type (Vptr.E32));
            if not Is_Sig then
               Update (32);
            end if;
         when Ghdl_Rtik_Type_B2 =>
            Disp_Enum_Value (Stream, Rti,
                             Ghdl_Index_Type (Ghdl_B2'Pos (Vptr.B2)));
            if not Is_Sig then
               Update (8);
            end if;
         when Ghdl_Rtik_Type_F64 =>
            Put_F64 (Stream, Vptr.F64);
            if not Is_Sig then
               Update (64);
            end if;
         when Ghdl_Rtik_Type_P64 =>
            Put_I64 (Stream, Vptr.I64);
            Put (Stream, " ");
            Put (Stream,
                 To_Ghdl_Rtin_Unit_Acc
                 (To_Ghdl_Rtin_Type_Physical_Acc (Rti).Units (0)).Name);
            if not Is_Sig then
               Update (64);
            end if;
         when others =>
            Internal_Error ("disp_rti.disp_scalar_value");
      end case;
   end Disp_Scalar_Value;

--    function Get_Scalar_Type_Kind (Rti : Ghdl_Rti_Access) return Ghdl_Rtik
--    is
--       Ndef : Ghdl_Rti_Access;
--    begin
--       if Rti.Kind = Ghdl_Rtik_Subtype_Scalar then
--          Ndef := To_Ghdl_Rtin_Subtype_Scalar_Acc (Rti).Basetype;
--       else
--          Ndef := Rti;
--       end if;
--       case Ndef.Kind is
--          when Ghdl_Rtik_Type_I32 =>
--             return Ndef.Kind;
--          when others =>
--             return Ghdl_Rtik_Error;
--       end case;
--    end Get_Scalar_Type_Kind;

   procedure Disp_Array_Value_1 (Stream : FILEs;
                                 El_Rti : Ghdl_Rti_Access;
                                 Ctxt : Rti_Context;
                                 Rngs : Ghdl_Range_Array;
                                 Rtis : Ghdl_Rti_Arr_Acc;
                                 Index : Ghdl_Index_Type;
                                 Obj : in out Address;
                                 Is_Sig : Boolean)
   is
      Length : Ghdl_Index_Type;
   begin
      Length := Range_To_Length (Rngs (Index), Get_Base_Type (Rtis (Index)));
      Put (Stream, "(");
      for I in 1 .. Length loop
         if I /= 1 then
            Put (Stream, ", ");
         end if;
         if Index = Rngs'Last then
            Disp_Value (Stream, El_Rti, Ctxt, Obj, Is_Sig);
         else
            Disp_Array_Value_1
              (Stream, El_Rti, Ctxt, Rngs, Rtis, Index + 1, Obj, Is_Sig);
         end if;
      end loop;
      Put (Stream, ")");
   end Disp_Array_Value_1;

   procedure Disp_Array_Value (Stream : FILEs;
                               Rti : Ghdl_Rtin_Type_Array_Acc;
                               Ctxt : Rti_Context;
                               Vals : Ghdl_Uc_Array_Acc;
                               Is_Sig : Boolean)
   is
      Nbr_Dim : constant Ghdl_Index_Type := Rti.Nbr_Dim;
      Rngs : Ghdl_Range_Array (0 .. Nbr_Dim - 1);
      Obj : Address;
   begin
      Bound_To_Range (Vals.Bounds, Rti, Rngs);
      Obj := Vals.Base;
      Disp_Array_Value_1
        (Stream, Rti.Element, Ctxt, Rngs, Rti.Indexes, 0, Obj, Is_Sig);
   end Disp_Array_Value;

   procedure Disp_Record_Value (Stream : FILEs;
                                Rti : Ghdl_Rtin_Type_Record_Acc;
                                Ctxt : Rti_Context;
                                Obj : Address;
                                Is_Sig : Boolean)
   is
      El : Ghdl_Rtin_Element_Acc;
      El_Addr : Address;
   begin
      Put (Stream, "(");
      for I in 1 .. Rti.Nbrel loop
         El := To_Ghdl_Rtin_Element_Acc (Rti.Elements (I - 1));
         if I /= 1 then
            Put (", ");
         end if;
         Put (Stream, El.Name);
         Put (" => ");
         if Is_Sig then
            El_Addr := Obj + El.Sig_Off;
         else
            El_Addr := Obj + El.Val_Off;
         end if;
         Disp_Value (Stream, El.Eltype, Ctxt, El_Addr, Is_Sig);
      end loop;
      Put (")");
      --  FIXME: update ADDR.
   end Disp_Record_Value;

   procedure Disp_Value
     (Stream : FILEs;
      Rti : Ghdl_Rti_Access;
      Ctxt : Rti_Context;
      Obj : in out Address;
      Is_Sig : Boolean)
   is
   begin
      case Rti.Kind is
         when Ghdl_Rtik_Subtype_Scalar =>
            Disp_Scalar_Value
              (Stream, To_Ghdl_Rtin_Subtype_Scalar_Acc (Rti).Basetype,
               Obj, Is_Sig);
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32
           | Ghdl_Rtik_Type_B2 =>
            Disp_Scalar_Value (Stream, Rti, Obj, Is_Sig);
         when Ghdl_Rtik_Type_Array =>
            Disp_Array_Value (Stream, To_Ghdl_Rtin_Type_Array_Acc (Rti), Ctxt,
                              To_Ghdl_Uc_Array_Acc (Obj), Is_Sig);
         when Ghdl_Rtik_Subtype_Array =>
            declare
               St : constant Ghdl_Rtin_Subtype_Array_Acc :=
                 To_Ghdl_Rtin_Subtype_Array_Acc (Rti);
               Bt : constant Ghdl_Rtin_Type_Array_Acc := St.Basetype;
               Rngs : Ghdl_Range_Array (0 .. Bt.Nbr_Dim - 1);
               B : Address;
            begin
               Bound_To_Range
                 (Loc_To_Addr (St.Common.Depth, St.Bounds, Ctxt), Bt, Rngs);
               B := Obj;
               Disp_Array_Value_1
                 (Stream, Bt.Element, Ctxt, Rngs, Bt.Indexes, 0, B, Is_Sig);
            end;
         when Ghdl_Rtik_Subtype_Array_Ptr =>
            declare
               St : constant Ghdl_Rtin_Subtype_Array_Acc :=
                 To_Ghdl_Rtin_Subtype_Array_Acc (Rti);
               Bt : constant Ghdl_Rtin_Type_Array_Acc := St.Basetype;
               Rngs : Ghdl_Range_Array (0 .. Bt.Nbr_Dim - 1);
               B : Address;
            begin
               Bound_To_Range
                 (Loc_To_Addr (St.Common.Depth, St.Bounds, Ctxt), Bt, Rngs);
               B := To_Addr_Acc (Obj).all;
               Disp_Array_Value_1
                 (Stream, Bt.Element, Ctxt, Rngs, Bt.Indexes, 0, B, Is_Sig);
            end;
         when Ghdl_Rtik_Type_File =>
            declare
               Vptr : Ghdl_Value_Ptr;
            begin
               Vptr := To_Ghdl_Value_Ptr (Obj);
               Put (Stream, "File#");
               Put_I32 (Stream, Vptr.I32);
               --  FIXME: update OBJ (not very useful since never in a
               --   composite type).
            end;
         when Ghdl_Rtik_Type_Record =>
            Disp_Record_Value
              (Stream, To_Ghdl_Rtin_Type_Record_Acc (Rti), Ctxt, Obj, Is_Sig);
         when others =>
            Put (Stream, "??");
      end case;
   end Disp_Value;

   procedure Disp_Kind (Kind : Ghdl_Rtik) is
   begin
      case Kind is
         when Ghdl_Rtik_Top =>
            Put ("ghdl_rtik_top");
         when Ghdl_Rtik_Package =>
            Put ("ghdl_rtik_package");
         when Ghdl_Rtik_Package_Body =>
            Put ("ghdl_rtik_package_body");
         when Ghdl_Rtik_Entity =>
            Put ("ghdl_rtik_entity");
         when Ghdl_Rtik_Architecture =>
            Put ("ghdl_rtik_architecture");

         when Ghdl_Rtik_Port =>
            Put ("ghdl_rtik_port");
         when Ghdl_Rtik_Generic =>
            Put ("ghdl_rtik_generic");
         when Ghdl_Rtik_Process =>
            Put ("ghdl_rtik_process");
         when Ghdl_Rtik_Component =>
            Put ("ghdl_rtik_component");
         when Ghdl_Rtik_Attribute =>
            Put ("ghdl_rtik_attribute");

         when Ghdl_Rtik_Attribute_Quiet =>
            Put ("ghdl_rtik_attribute_quiet");
         when Ghdl_Rtik_Attribute_Stable =>
            Put ("ghdl_rtik_attribute_stable");
         when Ghdl_Rtik_Attribute_Transaction =>
            Put ("ghdl_rtik_attribute_transaction");

         when Ghdl_Rtik_Constant =>
            Put ("ghdl_rtik_constant");
         when Ghdl_Rtik_Iterator =>
            Put ("ghdl_rtik_iterator");
         when Ghdl_Rtik_Signal =>
            Put ("ghdl_rtik_signal");
         when Ghdl_Rtik_Variable =>
            Put ("ghdl_rtik_variable");
         when Ghdl_Rtik_Guard =>
            Put ("ghdl_rtik_guard");
         when Ghdl_Rtik_File =>
            Put ("ghdl_rtik_file");

         when Ghdl_Rtik_Instance =>
            Put ("ghdl_rtik_instance");
         when Ghdl_Rtik_Block =>
            Put ("ghdl_rtik_block");
         when Ghdl_Rtik_If_Generate =>
            Put ("ghdl_rtik_if_generate");
         when Ghdl_Rtik_For_Generate =>
            Put ("ghdl_rtik_for_generate");

         when Ghdl_Rtik_Type_B2 =>
            Put ("ghdl_rtik_type_b2");
         when Ghdl_Rtik_Type_E8 =>
            Put ("ghdl_rtik_type_e8");
         when Ghdl_Rtik_Type_E32 =>
            Put ("ghdl_rtik_type_e32");
         when Ghdl_Rtik_Type_P64 =>
            Put ("ghdl_rtik_type_p64");
         when Ghdl_Rtik_Type_I32 =>
            Put ("ghdl_rtik_type_i32");

         when Ghdl_Rtik_Type_Array =>
            Put ("ghdl_rtik_type_array");
         when Ghdl_Rtik_Subtype_Array =>
            Put ("ghdl_rtik_subtype_array");
         when Ghdl_Rtik_Subtype_Array_Ptr =>
            Put ("ghdl_rtik_subtype_array_ptr");
         when Ghdl_Rtik_Type_Record =>
            Put ("ghdl_rtik_type_record");

         when Ghdl_Rtik_Type_Access =>
            Put ("ghdl_rtik_type_access");
         when Ghdl_Rtik_Type_File =>
            Put ("ghdl_rtik_type_file");

         when Ghdl_Rtik_Subtype_Scalar =>
            Put ("ghdl_rtik_subtype_scalar");

         when Ghdl_Rtik_Element =>
            Put ("ghdl_rtik_element");
         when Ghdl_Rtik_Unit =>
            Put ("ghdl_rtik_unit");

         when others =>
            Put ("ghdl_rtik_#");
            Put_I32 (stdout, Ghdl_Rtik'Pos (Kind));
      end case;
   end Disp_Kind;

   procedure Disp_Depth (Depth : Ghdl_Rti_Depth) is
   begin
      Put (", D=");
      Put_I32 (stdout, Ghdl_I32 (Depth));
   end Disp_Depth;

   procedure Disp_Indent (Indent : Natural) is
   begin
      for I in 1 .. Indent loop
         Put (' ');
      end loop;
   end Disp_Indent;

   --  Disp a subtype_indication.
   --  OBJ may be necessary when the subtype is an unconstrained array type,
   --  whose bounds are stored with the object.
   procedure Disp_Subtype_Indication
     (Def : Ghdl_Rti_Access; Ctxt : Rti_Context; Obj : Address);

   procedure Disp_Range
     (Stream : FILEs; Kind : Ghdl_Rtik; Rng : Ghdl_Range_Ptr)
   is
   begin
      case Kind is
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_P32 =>
            Put_I32 (Stream, Rng.I32.Left);
            Put_Dir (Stream, Rng.I32.Dir);
            Put_I32 (Stream, Rng.I32.Right);
         when Ghdl_Rtik_Type_F64 =>
            Put_F64 (Stream, Rng.F64.Left);
            Put_Dir (Stream, Rng.F64.Dir);
            Put_F64 (Stream, Rng.F64.Right);
         when Ghdl_Rtik_Type_P64 =>
            Put_I64 (Stream, Rng.P64.Left);
            Put_Dir (Stream, Rng.P64.Dir);
            Put_I64 (Stream, Rng.P64.Right);
         when others =>
            Put ("?Scal");
      end case;
   end Disp_Range;

   procedure Disp_Scalar_Type_Name (Def : Ghdl_Rti_Access) is
   begin
      case Def.Kind is
         when Ghdl_Rtik_Subtype_Scalar =>
            declare
               Rti : Ghdl_Rtin_Subtype_Scalar_Acc;
            begin
               Rti := To_Ghdl_Rtin_Subtype_Scalar_Acc (Def);
               if Rti.Name /= null then
                  Disp_Name (Rti.Name);
               else
                  Disp_Scalar_Type_Name (Rti.Basetype);
               end if;
            end;
         when Ghdl_Rtik_Type_B2
           | Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32 =>
            Disp_Name (To_Ghdl_Rtin_Type_Enum_Acc (Def).Name);
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_I64 =>
            Disp_Name (To_Ghdl_Rtin_Type_Scalar_Acc (Def).Name);
         when others =>
            Put ("#disp_scalar_type_name#");
      end case;
   end Disp_Scalar_Type_Name;

   procedure Disp_Type_Array_Name (Def : Ghdl_Rtin_Type_Array_Acc;
                                   Bounds_Ptr : Address)
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

      procedure Disp_Bounds (Def : Ghdl_Rti_Access)
      is
         Ndef : Ghdl_Rti_Access;
      begin
         if Bounds = Null_Address then
            Put ("?");
         else
            if Def.Kind = Ghdl_Rtik_Subtype_Scalar then
               Ndef := To_Ghdl_Rtin_Subtype_Scalar_Acc (Def).Basetype;
            else
               Ndef := Def;
            end if;
            case Ndef.Kind is
               when Ghdl_Rtik_Type_I32 =>
                  Align (Ghdl_Range_I32'Alignment);
                  Disp_Range (stdout, Ndef.Kind, To_Ghdl_Range_Ptr (Bounds));
                  Update (Ghdl_Range_I32'Size);
               when others =>
                  Disp_Kind (Ndef.Kind);
                  --  Bounds are not known anymore.
                  Bounds := Null_Address;
            end case;
         end if;
      end Disp_Bounds;
   begin
      Disp_Name (Def.Name);
      if Bounds_Ptr = Null_Address then
         return;
      end if;
      Put (" (");
      Bounds := Bounds_Ptr;
      for I in 0 .. Def.Nbr_Dim - 1 loop
         if I /= 0 then
            Put (", ");
         end if;
         Disp_Scalar_Type_Name (Def.Indexes (I));
         Put (" range ");
         Disp_Bounds (Def.Indexes (I));
      end loop;
      Put (")");
   end Disp_Type_Array_Name;

   procedure Disp_Subtype_Scalar_Range
     (Stream : FILEs; Def : Ghdl_Rtin_Subtype_Scalar_Acc; Ctxt : Rti_Context)
   is
      Range_Addr : Address;
      Rng : Ghdl_Range_Ptr;
   begin
      Range_Addr := Loc_To_Addr (Def.Common.Depth,
                                 Def.Range_Loc, Ctxt);
      Rng := To_Ghdl_Range_Ptr (Range_Addr);
      Disp_Range (Stream, Def.Basetype.Kind, Rng);
   end Disp_Subtype_Scalar_Range;

   procedure Disp_Subtype_Indication
     (Def : Ghdl_Rti_Access; Ctxt : Rti_Context; Obj : Address)
   is
   begin
      case Def.Kind is
         when Ghdl_Rtik_Subtype_Scalar =>
            declare
               Rti : Ghdl_Rtin_Subtype_Scalar_Acc;
            begin
               Rti := To_Ghdl_Rtin_Subtype_Scalar_Acc (Def);
               if Rti.Name /= null then
                  Disp_Name (Rti.Name);
               else
                  Disp_Subtype_Indication
                    (Rti.Basetype, Null_Context, Null_Address);
                  Put (" range ");
                  Disp_Subtype_Scalar_Range (stdout, Rti, Ctxt);
               end if;
            end;
            --Disp_Scalar_Subtype_Name (To_Ghdl_Rtin_Scalsubtype_Acc (Def),
            --                          Base);
         when Ghdl_Rtik_Type_B2
           | Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32 =>
            Disp_Name (To_Ghdl_Rtin_Type_Enum_Acc (Def).Name);
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_I64 =>
            Disp_Name (To_Ghdl_Rtin_Type_Scalar_Acc (Def).Name);
         when Ghdl_Rtik_Type_File
           | Ghdl_Rtik_Type_Access =>
            Disp_Name (To_Ghdl_Rtin_Type_Fileacc_Acc (Def).Name);
         when Ghdl_Rtik_Type_Record =>
            Disp_Name (To_Ghdl_Rtin_Type_Record_Acc (Def).Name);
         when Ghdl_Rtik_Type_Array =>
            declare
               Bounds : Address;
            begin
               if Obj = Null_Address then
                  Bounds := Null_Address;
               else
                  Bounds := To_Ghdl_Uc_Array_Acc (Obj).Bounds;
               end if;
               Disp_Type_Array_Name (To_Ghdl_Rtin_Type_Array_Acc (Def),
                                     Bounds);
            end;
         when Ghdl_Rtik_Subtype_Array
           | Ghdl_Rtik_Subtype_Array_Ptr =>
            declare
               Sdef : Ghdl_Rtin_Subtype_Array_Acc;
            begin
               Sdef := To_Ghdl_Rtin_Subtype_Array_Acc (Def);
               if Sdef.Name /= null then
                  Disp_Name (Sdef.Name);
               else
                  Disp_Type_Array_Name
                    (Sdef.Basetype,
                     Loc_To_Addr (Sdef.Common.Depth, Sdef.Bounds, Ctxt));
               end if;
            end;
         when others =>
            Disp_Kind (Def.Kind);
            Put (' ');
      end case;
   end Disp_Subtype_Indication;


   procedure Disp_Rti (Rti : Ghdl_Rti_Access;
                       Ctxt : Rti_Context;
                       Indent : Natural);

   procedure Disp_Rti_Arr (Nbr : Ghdl_Index_Type;
                           Arr : Ghdl_Rti_Arr_Acc;
                           Ctxt : Rti_Context;
                           Indent : Natural)
   is
   begin
      for I in 1 .. Nbr loop
         Disp_Rti (Arr (I - 1), Ctxt, Indent);
      end loop;
   end Disp_Rti_Arr;

   procedure Disp_Block (Blk : Ghdl_Rtin_Block_Acc;
                         Ctxt : Rti_Context;
                         Indent : Natural)
   is
      Nctxt : Rti_Context;
   begin
      Disp_Indent (Indent);
      Disp_Kind (Blk.Common.Kind);
      Disp_Depth (Blk.Common.Depth);
      Put (": ");
      Disp_Name (Blk.Name);
      New_Line;
      if Blk.Parent /= null then
         case Blk.Common.Kind is
            when Ghdl_Rtik_Architecture =>
               --  Disp entity.
               Disp_Rti (Blk.Parent, Ctxt, Indent + 1);
            when others =>
               null;
         end case;
      end if;
      case Blk.Common.Kind is
         when Ghdl_Rtik_Package
           | Ghdl_Rtik_Package_Body
           | Ghdl_Rtik_Entity
           | Ghdl_Rtik_Architecture
           | Ghdl_Rtik_Block
           | Ghdl_Rtik_Process =>
            Nctxt := (Base => Ctxt.Base + Blk.Loc.Off,
                      Block => To_Ghdl_Rti_Access (Blk));
            Disp_Rti_Arr (Blk.Nbr_Child, Blk.Children,
                          Nctxt, Indent + 1);
         when Ghdl_Rtik_For_Generate =>
            declare
               Length : Ghdl_Index_Type;
            begin
               Nctxt := (Base => To_Addr_Acc (Ctxt.Base + Blk.Loc.Off).all,
                         Block => To_Ghdl_Rti_Access (Blk));
               Length := Get_For_Generate_Length (Blk, Ctxt);
               for I in 1 .. Length loop
                  Disp_Rti_Arr (Blk.Nbr_Child, Blk.Children,
                                Nctxt, Indent + 1);
                  Nctxt.Base := Nctxt.Base + Blk.Size;
               end loop;
            end;
         when Ghdl_Rtik_If_Generate =>
            Nctxt := (Base => To_Addr_Acc (Ctxt.Base + Blk.Loc.Off).all,
                      Block => To_Ghdl_Rti_Access (Blk));
            if Nctxt.Base /= Null_Address then
               Disp_Rti_Arr (Blk.Nbr_Child, Blk.Children,
                             Nctxt, Indent + 1);
            end if;
         when others =>
            Internal_Error ("disp_block");
      end case;
   end Disp_Block;

   procedure Disp_Object (Obj : Ghdl_Rtin_Object_Acc;
                          Is_Sig : Boolean;
                          Ctxt : Rti_Context;
                          Indent : Natural)
   is
      Addr : Address;
      Obj_Type : Ghdl_Rti_Access;
   begin
      Disp_Indent (Indent);
      Disp_Kind (Obj.Common.Kind);
      Disp_Depth (Obj.Common.Depth);
      Put ("; ");
      Disp_Name (Obj.Name);
      Put (": ");
      Addr := Loc_To_Addr (Obj.Common.Depth, Obj.Loc, Ctxt);
      Obj_Type := Obj.Obj_Type;
      Disp_Subtype_Indication (Obj_Type, Ctxt, Addr);
      Put (" := ");

      --  FIXME: put this into a function.
      if (Obj_Type.Kind = Ghdl_Rtik_Subtype_Array
          or Obj_Type.Kind = Ghdl_Rtik_Type_Record)
        and then Obj_Type.Mode = 1
      then
         Addr := To_Addr_Acc (Addr).all;
      end if;
      Disp_Value (stdout, Obj_Type, Ctxt, Addr, Is_Sig);
      New_Line;
   end Disp_Object;

   procedure Disp_Attribute (Obj : Ghdl_Rtin_Object_Acc;
                             Ctxt : Rti_Context;
                             Indent : Natural)
   is
   begin
      Disp_Indent (Indent);
      Disp_Kind (Obj.Common.Kind);
      Disp_Depth (Obj.Common.Depth);
      Put ("; ");
      Disp_Name (Obj.Name);
      Put (": ");
      Disp_Subtype_Indication (Obj.Obj_Type, Ctxt, Null_Address);
      New_Line;
   end Disp_Attribute;

   procedure Disp_Component (Comp : Ghdl_Rtin_Component_Acc;
                             Indent : Natural)
   is
   begin
      Disp_Indent (Indent);
      Disp_Kind (Comp.Common.Kind);
      Disp_Depth (Comp.Common.Depth);
      Put (": ");
      Disp_Name (Comp.Name);
      New_Line;
      --Disp_Rti_Arr (Comp.Nbr_Child, Comp.Children, Base, Ident + 1);
   end Disp_Component;

   procedure Disp_Instance (Inst : Ghdl_Rtin_Instance_Acc;
                            Ctxt : Rti_Context;
                            Indent : Natural)
   is
      Inst_Addr : Address;
      Inst_Base : Address;
      Inst_Rti : Ghdl_Rti_Access;
      Nindent : Natural;
      Nctxt : Rti_Context;
   begin
      Disp_Indent (Indent);
      Disp_Kind (Inst.Common.Kind);
      Put (": ");
      Disp_Name (Inst.Name);
      New_Line;

      Inst_Addr := Ctxt.Base + Inst.Loc.Off;
      --  Read sub instance.
      Inst_Base := To_Addr_Acc (Inst_Addr).all;

      Nindent := Indent + 1;

      case Inst.Instance.Kind is
         when Ghdl_Rtik_Component =>
            declare
               Comp : Ghdl_Rtin_Component_Acc;
            begin
               Comp := To_Ghdl_Rtin_Component_Acc (Inst.Instance);
               Disp_Indent (Nindent);
               Disp_Kind (Comp.Common.Kind);
               Put (": ");
               Disp_Name (Comp.Name);
               New_Line;
               --  Disp components generics and ports.
               --  FIXME: the data to disp are at COMP_BASE.
               Nctxt := (Base => Inst_Addr,
                         Block => Inst.Instance);
               Nindent := Nindent + 1;
               Disp_Rti_Arr (Comp.Nbr_Child, Comp.Children, Nctxt, Nindent);
               Nindent := Nindent + 1;
            end;
         when Ghdl_Rtik_Entity =>
            null;
         when others =>
            null;
      end case;

      --  Read instance RTI.
      if Inst_Base /= Null_Address then
         Inst_Rti := To_Ghdl_Rti_Acc_Acc (Inst_Base).all;
         Nctxt := (Base => Inst_Base,
                   Block => Inst_Rti);
         Disp_Block (To_Ghdl_Rtin_Block_Acc (Inst_Rti),
                     Nctxt, Nindent);
      end if;
   end Disp_Instance;

   procedure Disp_Type_Enum_Decl (Enum : Ghdl_Rtin_Type_Enum_Acc;
                                  Indent : Natural)
   is
   begin
      Disp_Indent (Indent);
      Disp_Kind (Enum.Common.Kind);
      Put (": ");
      Disp_Name (Enum.Name);
      Put (" is (");
      Disp_Name (Enum.Names (0));
      for I in 1 .. Enum.Nbr - 1 loop
         Put (", ");
         Disp_Name (Enum.Names (I));
      end loop;
      Put (")");
      New_Line;
   end Disp_Type_Enum_Decl;

   procedure Disp_Subtype_Scalar_Decl (Def : Ghdl_Rtin_Subtype_Scalar_Acc;
                                       Ctxt : Rti_Context;
                                       Indent : Natural)
   is
      Bt : Ghdl_Rti_Access;
   begin
      Disp_Indent (Indent);
      Disp_Kind (Def.Common.Kind);
      Disp_Depth (Def.Common.Depth);
      Put (": ");
      Disp_Name (Def.Name);
      Put (" is ");
      Bt := Def.Basetype;
      case Bt.Kind is
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_F64 =>
            declare
               Bdef : Ghdl_Rtin_Type_Scalar_Acc;
            begin
               Bdef := To_Ghdl_Rtin_Type_Scalar_Acc (Bt);
               if Bdef.Name /= Def.Name then
                  Disp_Name (Bdef.Name);
                  Put (" range ");
               end if;
               --  This is the type definition.
               Disp_Subtype_Scalar_Range (stdout, Def, Ctxt);
            end;
         when Ghdl_Rtik_Type_P64
           | Ghdl_Rtik_Type_P32 =>
            declare
               Bdef : Ghdl_Rtin_Type_Physical_Acc;
               Unit : Ghdl_Rtin_Unit_Acc;
            begin
               Bdef := To_Ghdl_Rtin_Type_Physical_Acc (Bt);
               if Bdef.Name /= Def.Name then
                  Disp_Name (Bdef.Name);
                  Put (" range ");
               end if;
               --  This is the type definition.
               Disp_Subtype_Scalar_Range (stdout, Def, Ctxt);
               if Bdef.Name = Def.Name then
                  for I in 0 .. Bdef.Nbr - 1 loop
                     Unit := To_Ghdl_Rtin_Unit_Acc (Bdef.Units (I));
                     New_Line;
                     Disp_Indent (Indent + 1);
                     Disp_Kind (Unit.Common.Kind);
                     Put (": ");
                     Disp_Name (Unit.Name);
                     Put (" = ");
                     case Bt.Kind is
                        when Ghdl_Rtik_Type_P64 =>
                           if Bt.Mode = 0 then
                              Put_I64 (stdout, Unit.Value.Unit_64);
                           else
                              Put_I64 (stdout, Unit.Value.Unit_Addr.I64);
                           end if;
                        when Ghdl_Rtik_Type_P32 =>
                           if Bt.Mode = 0 then
                              Put_I32 (stdout, Unit.Value.Unit_32);
                           else
                              Put_I32 (stdout, Unit.Value.Unit_Addr.I32);
                           end if;
                        when others =>
                           null;
                     end case;
                  end loop;
               end if;
            end;
         when others =>
            Disp_Subtype_Indication
              (To_Ghdl_Rti_Access (Def), Ctxt, Null_Address);
      end case;
      New_Line;
   end Disp_Subtype_Scalar_Decl;

   procedure Disp_Type_Array_Decl (Def : Ghdl_Rtin_Type_Array_Acc;
                                   Ctxt : Rti_Context;
                                   Indent : Natural)
   is
   begin
      Disp_Indent (Indent);
      Disp_Kind (Def.Common.Kind);
      Put (": ");
      Disp_Name (Def.Name);
      Put (" is array (");
      for I in 0 .. Def.Nbr_Dim - 1 loop
         if I /= 0 then
            Put (", ");
         end if;
         Disp_Subtype_Indication (Def.Indexes (I), Ctxt, Null_Address);
         Put (" range <>");
      end loop;
      Put (") of ");
      Disp_Subtype_Indication (Def.Element, Ctxt, Null_Address);
      New_Line;
   end Disp_Type_Array_Decl;

   procedure Disp_Subtype_Array_Decl (Def : Ghdl_Rtin_Subtype_Array_Acc;
                                      Ctxt : Rti_Context;
                                      Indent : Natural)
   is
   begin
      Disp_Indent (Indent);
      Disp_Kind (Def.Common.Kind);
      Put (": ");
      Disp_Name (Def.Name);
      Put (" is ");
      Disp_Type_Array_Name
        (Def.Basetype, Loc_To_Addr (Def.Common.Depth, Def.Bounds, Ctxt));
      --  FIXME: If the subtype array contains a type array, then the
      --  definition is not complete: display the element type.
      New_Line;
   end Disp_Subtype_Array_Decl;

   procedure Disp_Type_File_Or_Access (Def : Ghdl_Rtin_Type_Fileacc_Acc;
                                       Ctxt : Rti_Context;
                                       Indent : Natural)
   is
   begin
      Disp_Indent (Indent);
      Disp_Kind (Def.Common.Kind);
      Put (": ");
      Disp_Name (Def.Name);
      Put (" is ");
      case Def.Common.Kind is
         when Ghdl_Rtik_Type_Access =>
            Put ("access ");
         when Ghdl_Rtik_Type_File =>
            Put ("file ");
         when others =>
            Put ("?? ");
      end case;
      Disp_Subtype_Indication (Def.Base, Ctxt, Null_Address);
      New_Line;
   end Disp_Type_File_Or_Access;

   procedure Disp_Type_Record (Def : Ghdl_Rtin_Type_Record_Acc;
                               Ctxt : Rti_Context;
                               Indent : Natural)
   is
      El : Ghdl_Rtin_Element_Acc;
   begin
      Disp_Indent (Indent);
      Disp_Kind (Def.Common.Kind);
      Put (": ");
      Disp_Name (Def.Name);
      Put (" is record");
      New_Line;
      for I in 1 .. Def.Nbrel loop
         El := To_Ghdl_Rtin_Element_Acc (Def.Elements (I - 1));
         Disp_Indent (Indent + 1);
         Disp_Kind (El.Common.Kind);
         Put (": ");
         Disp_Name (El.Name);
         Put (": ");
         Disp_Subtype_Indication (El.Eltype, Ctxt, Null_Address);
         New_Line;
      end loop;
   end Disp_Type_Record;

   procedure Disp_Rti (Rti : Ghdl_Rti_Access;
                       Ctxt : Rti_Context;
                       Indent : Natural)
   is
   begin
      if Rti = null then
         return;
      end if;

      case Rti.Kind is
         when Ghdl_Rtik_Entity
           | Ghdl_Rtik_Architecture
           | Ghdl_Rtik_Package
           | Ghdl_Rtik_Process
           | Ghdl_Rtik_Block
           | Ghdl_Rtik_If_Generate
           | Ghdl_Rtik_For_Generate =>
            Disp_Block (To_Ghdl_Rtin_Block_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Package_Body =>
            Disp_Rti (To_Ghdl_Rtin_Block_Acc (Rti).Parent, Ctxt, Indent);
            Disp_Block (To_Ghdl_Rtin_Block_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Port
           | Ghdl_Rtik_Signal
           | Ghdl_Rtik_Guard
           | Ghdl_Rtik_Attribute_Quiet
           | Ghdl_Rtik_Attribute_Stable
           | Ghdl_Rtik_Attribute_Transaction =>
            Disp_Object (To_Ghdl_Rtin_Object_Acc (Rti), True, Ctxt, Indent);
         when Ghdl_Rtik_Generic
           | Ghdl_Rtik_Constant
           | Ghdl_Rtik_Variable
           | Ghdl_Rtik_Iterator
           | Ghdl_Rtik_File =>
            Disp_Object (To_Ghdl_Rtin_Object_Acc (Rti), False, Ctxt, Indent);
         when Ghdl_Rtik_Component =>
            Disp_Component (To_Ghdl_Rtin_Component_Acc (Rti), Indent);
         when Ghdl_Rtik_Attribute =>
            Disp_Attribute (To_Ghdl_Rtin_Object_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Instance =>
            Disp_Instance (To_Ghdl_Rtin_Instance_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Type_B2
           | Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32 =>
            Disp_Type_Enum_Decl (To_Ghdl_Rtin_Type_Enum_Acc (Rti), Indent);
         when Ghdl_Rtik_Subtype_Scalar =>
            Disp_Subtype_Scalar_Decl (To_Ghdl_Rtin_Subtype_Scalar_Acc (Rti),
                                      Ctxt, Indent);
         when Ghdl_Rtik_Type_Array =>
            Disp_Type_Array_Decl
              (To_Ghdl_Rtin_Type_Array_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Subtype_Array
           | Ghdl_Rtik_Subtype_Array_Ptr =>
            Disp_Subtype_Array_Decl
              (To_Ghdl_Rtin_Subtype_Array_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Type_Access
           | Ghdl_Rtik_Type_File =>
            Disp_Type_File_Or_Access
              (To_Ghdl_Rtin_Type_Fileacc_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Type_Record =>
            Disp_Type_Record
              (To_Ghdl_Rtin_Type_Record_Acc (Rti), Ctxt, Indent);
         when others =>
            Disp_Indent (Indent);
            Disp_Kind (Rti.Kind);
            Put_Line (" ? ");
      end case;
   end Disp_Rti;

   Disp_Rti_Flag : Boolean := False;

   procedure Disp_All
   is
      Ctxt : Rti_Context;
   begin
      if not Disp_Rti_Flag then
         return;
      end if;

      Put ("DISP_RTI.Disp_All: ");
      Disp_Kind (Ghdl_Rti_Top_Ptr.Common.Kind);
      New_Line;
      Ctxt := (Base => To_Address (Ghdl_Rti_Top_Instance),
               Block => Ghdl_Rti_Top_Ptr.Parent);
      Disp_Rti_Arr (Ghdl_Rti_Top_Ptr.Nbr_Child,
                    Ghdl_Rti_Top_Ptr.Children,
                    Ctxt, 0);
      Disp_Rti (Ghdl_Rti_Top_Ptr.Parent, Ctxt, 0);

      --Disp_Hierarchy;
   end Disp_All;

   function Disp_Rti_Option (Opt : String) return Boolean
   is
   begin
      if Opt = "--dump-rti" then
         Disp_Rti_Flag := True;
         return True;
      else
         return False;
      end if;
   end Disp_Rti_Option;

   procedure Disp_Rti_Help
   is
      procedure P (Str : String) renames Put_Line;
   begin
      P (" --dump-rti        dump Run Time Information");
   end Disp_Rti_Help;

   Disp_Rti_Hooks : aliased constant Hooks_Type :=
     (Option => Disp_Rti_Option'Access,
      Help => Disp_Rti_Help'Access,
      Init => null,
      Start => Disp_All'Access,
      Finish => null);

   procedure Register is
   begin
      Register_Hooks (Disp_Rti_Hooks'Access);
   end Register;

end Grt.Disp_Rti;
