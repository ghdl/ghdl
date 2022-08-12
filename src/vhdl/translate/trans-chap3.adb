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

with Name_Table;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Evaluation; use Vhdl.Evaluation;
with Trans.Chap2;
with Trans.Chap4;
with Trans.Chap6;
with Trans.Chap7;
with Trans.Chap14;
with Trans_Decls; use Trans_Decls;
with Trans.Helpers2; use Trans.Helpers2;

package body Trans.Chap3 is
   use Trans.Helpers;

   function Create_Static_Type_Definition_Type_Range (Def : Iir)
                                                      return O_Cnode;
   procedure Elab_Scalar_Type_Range (Def : Iir; Target : O_Lnode);

   --  For scalar subtypes: creates info from the base type.
   procedure Create_Subtype_Info_From_Type (Def          : Iir;
                                            Base         : Iir;
                                            Subtype_Info : Type_Info_Acc);

   function Get_Composite_Type_Layout (Info : Type_Info_Acc) return Mnode
   is
      Res : O_Lnode;
   begin
      if Info.S.Subtype_Owner /= null then
         pragma Assert (Info.S.Composite_Layout = Null_Var);
         Res := M2Lv (Get_Composite_Type_Layout (Info.S.Subtype_Owner));
         if Info.S.Owner_Field = null then
            --  From an array.
            Res := New_Selected_Element
              (Res, Info.S.Subtype_Owner.B.Layout_Bounds);
            Res := New_Selected_Element
              (Res, Info.S.Subtype_Owner.B.Bounds_El);
         else
            --  From a record
            Res := New_Selected_Element
              (Res, Info.S.Owner_Field.Field_Bound);
         end if;
      else
         pragma Assert (Info.S.Composite_Layout /= Null_Var);
         Res := Get_Var (Info.S.Composite_Layout);
      end if;
      return Lv2M (Res,
                   Info, Mode_Value,
                   Info.B.Layout_Type,
                   Info.B.Layout_Ptr_Type);
   end Get_Composite_Type_Layout;

   function Get_Composite_Type_Layout_Alloc (Info : Type_Info_Acc)
                                             return Allocation_Kind is
   begin
      if Info.S.Subtype_Owner /= null then
         return Get_Composite_Type_Layout_Alloc (Info.S.Subtype_Owner);
      else
         return Get_Alloc_Kind_For_Var (Info.S.Composite_Layout);
      end if;
   end Get_Composite_Type_Layout_Alloc;

   function Layout_To_Bounds (B : Mnode) return Mnode
   is
      Info : constant Type_Info_Acc := Get_Type_Info (B);
   begin
      case Info.Type_Mode is
         when Type_Mode_Arrays =>
            return Lv2M (New_Selected_Element (M2Lv (B), Info.B.Layout_Bounds),
                         Info, Mode_Value,
                         Info.B.Bounds_Type, Info.B.Bounds_Ptr_Type);
         when Type_Mode_Records =>
            return B;
         when others =>
            raise Internal_Error;
      end case;
   end Layout_To_Bounds;

   function Layout_To_Sizes (B : Mnode) return O_Lnode
   is
      Info : constant Type_Info_Acc := Get_Type_Info (B);
   begin
      return New_Selected_Element (M2Lv (B), Info.B.Layout_Size);
   end Layout_To_Sizes;

   function Layout_To_Sizes (B : Mnode) return Mnode is
   begin
      return Lv2M (Layout_To_Sizes (B), Get_Type_Info (B), Mode_Value,
                   Ghdl_Sizes_Type, Ghdl_Sizes_Ptr);
   end Layout_To_Sizes;

   function Sizes_To_Size (Sizes : O_Lnode; Kind : Object_Kind_Type)
                          return O_Lnode
   is
      Field : O_Fnode;
   begin
      case Kind is
         when Mode_Value =>
            Field := Ghdl_Sizes_Val;
         when Mode_Signal =>
            Field := Ghdl_Sizes_Sig;
      end case;
      return New_Selected_Element (Sizes, Field);
   end Sizes_To_Size;

   function Layout_To_Size (Layout : Mnode; Kind : Object_Kind_Type)
                           return O_Lnode is
   begin
      return Sizes_To_Size (M2Lv (Layout_To_Sizes (Layout)), Kind);
   end Layout_To_Size;

   function Record_Layout_To_Element_Layout (B : Mnode; El : Iir) return Mnode
   is
      El_Type : constant Iir := Get_Type (El);
      El_Info : constant Field_Info_Acc := Get_Info (El);
      El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
   begin
      return Lv2M (New_Selected_Element (M2Lv (B),
                                         El_Info.Field_Bound),
                   El_Tinfo, Mode_Value,
                   El_Tinfo.B.Layout_Type, El_Tinfo.B.Layout_Ptr_Type);
   end Record_Layout_To_Element_Layout;

   function Record_Layout_To_Element_Offset
     (B : Mnode; El : Iir; Kind : Object_Kind_Type) return O_Lnode
   is
      El_Info : constant Field_Info_Acc := Get_Info (El);
   begin
      return New_Selected_Element (M2Lv (B), El_Info.Field_Node (Kind));
   end Record_Layout_To_Element_Offset;

   function Array_Bounds_To_Element_Layout (B : Mnode; Arr_Type : Iir)
                                           return Mnode
   is
      Arr_Tinfo : constant Type_Info_Acc := Get_Info (Arr_Type);
      El_Type : constant Iir := Get_Element_Subtype (Arr_Type);
      El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
   begin
      return Lv2M (New_Selected_Element (M2Lv (B), Arr_Tinfo.B.Bounds_El),
                   El_Tinfo, Mode_Value,
                   El_Tinfo.B.Layout_Type, El_Tinfo.B.Layout_Ptr_Type);
   end Array_Bounds_To_Element_Layout;

   function Array_Layout_To_Element_Layout (B : Mnode; Arr_Type : Iir)
                                           return Mnode is
   begin
      return Array_Bounds_To_Element_Layout (Layout_To_Bounds (B), Arr_Type);
   end Array_Layout_To_Element_Layout;

   procedure Declare_Value_Type (Info : Type_Info_Acc) is
   begin
      New_Type_Decl (Create_Identifier, Info.Ortho_Type (Mode_Value));
   end Declare_Value_Type;

   procedure Declare_Signal_Type (Info : Type_Info_Acc) is
   begin
      if Info.Ortho_Type (Mode_Signal) /= O_Tnode_Null then
         New_Type_Decl (Create_Identifier ("SIG"),
                        Info.Ortho_Type (Mode_Signal));
      end if;
   end Declare_Signal_Type;

   procedure Declare_Value_Ptr_Type (Info : Type_Info_Acc) is
   begin
      Info.Ortho_Ptr_Type (Mode_Value) :=
        New_Access_Type (Info.Ortho_Type (Mode_Value));
      New_Type_Decl (Create_Identifier ("PTR"),
                     Info.Ortho_Ptr_Type (Mode_Value));
   end Declare_Value_Ptr_Type;

   procedure Declare_Signal_Ptr_Type (Info : Type_Info_Acc) is
   begin
      if Info.Ortho_Type (Mode_Signal) /= O_Tnode_Null then
         Info.Ortho_Ptr_Type (Mode_Signal) :=
           New_Access_Type (Info.Ortho_Type (Mode_Signal));
         New_Type_Decl (Create_Identifier ("SIGPTR"),
                        Info.Ortho_Ptr_Type (Mode_Signal));
      else
         Info.Ortho_Ptr_Type (Mode_Signal) := O_Tnode_Null;
      end if;
   end Declare_Signal_Ptr_Type;

   --  Finish a type definition: declare the type, define and declare a
   --   pointer to the type.
   procedure Finish_Type_Definition
     (Info : Type_Info_Acc; Completion : Boolean := False) is
   begin
      --  Declare the type.
      if not Completion then
         Declare_Value_Type (Info);
      end if;

      --  Create an access to the type and declare it.
      Declare_Value_Ptr_Type (Info);

      --  Signal type.
      if Info.Type_Mode in Type_Mode_Scalar then
         Info.Ortho_Type (Mode_Signal) := Ghdl_Signal_Ptr;
         Info.Ortho_Ptr_Type (Mode_Signal) := O_Tnode_Null;
      else
         Declare_Signal_Type (Info);
         Declare_Signal_Ptr_Type (Info);
      end if;
   end Finish_Type_Definition;

   --  A builder set internal fields of object pointed by BASE_PTR, using
   --  memory from BASE_PTR and returns a pointer to the next memory byte
   --  to be used.
   procedure Create_Builder_Subprogram_Decl (Info : Type_Info_Acc;
                                             Name : Name_Id;
                                             Kind : Object_Kind_Type)
   is
      Interface_List : O_Inter_List;
      Ident          : O_Ident;
   begin
      case Kind is
         when Mode_Value =>
            Ident := Create_Identifier (Name, "_BUILDER");
         when Mode_Signal =>
            Ident := Create_Identifier (Name, "_SIGBUILDER");
      end case;
      --  FIXME: return the same type as its first parameter ???
      Start_Procedure_Decl (Interface_List, Ident, Global_Storage);
      Subprgs.Add_Subprg_Instance_Interfaces
        (Interface_List, Info.B.Builder (Kind).Builder_Instance);
      New_Interface_Decl
        (Interface_List, Info.B.Builder (Kind).Builder_Layout_Param,
         Get_Identifier ("layout_ptr"), Info.B.Layout_Ptr_Type);
      Finish_Subprogram_Decl
        (Interface_List, Info.B.Builder (Kind).Builder_Proc);
   end Create_Builder_Subprogram_Decl;

   procedure Gen_Call_Type_Builder
     (Layout : Mnode; Var_Type : Iir; Kind : Object_Kind_Type)
   is
      Binfo : constant Type_Info_Acc := Get_Info (Get_Base_Type (Var_Type));
      Assoc : O_Assoc_List;
   begin
      Start_Association (Assoc, Binfo.B.Builder (Kind).Builder_Proc);
      Subprgs.Add_Subprg_Instance_Assoc
        (Assoc, Binfo.B.Builder (Kind).Builder_Instance);
      New_Association (Assoc, M2Addr (Layout));
      New_Procedure_Call (Assoc);
   end Gen_Call_Type_Builder;

   ------------------
   --  Enumeration --
   ------------------

   procedure Set_Ortho_Literal (Target : Iir; Expr : O_Cnode)
   is
      Info : Ortho_Info_Acc;
   begin
      Info := Add_Info (Target, Kind_Enum_Lit);
      Info.Lit_Node := Expr;
   end Set_Ortho_Literal;

   function Translate_Enumeration_Literal (Lit : Iir_Enumeration_Literal)
                                           return O_Ident
   is
      El_Str : String (1 .. 4);
      Id     : Name_Id;
      N      : Integer;
      C      : Character;
   begin
      Id := Get_Identifier (Lit);
      if Name_Table.Is_Character (Id) then
         C := Name_Table.Get_Character (Id);
         El_Str (1) := 'C';
         case C is
            when 'A' .. 'Z'
               | 'a' .. 'z'
               | '0' .. '9' =>
               El_Str (2) := '_';
               El_Str (3) := C;
            when others =>
               N := Character'Pos (Name_Table.Get_Character (Id));
               El_Str (2) := N2hex (N / 16);
               El_Str (3) := N2hex (N mod 16);
         end case;
         return Get_Identifier (El_Str (1 .. 3));
      else
         return Create_Identifier_Without_Prefix (Lit);
      end if;
   end Translate_Enumeration_Literal;

   procedure Translate_Enumeration_Type
     (Def : Iir_Enumeration_Type_Definition)
   is
      El_List  : constant Iir_Flist := Get_Enumeration_Literal_List (Def);
      Nbr      : constant Natural := Get_Nbr_Elements (El_List);
      Info     : constant Type_Info_Acc := Get_Info (Def);
      El       : Iir_Enumeration_Literal;
      Constr   : O_Enum_List;
      Lit_Name : O_Ident;
      Val      : O_Cnode;
      Size     : Natural;
   begin
      if Nbr <= 256 then
         Size := 8;
      else
         Size := 32;
      end if;
      Start_Enum_Type (Constr, Size);
      for I in Flist_First .. Flist_Last (El_List) loop
         El := Get_Nth_Element (El_List, I);

         Lit_Name := Translate_Enumeration_Literal (El);
         New_Enum_Literal (Constr, Lit_Name, Val);
         Set_Ortho_Literal (El, Val);
      end loop;
      Finish_Enum_Type (Constr, Info.Ortho_Type (Mode_Value));
      if Nbr <= 256 then
         Info.Type_Mode := Type_Mode_E8;
         Info.B.Align := Align_8;
      else
         Info.Type_Mode := Type_Mode_E32;
         Info.B.Align := Align_32;
      end if;
      --  Enumerations are always in their range.
      Info.S.Nocheck_Low := True;
      Info.S.Nocheck_Hi := True;
      Finish_Type_Definition (Info);
   end Translate_Enumeration_Type;

   procedure Translate_Bool_Type (Def : Iir_Enumeration_Type_Definition)
   is
      Info    : constant Type_Info_Acc := Get_Info (Def);
      El_List : constant Iir_Flist := Get_Enumeration_Literal_List (Def);
      pragma Assert (Get_Nbr_Elements (El_List) = 2);

      False_Lit : constant Iir := Get_Nth_Element (El_List, 0);
      True_Lit  : constant Iir := Get_Nth_Element (El_List, 1);

      False_Node, True_Node : O_Cnode;
   begin
      New_Boolean_Type
        (Info.Ortho_Type (Mode_Value),
         Translate_Enumeration_Literal (False_Lit), False_Node,
         Translate_Enumeration_Literal (True_Lit), True_Node);
      Info.Type_Mode := Type_Mode_B1;
      Set_Ortho_Literal (False_Lit, False_Node);
      Set_Ortho_Literal (True_Lit, True_Node);
      Info.S.Nocheck_Low := True;
      Info.S.Nocheck_Hi := True;
      Info.B.Align := Align_8;
      Finish_Type_Definition (Info);
   end Translate_Bool_Type;

   ---------------
   --  Integer  --
   ---------------

   procedure Translate_Integer_Type (Def : Iir_Integer_Type_Definition)
   is
      Info : constant Type_Info_Acc := Get_Info (Def);
   begin
      case Get_Scalar_Size (Def) is
         when Scalar_32 =>
            Info.Ortho_Type (Mode_Value) := New_Signed_Type (32);
            Info.Type_Mode := Type_Mode_I32;
            Info.B.Align := Align_32;
         when Scalar_64 =>
            Info.Ortho_Type (Mode_Value) := New_Signed_Type (64);
            Info.Type_Mode := Type_Mode_I64;
            Info.B.Align := Align_64;
         when others =>
            raise Internal_Error;
      end case;
      --  Integers are always in their ranges.
      Info.S.Nocheck_Low := True;
      Info.S.Nocheck_Hi := True;

      Finish_Type_Definition (Info);
   end Translate_Integer_Type;

   ----------------------
   --  Floating types  --
   ----------------------

   procedure Translate_Floating_Type (Def : Iir_Floating_Type_Definition)
   is
      Info : constant Type_Info_Acc := Get_Info (Def);
   begin
      --  FIXME: should check precision
      Info.Type_Mode := Type_Mode_F64;
      Info.B.Align := Align_64;
      Info.Ortho_Type (Mode_Value) := New_Float_Type;
      --  Reals are always in their ranges.
      Info.S.Nocheck_Low := True;
      Info.S.Nocheck_Hi := True;

      Finish_Type_Definition (Info);
   end Translate_Floating_Type;

   ----------------
   --  Physical  --
   ----------------

   procedure Translate_Physical_Type (Def : Iir_Physical_Type_Definition)
   is
      Info : constant Type_Info_Acc := Get_Info (Def);
   begin
      case Get_Scalar_Size (Def) is
         when Scalar_32 =>
            Info.Ortho_Type (Mode_Value) := New_Signed_Type (32);
            Info.Type_Mode := Type_Mode_P32;
            Info.B.Align := Align_32;
         when Scalar_64 =>
            Info.Ortho_Type (Mode_Value) := New_Signed_Type (64);
            Info.Type_Mode := Type_Mode_P64;
            Info.B.Align := Align_64;
         when others =>
            raise Internal_Error;
      end case;
      --  Physical types are always in their ranges.
      Info.S.Nocheck_Low := True;
      Info.S.Nocheck_Hi := True;

      Finish_Type_Definition (Info);
   end Translate_Physical_Type;

   procedure Translate_Physical_Units (Def : Iir_Physical_Type_Definition)
   is
      Phy_Type : constant O_Tnode := Get_Ortho_Type (Def, Mode_Value);
      Unit     : Iir;
      Info     : Object_Info_Acc;
   begin
      Unit := Get_Unit_Chain (Def);
      while Unit /= Null_Iir loop
         Info := Add_Info (Unit, Kind_Object);
         Info.Object_Var :=
           Create_Var (Create_Var_Identifier (Unit), Phy_Type);
         Unit := Get_Chain (Unit);
      end loop;
   end Translate_Physical_Units;

   ------------
   --  File  --
   ------------

   procedure Translate_File_Type (Def : Iir_File_Type_Definition)
   is
      Info : Type_Info_Acc;
   begin
      Info := Get_Info (Def);
      Info.Ortho_Type (Mode_Value) := Ghdl_File_Index_Type;
      Info.Ortho_Ptr_Type (Mode_Value) := Ghdl_File_Index_Ptr_Type;
      Info.Type_Mode := Type_Mode_File;
      Info.B.Align := Align_32;
   end Translate_File_Type;

   procedure Create_File_Type_Var (Def : Iir_File_Type_Definition)
   is
      Type_Name : constant Iir := Get_Type (Get_File_Type_Mark (Def));
      Info      : Type_Info_Acc;
   begin
      if Get_Kind (Type_Name) in Iir_Kinds_Scalar_Type_And_Subtype_Definition
      then
         return;
      end if;
      declare
         Len : constant Natural := Get_File_Signature_Length (Type_Name);
         Sig : String (1 .. Len + 2);
         Off : Natural := Sig'First;
      begin
         Get_File_Signature (Type_Name, Sig, Off);
         Sig (Len + 1) := '.';
         Sig (Len + 2) := Character'Val (10);
         Info := Get_Info (Def);
         Info.B.File_Signature := Create_String
           (Sig, Create_Identifier ("FILESIG"), Global_Storage);
      end;
   end Create_File_Type_Var;

   -----------------------
   --  Unbounded types  --
   -----------------------

   function Type_To_Last_Object_Kind (Def : Iir) return Object_Kind_Type is
   begin
      if Get_Has_Signal_Flag (Def) then
         return Mode_Signal;
      else
         return Mode_Value;
      end if;
   end Type_To_Last_Object_Kind;

   --  A fat pointer is a struct with 2 fields:
   --  * pointer to the object base
   --  * pointer to the bounds (for arrays) or to the layout (for records)
   procedure Create_Unbounded_Type_Fat_Pointer (Info : Type_Info_Acc)
   is
      Constr : O_Element_List;
      Bounds_Type : O_Tnode;
   begin
      for Kind in Object_Kind_Type loop
         exit when Info.B.Base_Type (Kind) = O_Tnode_Null;

         Start_Record_Type (Constr);
         New_Record_Field
           (Constr, Info.B.Base_Field (Kind), Wki_Base,
            Info.B.Base_Ptr_Type (Kind));
         case Info.Type_Mode is
            when Type_Mode_Unbounded_Array =>
               Bounds_Type := Info.B.Bounds_Ptr_Type;
            when Type_Mode_Unbounded_Record =>
               Bounds_Type := Info.B.Layout_Ptr_Type;
            when others =>
               raise Internal_Error;
         end case;
         New_Record_Field
           (Constr, Info.B.Bounds_Field (Kind), Wki_Bounds,
            Bounds_Type);
         Finish_Record_Type (Constr, Info.Ortho_Type (Kind));
      end loop;
   end Create_Unbounded_Type_Fat_Pointer;

   procedure Finish_Unbounded_Type_Base (Info : Type_Info_Acc)
   is
      Id, Idptr : O_Ident;
   begin
      for Kind in Object_Kind_Type loop
         exit when Info.B.Base_Type (Kind) = O_Tnode_Null;

         case Kind is
            when Mode_Value =>
               --  For the values.
               Id := Create_Identifier ("BASE");
               Idptr := Create_Identifier ("BASEP");
            when Mode_Signal =>
               --  For the signals
               Id := Create_Identifier ("SIGBASE");
               Idptr := Create_Identifier ("SIGBASEP");
         end case;
         New_Type_Decl (Id, Info.B.Base_Type (Kind));
         Info.B.Base_Ptr_Type (Kind) :=
           New_Access_Type (Info.B.Base_Type (Kind));
         New_Type_Decl (Idptr, Info.B.Base_Ptr_Type (Kind));
      end loop;
   end Finish_Unbounded_Type_Base;

   --  Create the dope vector type declaration and access type.
   procedure Finish_Unbounded_Type_Bounds (Info : Type_Info_Acc) is
   begin
      New_Type_Decl (Create_Identifier ("BOUND"), Info.B.Bounds_Type);
      Info.B.Bounds_Ptr_Type := New_Access_Type (Info.B.Bounds_Type);
      New_Type_Decl (Create_Identifier ("BOUNDP"), Info.B.Bounds_Ptr_Type);
   end Finish_Unbounded_Type_Bounds;

   function Create_Static_Composite_Subtype_Sizes (Def : Iir) return O_Cnode
   is
      Info : constant Type_Info_Acc := Get_Info (Def);
      Sz_List : O_Record_Aggr_List;
      Sz : O_Cnode;
      Sz_Res : O_Cnode;
   begin
      Start_Record_Aggr (Sz_List, Ghdl_Sizes_Type);
      New_Record_Aggr_El
        (Sz_List, New_Sizeof (Info.Ortho_Type (Mode_Value), Ghdl_Index_Type));
      if Get_Has_Signal_Flag (Def) then
         Sz := New_Sizeof (Info.Ortho_Type (Mode_Signal), Ghdl_Index_Type);
      else
         Sz := Ghdl_Index_0;
      end if;
      New_Record_Aggr_El (Sz_List, Sz);
      Finish_Record_Aggr (Sz_List, Sz_Res);
      return Sz_Res;
   end Create_Static_Composite_Subtype_Sizes;

   function Create_Static_Array_Subtype_Bounds (Def : Iir) return O_Cnode
   is
      Base_Type : constant Iir := Get_Base_Type (Def);
      Binfo : constant Type_Info_Acc := Get_Info (Base_Type);
      Indexes_List : constant Iir_Flist := Get_Index_Subtype_List (Def);
      Index : Iir;
      El_Type : Iir;
      List : O_Record_Aggr_List;
      Res : O_Cnode;
   begin
      Start_Record_Aggr (List, Binfo.B.Bounds_Type);

      for I in Flist_First .. Flist_Last (Indexes_List) loop
         Index := Get_Index_Type (Indexes_List, I);
         New_Record_Aggr_El
           (List, Create_Static_Type_Definition_Type_Range (Index));
      end loop;

      if Binfo.B.Bounds_El /= O_Fnode_Null then
         --  For arrays of unbounded type.
         El_Type := Get_Element_Subtype (Def);
         New_Record_Aggr_El
           (List, Create_Static_Composite_Subtype_Layout (El_Type));
      end if;

      Finish_Record_Aggr (List, Res);
      return Res;
   end Create_Static_Array_Subtype_Bounds;

   function Create_Static_Record_Subtype_Bounds (Def : Iir) return O_Cnode
   is
      Base_Type : constant Iir := Get_Base_Type (Def);
      Binfo : constant Type_Info_Acc := Get_Info (Base_Type);
      El_List : constant Iir_Flist := Get_Elements_Declaration_List (Def);
      El_Blist : constant Iir_Flist :=
        Get_Elements_Declaration_List (Base_Type);
      Info : constant Type_Info_Acc := Get_Info (Def);
      List : O_Record_Aggr_List;
      Res : O_Cnode;
      El : Iir;
      El_Type : Iir;
      Bel : Iir;
      Bel_Info : Field_Info_Acc;
      Off : O_Cnode;
   begin
      Start_Record_Aggr (List, Binfo.B.Bounds_Type);

      New_Record_Aggr_El (List, Create_Static_Composite_Subtype_Sizes (Def));

      for I in Flist_First .. Flist_Last (El_Blist) loop
         Bel := Get_Nth_Element (El_Blist, I);
         Bel_Info := Get_Info (Bel);
         if Bel_Info.Field_Bound /= O_Fnode_Null then
            for Kind in Mode_Value .. Type_To_Last_Object_Kind (Base_Type)
            loop
               if Info.Ortho_Type (Kind) /= O_Tnode_Null then
                  Off := New_Offsetof
                    (Info.Ortho_Type (Kind),
                     Info.S.Rec_Fields (Iir_Index32 (I)).Fields (Kind),
                     Ghdl_Index_Type);
               else
                  Off := Ghdl_Index_0;
               end if;
               New_Record_Aggr_El (List, Off);
            end loop;
            El := Get_Nth_Element (El_List, I);
            El_Type := Get_Type (El);
            New_Record_Aggr_El
              (List, Create_Static_Composite_Subtype_Layout (El_Type));
         end if;
      end loop;

      Finish_Record_Aggr (List, Res);
      return Res;
   end Create_Static_Record_Subtype_Bounds;

   function Create_Static_Composite_Subtype_Layout (Def : Iir) return O_Cnode
   is
      Info : constant Type_Info_Acc := Get_Info (Def);
   begin
      case Info.Type_Mode is
         when Type_Mode_Static_Record
           | Type_Mode_Complex_Record =>
            return Create_Static_Record_Subtype_Bounds (Def);
         when Type_Mode_Static_Array
           | Type_Mode_Complex_Array =>
            declare
               List : O_Record_Aggr_List;
               Res : O_Cnode;
            begin
               Start_Record_Aggr (List, Info.B.Layout_Type);
               New_Record_Aggr_El
                 (List, Create_Static_Composite_Subtype_Sizes (Def));
               New_Record_Aggr_El
                 (List, Create_Static_Array_Subtype_Bounds (Def));
               Finish_Record_Aggr (List, Res);
               return Res;
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Create_Static_Composite_Subtype_Layout;

   procedure Elab_Composite_Subtype_Layout (Def : Iir; Target : Mnode)
   is
      Tinfo : constant Type_Info_Acc := Get_Info (Def);
   begin
      Open_Temp;

      case Get_Kind (Def) is
         when Iir_Kind_Array_Type_Definition
            | Iir_Kind_Record_Type_Definition =>
            --  Fully unconstrained, no layout to fill.
            null;

         when Iir_Kind_Array_Subtype_Definition =>
            declare
               Parent_Type : constant Iir := Get_Parent_Type (Def);
               Parent_Tinfo : constant Type_Info_Acc := Get_Info (Parent_Type);
               New_Indexes : constant Boolean :=
                 not Get_Index_Constraint_Flag (Parent_Type);
               Indexes_List : constant Iir_Flist :=
                 Get_Index_Subtype_List (Def);
               El_Type : Iir;
               El_Tinfo : Type_Info_Acc;
               Targ : Mnode;
               Rng : Mnode;
               Index : Iir;
            begin
               Targ := Layout_To_Bounds (Target);

               --  Indexes.
               --  Set only if the array subtype has indexes constraints.
               if Get_Index_Constraint_Flag (Def) then
                  if Tinfo.B.Bounds_El /= O_Fnode_Null
                    or else Get_Nbr_Elements (Indexes_List) > 1
                  then
                     Targ := Stabilize (Targ);
                  end if;
                  for I in Flist_First .. Flist_Last (Indexes_List) loop
                     Index := Get_Index_Type (Indexes_List, I);
                     Open_Temp;
                     Rng := Bounds_To_Range (Targ, Def, I + 1);
                     if New_Indexes then
                        Chap7.Translate_Discrete_Range (Rng, Index);
                     else
                        Gen_Memcpy
                          (M2Addr (Rng),
                           M2Addr
                             (Bounds_To_Range
                                (Layout_To_Bounds
                                   (Get_Composite_Type_Layout (Parent_Tinfo)),
                                 Parent_Type, I + 1)),
                           New_Lit (New_Sizeof (Rng.M1.Vtype,
                                                Ghdl_Index_Type)));
                     end if;
                     Close_Temp;
                  end loop;
               end if;

               --  Element.
               if Tinfo.B.Bounds_El /= O_Fnode_Null then
                  El_Type := Get_Element_Subtype (Def);
                  El_Tinfo := Get_Info (El_Type);
                  if Get_Constraint_State (El_Type) = Unconstrained then
                     --  Fully unconstrained, so there is no layout variable
                     --  for it.
                     null;
                  elsif El_Tinfo.S.Subtype_Owner /= Tinfo then
                     --  The element is not owned by this subtype, so it has
                     --  its own layout variable that must have been set.
                     --  Just copy the layout.
                     Gen_Memcpy
                       (M2Addr (Array_Bounds_To_Element_Layout (Targ, Def)),
                        M2Addr (Get_Composite_Type_Layout (El_Tinfo)),
                        New_Lit (New_Sizeof (El_Tinfo.B.Layout_Type,
                                             Ghdl_Index_Type)));
                  else
                     --  New constraints.
                     Elab_Composite_Subtype_Layout
                       (El_Type, Array_Bounds_To_Element_Layout (Targ, Def));
                  end if;
               end if;
            end;

         when Iir_Kind_Record_Subtype_Definition =>
            declare
               El_List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Def);
               Base_El_List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Get_Base_Type (Def));
               Targ : Mnode;
               El : Iir;
               Base_El : Iir;
               El_Type : Iir;
            begin
               Targ := Stabilize (Target);
               for I in Flist_First .. Flist_Last (El_List) loop
                  El := Get_Nth_Element (El_List, I);
                  Base_El := Get_Nth_Element (Base_El_List, I);
                  if Is_Unbounded_Type (Get_Info (Get_Type (Base_El))) then
                     --  FIXME: copy if not new.
                     El_Type := Get_Type (El);
                     Elab_Composite_Subtype_Layout
                       (El_Type,
                        Record_Layout_To_Element_Layout (Targ, El));
                  end if;
               end loop;
            end;

         when others =>
            Error_Kind ("elab_composite_subtype_layout", Def);
      end case;

      Close_Temp;
   end Elab_Composite_Subtype_Layout;

   --  Compute sizes for DEF (settings the size fields of layout variable
   --  TARGET) for all the new constraints.
   procedure Elab_Composite_Subtype_Size (Def : Iir; Target : Mnode)
   is
      Info : constant Type_Info_Acc := Get_Info (Def);
      T : Mnode;
   begin
      case Type_Mode_Composite (Info.Type_Mode) is
         when Type_Mode_Static_Record
            | Type_Mode_Static_Array =>
            --  Precomputed.
            null;
         when Type_Mode_Complex_Record
            | Type_Mode_Complex_Array =>
            Open_Temp;
            T := Stabilize (Target);
            Gen_Call_Type_Builder (T, Def, Mode_Value);
            if Get_Has_Signal_Flag (Def) then
               Gen_Call_Type_Builder (T, Def, Mode_Signal);
            end if;
            Close_Temp;
         when Type_Mode_Unbounded_Record =>
            declare
               El : Iir;
               El_Type : Iir;
            begin
               El := Get_Owned_Elements_Chain (Def);
               if El = Null_Iir then
                  --  No new constraints.
                  return;
               end if;
               Open_Temp;
               T := Stabilize (Target);
               while El /= Null_Iir loop
                  El_Type := Get_Type (El);
                  Elab_Composite_Subtype_Size
                    (El_Type,
                     Record_Layout_To_Element_Layout (T, El));
                  El := Get_Chain (El);
               end loop;
               Close_Temp;
            end;
         when Type_Mode_Unbounded_Array =>
            if Get_Array_Element_Constraint (Def) = Null_Iir then
               --  Element is defined by the subtype.
               return;
            end if;
            Elab_Composite_Subtype_Size
              (Get_Element_Subtype (Def),
               Array_Bounds_To_Element_Layout (Layout_To_Bounds (Target),
                 Def));
         when Type_Mode_Protected =>
            --  Not expected.
            raise Internal_Error;
      end case;
   end Elab_Composite_Subtype_Size;

   procedure Elab_Composite_Subtype_Layout (Def : Iir)
   is
      Info : constant Type_Info_Acc := Get_Info (Def);
   begin
      if Is_Static_Type (Info) then
         --  Created as a constant.
         return;
      end if;

      --  Fill ranges and length.
      Elab_Composite_Subtype_Layout (Def, Get_Composite_Type_Layout (Info));

      --  Compute sizes for this subtype.
      Elab_Composite_Subtype_Size (Def, Get_Composite_Type_Layout (Info));
   end Elab_Composite_Subtype_Layout;

   --  Create a variable containing the layout for composite subtype DEF.
   procedure Create_Composite_Subtype_Layout_Var
     (Def : Iir; Elab_Now : Boolean)
   is
      Info      : constant Type_Info_Acc := Get_Info (Def);
      Val       : O_Cnode;
   begin
      if Info.S.Composite_Layout /= Null_Var
        or else Info.S.Subtype_Owner /= null
      then
         --  Already created.
         return;
      end if;

      if Info.Type_Mode = Type_Mode_Static_Array
        or Info.Type_Mode = Type_Mode_Static_Record
      then
         if Global_Storage = O_Storage_External then
            --  Do not create the value of the type desc, since it
            --  is never dereferenced in a static type desc.
            Val := O_Cnode_Null;
         else
            Val := Create_Static_Composite_Subtype_Layout (Def);
         end if;
         Info.S.Composite_Layout := Create_Global_Const
           (Create_Identifier ("STL"),
            Info.B.Layout_Type, Global_Storage, Val);
      else
         Info.S.Composite_Layout := Create_Var
           (Create_Var_Identifier ("STL"), Info.B.Layout_Type);
         if Elab_Now then
            Elab_Composite_Subtype_Layout (Def);
         end if;
      end if;
   end Create_Composite_Subtype_Layout_Var;

   -------------
   --  Array  --
   -------------

   --  Declare the bounds types for array type definition DEF.
   --  Bounds type is a record with:
   --   * a range field for each dimension
   --   * an element layout if the element is unbounded.
   procedure Translate_Array_Type_Bounds
     (Def : Iir_Array_Type_Definition; Info : Type_Info_Acc)
   is
      Indexes_List    : constant Iir_Flist :=
        Get_Index_Subtype_Definition_List (Def);
      El_Type         : constant Iir := Get_Element_Subtype (Def);
      El_Info         : constant Type_Info_Acc := Get_Info (El_Type);
      Constr          : O_Element_List;
      Dim             : String (1 .. 8);
      N               : Natural;
      P               : Natural;
      Index           : Iir;
      Index_Info      : Index_Info_Acc;
      Index_Type_Mark : Iir;
   begin
      Start_Record_Type (Constr);
      for I in Flist_First .. Flist_Last (Indexes_List) loop
         Index_Type_Mark := Get_Nth_Element (Indexes_List, I);
         Index := Get_Index_Type (Index_Type_Mark);

         --  Index comes from a type mark.
         pragma Assert (not Is_Anonymous_Type_Definition (Index));

         Index_Info := Add_Info (Index_Type_Mark, Kind_Index);

         --  Build the name
         N := I + 1;
         P := Dim'Last;
         loop
            Dim (P) := Character'Val (Character'Pos ('0') + N mod 10);
            P := P - 1;
            N := N / 10;
            exit when N = 0;
         end loop;
         P := P - 3;
         Dim (P .. P + 3) := "dim_";

         New_Record_Field (Constr, Index_Info.Index_Field,
                           Get_Identifier (Dim (P .. Dim'Last)),
                           Get_Info (Get_Base_Type (Index)).B.Range_Type);
      end loop;

      if Is_Unbounded_Type (El_Info) then
         --  Add layout for the element.
         New_Record_Field
           (Constr, Info.B.Bounds_El,
            Get_Identifier ("el_layout"), El_Info.B.Layout_Type);
      end if;

      Finish_Record_Type (Constr, Info.B.Bounds_Type);
      Finish_Unbounded_Type_Bounds (Info);
   end Translate_Array_Type_Bounds;

   --  Create the layout type.  It is a record with:
   --  * the size field for the size of objects and signals
   --  * the bounds
   procedure Create_Array_Type_Layout_Type (Info : Type_Info_Acc)
   is
      Constr : O_Element_List;
   begin
      Start_Record_Type (Constr);
      New_Record_Field (Constr, Info.B.Layout_Size,
                        Get_Identifier ("size"), Ghdl_Sizes_Type);
      New_Record_Field (Constr, Info.B.Layout_Bounds,
                        Get_Identifier ("bounds"), Info.B.Bounds_Type);
      Finish_Record_Type (Constr, Info.B.Layout_Type);

      New_Type_Decl (Create_Identifier ("LAYOUT"), Info.B.Layout_Type);
      Info.B.Layout_Ptr_Type := New_Access_Type (Info.B.Layout_Type);
      New_Type_Decl (Create_Identifier ("LAYOUTP"), Info.B.Layout_Ptr_Type);
   end Create_Array_Type_Layout_Type;

   --  Return the type of INFO for MODE when used as a subelement (of either
   --  a record or an array).
   function Get_Ortho_Type_Subelement
     (Info : Type_Info_Acc; Mode : Object_Kind_Type) return O_Tnode is
   begin
      if Is_Unbounded_Type (Info) then
         return Info.B.Base_Type (Mode);
      else
         return Info.Ortho_Type (Mode);
      end if;
   end Get_Ortho_Type_Subelement;

   procedure Translate_Array_Type_Base
     (Def  : Iir_Array_Type_Definition; Info : Type_Info_Acc)
   is
      El_Type  : constant Iir := Get_Element_Subtype (Def);
      El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
   begin
      Info.B.Align := El_Tinfo.B.Align;

      for Kind in Mode_Value .. Type_To_Last_Object_Kind (Def) loop
         Info.B.Base_Type (Kind) :=
           New_Array_Type (Get_Ortho_Type_Subelement (El_Tinfo, Kind),
                           Ghdl_Index_Type);
      end loop;

      --  Declare the types.
      Finish_Unbounded_Type_Base (Info);
   end Translate_Array_Type_Base;

   procedure Translate_Array_Type (Def : Iir_Array_Type_Definition)
   is
      Info : constant Type_Info_Acc := Get_Info (Def);
   begin
      Info.Type_Mode := Type_Mode_Fat_Array;
      Info.B := Ortho_Info_Basetype_Array_Init;
      Info.S := Ortho_Info_Subtype_Array_Init;
      Translate_Array_Type_Base (Def, Info);
      Translate_Array_Type_Bounds (Def, Info);
      Info.Ortho_Type (Mode_Signal) := O_Tnode_Null;
      Create_Unbounded_Type_Fat_Pointer (Info);
      Finish_Type_Definition (Info, False);

      Create_Array_Type_Layout_Type (Info);

      Info.Type_Incomplete := False;
   end Translate_Array_Type;

   --  Get the length of DEF, ie the number of elements.
   --  If the length is not statically defined, returns -1.
   function Get_Array_Subtype_Length (Def : Iir_Array_Subtype_Definition)
                                      return Int64
   is
      Indexes_List : constant Iir_Flist := Get_Index_Subtype_List (Def);
      Index        : Iir;
      Idx_Len      : Int64;
      Len          : Int64;
   begin
      --  Check if the bounds of the array are locally static.
      Len := 1;
      for I in Flist_First .. Flist_Last (Indexes_List) loop
         Index := Get_Index_Type (Indexes_List, I);

         if Get_Type_Staticness (Index) /= Locally then
            return -1;
         end if;
         Idx_Len := Eval_Discrete_Type_Length (Index);
         if Idx_Len < 0 then
            return -1;
         end if;

         --  Do not consider very large arrays as static, to avoid overflow at
         --  compile time.
         if Idx_Len >= 2**31 then
            return -1;
         end if;
         Len := Len * Idx_Len;
         if Len >= 2**31 then
            return -1;
         end if;
      end loop;
      return Len;
   end Get_Array_Subtype_Length;

   procedure Translate_Bounded_Array_Subtype_Definition
     (Def : Iir_Array_Subtype_Definition; Parent_Type : Iir)
   is
      El_Type   : constant Iir := Get_Element_Subtype (Def);
      El_Info   : constant Type_Info_Acc := Get_Info (El_Type);

      Info      : constant Type_Info_Acc := Get_Info (Def);
      Pinfo     : constant Type_Info_Acc := Get_Info (Parent_Type);

      Last_Mode : constant Object_Kind_Type := Type_To_Last_Object_Kind (Def);

      Len : Int64;
   begin
      --  Note: info of indexes subtype are not created!

      Len := Get_Array_Subtype_Length (Def);
      Info.Type_Locally_Constrained := (Len >= 0);
      Info.B := Pinfo.B;
      Info.S := Ortho_Info_Subtype_Array_Init;

      if Info.Type_Locally_Constrained
        and then Is_Static_Type (El_Info)
      then
         --  Element and length are static.
         Info.Type_Mode := Type_Mode_Static_Array;

         --  Create a subtype.
         Info.Ortho_Type (Mode_Signal) := O_Tnode_Null;
         for K in Mode_Value .. Last_Mode loop
            Info.Ortho_Type (K) := New_Array_Subtype
              (Pinfo.B.Base_Type (K),
               El_Info.Ortho_Type (K),
               New_Index_Lit (Unsigned_64 (Len)));
         end loop;
         --  Declare the types.
         Declare_Value_Type (Info);
         Declare_Value_Ptr_Type (Info);
         if Last_Mode = Mode_Signal then
            Declare_Signal_Type (Info);
            Declare_Signal_Ptr_Type (Info);
         end if;
      else
         --  This is a complex type as the size is not known at compile
         --  time.
         Info.Type_Mode := Type_Mode_Complex_Array;

         --  Use the base type.
         Info.Ortho_Type := Pinfo.B.Base_Type;
         Info.Ortho_Ptr_Type := Pinfo.B.Base_Ptr_Type;
      end if;
   end Translate_Bounded_Array_Subtype_Definition;

   procedure Create_Array_Type_Builder
     (Def : Iir_Array_Type_Definition; Kind : Object_Kind_Type)
   is
      El_Type    : constant Iir := Get_Element_Subtype (Def);
      El_Info    : constant Type_Info_Acc := Get_Info (El_Type);
      Info       : constant Type_Info_Acc := Get_Info (Def);
      Layout_Param : constant O_Dnode :=
        Info.B.Builder (Kind).Builder_Layout_Param;
      Layout     : Mnode;
      El_Size    : O_Enode;
      Size       : O_Enode;
   begin
      Start_Subprogram_Body (Info.B.Builder (Kind).Builder_Proc);
      Subprgs.Start_Subprg_Instance_Use
        (Info.B.Builder (Kind).Builder_Instance);
      Open_Local_Temp;

      Layout := Dp2M (Layout_Param, Info, Kind,
                      Info.B.Layout_Type, Info.B.Layout_Ptr_Type);

      --  Call the builder to layout the element (only for unbounded elements)
      if Is_Unbounded_Type (El_Info) then
         Gen_Call_Type_Builder
           (Array_Layout_To_Element_Layout (Layout, Def), El_Type, Kind);

         El_Size := New_Value
           (Layout_To_Size (Array_Layout_To_Element_Layout (Layout, Def),
                            Kind));
      else
         El_Size := Get_Subtype_Size (El_Type, Mnode_Null, Kind);
      end if;

      --  Compute size.
      Size := New_Dyadic_Op
        (ON_Mul_Ov,
         El_Size,
         Get_Bounds_Length (Layout_To_Bounds (Layout), Def));

      --  Set size.
      New_Assign_Stmt (Layout_To_Size (Layout, Kind), Size);

      Close_Local_Temp;

      Subprgs.Finish_Subprg_Instance_Use
        (Info.B.Builder (Kind).Builder_Instance);
      Finish_Subprogram_Body;
   end Create_Array_Type_Builder;

   function Get_Element_Subtype_For_Info (Arr_Def : Iir) return Iir
   is
      Info : constant Type_Info_Acc := Get_Info (Arr_Def);
      Arr : Iir;
   begin
      if Info.Type_Locally_Constrained then
         Arr := Arr_Def;
      else
         --  When an element is constrained, no ortho array is created with
         --  the constrained element (unless it is statically constrained).
         --  So use the base type as a fall back (if the element cannot be
         --  constrained, it is the same as the base type element).
         Arr := Get_Base_Type (Arr_Def);
      end if;
      return Get_Element_Subtype (Arr);
   end Get_Element_Subtype_For_Info;

   procedure Translate_Array_Subtype_Definition (Def : Iir)
   is
      Parent_Type : constant Iir := Get_Parent_Type (Def);
      El_Type : constant Iir := Get_Element_Subtype (Def);
      El_Tinfo : Type_Info_Acc;
      Mark : Id_Mark_Type;
   begin
      --  Handle element subtype.
      El_Tinfo := Get_Info (El_Type);
      if El_Tinfo = null then
         --  Usually, if the array element subtype was not yet translated,
         --  it's because it is defined by the array subtype (the array
         --  subtype adds constraints to the elements).
         --  However, for an aggregate, the array type may not be the owner.

         --  Do not create vars for element subtype, but use
         --  the layout field of the array vars.
         Push_Identifier_Prefix (Mark, "ET");
         Translate_Subtype_Definition (El_Type, False);
         Pop_Identifier_Prefix (Mark);

         El_Tinfo := Get_Info (El_Type);
         case El_Tinfo.S.Kind is
            when Kind_Type_Array
              | Kind_Type_Record =>
               pragma Assert (El_Tinfo.S.Composite_Layout = Null_Var);
               El_Tinfo.S.Subtype_Owner := Get_Info (Def);
            when Kind_Type_Scalar =>
               pragma Assert (El_Tinfo.S.Range_Var /= Null_Var);
            when Kind_Type_File
              | Kind_Type_Protected =>
               raise Internal_Error;
         end case;
      end if;

      if Get_Constraint_State (Def) = Fully_Constrained then
         --  Index constrained.
         Translate_Bounded_Array_Subtype_Definition (Def, Parent_Type);
      else
         --  An unconstrained array subtype.  Use same infos as base
         --  type.
         --  FIXME: what if bounds are added.
         declare
            Tinfo : constant Type_Info_Acc := Get_Info (Def);
            Parent_Tinfo : constant Type_Info_Acc := Get_Info (Parent_Type);
         begin
            Tinfo.all := Parent_Tinfo.all;
            Tinfo.S.Composite_Layout := Null_Var;
            Tinfo.Type_Rti := O_Dnode_Null;
         end;
      end if;
   end Translate_Array_Subtype_Definition;

   --------------
   --  record  --
   --------------

   --  Get the alignment mask for *ortho* type ATYPE.
   function Get_Alignmask (Align : Alignment_Type) return O_Enode is
   begin
      return New_Dyadic_Op (ON_Sub_Ov,
                            New_Lit (Align_Val (Align)),
                            New_Lit (Ghdl_Index_1));
   end Get_Alignmask;

   --  Align VALUE (of unsigned type) for type ATYPE.
   --  The formulae is: (V + (A - 1)) and not (A - 1), where A is the
   --  alignment for ATYPE in bytes.
   function Realign (Value : O_Enode; Align : Alignment_Type) return O_Enode is
   begin
      return New_Dyadic_Op
        (ON_And,
         New_Dyadic_Op (ON_Add_Ov, Value, Get_Alignmask (Align)),
         New_Monadic_Op (ON_Not, Get_Alignmask (Align)));
   end Realign;

   function Realign (Value : O_Enode; Atype : Iir) return O_Enode
   is
      Tinfo : constant Type_Info_Acc := Get_Info (Atype);
   begin
      return Realign (Value, Tinfo.B.Align);
   end Realign;

   procedure Translate_Record_Type (Def : Iir_Record_Type_Definition)
   is
      Info       : constant Type_Info_Acc := Get_Info (Def);
      List       : constant Iir_Flist := Get_Elements_Declaration_List (Def);
      Is_Unbounded : constant Boolean :=
        Get_Constraint_State (Def) /= Fully_Constrained;
      El_List    : O_Element_List;
      El         : Iir_Element_Declaration;
      Field_Info : Ortho_Info_Acc;
      El_Type    : Iir;
      El_Tinfo   : Type_Info_Acc;
      Align      : Alignment_Type;

      --  True if a size variable will be created since the size of
      --  the record is not known at compile-time.
      Is_Complex : Boolean;

      Mark : Id_Mark_Type;
   begin
      --  First, translate the anonymous type of the elements.
      Align := Align_8;
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         El_Type := Get_Type (El);
         El_Tinfo := Get_Info (El_Type);
         if El_Tinfo = null then
            Push_Identifier_Prefix (Mark, Get_Identifier (El));
            Translate_Subtype_Indication (El_Type, True);
            Pop_Identifier_Prefix (Mark);
            El_Tinfo := Get_Info (El_Type);
         end if;
         Field_Info := Add_Info (El, Kind_Field);

         pragma Assert (El_Tinfo.B.Align /= Align_Undef);
         Align := Alignment_Type'Max (Align, El_Tinfo.B.Align);
      end loop;
      Info.B.Align := Align;

      --  Then create the record type.
      Info.S := Ortho_Info_Subtype_Record_Init;
      Info.Ortho_Type (Mode_Signal) := O_Tnode_Null;
      for Kind in Mode_Value .. Type_To_Last_Object_Kind (Def) loop
         Start_Record_Type (El_List);
         for Static in reverse Boolean loop
            --  First static fields, then non-static ones.
            for I in Flist_First .. Flist_Last (List) loop
               El := Get_Nth_Element (List, I);
               Field_Info := Get_Info (El);
               El_Tinfo := Get_Info (Get_Type (El));
               if Is_Static_Type (El_Tinfo) = Static then
                  New_Record_Field
                    (El_List, Field_Info.Field_Node (Kind),
                     Create_Identifier_Without_Prefix (El),
                     Get_Ortho_Type_Subelement (El_Tinfo, Kind));
               end if;
            end loop;
         end loop;
         Finish_Record_Type (El_List, Info.B.Base_Type (Kind));
      end loop;

      --  Create the bounds type
      Info.B.Bounds_Type := O_Tnode_Null;
      Start_Record_Type (El_List);
      New_Record_Field (El_List, Info.B.Layout_Size,
                        Get_Identifier ("size"), Ghdl_Sizes_Type);
      Is_Complex := False;
      for I in Flist_First .. Flist_Last (List) loop
         declare
            El         : constant Iir := Get_Nth_Element (List, I);
            Field_Info : constant Field_Info_Acc := Get_Info (El);
            El_Tinfo   : constant Type_Info_Acc := Get_Info (Get_Type (El));
            Unbounded_El : constant Boolean := Is_Unbounded_Type (El_Tinfo);
            Complex_El : constant Boolean := Is_Complex_Type (El_Tinfo);
         begin
            Is_Complex := Is_Complex or Complex_El;
            if Unbounded_El or Complex_El then
               --  Offset
               New_Record_Field
                 (El_List, Field_Info.Field_Node (Mode_Value),
                  Create_Identifier_Without_Prefix (El, "_OFF"),
                  Ghdl_Index_Type);
               if Get_Has_Signal_Flag (Def) then
                  New_Record_Field
                    (El_List, Field_Info.Field_Node (Mode_Signal),
                     Create_Identifier_Without_Prefix (El, "_SIGOFF"),
                     Ghdl_Index_Type);
               end if;
            end if;
            if Unbounded_El then
               New_Record_Field
                 (El_List, Field_Info.Field_Bound,
                  Create_Identifier_Without_Prefix (El, "_BND"),
                  El_Tinfo.B.Layout_Type);
            end if;
         end;
      end loop;
      Finish_Record_Type (El_List, Info.B.Bounds_Type);
      Finish_Unbounded_Type_Bounds (Info);

      --  For records: layout == bounds.
      Info.B.Layout_Type := Info.B.Bounds_Type;
      Info.B.Layout_Ptr_Type := Info.B.Bounds_Ptr_Type;

      if Is_Unbounded then
         Info.Type_Mode := Type_Mode_Unbounded_Record;
         Finish_Unbounded_Type_Base (Info);
         Create_Unbounded_Type_Fat_Pointer (Info);
         Finish_Type_Definition (Info);
      else
         if Is_Complex then
            Info.Type_Mode := Type_Mode_Complex_Record;
         else
            Info.Type_Mode := Type_Mode_Static_Record;
         end if;
         Info.Ortho_Type := Info.B.Base_Type;
         Finish_Type_Definition (Info);
         Info.B.Base_Ptr_Type := Info.Ortho_Ptr_Type;

         Create_Composite_Subtype_Layout_Var (Def, False);
      end if;
   end Translate_Record_Type;

   procedure Translate_Record_Subtype_Definition (Def : Iir)
   is
      Parent_Type : constant Iir := Get_Parent_Type (Def);
      Base_Type   : constant Iir := Get_Base_Type (Parent_Type);
      Info        : constant Type_Info_Acc := Get_Info (Def);
      El_List     : constant Iir_Flist := Get_Elements_Declaration_List (Def);
      El_Blist    : constant Iir_Flist :=
        Get_Elements_Declaration_List (Base_Type);
      Parent_Info : constant Type_Info_Acc := Get_Info (Parent_Type);
      El_Tm_List  : constant Iir_Flist :=
        Get_Elements_Declaration_List (Parent_Type);
      El, B_El    : Iir_Element_Declaration;

      Rec        : O_Element_Sublist;
      El_Tinfo   : Type_Info_Acc;

      Mode : Type_Mode_Type;
      Fields : Subtype_Fields_Array_Acc;
   begin
      --  Translate the newly constrained elements.
      El := Get_Owned_Elements_Chain (Def);
      while El /= Null_Iir loop
         declare
            El_Type : constant Iir := Get_Type (El);
            Pos     : constant Natural := Natural (Get_Element_Position (El));
            B_El    : constant Iir := Get_Nth_Element (El_Tm_List, Pos);
            El_Info : Field_Info_Acc;
            Mark    : Id_Mark_Type;
         begin
            --  Copy info (for the bound field).
            El_Info := Get_Info (B_El);
            Set_Info (El, El_Info);

            if Get_Info (El_Type) = null then
               --  Translate the new constraint.
               --  Not triggered on ownership, because of aggregate where
               --  the subtype of a whole aggregate may be defined with bounds
               --  from an element which can be a string or an aggregate that
               --  owns the bound.
               Push_Identifier_Prefix (Mark, Get_Identifier (El));
               Translate_Subtype_Definition (El_Type, False);
               Pop_Identifier_Prefix (Mark);

               El_Tinfo := Get_Info (El_Type);
               if Is_Composite (El_Tinfo) then
                  pragma Assert (El_Tinfo.S.Composite_Layout = Null_Var);
                  El_Tinfo.S.Subtype_Owner := Info;
                  El_Tinfo.S.Owner_Field := El_Info;
               end if;
            end if;
         end;
         El := Get_Chain (El);
      end loop;

      --  Mode of the subtype.
      Mode := Type_Mode_Static_Record;
      for I in Flist_First .. Flist_Last (El_List) loop
         declare
            El       : constant Iir := Get_Nth_Element (El_List, I);
            El_Type  : constant Iir := Get_Type (El);
            El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
         begin
            if Is_Unbounded_Type (El_Tinfo) then
               Mode := Type_Mode_Unbounded_Record;
               --  Cannot be 'worse' than unbounded.
               exit;
            elsif Is_Complex_Type (El_Tinfo) then
               Mode := Type_Mode_Complex_Record;
            end if;
         end;
      end loop;

      --  By default, use the same representation as the parent type.
      Info.all := Parent_Info.all;
      --  However, it is a different subtype which has its own rti.
      Info.Type_Rti := O_Dnode_Null;

      if Get_Owned_Elements_Chain (Def) = Null_Iir then
         --  That's considered as an alias of the type mark.  Maybe only the
         --  resolution is different.
         return;
      end if;
      Info.S := Ortho_Info_Subtype_Record_Init;

      case Type_Mode_Records (Mode) is
         when Type_Mode_Unbounded_Record =>
            pragma Assert (Parent_Info.Type_Mode = Type_Mode_Unbounded_Record);
            --  The subtype is not completly constrained: it cannot be used to
            --    create objects, so wait until it is completly constrained.
            --  The subtype is simply an alias.
            --  In both cases, use the same representation as its type mark.
            null;

         when Type_Mode_Complex_Record =>
            --  At least one field is not static.
            --  Do not over-optimize and consider all the fields that were
            --  initially unbounded as complex.
            Info.Type_Mode := Type_Mode_Complex_Record;

            Info.Ortho_Type := Parent_Info.B.Base_Type;
            Info.Ortho_Ptr_Type := Parent_Info.B.Base_Ptr_Type;

         when Type_Mode_Static_Record =>
            --  The subtype is static.
            Info.Type_Mode := Type_Mode_Static_Record;

            --  Create the subtypes.
            Info.Ortho_Type (Mode_Signal) := O_Tnode_Null;
            Fields := new Subtype_Fields_Array
              (0 .. Iir_Index32 (Get_Nbr_Elements (El_Blist)) - 1);
            Fields.all := (others => Subtype_Fields_Null);
            Info.S.Rec_Fields := Fields;
            for Kind in Mode_Value .. Type_To_Last_Object_Kind (Def) loop
               Start_Record_Subtype (Parent_Info.B.Base_Type (Kind), Rec);
               for Static in reverse Boolean loop
                  for I in Flist_First .. Flist_Last (El_Blist) loop
                     B_El := Get_Nth_Element (El_Blist, I);
                     El_Tinfo := Get_Info (Get_Type (B_El));
                     if Is_Static_Type (El_Tinfo) then
                        if Static then
                           --  First the bounded fields.
                           New_Subrecord_Field
                             (Rec, Fields (Iir_Index32 (I)).Fields (Kind),
                              El_Tinfo.Ortho_Type (Kind));
                           Fields (Iir_Index32 (I)).Tinfo := El_Tinfo;
                        end if;
                     else
                        if not Static then
                           --  Then the bounded subtype of unbounded fields.
                           El := Get_Nth_Element (El_List, I);
                           El_Tinfo := Get_Info (Get_Type (El));
                           New_Subrecord_Field
                             (Rec, Fields (Iir_Index32 (I)).Fields (Kind),
                              El_Tinfo.Ortho_Type (Kind));
                           Fields (Iir_Index32 (I)).Tinfo := El_Tinfo;
                        end if;
                     end if;
                  end loop;
               end loop;
               Finish_Record_Subtype (Rec, Info.Ortho_Type (Kind));
            end loop;

            Finish_Type_Definition (Info);
      end case;
   end Translate_Record_Subtype_Definition;

   procedure Create_Record_Type_Builder
     (Def : Iir_Record_Type_Definition; Kind : Object_Kind_Type)
   is
      Info : constant Type_Info_Acc := Get_Info (Def);
      Layout_Param : constant O_Dnode :=
        Info.B.Builder (Kind).Builder_Layout_Param;
      List : constant Iir_Flist := Get_Elements_Declaration_List (Def);

      Layout : Mnode;
      Off_Var    : O_Dnode;
      Off_Val    : O_Enode;
   begin
      Start_Subprogram_Body (Info.B.Builder (Kind).Builder_Proc);
      Subprgs.Start_Subprg_Instance_Use
        (Info.B.Builder (Kind).Builder_Instance);

      Layout := Dp2M (Layout_Param, Info, Kind,
                      Info.B.Layout_Type, Info.B.Layout_Ptr_Type);

      --  Declare OFF, the offset variable
      New_Var_Decl (Off_Var, Get_Identifier ("off"), O_Storage_Local,
                    Ghdl_Index_Type);

      --  Reserve memory for the record, ie:
      --  off = RECORD_SIZEOF (record).
      Off_Val := New_Lit
        (New_Record_Sizeof (Info.B.Base_Type (Kind), Ghdl_Index_Type));
      New_Assign_Stmt (New_Obj (Off_Var), Off_Val);

      --  Set memory for each complex element.
      for I in Flist_First .. Flist_Last (List) loop
         declare
            El : constant Iir := Get_Nth_Element (List, I);
            El_Type : constant Iir := Get_Type (El);
            El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
            El_Complex : constant Boolean := Is_Complex_Type (El_Tinfo);
            El_Unbounded : constant Boolean := Is_Unbounded_Type (El_Tinfo);
            El_Layout : Mnode;
            El_Size : O_Enode;
         begin
            if El_Unbounded then
               --  Set layout
               El_Layout := Record_Layout_To_Element_Layout (Layout, El);
               Gen_Call_Type_Builder (El_Layout, El_Type, Kind);
            end if;

            if El_Unbounded or El_Complex then
               --  Complex or unbounded type.  Field is an offset.

               --  Align on the innermost array element (which should be
               --  a record) for Mode_Value.  No need to align for signals,
               --  as all non-composite elements are accesses.
               Off_Val := New_Obj_Value (Off_Var);
               if Kind = Mode_Value then
                  Off_Val := Realign (Off_Val, El_Type);
               end if;
               New_Assign_Stmt (New_Obj (Off_Var), Off_Val);

               --  Set the offset.
               New_Assign_Stmt
                 (Record_Layout_To_Element_Offset (Layout, El, Kind),
                  New_Obj_Value (Off_Var));

               if El_Unbounded then
                  El_Layout := Record_Layout_To_Element_Layout (Layout, El);
                  El_Size := New_Value
                    (Sizes_To_Size (Layout_To_Sizes (El_Layout), Kind));
               else
                  El_Size := Get_Subtype_Size (El_Type, El_Layout, Kind);
               end if;

               New_Assign_Stmt (New_Obj (Off_Var),
                                New_Dyadic_Op (ON_Add_Ov,
                                               New_Obj_Value (Off_Var),
                                               El_Size));
            end if;
         end;
      end loop;

      --  Align the size to the object alignment.
      Off_Val := New_Obj_Value (Off_Var);
      if Kind = Mode_Value then
         Off_Val := Realign (Off_Val, Def);
      end if;

      --  Set size.
      New_Assign_Stmt (Layout_To_Size (Layout, Kind), Off_Val);

      Subprgs.Finish_Subprg_Instance_Use
        (Info.B.Builder (Kind).Builder_Instance);
      Finish_Subprogram_Body;
   end Create_Record_Type_Builder;

   --------------
   --  Access  --
   --------------

   --  Get the ortho designated type for access type DEF.
   function Get_Ortho_Designated_Type (Def : Iir_Access_Type_Definition)
                                      return O_Tnode
   is
      D_Type   : constant Iir := Get_Designated_Type (Def);
      D_Info   : constant Type_Info_Acc := Get_Info (D_Type);
   begin
      if not Is_Fully_Constrained_Type (D_Type) then
         return D_Info.B.Bounds_Type;
      else
         if D_Info.Type_Mode in Type_Mode_Arrays then
            --  The designated type cannot be a sub array inside ortho.
            --  FIXME: lift this restriction.
            return D_Info.B.Base_Type (Mode_Value);
         else
            return D_Info.Ortho_Type (Mode_Value);
         end if;
      end if;
   end Get_Ortho_Designated_Type;

   procedure Translate_Access_Type (Def : Iir_Access_Type_Definition)
   is
      D_Type   : constant Iir := Get_Designated_Type (Def);
      --  Info for designated type may not be a type info: it may be an
      --  incomplete type.
      D_Info   : constant Ortho_Info_Acc := Get_Info (D_Type);
      Def_Info : constant Type_Info_Acc := Get_Info (Def);
      Dtype    : O_Tnode;
   begin
      --  No access types for signals.
      Def_Info.Ortho_Type (Mode_Signal) := O_Tnode_Null;

      if not Is_Fully_Constrained_Type (D_Type) then
         --  An access type to an unconstrained type definition is a pointer
         --  to bounds and base.
         Def_Info.Type_Mode := Type_Mode_Bounds_Acc;
      else
         --  Otherwise, it is a thin pointer.
         Def_Info.Type_Mode := Type_Mode_Acc;
      end if;
      Def_Info.B.Align := Align_Ptr;

      if D_Info.Kind = Kind_Incomplete_Type then
         --  Incomplete access.
         Dtype := O_Tnode_Null;
      else
         Dtype := Get_Ortho_Designated_Type (Def);
      end if;

      Def_Info.Ortho_Type (Mode_Value) := New_Access_Type (Dtype);
      Finish_Type_Definition (Def_Info);
   end Translate_Access_Type;

   ------------------------
   --  Incomplete types  --
   ------------------------

   procedure Translate_Incomplete_Type (Def : Iir)
   is
      Info  : Incomplete_Type_Info_Acc;
      Ctype : Iir;
   begin
      if Is_Null (Get_Incomplete_Type_Ref_Chain (Def)) then
         --  FIXME:
         --  This is a work-around for dummy incomplete type (ie incomplete
         --  types not used before the full type declaration).
         return;
      end if;

      --  Get the complete type definition.
      Ctype := Get_Complete_Type_Definition (Def);
      Info := Add_Info (Ctype, Kind_Incomplete_Type);
      Info.Incomplete_Type := Def;
   end Translate_Incomplete_Type;

   procedure Translate_Complete_Type
     (Incomplete_Info : in out Incomplete_Type_Info_Acc)
   is
      Atype    : Iir;
      Def_Info : Type_Info_Acc;
   begin
      Atype := Get_Incomplete_Type_Ref_Chain (Incomplete_Info.Incomplete_Type);
      while Is_Valid (Atype) loop
         --  Only access type can be completed.
         pragma Assert (Get_Kind (Atype) = Iir_Kind_Access_Type_Definition);

         Def_Info := Get_Info (Atype);
         Finish_Access_Type (Def_Info.Ortho_Type (Mode_Value),
                             Get_Ortho_Designated_Type (Atype));

         Atype := Get_Incomplete_Type_Ref_Chain (Atype);
      end loop;
      Unchecked_Deallocation (Incomplete_Info);
   end Translate_Complete_Type;

   -----------------
   --  protected  --
   -----------------

   procedure Translate_Protected_Type (Def : Iir_Protected_Type_Declaration)
   is
      Info : constant Type_Info_Acc := Get_Info (Def);
      Mark : Id_Mark_Type;
   begin
      --  The protected type is represented by an incomplete record, that
      --  will be completed by the protected type body.
      Predeclare_Scope_Type (Info.B.Prot_Scope, Create_Identifier);
      Info.Ortho_Type (Mode_Value) := O_Tnode_Null;

      --  Create a pointer type to that record.
      Declare_Scope_Acc (Info.B.Prot_Scope,
                         Create_Identifier ("PTR"),
                         Info.Ortho_Ptr_Type (Mode_Value));

      --  A protected type cannot be used for signals.
      Info.Ortho_Type (Mode_Signal) := O_Tnode_Null;
      Info.Ortho_Ptr_Type (Mode_Signal) := O_Tnode_Null;

      Info.Type_Mode := Type_Mode_Protected;

      --  This is just use to set overload number on subprograms, and to
      --  translate interfaces.
      Push_Identifier_Prefix
        (Mark, Get_Identifier (Get_Type_Declarator (Def)));
      Chap4.Translate_Declaration_Chain (Def);
      Pop_Identifier_Prefix (Mark);
   end Translate_Protected_Type;

   procedure Translate_Protected_Type_Subprograms_Spec
     (Def : Iir_Protected_Type_Declaration)
   is
      Info                 : constant Type_Info_Acc := Get_Info (Def);
      El                   : Iir;
      Inter_List           : O_Inter_List;
      Mark                 : Id_Mark_Type;
      Prev_Subprg_Instance : Subprgs.Subprg_Instance_Stack;
   begin
      Push_Identifier_Prefix
        (Mark, Get_Identifier (Get_Type_Declarator (Def)));

      --  Init.
      Start_Function_Decl
        (Inter_List, Create_Identifier ("INIT"), Global_Storage,
         Info.Ortho_Ptr_Type (Mode_Value));
      Subprgs.Add_Subprg_Instance_Interfaces
        (Inter_List, Info.B.Prot_Init_Instance);
      Finish_Subprogram_Decl (Inter_List, Info.B.Prot_Init_Subprg);

      --  Use the object as instance.
      Subprgs.Push_Subprg_Instance (Info.B.Prot_Scope'Unrestricted_Access,
                                    Info.Ortho_Ptr_Type (Mode_Value),
                                    Wki_Obj,
                                    Prev_Subprg_Instance);

      --  Final.
      Start_Procedure_Decl
        (Inter_List, Create_Identifier ("FINI"), Global_Storage);
      Subprgs.Add_Subprg_Instance_Interfaces
        (Inter_List, Info.B.Prot_Final_Instance);
      Finish_Subprogram_Decl (Inter_List, Info.B.Prot_Final_Subprg);

      --  Methods.
      El := Get_Declaration_Chain (Def);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Function_Declaration
               | Iir_Kind_Procedure_Declaration =>
               --  Translate only if used.
               if Get_Info (El) /= null then
                  Chap2.Translate_Subprogram_Declaration (El);
               end if;
            when Iir_Kind_Attribute_Specification
               | Iir_Kind_Use_Clause =>
               null;
            when others =>
               Error_Kind ("translate_protected_type_subprograms_spec", El);
         end case;
         El := Get_Chain (El);
      end loop;

      Subprgs.Pop_Subprg_Instance (Wki_Obj, Prev_Subprg_Instance);

      Pop_Identifier_Prefix (Mark);
   end Translate_Protected_Type_Subprograms_Spec;

   procedure Translate_Protected_Type_Body (Bod : Iir)
   is
      Decl : constant Iir_Protected_Type_Declaration :=
        Get_Protected_Type_Declaration (Bod);
      Info : constant Type_Info_Acc := Get_Info (Decl);
      Mark : Id_Mark_Type;
   begin
      Push_Identifier_Prefix (Mark, Get_Identifier (Bod));

      --  Create the object type
      Push_Instance_Factory (Info.B.Prot_Scope'Unrestricted_Access);
      --  First, the previous instance.
      Subprgs.Add_Subprg_Instance_Field
        (Info.B.Prot_Subprg_Instance_Field, Info.B.Prot_Prev_Scope);
      --  Then the object lock
      Info.B.Prot_Lock_Field := Add_Instance_Factory_Field
        (Get_Identifier ("LOCK"), Ghdl_Ptr_Type);

      --  Translate declarations.
      Chap4.Translate_Declaration_Chain (Bod);

      Pop_Instance_Factory (Info.B.Prot_Scope'Unrestricted_Access);

      Pop_Identifier_Prefix (Mark);
   end Translate_Protected_Type_Body;

   procedure Call_Ghdl_Protected_Procedure (Type_Def : Iir; Proc : O_Dnode)
   is
      Info  : constant Type_Info_Acc := Get_Info (Type_Def);
      Assoc : O_Assoc_List;
   begin
      Start_Association (Assoc, Proc);
      New_Association
        (Assoc,
         New_Unchecked_Address
           (New_Selected_Element
                (Get_Instance_Ref (Info.B.Prot_Scope),
                 Info.B.Prot_Lock_Field),
            Ghdl_Ptr_Type));
      New_Procedure_Call (Assoc);
   end Call_Ghdl_Protected_Procedure;

   procedure Translate_Protected_Type_Body_Subprograms_Spec (Bod : Iir)
   is
      Mark  : Id_Mark_Type;
      Decl  : constant Iir := Get_Protected_Type_Declaration (Bod);
      Info  : constant Type_Info_Acc := Get_Info (Decl);
      Prev_Subprg_Instance : Subprgs.Subprg_Instance_Stack;
   begin
      Push_Identifier_Prefix (Mark, Get_Identifier (Bod));

      --  Subprograms of BOD.
      Subprgs.Push_Subprg_Instance (Info.B.Prot_Scope'Unrestricted_Access,
                                    Info.Ortho_Ptr_Type (Mode_Value),
                                    Wki_Obj,
                                    Prev_Subprg_Instance);

      --  Environment is referenced through the object.
      Subprgs.Start_Prev_Subprg_Instance_Use_Via_Field
        (Info.B.Prot_Prev_Scope, Info.B.Prot_Subprg_Instance_Field);

      Chap4.Translate_Declaration_Chain_Subprograms
        (Bod, Subprg_Translate_Spec_And_Body);

      Subprgs.Pop_Subprg_Instance (Wki_Obj, Prev_Subprg_Instance);

      Subprgs.Finish_Prev_Subprg_Instance_Use_Via_Field
        (Info.B.Prot_Prev_Scope, Info.B.Prot_Subprg_Instance_Field);

      Pop_Identifier_Prefix (Mark);
   end Translate_Protected_Type_Body_Subprograms_Spec;

   procedure Translate_Protected_Type_Body_Subprograms_Body (Bod : Iir)
   is
      Decl  : constant Iir := Get_Protected_Type_Declaration (Bod);
      Info  : constant Type_Info_Acc := Get_Info (Decl);
      Final : Boolean;
   begin
      pragma Assert (Global_Storage /= O_Storage_External);

      --  Init subprogram
      --  Contrary to other subprograms, no object is passed to it.
      declare
         Var_Obj : O_Dnode;
      begin
         Start_Subprogram_Body (Info.B.Prot_Init_Subprg);
         Subprgs.Start_Subprg_Instance_Use (Info.B.Prot_Init_Instance);
         New_Var_Decl (Var_Obj, Wki_Obj, O_Storage_Local,
                       Info.Ortho_Ptr_Type (Mode_Value));

         --  Allocate the object
         New_Assign_Stmt
           (New_Obj (Var_Obj),
            Gen_Alloc
              (Alloc_System,
               New_Lit (New_Sizeof (Get_Scope_Type (Info.B.Prot_Scope),
                                    Ghdl_Index_Type)),
               Info.Ortho_Ptr_Type (Mode_Value)));

         Subprgs.Set_Subprg_Instance_Field
           (Var_Obj, Info.B.Prot_Subprg_Instance_Field,
            Info.B.Prot_Init_Instance);

         Set_Scope_Via_Param_Ptr (Info.B.Prot_Scope, Var_Obj);

         --   Create lock.
         Call_Ghdl_Protected_Procedure (Decl, Ghdl_Protected_Init);

         --   Elaborate fields.
         Open_Temp;
         Chap4.Elab_Declaration_Chain (Bod, Final);
         Close_Temp;

         Clear_Scope (Info.B.Prot_Scope);

         New_Return_Stmt (New_Obj_Value (Var_Obj));
         Subprgs.Finish_Subprg_Instance_Use (Info.B.Prot_Init_Instance);

         Finish_Subprogram_Body;
      end;

--      Chap4.Translate_Declaration_Chain_Subprograms
--        (Bod, Subprg_Translate_Only_Body);

      --  Fini subprogram
      begin
         Start_Subprogram_Body (Info.B.Prot_Final_Subprg);
         Subprgs.Start_Subprg_Instance_Use (Info.B.Prot_Final_Instance);

         --   Deallocate fields.
         if Final or True then
            Chap4.Final_Declaration_Chain (Bod, True);
         end if;

         --   Destroy lock.
         Call_Ghdl_Protected_Procedure (Decl, Ghdl_Protected_Fini);

         Subprgs.Finish_Subprg_Instance_Use (Info.B.Prot_Final_Instance);
         Finish_Subprogram_Body;
      end;

   end Translate_Protected_Type_Body_Subprograms_Body;

   ---------------
   --  Scalars  --
   ---------------

   --  Create a type_range structure.
   procedure Elab_Scalar_Type_Range (Def : Iir; Target : O_Lnode)
   is
      T_Info    : constant Type_Info_Acc := Get_Info (Get_Base_Type (Def));
   begin
      Chap7.Translate_Range
        (Lv2M (Target, T_Info, Mode_Value,
               T_Info.B.Range_Type, T_Info.B.Range_Ptr_Type),
         Get_Range_Constraint (Def), Def);
   end Elab_Scalar_Type_Range;

   function Create_Static_Scalar_Type_Range (Def : Iir) return O_Cnode is
   begin
      return Chap7.Translate_Static_Range (Get_Range_Constraint (Def),
                                           Get_Base_Type (Def));
   end Create_Static_Scalar_Type_Range;

   procedure Create_Scalar_Type_Range_Type
     (Def : Iir; With_Length : Boolean)
   is
      Constr : O_Element_List;
      Info   : Ortho_Info_Acc;
   begin
      Info := Get_Info (Def);
      Start_Record_Type (Constr);
      New_Record_Field
        (Constr, Info.B.Range_Left, Wki_Left,
         Info.Ortho_Type (Mode_Value));
      New_Record_Field
        (Constr, Info.B.Range_Right, Wki_Right,
         Info.Ortho_Type (Mode_Value));
      New_Record_Field
        (Constr, Info.B.Range_Dir, Wki_Dir, Ghdl_Dir_Type_Node);
      if With_Length then
         New_Record_Field
           (Constr, Info.B.Range_Length, Wki_Length, Ghdl_Index_Type);
      else
         Info.B.Range_Length := O_Fnode_Null;
      end if;
      Finish_Record_Type (Constr, Info.B.Range_Type);
      New_Type_Decl (Create_Identifier ("TRT"), Info.B.Range_Type);
      Info.B.Range_Ptr_Type := New_Access_Type (Info.B.Range_Type);
      New_Type_Decl (Create_Identifier ("TRPTR"),
                     Info.B.Range_Ptr_Type);
   end Create_Scalar_Type_Range_Type;

   function Create_Static_Type_Definition_Type_Range (Def : Iir)
                                                      return O_Cnode
   is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Enumeration_Type_Definition
            | Iir_Kinds_Scalar_Subtype_Definition =>
            return Create_Static_Scalar_Type_Range (Def);

         when Iir_Kind_Array_Subtype_Definition =>
            return Create_Static_Array_Subtype_Bounds (Def);

         when Iir_Kind_Array_Type_Definition =>
            return O_Cnode_Null;

         when others =>
            Error_Kind ("create_static_type_definition_type_range", Def);
      end case;
   end Create_Static_Type_Definition_Type_Range;

   procedure Elab_Type_Definition_Type_Range (Def : Iir)
   is
      Target : O_Lnode;
      Info   : Type_Info_Acc;
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Enumeration_Type_Definition =>
            Info := Get_Info (Def);
            if not Info.S.Same_Range then
               Target := Get_Var (Info.S.Range_Var);
               Elab_Scalar_Type_Range (Def, Target);
            end if;

         when Iir_Kind_Array_Type_Definition =>
            declare
               Index_List : constant Iir_Flist :=
                 Get_Index_Subtype_List (Def);
               Index      : Iir;
            begin
               for I in Flist_First .. Flist_Last (Index_List) loop
                  Index := Get_Index_Type (Index_List, I);
                  if Is_Anonymous_Type_Definition (Index) then
                     Elab_Type_Definition_Type_Range (Index);
                  end if;
               end loop;
            end;
            return;

         when Iir_Kind_Record_Type_Definition =>
            Info := Get_Info (Def);
            if Info.S.Composite_Layout /= Null_Var then
               Elab_Composite_Subtype_Layout (Def);
            end if;

         when Iir_Kind_Access_Type_Definition
            | Iir_Kind_File_Type_Definition
            | Iir_Kind_Protected_Type_Declaration =>
            return;

         when others =>
            Error_Kind ("elab_type_definition_type_range", Def);
      end case;
   end Elab_Type_Definition_Type_Range;

   --  Return TRUE iff LIT is equal to the high (IS_HI=TRUE) or low
   --  (IS_HI=false) limit of the base type of DEF.  MODE is the mode of
   --  DEF.
   function Is_Equal_Limit (Lit   : Iir;
                            Is_Hi : Boolean;
                            Def   : Iir;
                            Mode  : Type_Mode_Type) return Boolean
   is
   begin
      case Mode is
         when Type_Mode_B1 =>
            declare
               V : Iir_Int32;
            begin
               V := Iir_Int32 (Eval_Pos (Lit));
               if Is_Hi then
                  return V = 1;
               else
                  return V = 0;
               end if;
            end;
         when Type_Mode_E8 =>
            declare
               V         : Iir_Int32;
               Base_Type : Iir;
            begin
               V := Iir_Int32 (Eval_Pos (Lit));
               if Is_Hi then
                  Base_Type := Get_Base_Type (Def);
                  return V = Iir_Int32
                    (Get_Nbr_Elements
                       (Get_Enumeration_Literal_List (Base_Type))) - 1;
               else
                  return V = 0;
               end if;
            end;
         when Type_Mode_I32 =>
            declare
               V : Int64;
            begin
               V := Get_Value (Lit);
               if Is_Hi then
                  return V = Int64 (Iir_Int32'Last);
               else
                  return V = Int64 (Iir_Int32'First);
               end if;
            end;
         when Type_Mode_P32 =>
            declare
               V : Iir_Int32;
            begin
               V := Iir_Int32 (Get_Physical_Value (Lit));
               if Is_Hi then
                  return V = Iir_Int32'Last;
               else
                  return V = Iir_Int32'First;
               end if;
            end;
         when Type_Mode_I64 =>
            declare
               V : Int64;
            begin
               V := Get_Value (Lit);
               if Is_Hi then
                  return V = Int64'Last;
               else
                  return V = Int64'First;
               end if;
            end;
         when Type_Mode_P64 =>
            declare
               V : Int64;
            begin
               V := Get_Physical_Value (Lit);
               if Is_Hi then
                  return V = Int64'Last;
               else
                  return V = Int64'First;
               end if;
            end;
         when Type_Mode_F64 =>
            --  Don't include +/- Inf
            return False;
         when others =>
            Error_Kind ("is_equal_limit " & Type_Mode_Type'Image (Mode),
                        Lit);
      end case;
   end Is_Equal_Limit;

   --  For scalar subtypes: creates info from the base type.
   procedure Create_Subtype_Info_From_Type (Def          : Iir;
                                            Base         : Iir;
                                            Subtype_Info : Type_Info_Acc)
   is
      Base_Info : constant Type_Info_Acc := Get_Info (Base);
      Rng    : constant Iir := Get_Range_Constraint (Def);
      Lo, Hi : Iir;
   begin
      Subtype_Info.Ortho_Type := Base_Info.Ortho_Type;
      Subtype_Info.Ortho_Ptr_Type := Base_Info.Ortho_Ptr_Type;
      Subtype_Info.Type_Mode := Base_Info.Type_Mode;
      Subtype_Info.B := Base_Info.B;
      Subtype_Info.S := Base_Info.S;

      --  If the range is the same as its parent (its type_mark), set
      --  Same_Range and return (so that no new range variable would be
      --  created).
      case Get_Kind (Base) is
         when Iir_Kinds_Scalar_Subtype_Definition =>
            declare
               Tm_Rng : constant Iir := Get_Range_Constraint (Base);
            begin
               if Tm_Rng = Rng then
                  Subtype_Info.S.Same_Range := True;
                  return;
               elsif Get_Kind (Rng) = Iir_Kind_Range_Expression
                 and then Get_Kind (Tm_Rng) = Iir_Kind_Range_Expression
                 and then Get_Left_Limit (Rng) = Get_Left_Limit (Tm_Rng)
                 and then Get_Right_Limit (Rng) = Get_Right_Limit (Tm_Rng)
                 and then Get_Direction (Rng) = Get_Direction (Tm_Rng)
               then
                  Subtype_Info.S.Same_Range := True;
                  return;
               end if;
            end;
         when Iir_Kind_Enumeration_Type_Definition =>
            if Get_Kind (Rng) = Iir_Kind_Range_Expression
              and then Get_Direction (Rng) = Dir_To
            then
               declare
                  Left : constant Iir := Get_Left_Limit (Rng);
                  Right : constant Iir := Get_Right_Limit (Rng);
               begin
                  if Get_Kind (Left) = Iir_Kind_Enumeration_Literal
                    and then Get_Enum_Pos (Left) = 0
                    and then Get_Kind (Right) = Iir_Kind_Enumeration_Literal
                    and then Natural (Get_Enum_Pos (Right))
                               = (Get_Nbr_Elements
                                    (Get_Enumeration_Literal_List (Base)) - 1)
                  then
                     Subtype_Info.S.Same_Range := True;
                     return;
                  end if;
               end;
            end if;
         when others =>
            null;
      end case;

      --  So range is not the same.
      Subtype_Info.S.Same_Range := False;
      Subtype_Info.S.Range_Var := Null_Var;

      if Get_Expr_Staticness (Rng) /= Locally then
         --  Bounds are not known.
         --  Do the checks.
         Subtype_Info.S.Nocheck_Hi := False;
         Subtype_Info.S.Nocheck_Low := False;
      else
         --  Bounds are locally static.
         Get_Low_High_Limit (Rng, Lo, Hi);
         if Is_Overflow_Literal (Hi) or else Is_Overflow_Literal (Lo) then
            Subtype_Info.S.Nocheck_Hi := True;
            Subtype_Info.S.Nocheck_Low := True;
         else
            Subtype_Info.S.Nocheck_Hi :=
              Is_Equal_Limit (Hi, True, Def, Base_Info.Type_Mode);
            Subtype_Info.S.Nocheck_Low :=
              Is_Equal_Limit (Lo, False, Def, Base_Info.Type_Mode);
         end if;
      end if;
   end Create_Subtype_Info_From_Type;

   procedure Create_Type_Range_Var (Def : Iir)
   is
      Info      : constant Type_Info_Acc := Get_Info (Def);
      Base_Info : Type_Info_Acc;
      Val       : O_Cnode;
      Suffix    : String (1 .. 3) := "xTR";
   begin
      pragma Assert (Info.S.Range_Var = Null_Var);

      case Get_Kind (Def) is
         when Iir_Kinds_Subtype_Definition =>
            Suffix (1) := 'S'; -- "STR";
         when Iir_Kind_Enumeration_Type_Definition =>
            Suffix (1) := 'B'; -- "BTR";
         when others =>
            raise Internal_Error;
      end case;
      Base_Info := Get_Info (Get_Base_Type (Def));
      case Get_Type_Staticness (Def) is
         when None
            | Globally =>
            Info.S.Range_Var := Create_Var
              (Create_Var_Identifier (Suffix), Base_Info.B.Range_Type);
         when Locally =>
            if Global_Storage = O_Storage_External then
               --  Do not create the value of the type desc, since it
               --  is never dereferenced in a static type desc.
               Val := O_Cnode_Null;
            else
               Val := Create_Static_Type_Definition_Type_Range (Def);
            end if;
            Info.S.Range_Var := Create_Global_Const
              (Create_Identifier (Suffix),
               Base_Info.B.Range_Type, Global_Storage, Val);
         when Unknown =>
            raise Internal_Error;
      end case;
   end Create_Type_Range_Var;


   --  Call HANDLE_A_SUBTYPE for all type/subtypes declared with DEF
   --  (of course, this is a noop if DEF is not a composite type).
   generic
      with procedure Handle_A_Subtype (Atype : Iir);
   procedure Handle_Anonymous_Subtypes (Def : Iir);

   procedure Handle_Anonymous_Subtypes (Def : Iir) is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Array_Type_Definition
            | Iir_Kind_Array_Subtype_Definition =>
            declare
               Asub : Iir;
            begin
               Asub := Get_Element_Subtype (Def);
               if Is_Anonymous_Type_Definition (Asub) then
                  Handle_A_Subtype (Asub);
               end if;
            end;
         when Iir_Kind_Record_Type_Definition =>
            declare
               List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Def);
               El   : Iir;
               Asub : Iir;
            begin
               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  Asub := Get_Type (El);
                  if Is_Anonymous_Type_Definition (Asub) then
                     Handle_A_Subtype (Asub);
                  end if;
               end loop;
            end;
         when Iir_Kind_Record_Subtype_Definition =>
            declare
               List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Def);
               El   : Iir;
               Asub : Iir;
            begin
               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  if Get_Kind (El) = Iir_Kind_Record_Element_Constraint then
                     Asub := Get_Type (El);
                     if Is_Anonymous_Type_Definition (Asub) then
                        Handle_A_Subtype (Asub);
                     end if;
                  end if;
               end loop;
            end;
         when others =>
            null;
      end case;
   end Handle_Anonymous_Subtypes;

   procedure Translate_Array_Element_Definition (Def : Iir)
   is
      El_Type : constant Iir := Get_Element_Subtype (Def);
      Mark    : Id_Mark_Type;
   begin
      if Get_Info (El_Type) = null then
         Push_Identifier_Prefix (Mark, "ET");
         Translate_Subtype_Indication (El_Type, True);
         Pop_Identifier_Prefix (Mark);
      end if;
   end Translate_Array_Element_Definition;

   --  Note: boolean types are translated by translate_bool_type_definition!
   procedure Translate_Type_Definition (Def : Iir)
   is
      Info          : Ortho_Info_Acc;
      Complete_Info : Incomplete_Type_Info_Acc;
   begin
      --  Handle the special case of incomplete type.
      if Get_Kind (Def) = Iir_Kind_Incomplete_Type_Definition then
         Translate_Incomplete_Type (Def);
         return;
      end if;

      --  If the definition is already translated, return now.
      Info := Get_Info (Def);
      if Info /= null then
         case Info.Kind is
            when Kind_Type =>
               --  The subtype was already translated.
               return;
            when Kind_Incomplete_Type =>
               --  Type is being completed.
               Complete_Info := Info;
               Clear_Info (Def);
            when others =>
               raise Internal_Error;
         end case;
      else
         Complete_Info := null;
      end if;

      Info := Add_Info (Def, Kind_Type);

      case Get_Kind (Def) is
         when Iir_Kind_Enumeration_Type_Definition =>
            Translate_Enumeration_Type (Def);
            Create_Scalar_Type_Range_Type (Def, True);
            Create_Type_Range_Var (Def);

         when Iir_Kind_Integer_Type_Definition =>
            Translate_Integer_Type (Def);
            Create_Scalar_Type_Range_Type (Def, True);

         when Iir_Kind_Physical_Type_Definition =>
            Translate_Physical_Type (Def);
            Create_Scalar_Type_Range_Type (Def, False);
            if Get_Type_Staticness (Def) /= Locally then
               Translate_Physical_Units (Def);
            else
               Info.S.Range_Var := Null_Var;
            end if;

         when Iir_Kind_Floating_Type_Definition =>
            Translate_Floating_Type (Def);
            Create_Scalar_Type_Range_Type (Def, False);

         when Iir_Kind_Array_Type_Definition =>
            Translate_Array_Element_Definition (Def);
            Translate_Array_Type (Def);

         when Iir_Kind_Record_Type_Definition =>
            Info.B := Ortho_Info_Basetype_Record_Init;
            Translate_Record_Type (Def);

         when Iir_Kind_Access_Type_Definition =>
            declare
               Dtype : constant Iir := Get_Designated_Type (Def);
               Mark : Id_Mark_Type;
            begin
               --  Translate the subtype
               if Is_Anonymous_Type_Definition (Dtype) then
                  Push_Identifier_Prefix (Mark, "AT");
                  Translate_Subtype_Indication (Dtype, True);
                  Pop_Identifier_Prefix (Mark);
               end if;
               Translate_Access_Type (Def);
            end;

         when Iir_Kind_File_Type_Definition =>
            Info.B := Ortho_Info_Basetype_File_Init;
            Translate_File_Type (Def);
            Create_File_Type_Var (Def);

         when Iir_Kind_Protected_Type_Declaration =>
            Info.B := Ortho_Info_Basetype_Prot_Init;
            Translate_Protected_Type (Def);

         when others =>
            Error_Kind ("translate_type_definition", Def);
      end case;

      if Complete_Info /= null then
         Translate_Complete_Type (Complete_Info);
      end if;
   end Translate_Type_Definition;

   procedure Translate_Bool_Type_Definition (Def : Iir)
   is
      Info : Type_Info_Acc;
      pragma Unreferenced (Info);
   begin
      --  Not already translated.
      pragma Assert (Get_Info (Def) = null);

      --  A boolean type is an enumerated type.
      pragma Assert (Get_Kind (Def) = Iir_Kind_Enumeration_Type_Definition);

      Info := Add_Info (Def, Kind_Type);

      Translate_Bool_Type (Def);

      --  This is usually done in translate_type_definition, but boolean
      --  types are not handled by translate_type_definition.
      Create_Scalar_Type_Range_Type (Def, True);
      Create_Type_Range_Var (Def);
   end Translate_Bool_Type_Definition;

   procedure Translate_Subtype_Definition
     (Def : Iir; With_Vars : Boolean := True)
   is
      Info          : Ortho_Info_Acc;
      Complete_Info : Incomplete_Type_Info_Acc;
   begin
      --  If the definition is already translated, return now.
      Info := Get_Info (Def);
      if Info /= null then
         case Info.Kind is
            when Kind_Type =>
               --  The subtype was already translated.
               return;
            when Kind_Incomplete_Type =>
               --  Type is being completed.
               Complete_Info := Info;
               Clear_Info (Def);
            when others =>
               raise Internal_Error;
         end case;
      else
         Complete_Info := null;
      end if;

      Info := Add_Info (Def, Kind_Type);

      case Get_Kind (Def) is
         when Iir_Kinds_Scalar_Subtype_Definition =>
            Create_Subtype_Info_From_Type (Def, Get_Parent_Type (Def), Info);
            if With_Vars and then not Info.S.Same_Range then
               Create_Type_Range_Var (Def);
            end if;

         when Iir_Kind_Array_Subtype_Definition =>
            Translate_Array_Subtype_Definition (Def);
            if With_Vars
--              and then Get_Index_Constraint_Flag (Def)
            then
               Create_Composite_Subtype_Layout_Var (Def, False);
            end if;

         when Iir_Kind_Record_Subtype_Definition =>
            Translate_Record_Subtype_Definition (Def);
            if With_Vars
              and then Get_Owned_Elements_Chain (Def) /= Null_Iir
            then
               Create_Composite_Subtype_Layout_Var (Def, False);
            end if;

         when Iir_Kind_Access_Subtype_Definition =>
            declare
               Ind : constant Iir := Get_Designated_Subtype_Indication (Def);
               Mark : Id_Mark_Type;
            begin
               if Ind /= Null_Iir then
                  Push_Identifier_Prefix (Mark, "EST");
                  Translate_Subtype_Definition (Ind, With_Vars);
                  Pop_Identifier_Prefix (Mark);
               end if;

               --  Like the access type.
               Free_Info (Def);
               Set_Info (Def, Get_Info (Get_Parent_Type (Def)));
            end;

         when Iir_Kind_File_Subtype_Definition =>
            --  Same as parent.
            Free_Info (Def);
            Set_Info (Def, Get_Info (Get_Parent_Type (Def)));

         when others =>
            Error_Kind ("translate_subtype_definition", Def);
      end case;

      if Complete_Info /= null then
         Translate_Complete_Type (Complete_Info);
      end if;
   end Translate_Subtype_Definition;

   procedure Translate_Type_Subprograms
     (Decl : Iir; Kind : Subprg_Translate_Kind)
   is
      Def   : constant Iir := Get_Type_Definition (Decl);
      Tinfo : Type_Info_Acc;
      Id    : Name_Id;
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Incomplete_Type_Definition =>
            return;
         when Iir_Kind_Protected_Type_Declaration =>
            if Kind in Subprg_Translate_Spec then
               Translate_Protected_Type_Subprograms_Spec (Def);
            end if;
            return;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition =>
            null;
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Access_Type_Definition =>
            --  Never complex.
            return;
         when others =>
            raise Internal_Error;
      end case;

      --  Create builder for arrays and non-static records
      Tinfo := Get_Info (Def);
      case Tinfo.Type_Mode is
         when Type_Mode_Fat_Array
           | Type_Mode_Unbounded_Record
           | Type_Mode_Complex_Record =>
            null;
         when Type_Mode_Static_Record =>
            return;
         when others =>
            --  Must have been filtered out above.
            raise Internal_Error;
      end case;

      if Kind in Subprg_Translate_Spec then
         --  Declare subprograms.
         Id := Get_Identifier (Decl);
         for Kind in Mode_Value .. Type_To_Last_Object_Kind (Def) loop
            Create_Builder_Subprogram_Decl (Tinfo, Id, Kind);
         end loop;
      end if;

      if Kind in Subprg_Translate_Body then
         if Global_Storage = O_Storage_External then
            return;
         end if;

         --  Define subprograms.
         case Get_Kind (Def) is
            when Iir_Kind_Array_Type_Definition =>
               for Kind in Mode_Value .. Type_To_Last_Object_Kind (Def) loop
                  Create_Array_Type_Builder (Def, Kind);
               end loop;
            when Iir_Kind_Record_Type_Definition =>
               for Kind in Mode_Value .. Type_To_Last_Object_Kind (Def) loop
                  Create_Record_Type_Builder (Def, Kind);
               end loop;
            when others =>
               Error_Kind ("translate_type_subprograms", Def);
         end case;
      end if;
   end Translate_Type_Subprograms;

   --  Initialize the objects related to a type (type range and type
   --  descriptor).
   procedure Elab_Type_Definition (Def : Iir);
   procedure Elab_Subtype_Definition (Def : Iir);

   procedure Elab_Type_Definition_Depend is new Handle_Anonymous_Subtypes
     (Handle_A_Subtype => Elab_Subtype_Definition);

   procedure Elab_Type_Definition (Def : Iir) is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Incomplete_Type_Definition =>
            --  Nothing to do.
            return;
         when Iir_Kind_Protected_Type_Declaration =>
            --  Elaboration subprograms interfaces.
            declare
               Final : Boolean;
            begin
               Chap4.Elab_Declaration_Chain (Def, Final);

               --  No finalizer in protected types (only subprograms).
               pragma Assert (Final = False);
            end;
            return;
         when others =>
            null;
      end case;

      if Get_Type_Staticness (Def) = Locally then
         return;
      end if;

      Elab_Type_Definition_Depend (Def);

      Elab_Type_Definition_Type_Range (Def);
   end Elab_Type_Definition;

   procedure Translate_Subtype_Indication (Def : Iir; With_Vars : Boolean) is
   begin
      Translate_Subtype_Definition (Def, With_Vars);
   end Translate_Subtype_Indication;

   procedure Translate_Named_Subtype_Definition (Def : Iir; Id : Name_Id)
   is
      Mark : Id_Mark_Type;
   begin
      Push_Identifier_Prefix (Mark, Id);
      Chap3.Translate_Subtype_Indication (Def, True);
      Pop_Identifier_Prefix (Mark);
   end Translate_Named_Subtype_Definition;

   procedure Translate_Anonymous_Subtype_Definition
     (Def : Iir; With_Vars : Boolean)
   is
      Type_Info : constant Type_Info_Acc := Get_Info (Def);
      Mark      : Id_Mark_Type;
   begin
      if Type_Info /= null then
         return;
      end if;
      Push_Identifier_Prefix_Uniq (Mark);
      Chap3.Translate_Subtype_Definition (Def, With_Vars);
      Pop_Identifier_Prefix (Mark);
   end Translate_Anonymous_Subtype_Definition;

   procedure Translate_Object_Subtype_Indication (Decl      : Iir;
                                                  With_Vars : Boolean := True)
   is
      Def : Iir;
      Ind : Iir;
      Mark  : Id_Mark_Type;
      Mark2 : Id_Mark_Type;
   begin
      --  Notes about subtype_indication and type in a declaration:
      --  1) The subtype_indication is owned by the first declared
      --     object when there is a list of identifiers.  The following
      --     declarations are ref.
      if Get_Is_Ref (Decl) then
         return;
      end if;

      --  3) An object alias always have a type but may have no subtype
      --     indication.  Maybe this should be handled separately.
      --  4) An anonymous_signal_declaration has no subtype indication.
      --  5) It is not possible to translate the type when the subtype
      --     indication is a subtype_attribute.  So this is an exception
      --     TODO: if there is a list of identifiers.

      Push_Identifier_Prefix (Mark, Get_Identifier (Decl));

      Def := Get_Type (Decl);

      --  2) Constants may have a type that is different from the subtype
      --     indication, when the subtype indication is not fully constrained.
      --     This is new with vhdl 2008, where the subtype indication may
      --     add some constraints on the type mark and the initial value add
      --     even more constraints.
      if Get_Kind (Decl) = Iir_Kind_Constant_Declaration then
         Ind := Get_Subtype_Indication (Decl);
         Ind := Get_Type_Of_Subtype_Indication (Ind);
         if Ind /= Def then
            Push_Identifier_Prefix (Mark2, "OTI");
            Chap3.Translate_Subtype_Definition (Ind, With_Vars);
            Pop_Identifier_Prefix (Mark2);
         end if;
      end if;

      Push_Identifier_Prefix (Mark2, "OT");
      Chap3.Translate_Subtype_Definition (Def, With_Vars);
      Pop_Identifier_Prefix (Mark2);

      Pop_Identifier_Prefix (Mark);
   end Translate_Object_Subtype_Indication;

   procedure Elab_Object_Subtype_Indication (Decl : Iir)
   is
      Def : constant Iir := Get_Type (Decl);
   begin
      if not Is_Anonymous_Type_Definition (Def) then
         --  The type refers to a declared type, so already handled.
         return;
      end if;

      declare
         Ind : constant Iir := Get_Subtype_Indication (Decl);
      begin
         if Ind /= Null_Iir
           and then Get_Kind (Ind) = Iir_Kind_Subtype_Attribute
         then
            if Is_Fully_Constrained_Type (Get_Type (Get_Prefix (Ind))) then
               return;
            end if;
            raise Internal_Error;
         else
            Elab_Subtype_Definition (Def);
         end if;
      end;
   end Elab_Object_Subtype_Indication;

   procedure Elab_Type_Declaration (Decl : Iir) is
   begin
      Elab_Type_Definition (Get_Type_Definition (Decl));
   end Elab_Type_Declaration;

   procedure Elab_Subtype_Definition (Def : Iir)
   is
      Target : O_Lnode;
      Info   : Type_Info_Acc;
   begin
      if Get_Type_Staticness (Def) = Locally then
         return;
      end if;

      case Get_Kind (Def) is
         when Iir_Kinds_Scalar_Subtype_Definition =>
            Info := Get_Info (Def);
            if not Info.S.Same_Range then
               Target := Get_Var (Info.S.Range_Var);
               Elab_Scalar_Type_Range (Def, Target);
            end if;

         when Iir_Kind_Record_Subtype_Definition
            | Iir_Kind_Array_Subtype_Definition =>
            Info := Get_Info (Def);
            if Info.S.Composite_Layout /= Null_Var then
               Elab_Composite_Subtype_Layout (Def);
            end if;

         when Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_File_Subtype_Definition =>
            null;

         when others =>
            Error_Kind ("elab_subtype_definition", Def);
      end case;
   end Elab_Subtype_Definition;

   procedure Elab_Subtype_Declaration (Decl : Iir_Subtype_Declaration)
   is
      Def : constant Iir := Get_Type (Decl);
   begin
      Elab_Subtype_Definition (Def);
   end Elab_Subtype_Declaration;

   function Get_Static_Array_Length (Atype : Iir) return Int64
   is
      Indexes_List : constant Iir_Flist := Get_Index_Subtype_List (Atype);
      Nbr_Dim      : constant Natural := Get_Nbr_Elements (Indexes_List);
      Index        : Iir;
      Val          : Int64;
      Rng          : Iir;
   begin
      Val := 1;
      for I in 0 .. Nbr_Dim - 1 loop
         Index := Get_Index_Type (Indexes_List, I);
         Rng := Get_Range_Constraint (Index);
         Val := Val * Eval_Discrete_Range_Length (Rng);
      end loop;
      return Val;
      --  return New_Unsigned_Literal (Ghdl_Index_Type, Unsigned_64 (Val));
   end Get_Static_Array_Length;

   function Get_Thin_Array_Length (Atype : Iir) return O_Cnode is
   begin
      return New_Index_Lit (Unsigned_64 (Get_Static_Array_Length (Atype)));
   end Get_Thin_Array_Length;

   function Bounds_To_Range (B : Mnode; Atype : Iir; Dim : Positive)
                             return Mnode
   is
      Indexes_List    : constant Iir_Flist :=
        Get_Index_Subtype_Definition_List (Get_Base_Type (Atype));
      Index_Type_Mark : constant Iir :=
        Get_Nth_Element (Indexes_List, Dim - 1);
      Index_Type      : constant Iir := Get_Index_Type (Index_Type_Mark);
      Base_Index_Info : constant Index_Info_Acc :=
        Get_Info (Index_Type_Mark);
      Iinfo           : constant Type_Info_Acc :=
        Get_Info (Get_Base_Type (Index_Type));
   begin
      return Lv2M (New_Selected_Element (M2Lv (B),
                                         Base_Index_Info.Index_Field),
                   Iinfo, Mode_Value,
                   Iinfo.B.Range_Type, Iinfo.B.Range_Ptr_Type);
   end Bounds_To_Range;

   function Record_Bounds_To_Element_Bounds (B : Mnode; El : Iir)
                                            return Mnode is
   begin
      return Layout_To_Bounds (Record_Layout_To_Element_Layout (B, El));
   end Record_Bounds_To_Element_Bounds;

   function Array_Bounds_To_Element_Bounds (B : Mnode; Arr_Type : Iir)
                                           return Mnode is
   begin
      return Layout_To_Bounds (Array_Bounds_To_Element_Layout (B, Arr_Type));
   end Array_Bounds_To_Element_Bounds;

   function Array_Bounds_To_Element_Size
     (B : Mnode; Arr_Type : Iir; Mode : Object_Kind_Type) return O_Lnode is
   begin
      return Layout_To_Size
        (Array_Bounds_To_Element_Layout (B, Arr_Type), Mode);
   end Array_Bounds_To_Element_Size;

   function Type_To_Range (Atype : Iir) return Mnode
   is
      Info : constant Type_Info_Acc := Get_Info (Atype);
   begin
      return Varv2M (Info.S.Range_Var, Info, Mode_Value,
                     Info.B.Range_Type, Info.B.Range_Ptr_Type);
   end Type_To_Range;

   function Range_To_Length (R : Mnode) return Mnode
   is
      Tinfo : constant Type_Info_Acc := Get_Type_Info (R);
   begin
      return Lv2M (New_Selected_Element (M2Lv (R),
                   Tinfo.B.Range_Length),
                   Tinfo,
                   Mode_Value);
   end Range_To_Length;

   function Range_To_Dir (R : Mnode) return Mnode
   is
      Tinfo : constant Type_Info_Acc := Get_Type_Info (R);
   begin
      return Lv2M (New_Selected_Element (M2Lv (R),
                   Tinfo.B.Range_Dir),
                   Tinfo,
                   Mode_Value);
   end Range_To_Dir;

   function Range_To_Left (R : Mnode) return Mnode
   is
      Tinfo : Type_Info_Acc;
   begin
      Tinfo := Get_Type_Info (R);
      return Lv2M (New_Selected_Element (M2Lv (R),
                   Tinfo.B.Range_Left),
                   Tinfo,
                   Mode_Value);
   end Range_To_Left;

   function Range_To_Right (R : Mnode) return Mnode
   is
      Tinfo : Type_Info_Acc;
   begin
      Tinfo := Get_Type_Info (R);
      return Lv2M (New_Selected_Element (M2Lv (R),
                   Tinfo.B.Range_Right),
                   Tinfo,
                   Mode_Value);
   end Range_To_Right;

   function Get_Composite_Type_Bounds (Atype : Iir) return Mnode is
   begin
      return Layout_To_Bounds (Get_Composite_Type_Layout (Get_Info (Atype)));
   end Get_Composite_Type_Bounds;

   function Get_Composite_Bounds (Obj : Mnode) return Mnode
   is
      Info : constant Type_Info_Acc := Get_Type_Info (Obj);
   begin
      case Info.Type_Mode is
         when Type_Mode_Unbounded_Array
           | Type_Mode_Unbounded_Record =>
            declare
               Kind : constant Object_Kind_Type := Get_Object_Kind (Obj);
            begin
               return Lp2M
                 (New_Selected_Element (M2Lv (Obj),
                                        Info.B.Bounds_Field (Kind)),
                  Info,
                  Kind,
                  Info.B.Bounds_Type,
                  Info.B.Bounds_Ptr_Type);
            end;
         when Type_Mode_Bounded_Arrays =>
            return Layout_To_Bounds (Get_Composite_Type_Layout (Info));
         when Type_Mode_Bounded_Records =>
            return Get_Composite_Type_Layout (Info);
         when Type_Mode_Bounds_Acc =>
            return Lp2M (M2Lv (Obj), Info, Mode_Value);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Composite_Bounds;

   function Get_Array_Range (Arr : Mnode; Atype : Iir; Dim : Positive)
                             return Mnode is
   begin
      return Bounds_To_Range (Get_Composite_Bounds (Arr), Atype, Dim);
   end Get_Array_Range;

   function Get_Bounds_Length (Bounds : Mnode; Atype : Iir) return O_Enode
   is
      Type_Info     : constant Type_Info_Acc := Get_Info (Atype);
      Index_List    : constant Iir_Flist := Get_Index_Subtype_List (Atype);
      Nbr_Dim       : constant Natural := Get_Nbr_Elements (Index_List);
      Dim_Length    : O_Enode;
      Res           : O_Enode;
      Bounds_Stable : Mnode;
   begin
      if Type_Info.Type_Locally_Constrained then
         return New_Lit (Get_Thin_Array_Length (Atype));
      end if;

      if Nbr_Dim > 1 then
         Bounds_Stable := Stabilize (Bounds);
      else
         Bounds_Stable := Bounds;
      end if;

      for Dim in 1 .. Nbr_Dim loop
         Dim_Length :=
           M2E (Range_To_Length
                (Bounds_To_Range (Bounds_Stable, Atype, Dim)));
         if Dim = 1 then
            Res := Dim_Length;
         else
            Res := New_Dyadic_Op (ON_Mul_Ov, Res, Dim_Length);
         end if;
      end loop;
      return Res;
   end Get_Bounds_Length;

   function Get_Array_Type_Length (Atype : Iir) return O_Enode
   is
      Type_Info : constant Type_Info_Acc := Get_Info (Atype);
   begin
      if Type_Info.Type_Locally_Constrained then
         return New_Lit (Get_Thin_Array_Length (Atype));
      else
         return Get_Bounds_Length (Get_Composite_Type_Bounds (Atype), Atype);
      end if;
   end Get_Array_Type_Length;

   function Get_Array_Length (Arr : Mnode; Atype : Iir) return O_Enode
   is
      Type_Info : constant Type_Info_Acc := Get_Info (Atype);
   begin
      if Type_Info.Type_Locally_Constrained then
         return New_Lit (Get_Thin_Array_Length (Atype));
      else
         return Get_Bounds_Length (Get_Composite_Bounds (Arr), Atype);
      end if;
   end Get_Array_Length;

   --  Get the base part of a dope vector.
   function Get_Unbounded_Base (Arr : Mnode) return Mnode
   is
      Info : constant Type_Info_Acc := Get_Type_Info (Arr);
      Kind : constant Object_Kind_Type := Get_Object_Kind (Arr);
   begin
      pragma Assert (Info.Type_Mode in Type_Mode_Unbounded);
      return Lp2M
        (New_Selected_Element (M2Lv (Arr), Info.B.Base_Field (Kind)),
         Info, Kind,
         Info.B.Base_Type (Kind), Info.B.Base_Ptr_Type (Kind));
   end Get_Unbounded_Base;

   function Get_Composite_Base (Obj : Mnode) return Mnode
   is
      Info : constant Type_Info_Acc := Get_Type_Info (Obj);
   begin
      case Info.Type_Mode is
         when Type_Mode_Unbounded_Array
           | Type_Mode_Unbounded_Record =>
            return Get_Unbounded_Base (Obj);
         when Type_Mode_Bounded_Arrays
           | Type_Mode_Bounded_Records =>
            return Obj;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Composite_Base;

   function Get_Composite_Unbounded_Base (Obj : Mnode) return Mnode
   is
      Info : constant Type_Info_Acc := Get_Type_Info (Obj);
   begin
      case Info.Type_Mode is
         when Type_Mode_Unbounded_Array
           | Type_Mode_Unbounded_Record =>
            return Get_Unbounded_Base (Obj);
         when Type_Mode_Bounded_Arrays =>
            --  This works in ortho as an access to unconstrained array is
            --  also an access to a constrained array.
            return Obj;
         when Type_Mode_Bounded_Records =>
            return Obj;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Composite_Unbounded_Base;

   function Create_Maybe_Fat_Array_Element (Arr : Mnode; Arr_Type : Iir)
                                           return Mnode
   is
      El_Type : constant Iir := Get_Element_Subtype (Arr_Type);
      El_Info : constant Type_Info_Acc := Get_Info (El_Type);
      El_Unbounded : constant Boolean := Is_Unbounded_Type (El_Info);
      Kind : constant Object_Kind_Type := Get_Object_Kind (Arr);
      Var_El : Mnode;
   begin
      if El_Unbounded then
         Var_El := Create_Temp (El_Info, Kind);
         New_Assign_Stmt
           (M2Lp (Chap3.Get_Composite_Bounds (Var_El)),
            M2Addr (Chap3.Array_Bounds_To_Element_Bounds
                      (Chap3.Get_Composite_Bounds (Arr), Arr_Type)));
         return Var_El;
      else
         return Mnode_Null;
      end if;
   end Create_Maybe_Fat_Array_Element;

   function Assign_Maybe_Fat_Array_Element (Var : Mnode; El : Mnode)
                                           return Mnode is
   begin
      if Var = Mnode_Null then
         return El;
      else
         New_Assign_Stmt (M2Lp (Chap3.Get_Composite_Base (Var)), M2Addr (El));
         return Var;
      end if;
   end Assign_Maybe_Fat_Array_Element;

   function Get_Bounds_Acc_Base
     (Acc : O_Enode; D_Type : Iir) return O_Enode
   is
      D_Info : constant Type_Info_Acc := Get_Info (D_Type);
   begin
      return Add_Pointer
        (Acc,
         New_Lit (New_Sizeof (D_Info.B.Bounds_Type, Ghdl_Index_Type)),
         D_Info.B.Base_Ptr_Type (Mode_Value));
   end Get_Bounds_Acc_Base;

   function Reindex_Complex_Array
     (Base : Mnode; Atype : Iir; Index : O_Enode; Res_Info : Type_Info_Acc)
      return Mnode
   is
      Kind     : constant Object_Kind_Type := Get_Object_Kind (Base);
      El_Type  : constant Iir := Get_Element_Subtype (Atype);
      Stride   : O_Enode;
      Res      : O_Enode;
   begin
      Stride := Get_Subtype_Size (El_Type, Mnode_Null, Kind);
      Res := Add_Pointer (M2E (Base),
                          New_Dyadic_Op (ON_Mul_Ov, Stride, Index),
                          Res_Info.Ortho_Ptr_Type (Kind));
      return E2M (Res, Res_Info, Kind);
   end Reindex_Complex_Array;

   function Index_Base (Base : Mnode;
                        Atype : Iir;
                        Index : O_Enode;
                        Stride : O_Enode := O_Enode_Null) return Mnode
   is
      Arr_Tinfo : constant Type_Info_Acc := Get_Type_Info (Base);
      Kind      : constant Object_Kind_Type := Get_Object_Kind (Base);
      El_Type   : constant Iir := Get_Element_Subtype (Atype);
      El_Tinfo  : constant Type_Info_Acc := Get_Info (El_Type);
   begin
      if Arr_Tinfo.Type_Mode = Type_Mode_Static_Array
        or else Is_Static_Type (Get_Info (Get_Element_Subtype
                                            (Get_Base_Type (Atype))))
      then
         --  If the array is fully constrained it can be indexed.
         return Lv2M (New_Indexed_Element (M2Lv (Base), Index),
                      El_Tinfo, Kind);
      elsif Is_Unbounded_Type (El_Tinfo) then
         pragma Assert (Stride /= O_Enode_Null);
         return E2M (Add_Pointer (M2E (Base),
                                  New_Dyadic_Op (ON_Mul_Ov, Index, Stride),
                                  El_Tinfo.B.Base_Ptr_Type (Kind)),
                     El_Tinfo, Kind,
                     El_Tinfo.B.Base_Type (Kind),
                     El_Tinfo.B.Base_Ptr_Type (Kind));
      end if;

      --  If the element type of the base type is static, the array
      --  can be directly indexed.
      return Reindex_Complex_Array (Base, Atype, Index, El_Tinfo);
   end Index_Base;

   function Convert_Array_Base (Arr : Mnode) return Mnode
   is
      Type_Info : constant Type_Info_Acc := Get_Type_Info (Arr);
      Mode : constant Object_Kind_Type := Get_Object_Kind (Arr);
   begin
      if Type_Info.Ortho_Ptr_Type (Mode) /= Type_Info.B.Base_Ptr_Type (Mode)
      then
         return E2M
           (New_Convert_Ov (M2E (Arr), Type_Info.B.Base_Ptr_Type (Mode)),
            Type_Info, Mode);
      else
         return Arr;
      end if;
   end Convert_Array_Base;

   --  For array with unbounded elements, return the stride.
   --  Otherwise, return O_Enode_Null.
   function Get_Array_Stride (Arr : Mnode; Atype : Iir) return O_Enode
   is
      El_Type  : constant Iir := Get_Element_Subtype (Atype);
      El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
      Kind     : constant Object_Kind_Type := Get_Object_Kind (Arr);
   begin
      if Is_Unbounded_Type (El_Tinfo) then
         return New_Value
           (Array_Bounds_To_Element_Size
              (Get_Composite_Bounds (Arr), Atype, Kind));
      else
         return O_Enode_Null;
      end if;
   end Get_Array_Stride;

   function Index_Array (Arr : Mnode; Atype : Iir; Index : O_Enode)
                        return Mnode
   is
      Base     : Mnode;
      Stride   : O_Enode;
   begin
      Base := Get_Composite_Base (Arr);
      --  For indexing, we need to consider the size of elements.
      Stride := Get_Array_Stride (Arr, Atype);
      return Index_Base (Base, Atype, Index, Stride);
   end Index_Array;

   function Slice_Base
     (Base : Mnode; Atype : Iir; Index : O_Enode; Stride : O_Enode)
     return Mnode
   is
      T_Info   : constant Type_Info_Acc := Get_Info (Atype);
      El_Type  : constant Iir := Get_Element_Subtype_For_Info (Atype);
      El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
      Kind     : constant Object_Kind_Type := Get_Object_Kind (Base);
   begin
      if not Is_Static_Type (El_Tinfo) then
         pragma Assert (T_Info.Type_Mode /= Type_Mode_Static_Array);
         if Stride /= O_Enode_Null then
            return E2M
              (Add_Pointer (M2E (Base),
                            New_Dyadic_Op (ON_Mul_Ov, Stride, Index),
                            T_Info.Ortho_Ptr_Type (Kind)),
               T_Info, Kind);
         else
            return Reindex_Complex_Array (Base, Atype, Index, T_Info);
         end if;
      end if;

      if T_Info.Type_Mode = Type_Mode_Static_Array then
         --  Static array.  Use the type of the array.
         return Lv2M (New_Slice (M2Lv (Base),
                                 T_Info.Ortho_Type (Kind),
                                 Index),
                      T_Info, Kind,
                      T_Info.Ortho_Type (Kind),
                      T_Info.Ortho_Ptr_Type (Kind));
      else
         --  The base is sliced, so use the ortho type of the base.
         return Lv2M (New_Slice (M2Lv (Base),
                                 T_Info.B.Base_Type (Kind),
                                 Index),
                      T_Info, Kind,
                      T_Info.B.Base_Type (Kind),
                      T_Info.B.Base_Ptr_Type (Kind));
      end if;
   end Slice_Base;

   procedure Allocate_Unbounded_Composite_Base (Alloc_Kind : Allocation_Kind;
                                                Res        : Mnode;
                                                Arr_Type   : Iir)
   is
      Dinfo  : constant Type_Info_Acc :=
        Get_Info (Get_Base_Type (Arr_Type));
      Kind   : constant Object_Kind_Type := Get_Object_Kind (Res);
      Length : O_Enode;
   begin
      --  Compute array size.
      Length := Get_Object_Size (Res, Arr_Type);
      --  Allocate the storage for the elements.
      New_Assign_Stmt
        (M2Lp (Chap3.Get_Composite_Base (Res)),
         Gen_Alloc (Alloc_Kind, Length, Dinfo.B.Base_Ptr_Type (Kind)));
   end Allocate_Unbounded_Composite_Base;

   procedure Allocate_Unbounded_Composite_Bounds
     (Alloc_Kind : Allocation_Kind;
      Res        : Mnode;
      Obj_Type   : Iir)
   is
      Tinfo : constant Type_Info_Acc := Get_Info (Obj_Type);
   begin
      pragma Assert (Tinfo.Type_Mode in Type_Mode_Unbounded);
      --  Allocate memory for bounds.
      New_Assign_Stmt
        (M2Lp (Chap3.Get_Composite_Bounds (Res)),
         Gen_Alloc (Alloc_Kind,
                    New_Lit (New_Sizeof (Tinfo.B.Bounds_Type,
                                         Ghdl_Index_Type)),
                    Tinfo.B.Bounds_Ptr_Type));
   end Allocate_Unbounded_Composite_Bounds;

   --  For aliases of a slice.
   procedure Translate_Array_Subtype (Arr_Type : Iir) is
   begin
      Translate_Subtype_Definition (Arr_Type, False);
      Create_Composite_Subtype_Layout_Var (Arr_Type, False);
   end Translate_Array_Subtype;

   procedure Elab_Array_Subtype (Arr_Type : Iir) is
   begin
      Chap3.Elab_Composite_Subtype_Layout (Arr_Type);
   end Elab_Array_Subtype;

   procedure Create_Composite_Subtype (Sub_Type : Iir; Elab : Boolean := True)
   is
      Mark : Id_Mark_Type;
   begin
      Push_Identifier_Prefix_Uniq (Mark);
      if Get_Info (Sub_Type) = null then
         --  Minimal subtype creation.
         Translate_Subtype_Definition (Sub_Type, False);
      end if;
      --  Force creation of variables.
      Chap3.Create_Composite_Subtype_Layout_Var (Sub_Type, Elab);
      Pop_Identifier_Prefix (Mark);
   end Create_Composite_Subtype;

   --  Copy SRC to DEST.
   --  Both have the same type, OTYPE.
   procedure Translate_Object_Copy (Dest     : Mnode;
                                    Src      : Mnode;
                                    Obj_Type : Iir)
   is
      Info : constant Type_Info_Acc := Get_Info (Obj_Type);
      D    : Mnode;
   begin
      case Info.Type_Mode is
         when Type_Mode_Scalar
           | Type_Mode_Acc
           | Type_Mode_Bounds_Acc
           | Type_Mode_File =>
            --  Scalar or thin pointer.
            New_Assign_Stmt (M2Lv (Dest), M2E (Src));
         when Type_Mode_Unbounded_Array
           | Type_Mode_Unbounded_Record =>
            --  a fat array.
            D := Stabilize (Dest);
            Gen_Memcpy (M2Addr (Get_Composite_Base (D)),
                        M2Addr (Get_Composite_Base (Src)),
                        Get_Object_Size (D, Obj_Type));
         when Type_Mode_Bounded_Arrays
            | Type_Mode_Bounded_Records =>
            D := Stabilize (Dest);
            Gen_Memcpy (M2Addr (D), M2Addr (Src),
                        Get_Object_Size (D, Obj_Type));
         when Type_Mode_Unknown
            | Type_Mode_Protected =>
            raise Internal_Error;
      end case;
   end Translate_Object_Copy;

   function Get_Subtype_Size
     (Atype : Iir; Bounds : Mnode; Kind : Object_Kind_Type) return O_Enode
   is
      Type_Info : constant Type_Info_Acc := Get_Info (Atype);
   begin
      case Type_Info.Type_Mode is
         when Type_Mode_Non_Composite
            | Type_Mode_Static_Array
            | Type_Mode_Static_Record =>
            return New_Lit (New_Sizeof (Type_Info.Ortho_Type (Kind),
                                        Ghdl_Index_Type));
         when Type_Mode_Complex_Array
           | Type_Mode_Complex_Record =>
            --  The length is pre-computed for a complex bounded type.
            return New_Value
              (Layout_To_Size (Get_Composite_Type_Layout (Type_Info), Kind));
         when Type_Mode_Unbounded_Array =>
            declare
               El_Type  : constant Iir := Get_Element_Subtype (Atype);
               El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
               El_Sz    : O_Enode;
               Bounds1  : Mnode;
            begin
               if El_Tinfo.Type_Mode in Type_Mode_Unbounded then
                  Bounds1 := Stabilize (Bounds);
                  El_Sz := New_Value
                    (Layout_To_Size
                       (Array_Bounds_To_Element_Layout (Bounds1, Atype),
                        Kind));
               else
                  Bounds1 := Bounds;
                  El_Sz := Get_Subtype_Size (El_Type, Mnode_Null, Kind);
               end if;
               return New_Dyadic_Op
                 (ON_Mul_Ov, Chap3.Get_Bounds_Length (Bounds1, Atype), El_Sz);
            end;
         when Type_Mode_Unbounded_Record =>
            return New_Value (Sizes_To_Size (Layout_To_Sizes (Bounds), Kind));
         when others =>
            raise Internal_Error;
      end case;
   end Get_Subtype_Size;

   function Get_Object_Size (Obj : Mnode; Obj_Type : Iir) return O_Enode
   is
      Type_Info : constant Type_Info_Acc := Get_Type_Info (Obj);
      Kind      : constant Object_Kind_Type := Get_Object_Kind (Obj);
   begin
      if Type_Info.Type_Mode in Type_Mode_Unbounded then
         return Get_Subtype_Size (Obj_Type, Get_Composite_Bounds (Obj), Kind);
      else
         return Get_Subtype_Size (Obj_Type, Mnode_Null, Kind);
      end if;
   end Get_Object_Size;

   procedure Copy_Bounds (Dest : O_Enode; Src : O_Enode; Obj_Type : Iir)
   is
      Tinfo : constant Type_Info_Acc := Get_Info (Obj_Type);
   begin
      Gen_Memcpy
        (Dest, Src,
         New_Lit (New_Sizeof (Tinfo.B.Bounds_Type, Ghdl_Index_Type)));
   end Copy_Bounds;

   procedure Copy_Bounds (Dest : Mnode; Src : Mnode; Obj_Type : Iir) is
   begin
      Copy_Bounds (M2Addr (Dest), M2Addr (Src), Obj_Type);
   end Copy_Bounds;

   procedure Translate_Object_Allocation
     (Res        : in out Mnode;
      Alloc_Kind : Allocation_Kind;
      Obj_Type   : Iir;
      Bounds     : Mnode)
   is
      Tinfo : constant Type_Info_Acc := Get_Info (Obj_Type);
      Kind  : constant Object_Kind_Type := Get_Object_Kind (Res);
   begin
      if Tinfo.Type_Mode in Type_Mode_Unbounded then
         --  Allocate bounds and copy.
         Allocate_Unbounded_Composite_Bounds (Alloc_Kind, Res, Obj_Type);
         Copy_Bounds (Chap3.Get_Composite_Bounds (Res), Bounds, Obj_Type);
         --  Allocate base.
         Allocate_Unbounded_Composite_Base (Alloc_Kind, Res, Obj_Type);
      else
         New_Assign_Stmt
           (M2Lp (Res),
            Gen_Alloc (Alloc_Kind,
                       Chap3.Get_Object_Size (T2M (Obj_Type, Kind), Obj_Type),
                       Tinfo.Ortho_Ptr_Type (Kind)));
      end if;
   end Translate_Object_Allocation;

   procedure Gen_Deallocate (Obj : O_Enode)
   is
      Assocs : O_Assoc_List;
   begin
      Start_Association (Assocs, Ghdl_Deallocate);
      New_Association (Assocs, New_Convert_Ov (Obj, Ghdl_Ptr_Type));
      New_Procedure_Call (Assocs);
   end Gen_Deallocate;

   --  Performs deallocation of PARAM (the parameter of a deallocate call).
   procedure Translate_Object_Deallocation (Param : Iir)
   is
      Param_Type : constant Iir := Get_Type (Param);
      Info       : constant Type_Info_Acc := Get_Info (Param_Type);
      Val        : Mnode;
   begin
      --  Compute parameter
      Val := Chap6.Translate_Name (Param, Mode_Value);
      Stabilize (Val);

      --  Call deallocator.
      Gen_Deallocate (New_Value (M2Lv (Val)));

      --  Set the value to null.
      New_Assign_Stmt
        (M2Lv (Val), New_Lit (New_Null_Access (Info.Ortho_Type (Mode_Value))));
   end Translate_Object_Deallocation;

   function Not_In_Range (Value : O_Dnode; Atype : Iir) return O_Enode
   is
      Constr : constant Iir := Get_Range_Constraint (Atype);
      Info   : constant Type_Info_Acc := Get_Info (Atype);

      function Gen_Compare (Low : O_Enode; Hi : O_Enode) return O_Enode
      is
         L, H : O_Enode;
      begin
         if not Info.S.Nocheck_Low then
            L := New_Compare_Op
              (ON_Lt, New_Obj_Value (Value), Low, Ghdl_Bool_Type);
         end if;
         if not Info.S.Nocheck_Hi then
            H := New_Compare_Op
              (ON_Gt, New_Obj_Value (Value), Hi, Ghdl_Bool_Type);
         end if;
         if Info.S.Nocheck_Hi then
            if Info.S.Nocheck_Low then
               --  Should not happen!
               return New_Lit (Ghdl_Bool_False_Node);
            else
               return L;
            end if;
         else
            if Info.S.Nocheck_Low then
               return H;
            else
               return New_Dyadic_Op (ON_Or, L, H);
            end if;
         end if;
      end Gen_Compare;

      function Gen_Compare_To return O_Enode is
      begin
         return Gen_Compare
           (Chap14.Translate_Left_Type_Attribute (Atype),
            Chap14.Translate_Right_Type_Attribute (Atype));
      end Gen_Compare_To;

      function Gen_Compare_Downto return O_Enode is
      begin
         return Gen_Compare
           (Chap14.Translate_Right_Type_Attribute (Atype),
            Chap14.Translate_Left_Type_Attribute (Atype));
      end Gen_Compare_Downto;

      Var_Res : O_Dnode;
      If_Blk  : O_If_Block;
   begin
      if Get_Kind (Constr) = Iir_Kind_Range_Expression then
         --  Constraint is a range expression, therefore, direction is
         --  known.
         case Get_Direction (Constr) is
            when Dir_To =>
               return Gen_Compare_To;
            when Dir_Downto =>
               return Gen_Compare_Downto;
         end case;
      end if;

      --  Range constraint is not static
      --    full check (lot's of code ?).
      Var_Res := Create_Temp (Ghdl_Bool_Type);
      Start_If_Stmt
        (If_Blk,
         New_Compare_Op (ON_Eq,
           Chap14.Translate_Dir_Type_Attribute (Atype),
           New_Lit (Ghdl_Dir_To_Node),
           Ghdl_Bool_Type));
      --  To.
      New_Assign_Stmt (New_Obj (Var_Res), Gen_Compare_To);
      New_Else_Stmt (If_Blk);
      --  Downto
      New_Assign_Stmt (New_Obj (Var_Res), Gen_Compare_Downto);
      Finish_If_Stmt (If_Blk);
      return New_Obj_Value (Var_Res);
   end Not_In_Range;

   function Need_Range_Check (Expr : Iir; Atype : Iir) return Boolean
   is
      Info : constant Type_Info_Acc := Get_Info (Atype);
   begin
      if Info.S.Nocheck_Low and Info.S.Nocheck_Hi then
         return False;
      end if;
      if Expr /= Null_Iir and then Get_Type (Expr) = Atype then
         return False;
      end if;
      return True;
   end Need_Range_Check;

   procedure Check_Range
     (Value : O_Dnode; Expr : Iir; Atype : Iir; Loc : Iir)
   is
      If_Blk : O_If_Block;
   begin
      if not Need_Range_Check (Expr, Atype) then
         return;
      end if;

      if Expr /= Null_Iir
        and then Get_Expr_Staticness (Expr) = Locally
        and then Get_Type_Staticness (Atype) = Locally
      then
         if not Eval_Is_In_Bound (Eval_Static_Expr (Expr), Atype) then
            Chap6.Gen_Bound_Error (Loc);
         end if;
      else
         Open_Temp;
         Start_If_Stmt (If_Blk, Not_In_Range (Value, Atype));
         Chap6.Gen_Bound_Error (Loc);
         Finish_If_Stmt (If_Blk);
         Close_Temp;
      end if;
   end Check_Range;

   function Insert_Scalar_Check
     (Value : O_Enode; Expr : Iir; Atype : Iir; Loc : Iir) return O_Enode
   is
      Var : O_Dnode;
   begin
      Var := Create_Temp_Init
        (Get_Ortho_Type (Get_Base_Type (Atype), Mode_Value), Value);
      Check_Range (Var, Expr, Atype, Loc);
      return New_Obj_Value (Var);
   end Insert_Scalar_Check;

   function Maybe_Insert_Scalar_Check
     (Value : O_Enode; Expr : Iir; Atype : Iir) return O_Enode
   is
      Expr_Type : constant Iir := Get_Type (Expr);
   begin
      --  pragma Assert (Base_Type = Get_Base_Type (Atype));
      if Get_Kind (Expr_Type) in Iir_Kinds_Scalar_Type_And_Subtype_Definition
        and then Need_Range_Check (Expr, Atype)
      then
         return Insert_Scalar_Check (Value, Expr, Atype, Expr);
      else
         return Value;
      end if;
   end Maybe_Insert_Scalar_Check;

   function Locally_Array_Match (L_Type, R_Type : Iir) return Tri_State_Type
   is
      L_Indexes : constant Iir_Flist := Get_Index_Subtype_List (L_Type);
      R_Indexes : constant Iir_Flist := Get_Index_Subtype_List (R_Type);
      L_El      : Iir;
      R_El      : Iir;
   begin
      for I in Flist_First .. Flist_Last (L_Indexes) loop
         L_El := Get_Index_Type (L_Indexes, I);
         R_El := Get_Index_Type (R_Indexes, I);
         if Get_Type_Staticness (L_El) /= Locally
           or else Get_Type_Staticness (R_El) /= Locally
         then
            return Unknown;
         end if;
         if Eval_Discrete_Type_Length (L_El)
           /= Eval_Discrete_Type_Length (R_El)
         then
            return False;
         end if;
      end loop;
      return Locally_Types_Match (Get_Element_Subtype (L_Type),
                                  Get_Element_Subtype (R_Type));
   end Locally_Array_Match;

   function Locally_Record_Match (L_Type : Iir; R_Type : Iir)
                                 return Tri_State_Type
   is
      L_List : constant Iir_Flist := Get_Elements_Declaration_List (L_Type);
      R_List : constant Iir_Flist := Get_Elements_Declaration_List (R_Type);
      Res : Tri_State_Type;
   begin
      Res := True;
      for I in Flist_First .. Flist_Last (L_List) loop
         case Locally_Types_Match (Get_Type (Get_Nth_Element (L_List, I)),
                                   Get_Type (Get_Nth_Element (R_List, I))) is
            when False =>
               return False;
            when True =>
               null;
            when Unknown =>
               Res := Unknown;
         end case;
      end loop;
      return Res;
   end Locally_Record_Match;

   --  Return True IFF locally static types L_TYPE and R_TYPE matches.
   function Locally_Types_Match (L_Type : Iir; R_Type : Iir)
                                return Tri_State_Type is
   begin
      if L_Type = R_Type then
         return True;
      end if;
      case Get_Kind (L_Type) is
         when Iir_Kind_Array_Subtype_Definition =>
            return Locally_Array_Match (L_Type, R_Type);
         when Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Record_Type_Definition =>
            return Locally_Record_Match (L_Type, R_Type);
         when Iir_Kinds_Scalar_Type_And_Subtype_Definition =>
            return True;
         when Iir_Kind_Access_Type_Definition
           | Iir_Kind_Access_Subtype_Definition =>
            return True;
         when others =>
            Error_Kind ("locally_types_match", L_Type);
      end case;
   end Locally_Types_Match;

   function Types_Match (L_Type : Iir; R_Type : Iir) return Tri_State_Type is
   begin
      if Get_Kind (L_Type) not in Iir_Kinds_Composite_Type_Definition then
         return True;
      end if;
      if Get_Constraint_State (L_Type) /= Fully_Constrained
        or else Get_Constraint_State (R_Type) /= Fully_Constrained
      then
         --  If one of the type is not fully constrained, the check is dynamic.
         return Unknown;
      end if;
      if L_Type = R_Type then
         --  If the type is the same, they match (they are constrained).
         return True;
      end if;
      --  We cannot use type staticness, as a record may not be locally static
      --  because it has one scalar element with non-locally static bounds.
      return Locally_Types_Match (L_Type, R_Type);
   end Types_Match;

   function Check_Match_Cond (L_Type : Iir;
                              L_Bounds : Mnode;
                              R_Type : Iir;
                              R_Bounds : Mnode) return O_Enode is
   begin
      case Iir_Kinds_Composite_Type_Definition (Get_Kind (L_Type)) is
         when Iir_Kinds_Array_Type_Definition =>
            --  Check length match.
            declare
               Index_List : constant Iir_Flist :=
                 Get_Index_Subtype_List (L_Type);
               Nbr_Dim : constant Natural := Get_Nbr_Elements (Index_List);
               L_El : constant Iir := Get_Element_Subtype (L_Type);
               R_El : constant Iir := Get_Element_Subtype (R_Type);
               El_Match : Tri_State_Type;
               Cond       : O_Enode;
               Sub_Cond   : O_Enode;
               L_Bounds1 : Mnode;
               R_Bounds1 : Mnode;
            begin
               --  FIXME: stabilize.
               El_Match := Types_Match (L_El, R_El);
               if El_Match = Unknown or Nbr_Dim > 1 then
                  L_Bounds1 := Stabilize (L_Bounds);
                  R_Bounds1 := Stabilize (R_Bounds);
               else
                  L_Bounds1 := L_Bounds;
                  R_Bounds1 := R_Bounds;
               end if;

               for I in 1 .. Nbr_Dim loop
                  Sub_Cond := New_Compare_Op
                    (ON_Neq,
                     M2E (Range_To_Length
                            (Bounds_To_Range (L_Bounds1, L_Type, I))),
                     M2E (Range_To_Length
                            (Bounds_To_Range (R_Bounds1, R_Type, I))),
                     Ghdl_Bool_Type);
                  if I = 1 then
                     Cond := Sub_Cond;
                  else
                     Cond := New_Dyadic_Op (ON_Or, Cond, Sub_Cond);
                  end if;
               end loop;
               if El_Match = Unknown then
                  Sub_Cond := Check_Match_Cond
                    (L_El, Array_Bounds_To_Element_Bounds (L_Bounds1, L_Type),
                     R_El, Array_Bounds_To_Element_Bounds (R_Bounds1, R_Type));
                  Cond := New_Dyadic_Op (ON_Or, Cond, Sub_Cond);
               end if;
               return Cond;
            end;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            declare
               L_El_List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (L_Type);
               R_El_List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (R_Type);
               Cond : O_Enode;
               Sub_Cond : O_Enode;
               L_Bounds1 : Mnode;
               R_Bounds1 : Mnode;
            begin
               L_Bounds1 := Stabilize (L_Bounds);
               R_Bounds1 := Stabilize (R_Bounds);
               Cond := O_Enode_Null;
               for I in Flist_First .. Flist_Last (L_El_List) loop
                  declare
                     L_El : constant Iir := Get_Nth_Element (L_El_List, I);
                     R_El : constant Iir := Get_Nth_Element (R_El_List, I);
                     L_El_Type : constant Iir := Get_Type (L_El);
                     R_El_Type : constant Iir := Get_Type (R_El);
                  begin
                     if Types_Match (L_El_Type, R_El_Type) = Unknown then
                        Sub_Cond := Check_Match_Cond
                          (L_El_Type,
                           Record_Bounds_To_Element_Bounds (L_Bounds1, L_El),
                           R_El_Type,
                           Record_Bounds_To_Element_Bounds (R_Bounds1, R_El));
                        if Cond = O_Enode_Null then
                           Cond := Sub_Cond;
                        else
                           Cond := New_Dyadic_Op (ON_Or, Cond, Sub_Cond);
                        end if;
                     end if;
                  end;
               end loop;
               pragma Assert (Cond /= O_Enode_Null);
               return Cond;
            end;
      end case;
   end Check_Match_Cond;

   procedure Check_Composite_Match (L_Type : Iir;
                                    L_Node : Mnode;
                                    R_Type : Iir;
                                    R_Node : Mnode;
                                    Loc    : Iir)
   is
      Res : O_Enode;
   begin
      case Types_Match (L_Type, R_Type) is
         when True =>
            return;
         when False =>
            --  FIXME: emit a warning ?
            Chap6.Gen_Bound_Error (Loc);
            return;
         when Unknown =>
            Res := Check_Match_Cond (L_Type, Get_Composite_Bounds (L_Node),
                                     R_Type, Get_Composite_Bounds (R_Node));
            Chap6.Check_Bound_Error (Res, Loc);
      end case;
   end Check_Composite_Match;

   procedure Create_Range_From_Array_Attribute_And_Length
     (Array_Attr : Iir; Length : O_Dnode; Res : Mnode)
   is
      Attr_Kind : Iir_Kind;
      Arr_Rng   : Mnode;
      Iinfo     : Type_Info_Acc;

      Dir        : O_Enode;
      Diff       : O_Dnode;
      Left_Bound : Mnode;
      If_Blk     : O_If_Block;
      If_Blk1    : O_If_Block;
   begin
      Open_Temp;
      Arr_Rng := Chap14.Translate_Array_Attribute_To_Range (Array_Attr);
      Iinfo := Get_Type_Info (Arr_Rng);
      Stabilize (Arr_Rng);

      --  Length.
      New_Assign_Stmt (M2Lv (Range_To_Length (Res)),
                       New_Obj_Value (Length));

      --  Direction.
      Attr_Kind := Get_Kind (Array_Attr);
      Dir := M2E (Range_To_Dir (Arr_Rng));
      case Attr_Kind is
         when Iir_Kind_Range_Array_Attribute =>
            New_Assign_Stmt (M2Lv (Range_To_Dir (Res)), Dir);
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            Start_If_Stmt (If_Blk,
                           New_Compare_Op (ON_Eq,
                             Dir,
                             New_Lit (Ghdl_Dir_To_Node),
                             Ghdl_Bool_Type));
            New_Assign_Stmt
              (M2Lv (Range_To_Dir (Res)), New_Lit (Ghdl_Dir_Downto_Node));
            New_Else_Stmt (If_Blk);
            New_Assign_Stmt
              (M2Lv (Range_To_Dir (Res)), New_Lit (Ghdl_Dir_To_Node));
            Finish_If_Stmt (If_Blk);
         when others =>
            Error_Kind ("Create_Range_From_Array_Attribute_And_Length",
                        Array_Attr);
      end case;

      Start_If_Stmt
        (If_Blk,
         New_Compare_Op (ON_Eq,
                         New_Obj_Value (Length),
                         New_Lit (Ghdl_Index_0),
                         Ghdl_Bool_Type));
      --  Null range.
      case Attr_Kind is
         when Iir_Kind_Range_Array_Attribute =>
            New_Assign_Stmt (M2Lv (Range_To_Left (Res)),
                             M2E (Range_To_Right (Arr_Rng)));
            New_Assign_Stmt (M2Lv (Range_To_Right (Res)),
                             M2E (Range_To_Left (Arr_Rng)));
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            New_Assign_Stmt (M2Lv (Range_To_Left (Res)),
                             M2E (Range_To_Left (Arr_Rng)));
            New_Assign_Stmt (M2Lv (Range_To_Right (Res)),
                             M2E (Range_To_Right (Arr_Rng)));
         when others =>
            raise Internal_Error;
      end case;

      New_Else_Stmt (If_Blk);

      --  LEFT.
      case Attr_Kind is
         when Iir_Kind_Range_Array_Attribute =>
            Left_Bound := Range_To_Left (Arr_Rng);
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            Left_Bound := Range_To_Right (Arr_Rng);
         when others =>
            raise Internal_Error;
      end case;
      Stabilize (Left_Bound);
      New_Assign_Stmt (M2Lv (Range_To_Left (Res)), M2E (Left_Bound));

      --  RIGHT.
      Diff := Create_Temp_Init
        (Iinfo.Ortho_Type (Mode_Value),
         New_Convert_Ov
           (New_Dyadic_Op (ON_Sub_Ov,
            New_Obj_Value (Length),
            New_Lit (Ghdl_Index_1)),
            Iinfo.Ortho_Type (Mode_Value)));

      Start_If_Stmt (If_Blk1, New_Compare_Op (ON_Eq,
                     M2E (Range_To_Dir (Res)),
                     New_Lit (Ghdl_Dir_To_Node),
                     Ghdl_Bool_Type));
      New_Assign_Stmt (M2Lv (Range_To_Right (Res)),
                       New_Dyadic_Op (ON_Add_Ov,
                         M2E (Left_Bound),
                         New_Obj_Value (Diff)));
      New_Else_Stmt (If_Blk1);
      New_Assign_Stmt (M2Lv (Range_To_Right (Res)),
                       New_Dyadic_Op (ON_Sub_Ov,
                         M2E (Left_Bound),
                         New_Obj_Value (Diff)));
      Finish_If_Stmt (If_Blk1);

      --  FIXME: check right bounds is inside bounds.
      Finish_If_Stmt (If_Blk);
      Close_Temp;
   end Create_Range_From_Array_Attribute_And_Length;

   procedure Create_Range_From_Length
     (Index_Type : Iir; Length : O_Dnode; Res : Mnode; Loc : Iir)
   is
      Iinfo        : constant Type_Info_Acc := Get_Info (Index_Type);
      Range_Constr : constant Iir := Get_Range_Constraint (Index_Type);
      Op           : ON_Op_Kind;
      Diff         : O_Enode;
      Left_Bound   : O_Enode;
      Var_Right    : O_Dnode;
      If_Blk       : O_If_Block;
      Res_Range    : Mnode;
   begin
      if Get_Kind (Range_Constr) /= Iir_Kind_Range_Expression then
         Open_Temp;
         Res_Range := Stabilize (Res);

         Create_Range_From_Array_Attribute_And_Length
           (Range_Constr, Length, Res_Range);

         Close_Temp;
         return;
      end if;

      Start_Declare_Stmt;
      Open_Local_Temp;
      Res_Range := Stabilize (Res);

      New_Var_Decl (Var_Right, Get_Identifier ("right_bound"),
                    O_Storage_Local, Iinfo.Ortho_Type (Mode_Value));
      New_Assign_Stmt
        (M2Lv (Range_To_Length (Res_Range)), New_Obj_Value (Length));
      New_Assign_Stmt
        (M2Lv (Range_To_Dir (Res_Range)),
         New_Lit (Chap7.Translate_Static_Range_Dir (Range_Constr)));

      case Get_Direction (Range_Constr) is
         when Dir_To =>
            Op := ON_Add_Ov;
         when Dir_Downto =>
            Op := ON_Sub_Ov;
      end case;

      Start_If_Stmt (If_Blk, New_Compare_Op (ON_Eq,
                                             New_Obj_Value (Length),
                                             New_Lit (Ghdl_Index_0),
                                             Ghdl_Bool_Type));
      --  Null range.
      New_Assign_Stmt
        (M2Lv (Range_To_Left (Res_Range)),
         Chap7.Translate_Range_Expression_Right (Range_Constr, Index_Type));
      New_Assign_Stmt
        (M2Lv (Range_To_Right (Res_Range)),
         Chap7.Translate_Range_Expression_Left (Range_Constr, Index_Type));

      New_Else_Stmt (If_Blk);
      New_Assign_Stmt
        (M2Lv (Range_To_Left (Res_Range)),
         Chap7.Translate_Range_Expression_Left (Range_Constr, Index_Type));
      Left_Bound := Chap7.Translate_Range_Expression_Left
        (Range_Constr, Index_Type);
      Diff := New_Convert_Ov
        (New_Dyadic_Op (ON_Sub_Ov,
         New_Obj_Value (Length),
         New_Lit (Ghdl_Index_1)),
         Iinfo.Ortho_Type (Mode_Value));
      New_Assign_Stmt (New_Obj (Var_Right),
                       New_Dyadic_Op (Op, Left_Bound, Diff));

      --   Check the right bounds is inside the bounds of the index type.
      Chap3.Check_Range (Var_Right, Null_Iir, Index_Type, Loc);
      New_Assign_Stmt
        (M2Lv (Range_To_Right (Res_Range)), New_Obj_Value (Var_Right));
      Finish_If_Stmt (If_Blk);

      Close_Local_Temp;
      Finish_Declare_Stmt;
   end Create_Range_From_Length;
end Trans.Chap3;
