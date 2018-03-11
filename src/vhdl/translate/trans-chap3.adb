--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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

with Name_Table;
with Errorout; use Errorout;
with Iirs_Utils; use Iirs_Utils;
with Evaluation; use Evaluation;
with Trans.Chap2;
with Trans.Chap4;
with Trans.Chap6;
with Trans.Chap7;
with Trans.Chap14;
with Trans_Decls; use Trans_Decls;
with Trans.Helpers2; use Trans.Helpers2;
with Translation;

package body Trans.Chap3 is
   use Trans.Helpers;

   function Unbox_Record (Arr : Mnode) return Mnode;

   function Create_Static_Type_Definition_Type_Range (Def : Iir)
                                                      return O_Cnode;
   procedure Create_Scalar_Type_Range (Def : Iir; Target : O_Lnode);

   --  For scalar subtypes: creates info from the base type.
   procedure Create_Subtype_Info_From_Type (Def          : Iir;
                                            Base         : Iir;
                                            Subtype_Info : Type_Info_Acc);

   --  Finish a type definition: declare the type, define and declare a
   --   pointer to the type.
   procedure Finish_Type_Definition
     (Info : Type_Info_Acc; Completion : Boolean := False)
   is
   begin
      --  Declare the type.
      if not Completion then
         New_Type_Decl (Create_Identifier, Info.Ortho_Type (Mode_Value));
      end if;

      --  Create an access to the type and declare it.
      Info.Ortho_Ptr_Type (Mode_Value) :=
        New_Access_Type (Info.Ortho_Type (Mode_Value));
      New_Type_Decl (Create_Identifier ("PTR"),
                     Info.Ortho_Ptr_Type (Mode_Value));

      --  Signal type.
      if Info.Type_Mode in Type_Mode_Scalar then
         Info.Ortho_Type (Mode_Signal) := Ghdl_Signal_Ptr;
      else
         if Info.Ortho_Type (Mode_Signal) /= O_Tnode_Null then
            New_Type_Decl (Create_Identifier ("SIG"),
                           Info.Ortho_Type (Mode_Signal));
         end if;
      end if;

      --  Signal pointer type.
      if Info.Type_Mode in Type_Mode_Composite
        and then Info.Ortho_Type (Mode_Signal) /= O_Tnode_Null
      then
         Info.Ortho_Ptr_Type (Mode_Signal) :=
           New_Access_Type (Info.Ortho_Type (Mode_Signal));
         New_Type_Decl (Create_Identifier ("SIGPTR"),
                        Info.Ortho_Ptr_Type (Mode_Signal));
      else
         Info.Ortho_Ptr_Type (Mode_Signal) := O_Tnode_Null;
      end if;
   end Finish_Type_Definition;

   procedure Set_Complex_Type (Info : Type_Info_Acc; Need_Builder : Boolean) is
   begin
      pragma Assert (Info.C = null);
      Info.C := new Complex_Type_Arr_Info;
      --  No size variable for unconstrained array type.
      for Mode in Object_Kind_Type loop
         Info.C (Mode).Builder_Need_Func := Need_Builder;
      end loop;
   end Set_Complex_Type;

   procedure Copy_Complex_Type (Dest : Type_Info_Acc; Src : Type_Info_Acc) is
   begin
      Dest.C := new Complex_Type_Arr_Info'(Src.C.all);
   end Copy_Complex_Type;

   procedure Create_Size_Var (Def : Iir; Info : Type_Info_Acc) is
   begin
      Info.C (Mode_Value).Size_Var := Create_Var
        (Create_Var_Identifier ("SIZE"), Ghdl_Index_Type);
      if Get_Has_Signal_Flag (Def) then
         Info.C (Mode_Signal).Size_Var := Create_Var
           (Create_Var_Identifier ("SIGSIZE"), Ghdl_Index_Type);
      end if;
   end Create_Size_Var;

   --  A builder set internal fields of object pointed by BASE_PTR, using
   --  memory from BASE_PTR and returns a pointer to the next memory byte
   --  to be used.
   procedure Create_Builder_Subprogram_Decl (Info : Type_Info_Acc;
                                             Name : Name_Id;
                                             Kind : Object_Kind_Type)
   is
      Interface_List : O_Inter_List;
      Ident          : O_Ident;
      Ptype          : O_Tnode;
   begin
      case Kind is
         when Mode_Value =>
            Ident := Create_Identifier (Name, "_BUILDER");
         when Mode_Signal =>
            Ident := Create_Identifier (Name, "_SIGBUILDER");
      end case;
      --  FIXME: return the same type as its first parameter ???
      Start_Function_Decl
        (Interface_List, Ident, Global_Storage, Ghdl_Index_Type);
      Subprgs.Add_Subprg_Instance_Interfaces
        (Interface_List, Info.C (Kind).Builder_Instance);
      case Info.Type_Mode is
         when Type_Mode_Unbounded =>
            Ptype := Info.B.Base_Ptr_Type (Kind);
         when Type_Mode_Complex_Record =>
            Ptype := Info.Ortho_Ptr_Type (Kind);
         when others =>
            raise Internal_Error;
      end case;
      New_Interface_Decl
        (Interface_List, Info.C (Kind).Builder_Base_Param,
         Get_Identifier ("base_ptr"), Ptype);
      --  Add parameter for array bounds.
      if Info.Type_Mode in Type_Mode_Unbounded then
         New_Interface_Decl
           (Interface_List, Info.C (Kind).Builder_Bound_Param,
            Get_Identifier ("bound"), Info.B.Bounds_Ptr_Type);
      end if;
      Finish_Subprogram_Decl (Interface_List, Info.C (Kind).Builder_Func);
   end Create_Builder_Subprogram_Decl;

   function Gen_Call_Type_Builder (Var : Mnode; Var_Type : Iir) return O_Enode
   is
      Kind  : constant Object_Kind_Type := Get_Object_Kind (Var);
      Binfo : constant Type_Info_Acc := Get_Info (Get_Base_Type (Var_Type));
      Assoc : O_Assoc_List;
   begin
      --  Build the field
      Start_Association (Assoc, Binfo.C (Kind).Builder_Func);
      Subprgs.Add_Subprg_Instance_Assoc
        (Assoc, Binfo.C (Kind).Builder_Instance);

      --  Note: a fat array can only be at the top of a complex type;
      --  the bounds must have been set.
      New_Association
        (Assoc, M2Addr (Chap3.Get_Composite_Base (Var)));

      if Binfo.Type_Mode in Type_Mode_Unbounded then
         New_Association (Assoc, M2Addr (Chap3.Get_Composite_Bounds (Var)));
      end if;

      return New_Function_Call (Assoc);
   end Gen_Call_Type_Builder;

   procedure Gen_Call_Type_Builder (Var : Mnode; Var_Type : Iir)
   is
      Mem : O_Dnode;
      V   : Mnode;
   begin
      Open_Temp;
      V := Stabilize (Var);
      Mem := Create_Temp (Ghdl_Index_Type);
      New_Assign_Stmt (New_Obj (Mem), Gen_Call_Type_Builder (V, Var_Type));
      Close_Temp;
   end Gen_Call_Type_Builder;

   ------------------
   --  Enumeration --
   ------------------

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
         Set_Ortho_Expr (El, Val);
      end loop;
      Finish_Enum_Type (Constr, Info.Ortho_Type (Mode_Value));
      if Nbr <= 256 then
         Info.Type_Mode := Type_Mode_E8;
      else
         Info.Type_Mode := Type_Mode_E32;
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
      Set_Ortho_Expr (False_Lit, False_Node);
      Set_Ortho_Expr (True_Lit, True_Node);
      Info.S.Nocheck_Low := True;
      Info.S.Nocheck_Hi := True;
      Finish_Type_Definition (Info);
   end Translate_Bool_Type;

   ---------------
   --  Integer  --
   ---------------

   --  Return the number of bits (32 or 64) required to represent the
   --  (integer or physical) type definition DEF.
   type Type_Precision is (Precision_32, Precision_64);
   function Get_Type_Precision (Def : Iir) return Type_Precision
   is
      St     : constant Iir :=
        Get_Subtype_Definition (Get_Type_Declarator (Def));
      L, H   : Iir;
      Lv, Hv : Iir_Int64;
   begin
      Get_Low_High_Limit (Get_Range_Constraint (St), L, H);
      Lv := Get_Value (L);
      Hv := Get_Value (H);
      if Lv >= -(2 ** 31) and then Hv <= (2 ** 31 - 1) then
         return Precision_32;
      else
         if Translation.Flag_Only_32b then
            Error_Msg_Sem
              (+St, "range of %n is too large", +Get_Type_Declarator (St));
            return Precision_32;
         end if;
         return Precision_64;
      end if;
   end Get_Type_Precision;

   procedure Translate_Integer_Type (Def : Iir_Integer_Type_Definition)
   is
      Info : constant Type_Info_Acc := Get_Info (Def);
   begin
      case Get_Type_Precision (Def) is
         when Precision_32 =>
            Info.Ortho_Type (Mode_Value) := New_Signed_Type (32);
            Info.Type_Mode := Type_Mode_I32;
         when Precision_64 =>
            Info.Ortho_Type (Mode_Value) := New_Signed_Type (64);
            Info.Type_Mode := Type_Mode_I64;
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
      case Get_Type_Precision (Def) is
         when Precision_32 =>
            Info.Ortho_Type (Mode_Value) := New_Signed_Type (32);
            Info.Type_Mode := Type_Mode_P32;
         when Precision_64 =>
            Info.Ortho_Type (Mode_Value) := New_Signed_Type (64);
            Info.Type_Mode := Type_Mode_P64;
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
   end Translate_File_Type;

   function Get_File_Signature_Length (Def : Iir) return Natural is
   begin
      case Get_Kind (Def) is
         when Iir_Kinds_Scalar_Type_And_Subtype_Definition =>
            return 1;
         when Iir_Kind_Array_Type_Definition
            | Iir_Kind_Array_Subtype_Definition =>
            return 2
              + Get_File_Signature_Length (Get_Element_Subtype (Def));
         when Iir_Kind_Record_Type_Definition
            | Iir_Kind_Record_Subtype_Definition =>
            declare
               List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Get_Base_Type (Def));
               El   : Iir;
               Res  : Natural;
            begin
               Res := 2;
               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  Res := Res + Get_File_Signature_Length (Get_Type (El));
               end loop;
               return Res;
            end;
         when others =>
            Error_Kind ("get_file_signature_length", Def);
      end case;
   end Get_File_Signature_Length;

   procedure Get_File_Signature (Def : Iir;
                                 Res : in out String;
                                 Off : in out Natural)
   is
      Scalar_Map : constant array (Type_Mode_Scalar) of Character
        := "beEiIpPF";
   begin
      case Get_Kind (Def) is
         when Iir_Kinds_Scalar_Type_And_Subtype_Definition =>
            Res (Off) := Scalar_Map (Get_Info (Def).Type_Mode);
            Off := Off + 1;
         when Iir_Kind_Array_Type_Definition
            | Iir_Kind_Array_Subtype_Definition =>
            Res (Off) := '[';
            Off := Off + 1;
            Get_File_Signature (Get_Element_Subtype (Def), Res, Off);
            Res (Off) := ']';
            Off := Off + 1;
         when Iir_Kind_Record_Type_Definition
            | Iir_Kind_Record_Subtype_Definition =>
            declare
               List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Get_Base_Type (Def));
               El   : Iir;
            begin
               Res (Off) := '<';
               Off := Off + 1;
               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  Get_File_Signature (Get_Type (El), Res, Off);
               end loop;
               Res (Off) := '>';
               Off := Off + 1;
            end;
         when others =>
            Error_Kind ("get_file_signature", Def);
      end case;
   end Get_File_Signature;

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

   procedure Create_Unbounded_Type_Fat_Pointer (Info : Type_Info_Acc)
   is
      Constr : O_Element_List;
   begin
      for Kind in Object_Kind_Type loop
         exit when Info.B.Base_Type (Kind) = O_Tnode_Null;

         Start_Record_Type (Constr);
         New_Record_Field
           (Constr, Info.B.Base_Field (Kind), Wki_Base,
            Info.B.Base_Ptr_Type (Kind));
         New_Record_Field
           (Constr, Info.B.Bounds_Field (Kind), Wki_Bounds,
            Info.B.Bounds_Ptr_Type);
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

   function Create_Static_Composite_Subtype_Bounds (Def : Iir) return O_Cnode
   is
      Binfo : constant Type_Info_Acc := Get_Info (Get_Base_Type (Def));
      List : O_Record_Aggr_List;
      Res : O_Cnode;
   begin
      Start_Record_Aggr (List, Binfo.B.Bounds_Type);

      case Get_Kind (Def) is
         when Iir_Kind_Array_Subtype_Definition =>
            declare
               Indexes_List : constant Iir_Flist :=
                 Get_Index_Subtype_List (Def);
               Index : Iir;
            begin
               for I in Flist_First .. Flist_Last (Indexes_List) loop
                  Index := Get_Index_Type (Indexes_List, I);
                  New_Record_Aggr_El
                    (List, Create_Static_Type_Definition_Type_Range (Index));
               end loop;
            end;
            if Binfo.B.El_Size /= O_Fnode_Null then
               --  For arrays of unbounded type.
               declare
                  El_Type : constant Iir := Get_Element_Subtype (Def);
                  El_Info : constant Type_Info_Acc := Get_Info (El_Type);
                  Sz_List : O_Record_Aggr_List;
                  Sz_Res : O_Cnode;
               begin
                  New_Record_Aggr_El
                    (List, Create_Static_Composite_Subtype_Bounds (El_Type));

                  Start_Record_Aggr (Sz_List, Ghdl_Sizes_Type);
                  New_Record_Aggr_El
                    (Sz_List, New_Sizeof (El_Info.Ortho_Type (Mode_Value),
                                          Ghdl_Index_Type));
                  New_Record_Aggr_El
                    (Sz_List, New_Sizeof (El_Info.Ortho_Type (Mode_Signal),
                                          Ghdl_Index_Type));
                  Finish_Record_Aggr (Sz_List, Sz_Res);
                  New_Record_Aggr_El (List, Sz_Res);
               end;
            end if;

         when Iir_Kind_Record_Subtype_Definition =>
            declare
               El_List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Def);
               El_Blist : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Get_Base_Type (Def));
               El : Iir;
               Bel : Iir;
               Bel_Info : Field_Info_Acc;
            begin
               for I in Flist_First .. Flist_Last (El_Blist) loop
                  Bel := Get_Nth_Element (El_Blist, I);
                  Bel_Info := Get_Info (Bel);
                  if Bel_Info.Field_Bound /= O_Fnode_Null then
                     El := Get_Nth_Element (El_List, I);
                     New_Record_Aggr_El
                       (List,
                        Create_Static_Composite_Subtype_Bounds
                          (Get_Type (El)));
                  end if;
               end loop;
            end;

         when others =>
            Error_Kind ("create_static_composite_subtype_bounds", Def);
      end case;

      Finish_Record_Aggr (List, Res);
      return Res;
   end Create_Static_Composite_Subtype_Bounds;

   procedure Create_Composite_Subtype_Bounds (Def : Iir; Target : O_Lnode)
   is
      Info             : constant Type_Info_Acc := Get_Info (Def);
      Base_Type        : constant Iir := Get_Base_Type (Def);
      Targ             : Mnode;
   begin
      Targ := Lv2M (Target, null, Mode_Value,
                    Info.B.Bounds_Type, Info.B.Bounds_Ptr_Type);
      Open_Temp;

      case Get_Kind (Def) is
         when Iir_Kind_Array_Subtype_Definition =>
            declare
               Indexes_List : constant Iir_Flist :=
                 Get_Index_Subtype_List (Def);
               Indexes_Def_List : constant Iir_Flist :=
                 Get_Index_Subtype_Definition_List (Base_Type);
               Index : Iir;
            begin
               if Get_Nbr_Elements (Indexes_List) > 1 then
                  Targ := Stabilize (Targ);
               end if;
               for I in Flist_First .. Flist_Last (Indexes_List) loop
                  Index := Get_Index_Type (Indexes_List, I);
                  declare
                     Index_Type : constant Iir := Get_Base_Type (Index);
                     Index_Info : constant Type_Info_Acc :=
                       Get_Info (Index_Type);
                     Base_Index_Info : constant Index_Info_Acc :=
                       Get_Info (Get_Nth_Element (Indexes_Def_List, I));
                     D : O_Dnode;
                  begin
                     Open_Temp;
                     D := Create_Temp_Ptr
                       (Index_Info.B.Range_Ptr_Type,
                        New_Selected_Element (M2Lv (Targ),
                                              Base_Index_Info.Index_Field));
                     Chap7.Translate_Discrete_Range
                       (Dp2M (D, Index_Info, Mode_Value,
                              Index_Info.B.Range_Type,
                              Index_Info.B.Range_Ptr_Type),
                        Index);
                     Close_Temp;
                  end;
               end loop;
            end;

         when Iir_Kind_Record_Subtype_Definition =>
            declare
               El_List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Def);
               El : Iir;
               El_Info : Field_Info_Acc;
            begin
               Targ := Stabilize (Targ);
               for I in Flist_First .. Flist_Last (El_List) loop
                  El := Get_Nth_Element (El_List, I);
                  El_Info := Get_Info (Get_Base_Element_Declaration (El));
                  if El_Info.Field_Bound /= O_Fnode_Null then
                     Create_Composite_Subtype_Bounds
                       (Get_Type (El),
                        New_Selected_Element (M2Lv (Targ),
                                              El_Info.Field_Bound));
                  end if;
               end loop;
            end;

         when others =>
            Error_Kind ("create_composite_subtype_bounds", Def);
      end case;

      Close_Temp;
   end Create_Composite_Subtype_Bounds;

   procedure Create_Composite_Subtype_Bounds (Def : Iir)
   is
      Info : constant Type_Info_Acc := Get_Info (Def);
   begin
      if not Info.S.Static_Bounds then
         Create_Composite_Subtype_Bounds
           (Def, Get_Var (Info.S.Composite_Bounds));
      end if;
   end Create_Composite_Subtype_Bounds;

   --  Create a variable containing the bounds for array subtype DEF.
   procedure Create_Composite_Subtype_Bounds_Var
     (Def : Iir; Elab_Now : Boolean)
   is
      Info      : constant Type_Info_Acc := Get_Info (Def);
      Base_Info : Type_Info_Acc;
      Val       : O_Cnode;
   begin
      if Info.S.Composite_Bounds /= Null_Var then
         return;
      end if;
      Base_Info := Get_Info (Get_Base_Type (Def));
      if Are_Bounds_Locally_Static (Def) then
         Info.S.Static_Bounds := True;
         if Global_Storage = O_Storage_External then
            --  Do not create the value of the type desc, since it
            --  is never dereferenced in a static type desc.
            Val := O_Cnode_Null;
         else
            Val := Create_Static_Composite_Subtype_Bounds (Def);
         end if;
         Info.S.Composite_Bounds := Create_Global_Const
           (Create_Identifier ("STB"),
            Base_Info.B.Bounds_Type, Global_Storage, Val);
      else
         pragma Assert (Get_Type_Staticness (Def) /= Locally);
         Info.S.Static_Bounds := False;
         Info.S.Composite_Bounds := Create_Var
           (Create_Var_Identifier ("STB"), Base_Info.B.Bounds_Type);
         if Elab_Now then
            Create_Composite_Subtype_Bounds (Def);
         end if;
      end if;
   end Create_Composite_Subtype_Bounds_Var;

   -------------
   --  Array  --
   -------------

   --  Declare the bounds types for DEF.
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
         --  Bounds and size for element.
         New_Record_Field (Constr, Info.B.El_Bounds,
                           Get_Identifier ("el_bound"), El_Info.B.Bounds_Type);
         New_Record_Field (Constr, Info.B.El_Size, Get_Identifier ("el_size"),
                           Ghdl_Sizes_Type);
      end if;

      Finish_Record_Type (Constr, Info.B.Bounds_Type);
      Finish_Unbounded_Type_Bounds (Info);
   end Translate_Array_Type_Bounds;

   procedure Translate_Array_Type_Base
     (Def      : Iir_Array_Type_Definition;
      Info     : Type_Info_Acc)
   is
      El_Type   : constant Iir := Get_Element_Subtype (Def);
      El_Tinfo  : constant Type_Info_Acc := Get_Info (El_Type);
   begin
      if Is_Complex_Type (El_Tinfo) or else Is_Unbounded_Type (El_Tinfo) then
         if El_Tinfo.Type_Mode in Type_Mode_Arrays then
            Info.B.Base_Type := El_Tinfo.B.Base_Ptr_Type;
            Info.B.Base_Ptr_Type := El_Tinfo.B.Base_Ptr_Type;
         else
            Info.B.Base_Type := El_Tinfo.Ortho_Ptr_Type;
            Info.B.Base_Ptr_Type := El_Tinfo.Ortho_Ptr_Type;
         end if;
      else
         for Kind in Mode_Value .. Type_To_Last_Object_Kind (Def) loop
            Info.B.Base_Type (Kind) :=
              New_Array_Type (El_Tinfo.Ortho_Type (Kind), Ghdl_Index_Type);
         end loop;
         Finish_Unbounded_Type_Base (Info);
      end if;
   end Translate_Array_Type_Base;

   procedure Translate_Array_Type_Definition
     (Def : Iir_Array_Type_Definition)
   is
      Info       : constant Type_Info_Acc := Get_Info (Def);
      El_Tinfo   : Type_Info_Acc;
   begin
      Info.Type_Mode := Type_Mode_Fat_Array;
      Info.B := Ortho_Info_Basetype_Array_Init;
      Info.S := Ortho_Info_Subtype_Array_Init;
      Translate_Array_Type_Base (Def, Info);
      Translate_Array_Type_Bounds (Def, Info);
      Info.Ortho_Type (Mode_Signal) := O_Tnode_Null;
      Create_Unbounded_Type_Fat_Pointer (Info);
      Finish_Type_Definition (Info, False);

      El_Tinfo := Get_Info (Get_Element_Subtype (Def));
      if Is_Complex_Type (El_Tinfo) then
         --  This is a complex type.
         --  No size variable for unconstrained array type.
         Set_Complex_Type (Info, El_Tinfo.C (Mode_Value).Builder_Need_Func);
      end if;
      Info.Type_Incomplete := False;
   end Translate_Array_Type_Definition;

   --  Get the length of DEF, ie the number of elements.
   --  If the length is not statically defined, returns -1.
   function Get_Array_Subtype_Length (Def : Iir_Array_Subtype_Definition)
                                      return Iir_Int64
   is
      Indexes_List : constant Iir_Flist := Get_Index_Subtype_List (Def);
      Index        : Iir;
      Idx_Len      : Iir_Int64;
      Len          : Iir_Int64;
   begin
      --  Check if the bounds of the array are locally static.
      Len := 1;
      for I in Flist_First .. Flist_Last (Indexes_List) loop
         Index := Get_Index_Type (Indexes_List, I);

         if Get_Type_Staticness (Index) /= Locally then
            return -1;
         end if;
         Idx_Len := Eval_Discrete_Type_Length (Index);

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

   --  Create ortho unconstrained arrays for DEF, whose element subtype was
   --  newly constrained.  The element subtype must be a static type, so that
   --  an array can indeed be created.
   procedure Create_Array_For_Array_Subtype
     (Def : Iir_Array_Subtype_Definition;
      Base : out O_Tnode_Array;
      Ptr : out O_Tnode_Array)
   is
      El_Tinfo : constant Type_Info_Acc :=
        Get_Info (Get_Element_Subtype (Def));
      pragma Assert (Is_Static_Type (El_Tinfo));
      Id : O_Ident;
   begin
      Base (Mode_Signal) := O_Tnode_Null;
      Ptr (Mode_Signal) := O_Tnode_Null;
      for I in Mode_Value .. Type_To_Last_Object_Kind (Def) loop
         --  Element has been constrained by this subtype, so create the
         --  base array (and the pointer).
         case I is
            when Mode_Value =>
               Id := Create_Identifier ("BARR");
            when Mode_Signal =>
               Id := Create_Identifier ("BARRSIG");
         end case;
         Base (I) := New_Array_Type
           (El_Tinfo.Ortho_Type (I), Ghdl_Index_Type);
         New_Type_Decl (Id, Base (I));

         case I is
            when Mode_Value =>
               Id := Create_Identifier ("BARRPTR");
            when Mode_Signal =>
               Id := Create_Identifier ("BARRSIGPTR");
         end case;
         Ptr (I) := New_Access_Type (Base (I));
         New_Type_Decl (Id, Ptr (I));
      end loop;
   end Create_Array_For_Array_Subtype;

   procedure Translate_Array_Subtype_Definition
     (Def : Iir_Array_Subtype_Definition; Parent_Type : Iir)
   is
      Info      : constant Type_Info_Acc := Get_Info (Def);
      Pinfo     : constant Type_Info_Acc := Get_Info (Parent_Type);

      Len : Iir_Int64;

      Id : O_Ident;
      El_Constrained : Boolean;
      Base : O_Tnode_Array;
   begin
      --  Note: info of indexes subtype are not created!

      Len := Get_Array_Subtype_Length (Def);
      Info.Type_Locally_Constrained := (Len >= 0);
      Info.B := Pinfo.B;
      Info.S := Pinfo.S;
      if Is_Complex_Type (Pinfo)
        or else not Info.Type_Locally_Constrained
      then
         --  This is a complex type as the size is not known at compile
         --  time.
         Info.Type_Mode := Type_Mode_Complex_Array;
         Info.Ortho_Type := Pinfo.B.Base_Ptr_Type;
         Info.Ortho_Ptr_Type := Pinfo.B.Base_Ptr_Type;

         --  If the base type need a builder, so does the subtype.
         if Is_Complex_Type (Pinfo)
           and then Pinfo.C (Mode_Value).Builder_Need_Func
         then
            Copy_Complex_Type (Info, Pinfo);
         else
            Set_Complex_Type (Info, False);
         end if;

         --  Type is bounded, but not statically.
         Create_Size_Var (Def, Info);
      else
         --  Length is known.  Create a constrained array.
         El_Constrained := Get_Array_Element_Constraint (Def) /= Null_Iir;
         Info.Type_Mode := Type_Mode_Static_Array;
         Info.Ortho_Type (Mode_Signal) := O_Tnode_Null;
         Info.Ortho_Ptr_Type (Mode_Signal) := O_Tnode_Null;
         if El_Constrained then
            --  Element has been constrained by this subtype, so create the
            --  base array (and the pointer).
            Create_Array_For_Array_Subtype (Def, Base, Info.Ortho_Ptr_Type);
         else
            Base := Pinfo.B.Base_Type;
            Info.Ortho_Ptr_Type := Pinfo.B.Base_Ptr_Type;
         end if;
         for I in Mode_Value .. Type_To_Last_Object_Kind (Def) loop
            case I is
               when Mode_Value =>
                  Id := Create_Identifier;
               when Mode_Signal =>
                  Id := Create_Identifier ("SIG");
            end case;
            Info.Ortho_Type (I) := New_Constrained_Array_Type
              (Base (I), New_Index_Lit (Unsigned_64 (Len)));
            New_Type_Decl (Id, Info.Ortho_Type (I));
         end loop;
      end if;
   end Translate_Array_Subtype_Definition;

   procedure Translate_Array_Subtype_Definition_Constrained_Element
     (Def : Iir_Array_Subtype_Definition; Parent_Type : Iir)
   is
      Info      : constant Type_Info_Acc := Get_Info (Def);
      Pinfo     : constant Type_Info_Acc := Get_Info (Parent_Type);
   begin
      --  Note: info of indexes subtype are not created!
      Info.Type_Locally_Constrained := False;
      Info.Ortho_Type := Pinfo.Ortho_Type;
      Info.Ortho_Ptr_Type := Pinfo.Ortho_Ptr_Type;
      Info.B := Pinfo.B;
      Info.S := Pinfo.S;

      --  This is a complex type as the size is not known at compile time.
      Info.Type_Mode := Type_Mode_Unbounded_Array;
      Create_Array_For_Array_Subtype
        (Def, Info.B.Base_Type, Info.B.Base_Ptr_Type);

      --  If the base type need a builder, so does the subtype.
      if Is_Complex_Type (Pinfo) then
         if Pinfo.C (Mode_Value).Builder_Need_Func then
            Copy_Complex_Type (Info, Pinfo);
         else
            Set_Complex_Type (Info, False);
         end if;
      end if;
   end Translate_Array_Subtype_Definition_Constrained_Element;

   procedure Create_Array_Type_Builder
     (Def : Iir_Array_Type_Definition; Kind : Object_Kind_Type)
   is
      Info       : constant Type_Info_Acc := Get_Info (Def);
      Base       : constant O_Dnode := Info.C (Kind).Builder_Base_Param;
      Bound      : constant O_Dnode := Info.C (Kind).Builder_Bound_Param;
      Var_Off    : O_Dnode;
      Var_Mem    : O_Dnode;
      Var_Length : O_Dnode;
      El_Type    : Iir;
      El_Info    : Type_Info_Acc;
      Label      : O_Snode;
   begin
      Start_Subprogram_Body (Info.C (Kind).Builder_Func);
      Subprgs.Start_Subprg_Instance_Use (Info.C (Kind).Builder_Instance);

      --  Compute length of the array.
      New_Var_Decl (Var_Length, Wki_Length, O_Storage_Local,
                    Ghdl_Index_Type);
      New_Var_Decl (Var_Mem, Get_Identifier ("mem"), O_Storage_Local,
                    Info.B.Base_Ptr_Type (Kind));
      New_Var_Decl (Var_Off, Get_Identifier ("off"), O_Storage_Local,
                    Ghdl_Index_Type);

      El_Type := Get_Element_Subtype (Def);
      El_Info := Get_Info (El_Type);

      New_Assign_Stmt
        (New_Obj (Var_Length),
         New_Dyadic_Op (ON_Mul_Ov,
                        New_Value (Get_Var (El_Info.C (Kind).Size_Var)),
                        Get_Bounds_Length (Dp2M (Bound, Info,
                                                 Mode_Value,
                                                 Info.B.Bounds_Type,
                                                 Info.B.Bounds_Ptr_Type),
                                           Def)));

      --  Find the innermost non-array element.
      while El_Info.Type_Mode = Type_Mode_Complex_Array loop
         El_Type := Get_Element_Subtype (El_Type);
         El_Info := Get_Info (El_Type);
      end loop;

      --  Set each index of the array.
      Init_Var (Var_Off);
      Start_Loop_Stmt (Label);
      Gen_Exit_When (Label, New_Compare_Op (ON_Eq,
                                            New_Obj_Value (Var_Off),
                                            New_Obj_Value (Var_Length),
                                            Ghdl_Bool_Type));

      New_Assign_Stmt
        (New_Obj (Var_Mem),
         New_Unchecked_Address
           (New_Slice (New_Access_Element
                         (New_Convert_Ov (New_Obj_Value (Base),
                                          Char_Ptr_Type)),
                       Chararray_Type,
                       New_Obj_Value (Var_Off)),
            Info.B.Base_Ptr_Type (Kind)));

      New_Assign_Stmt
        (New_Obj (Var_Off),
         New_Dyadic_Op (ON_Add_Ov,
           New_Obj_Value (Var_Off),
           Gen_Call_Type_Builder (Dp2M (Var_Mem, El_Info, Kind), El_Type)));
      Finish_Loop_Stmt (Label);

      New_Return_Stmt (New_Obj_Value (Var_Off));

      Subprgs.Finish_Subprg_Instance_Use (Info.C (Kind).Builder_Instance);
      Finish_Subprogram_Body;
   end Create_Array_Type_Builder;

   --------------
   --  record  --
   --------------

   --  Get the alignment mask for *ortho* type ATYPE.
   function Get_Type_Alignmask (Atype : O_Tnode) return O_Enode is
   begin
      return New_Dyadic_Op
        (ON_Sub_Ov,
         New_Lit (New_Alignof (Atype, Ghdl_Index_Type)),
         New_Lit (Ghdl_Index_1));
   end Get_Type_Alignmask;

   --  Align VALUE (of unsigned type) for type ATYPE.
   --  The formulae is: (V + (A - 1)) and not (A - 1), where A is the
   --  alignment for ATYPE in bytes.
   function Realign (Value : O_Enode; Atype : O_Tnode) return O_Enode is
   begin
      return New_Dyadic_Op
        (ON_And,
         New_Dyadic_Op (ON_Add_Ov, Value, Get_Type_Alignmask (Atype)),
         New_Monadic_Op (ON_Not, Get_Type_Alignmask (Atype)));
   end Realign;

   function Realign (Value : O_Enode; Atype : Iir) return O_Enode
   is
      Tinfo : constant Type_Info_Acc := Get_Info (Atype);
      Otype : O_Tnode;
   begin
      if Is_Unbounded_Type (Tinfo) then
         Otype := Tinfo.B.Base_Type (Mode_Value);
      else
         Otype := Tinfo.Ortho_Type (Mode_Value);
      end if;
      return Realign (Value, Otype);
   end Realign;

   function Realign (Value : O_Enode; Mask : O_Dnode) return O_Enode is
   begin
      return New_Dyadic_Op
        (ON_And,
         New_Dyadic_Op (ON_Add_Ov, Value, New_Obj_Value (Mask)),
         New_Monadic_Op (ON_Not, New_Obj_Value (Mask)));
   end Realign;

   --  Find the innermost non-array element.
   function Get_Innermost_Non_Array_Element (Atype : Iir) return Iir
   is
      Res : Iir := Atype;
   begin
      while Get_Kind (Res) in Iir_Kinds_Array_Type_Definition loop
         Res := Get_Element_Subtype (Res);
      end loop;
      return Res;
   end Get_Innermost_Non_Array_Element;

   --  Declare the bounds types for DEF.
   procedure Translate_Record_Type_Bounds
     (Def : Iir_Record_Type_Definition; Info : Type_Info_Acc)
   is
      List : constant Iir_Flist := Get_Elements_Declaration_List (Def);
      El : Iir;
      El_Tinfo : Type_Info_Acc;
      El_Info : Field_Info_Acc;
      Constr : O_Element_List;
   begin
      Start_Record_Type (Constr);
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         El_Tinfo := Get_Info (Get_Type (El));
         if Is_Unbounded_Type (El_Tinfo) then
            El_Info := Get_Info (El);
            New_Record_Field (Constr, El_Info.Field_Bound,
                              Create_Identifier_Without_Prefix (El),
                              El_Tinfo.B.Bounds_Type);
         end if;
      end loop;
      Finish_Record_Type (Constr, Info.B.Bounds_Type);
      Finish_Unbounded_Type_Bounds (Info);
   end Translate_Record_Type_Bounds;

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
      El_Tnode   : O_Tnode;

      --  True if a size variable will be created since the size of
      --  the record is not known at compile-time.
      Need_Size : Boolean;

      Mark : Id_Mark_Type;
   begin
      Need_Size := False;

      --  First, translate the anonymous type of the elements.
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         El_Type := Get_Type (El);
         if Get_Info (El_Type) = null then
            Push_Identifier_Prefix (Mark, Get_Identifier (El));
            Translate_Subtype_Indication (El_Type, True);
            Pop_Identifier_Prefix (Mark);
         end if;
         Need_Size := Need_Size or else Is_Complex_Type (Get_Info (El_Type));
         Field_Info := Add_Info (El, Kind_Field);
      end loop;

      --  Then create the record type.
      Info.S := Ortho_Info_Subtype_Record_Init;
      Info.Ortho_Type (Mode_Signal) := O_Tnode_Null;
      for Kind in Mode_Value .. Type_To_Last_Object_Kind (Def) loop
         Start_Record_Type (El_List);
         for I in Flist_First .. Flist_Last (List) loop
            El := Get_Nth_Element (List, I);
            Field_Info := Get_Info (El);
            El_Tinfo := Get_Info (Get_Type (El));
            if Is_Complex_Type (El_Tinfo)
              or else Is_Unbounded_Type (El_Tinfo)
            then
               --  Always use an offset for a complex type.
               El_Tnode := Ghdl_Index_Type;
            else
               El_Tnode := El_Tinfo.Ortho_Type (Kind);
            end if;

            New_Record_Field (El_List, Field_Info.Field_Node (Kind),
                              Create_Identifier_Without_Prefix (El),
                              El_Tnode);
         end loop;
         Finish_Record_Type (El_List, Info.B.Base_Type (Kind));
      end loop;

      if Is_Unbounded then
         Info.Type_Mode := Type_Mode_Unbounded_Record;
         Finish_Unbounded_Type_Base (Info);
         Translate_Record_Type_Bounds (Def, Info);
         Create_Unbounded_Type_Fat_Pointer (Info);
         Finish_Type_Definition (Info);

         --  There are internal fields for unbounded records, so the objects
         --  must be built.
         Set_Complex_Type (Info, True);
      else
         if Need_Size then
            Info.Type_Mode := Type_Mode_Complex_Record;
         else
            Info.Type_Mode := Type_Mode_Static_Record;
         end if;
         Info.Ortho_Type := Info.B.Base_Type;
         Finish_Type_Definition (Info);
         Info.B.Base_Ptr_Type := Info.Ortho_Ptr_Type;

         if Need_Size then
            Set_Complex_Type (Info, True);
            Create_Size_Var (Def, Info);
         end if;
      end if;
   end Translate_Record_Type;

   procedure Translate_Record_Subtype (Def : Iir; With_Vars : Boolean)
   is
      Base_Type  : constant Iir := Get_Base_Type (Def);
      Base_Info  : constant Type_Info_Acc := Get_Info (Base_Type);
      Info       : constant Type_Info_Acc := Get_Info (Def);
      El_List    : constant Iir_Flist := Get_Elements_Declaration_List (Def);
      Type_Mark  : constant Iir := Get_Subtype_Type_Mark (Def);
      El_Blist   : constant Iir_Flist :=
        Get_Elements_Declaration_List (Base_Type);
      El_Tm_List : Iir_Flist;
      El, B_El   : Iir_Element_Declaration;
      El_Type    : Iir;
      El_Btype   : Iir;

      Has_New_Constraints : Boolean;
      Has_Boxed_Elements : Boolean;

      Rec        : O_Element_List;
      Field_Info : Ortho_Info_Acc;
      El_Tinfo   : Type_Info_Acc;
      El_Tnode   : O_Tnode;

      Mark : Id_Mark_Type;
   begin
      --  Translate the newly constrained elements.
      if Is_Valid (Type_Mark) then
         --  Type_mark may be null for anonymous subtype.
         El_Tm_List := Get_Elements_Declaration_List
           (Get_Type (Get_Named_Entity (Type_Mark)));
      else
         El_Tm_List := El_Blist;
      end if;
      Has_New_Constraints := False;
      Has_Boxed_Elements := False;
      for I in Flist_First .. Flist_Last (El_List) loop
         El := Get_Nth_Element (El_List, I);
         El_Type := Get_Type (El);
         if Is_Fully_Constrained_Type (El) then
            El_Btype := Get_Type (Get_Nth_Element (El_Tm_List, I));
            if not Is_Fully_Constrained_Type (El_Btype) then
               Has_New_Constraints := True;
               if Get_Type_Staticness (El_Type) = Locally then
                  Has_Boxed_Elements := True;
               end if;
               Push_Identifier_Prefix (Mark, Get_Identifier (El));
               Translate_Subtype_Definition (El_Type, El_Btype, With_Vars);
               Pop_Identifier_Prefix (Mark);
            end if;
         end if;
      end loop;

      --  By default, use the same representation as the base type.
      Info.all := Base_Info.all;
      Info.S := Ortho_Info_Subtype_Record_Init;
      --  However, it is a different subtype which has its own rti.
      Info.Type_Rti := O_Dnode_Null;

      if Get_Constraint_State (Def) /= Fully_Constrained
        or else not Has_New_Constraints
      then
         --  The subtype is not completly constrained: it cannot be used to
         --    create objects, so wait until it is completly constrained.
         --  The subtype is simply an alias.
         --  In both cases, use the same representation as its type mark.
         return;
      end if;

      --  Record is constrained.
      if Get_Type_Staticness (Def) = Locally then
         Info.Type_Mode := Type_Mode_Static_Record;
      else
         Info.Type_Mode := Type_Mode_Complex_Record;
      end if;

      --  Base type is complex (unbounded record)
      Copy_Complex_Type (Info, Base_Info);

      --  Then create the record type, containing the base record and the
      --  fields.
      if Has_Boxed_Elements then
         Info.Ortho_Type (Mode_Signal) := O_Tnode_Null;
         for Kind in Mode_Value .. Type_To_Last_Object_Kind (Def) loop
            Start_Record_Type (Rec);
            New_Record_Field (Rec, Info.S.Box_Field (Kind), Wki_Base,
                              Info.B.Base_Type (Kind));
            for I in Flist_First .. Flist_Last (El_Blist) loop
               B_El := Get_Nth_Element (El_Blist, I);
               El := Get_Nth_Element (El_List, I);

               --  This element has been locally constrained.
               if Is_Unbounded_Type (Get_Info (Get_Type (B_El)))
                 and then Get_Type_Staticness (Get_Type(El)) = Locally
               then
                  if Kind = Mode_Value then
                     Field_Info := Add_Info (El, Kind_Field);
                  else
                     Field_Info := Get_Info (El);
                  end if;
                  El := Get_Nth_Element (El_List, I);
                  El_Tinfo := Get_Info (Get_Type (El));
                  El_Tnode := El_Tinfo.Ortho_Type (Kind);
                  New_Record_Field (Rec, Field_Info.Field_Node (Kind),
                                    Create_Identifier_Without_Prefix (El),
                                    El_Tnode);
               end if;
            end loop;
            Finish_Record_Type (Rec, Info.Ortho_Type (Kind));
         end loop;

         Finish_Type_Definition (Info);
      else
         --  This is a complex type as the size is not known at compile
         --  time.
         Info.Ortho_Type := Base_Info.B.Base_Type;
         Info.Ortho_Ptr_Type := Base_Info.B.Base_Ptr_Type;
      end if;

      if Get_Type_Staticness (Def) /= Locally then
         Create_Size_Var (Def, Info);
      end if;

      if With_Vars then
         Create_Composite_Subtype_Bounds_Var (Def, False);
      end if;
   end Translate_Record_Subtype;

   procedure Create_Record_Type_Builder
     (Def : Iir_Record_Type_Definition; Kind : Object_Kind_Type)
   is
      Info : constant Type_Info_Acc := Get_Info (Def);
      Base : constant O_Dnode := Info.C (Kind).Builder_Base_Param;
      List : constant Iir_Flist := Get_Elements_Declaration_List (Def);
      El   : Iir_Element_Declaration;

      Off_Var    : O_Dnode;
      Off_Val    : O_Enode;
      El_Off     : O_Enode;
      Sub_Bound  : Mnode;
      El_Type    : Iir;
      Inner_Type : Iir;
      El_Tinfo   : Type_Info_Acc;
   begin
      Start_Subprogram_Body (Info.C (Kind).Builder_Func);
      Subprgs.Start_Subprg_Instance_Use (Info.C (Kind).Builder_Instance);

      --  Declare OFF, the offset variable
      New_Var_Decl (Off_Var, Get_Identifier ("off"), O_Storage_Local,
                    Ghdl_Index_Type);

      --  Reserve memory for the record, ie:
      --  OFF = SIZEOF (record).
      --  Align for signals, as the base type may contain a single index.
      Off_Val := New_Lit
        (New_Sizeof (Info.B.Base_Type (Kind), Ghdl_Index_Type));
      if Kind = Mode_Signal then
         Off_Val := Realign (Off_Val, Ghdl_Signal_Ptr);
      end if;
      New_Assign_Stmt (New_Obj (Off_Var), Off_Val);

      --  Set memory for each complex element.
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         El_Type := Get_Type (El);
         El_Tinfo := Get_Info (El_Type);
         if Is_Complex_Type (El_Tinfo)
           or else Is_Unbounded_Type (El_Tinfo)
         then
            --  Complex or unbounded type.  Field is an offset.

            --  Align on the innermost array element (which should be
            --  a record) for Mode_Value.  No need to align for signals,
            --  as all non-composite elements are accesses.
            Inner_Type := Get_Innermost_Non_Array_Element (El_Type);
            Off_Val := New_Obj_Value (Off_Var);
            if Kind = Mode_Value then
               Off_Val := Realign (Off_Val, Inner_Type);
            end if;
            New_Assign_Stmt (New_Obj (Off_Var), Off_Val);

            --  Set the offset.
            New_Assign_Stmt
              (New_Selected_Element (New_Acc_Value (New_Obj (Base)),
               Get_Info (El).Field_Node (Kind)),
               New_Obj_Value (Off_Var));

            Open_Temp;

            if Is_Complex_Type (El_Tinfo)
              and then El_Tinfo.C (Kind).Builder_Need_Func
            then
               --  This type needs a builder, call it.
               declare
                  Base2 : Mnode;
                  Ptr_Var    : O_Dnode;
               begin
                  if Is_Unbounded_Type (Info) then
                     Base2 := Create_Temp (Info, Kind);
                     New_Assign_Stmt
                       (M2Lp (Get_Composite_Bounds (Base2)),
                        New_Obj_Value (Info.C (Kind).Builder_Bound_Param));
                     New_Assign_Stmt
                       (M2Lp (Get_Composite_Base (Base2)),
                        New_Obj_Value (Info.C (Kind).Builder_Base_Param));
                  else
                     Base2 := Dp2M (Base, Info, Kind);
                  end if;

                  Ptr_Var := Create_Temp (El_Tinfo.Ortho_Ptr_Type (Kind));

                  New_Assign_Stmt
                    (New_Obj (Ptr_Var),
                     M2E (Chap6.Translate_Selected_Element (Base2, El)));

                  El_Off := Gen_Call_Type_Builder
                    (Dp2M (Ptr_Var, El_Tinfo, Kind), El_Type);
               end;
            else
               if Is_Unbounded_Type (El_Tinfo) then
                  Sub_Bound := Bounds_To_Element_Bounds
                    (Dp2M (Info.C (Kind).Builder_Bound_Param,
                           Info, Mode_Value,
                           Info.B.Bounds_Type, Info.B.Bounds_Ptr_Type),
                     El);
               else
                  Sub_Bound := Mnode_Null;
               end if;

               --  Allocate memory.
               El_Off := Get_Subtype_Size (El_Type, Sub_Bound, Kind);
            end if;

            New_Assign_Stmt
              (New_Obj (Off_Var),
               New_Dyadic_Op (ON_Add_Ov,
                              New_Obj_Value (Off_Var), El_Off));

            Close_Temp;
         end if;
      end loop;

      --  Align the size to the object alignment.
      Off_Val := New_Obj_Value (Off_Var);
      if Kind = Mode_Value then
         Off_Val := Realign (Off_Val, Def);
      end if;

      New_Return_Stmt (Off_Val);

      Subprgs.Finish_Subprg_Instance_Use (Info.C (Kind).Builder_Instance);
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

      --  A protected type is a complex type, as its size is not known
      --  at definition point (will be known at body declaration).
      Info.C := new Complex_Type_Arr_Info;
      Info.C (Mode_Value).Builder_Need_Func := False;

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
            when others =>
               Error_Kind ("translate_protected_type_subprograms", El);
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
   procedure Create_Scalar_Type_Range (Def : Iir; Target : O_Lnode)
   is
      T_Info    : constant Type_Info_Acc := Get_Info (Get_Base_Type (Def));
   begin
      Chap7.Translate_Range
        (Lv2M (Target, T_Info, Mode_Value,
               T_Info.B.Range_Type, T_Info.B.Range_Ptr_Type),
         Get_Range_Constraint (Def), Def);
   end Create_Scalar_Type_Range;

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
            return Create_Static_Composite_Subtype_Bounds (Def);

         when Iir_Kind_Array_Type_Definition =>
            return O_Cnode_Null;

         when others =>
            Error_Kind ("create_static_type_definition_type_range", Def);
      end case;
   end Create_Static_Type_Definition_Type_Range;

   procedure Create_Type_Definition_Type_Range (Def : Iir)
   is
      Target : O_Lnode;
      Info   : Type_Info_Acc;
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kinds_Scalar_Subtype_Definition =>
            Info := Get_Info (Def);
            if not Info.S.Same_Range then
               Target := Get_Var (Info.S.Range_Var);
               Create_Scalar_Type_Range (Def, Target);
            end if;

         when Iir_Kind_Array_Subtype_Definition =>
            if Get_Constraint_State (Def) = Fully_Constrained then
               Create_Composite_Subtype_Bounds (Def);
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
                     Create_Type_Definition_Type_Range (Index);
                  end if;
               end loop;
            end;
            return;

         when Iir_Kind_Record_Subtype_Definition =>
            Info := Get_Info (Def);
            if Info.S.Composite_Bounds /= Null_Var then
               Create_Composite_Subtype_Bounds (Def);
            end if;

         when Iir_Kind_Access_Type_Definition
            | Iir_Kind_Access_Subtype_Definition
            | Iir_Kind_File_Type_Definition
            | Iir_Kind_Record_Type_Definition
            | Iir_Kind_Protected_Type_Declaration =>
            return;

         when others =>
            Error_Kind ("create_type_definition_type_range", Def);
      end case;
   end Create_Type_Definition_Type_Range;

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
               V : Iir_Int32;
            begin
               V := Iir_Int32 (Get_Value (Lit));
               if Is_Hi then
                  return V = Iir_Int32'Last;
               else
                  return V = Iir_Int32'First;
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
               V : Iir_Int64;
            begin
               V := Get_Value (Lit);
               if Is_Hi then
                  return V = Iir_Int64'Last;
               else
                  return V = Iir_Int64'First;
               end if;
            end;
         when Type_Mode_P64 =>
            declare
               V : Iir_Int64;
            begin
               V := Get_Physical_Value (Lit);
               if Is_Hi then
                  return V = Iir_Int64'Last;
               else
                  return V = Iir_Int64'First;
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
      if Get_Kind (Base) in Iir_Kinds_Scalar_Subtype_Definition then
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
      end if;

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
         Subtype_Info.S.Nocheck_Hi :=
           Is_Equal_Limit (Hi, True, Def, Base_Info.Type_Mode);
         Subtype_Info.S.Nocheck_Low :=
           Is_Equal_Limit (Lo, False, Def, Base_Info.Type_Mode);
      end if;
   end Create_Subtype_Info_From_Type;

   procedure Create_Type_Definition_Size_Var (Def : Iir);

   procedure Create_Record_Size_Var (Def : Iir; Kind : Object_Kind_Type)
   is
      Info        : constant Type_Info_Acc := Get_Info (Def);
      List        : constant Iir_Flist := Get_Elements_Declaration_List (Def);
      El          : Iir_Element_Declaration;
      El_Type     : Iir;
      El_Tinfo    : Type_Info_Acc;
      Inner_Type  : Iir;
      Res         : O_Enode;
      Align_Var   : O_Dnode;
   begin
      Open_Temp;

      --  Start with the size of the 'base' record, that
      --  contains all non-complex types and an offset for
      --  each complex types.
      Res := New_Lit (New_Sizeof (Info.Ortho_Type (Kind), Ghdl_Index_Type));

      --  Start with alignment of the record.
      --  ALIGN = ALIGNOF (record)
      case Kind is
         when Mode_Value =>
            Align_Var := Create_Temp (Ghdl_Index_Type);
            New_Assign_Stmt
              (New_Obj (Align_Var),
               Get_Type_Alignmask (Info.Ortho_Type (Kind)));
         when Mode_Signal =>
            Res := Realign (Res, Ghdl_Signal_Ptr);
      end case;

      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         El_Type := Get_Type (El);
         El_Tinfo := Get_Info (El_Type);
         if Get_Type_Staticness (El_Type) /= Locally
           and then
           (Is_Complex_Type (El_Tinfo)
              or else Get_Kind (El) = Iir_Kind_Record_Element_Constraint)
         then
            Inner_Type := Get_Innermost_Non_Array_Element (El_Type);

            --  Align (only for Mode_Value) the size,
            --  and add the size of the element.
            if Kind = Mode_Value then
               --  Largest alignment.
               New_Assign_Stmt
                 (New_Obj (Align_Var),
                  New_Dyadic_Op
                    (ON_Or,
                     New_Obj_Value (Align_Var),
                     Get_Type_Alignmask
                       (Get_Ortho_Type (Inner_Type, Mode_Value))));
               Res := Realign (Res, Inner_Type);
            end if;

            Res := New_Dyadic_Op
              (ON_Add_Ov,
               Res, New_Value (Get_Var (El_Tinfo.C (Kind).Size_Var)));
         end if;
      end loop;
      if Kind = Mode_Value then
         Res := Realign (Res, Align_Var);
      end if;
      New_Assign_Stmt (Get_Var (Info.C (Kind).Size_Var), Res);
      Close_Temp;
   end Create_Record_Size_Var;

   procedure Create_Array_Size_Var (Def : Iir; Kind : Object_Kind_Type)
   is
      Info    : constant Type_Info_Acc := Get_Info (Def);
      El_Type : constant Iir := Get_Element_Subtype (Def);
      Res     : O_Enode;
   begin
      Res := New_Dyadic_Op
        (ON_Mul_Ov,
         Get_Array_Type_Length (Def),
         Chap3.Get_Object_Size (T2M (El_Type, Kind), El_Type));
      New_Assign_Stmt (Get_Var (Info.C (Kind).Size_Var), Res);
   end Create_Array_Size_Var;

   procedure Create_Type_Definition_Size_Var (Def : Iir)
   is
      Info : constant Type_Info_Acc := Get_Info (Def);
   begin
      if not Is_Complex_Type (Info) then
         return;
      end if;

      for Kind in Mode_Value .. Type_To_Last_Object_Kind (Def) loop
         if Info.C (Kind).Size_Var /= Null_Var then
            case Info.Type_Mode is
               when Type_Mode_Non_Composite
                 | Type_Mode_Unbounded_Array
                 | Type_Mode_Unbounded_Record
                 | Type_Mode_Unknown
                 | Type_Mode_Protected =>
                  raise Internal_Error;
               when Type_Mode_Static_Record
                 | Type_Mode_Static_Array =>
                  --  No need to create a size var, the size is known.
                  raise Internal_Error;
               when Type_Mode_Complex_Record =>
                  Create_Record_Size_Var (Def, Kind);
               when Type_Mode_Complex_Array =>
                  Create_Array_Size_Var (Def, Kind);
            end case;
         end if;
      end loop;
   end Create_Type_Definition_Size_Var;

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
            Translate_Array_Type_Definition (Def);

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
   end Translate_Bool_Type_Definition;

   procedure Translate_Subtype_Definition
     (Def : Iir; Parent_Type : Iir; With_Vars : Boolean := True)
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
            Create_Subtype_Info_From_Type (Def, Parent_Type, Info);
            if With_Vars and then not Info.S.Same_Range then
               Create_Type_Range_Var (Def);
            end if;

         when Iir_Kind_Array_Subtype_Definition =>
            --  Handle element subtype.
            declare
               El_Type : constant Iir := Get_Element_Subtype (Def);
               Parent_El_Type : constant Iir :=
                 Get_Element_Subtype (Parent_Type);
               Mark : Id_Mark_Type;
            begin
               if El_Type /= Parent_El_Type then
                  Push_Identifier_Prefix (Mark, "ET");
                  Translate_Subtype_Definition
                    (El_Type, Parent_El_Type, With_Vars);
                  Pop_Identifier_Prefix (Mark);
               end if;

               if Get_Constraint_State (Def) = Fully_Constrained then
                  Translate_Array_Subtype_Definition (Def, Parent_Type);
                  if With_Vars then
                     Create_Composite_Subtype_Bounds_Var (Def, False);
                  end if;
               elsif Is_Fully_Constrained_Type (El_Type)
                 and then not Is_Fully_Constrained_Type (Parent_El_Type)
                 and then Is_Static_Type (Get_Info (El_Type))
               then
                  --  The array subtype is not constrained, but the element
                  --  subtype was just contrained.  Create an array for
                  --  ortho, if the element subtype is static.
                  Translate_Array_Subtype_Definition_Constrained_Element
                    (Def, Parent_Type);
               else
                  --  An unconstrained array subtype.  Use same infos as base
                  --  type.
                  Free_Info (Def);
                  Set_Info (Def, Get_Info (Parent_Type));
               end if;
            end;

         when Iir_Kind_Record_Subtype_Definition =>
            Translate_Record_Subtype (Def, With_Vars);

         when Iir_Kind_Access_Subtype_Definition =>
            --  Like the access type.
            Free_Info (Def);
            Set_Info (Def, Get_Info (Parent_Type));

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

      Tinfo := Get_Info (Def);
      if not Is_Complex_Type (Tinfo)
        or else Tinfo.C (Mode_Value).Builder_Need_Func = False
      then
         return;
      end if;

      if Kind in Subprg_Translate_Spec then
         --  Declare subprograms.
         Id := Get_Identifier (Decl);
         Create_Builder_Subprogram_Decl (Tinfo, Id, Mode_Value);
         if Get_Has_Signal_Flag (Def) then
            Create_Builder_Subprogram_Decl (Tinfo, Id, Mode_Signal);
         end if;
      end if;

      if Kind in Subprg_Translate_Body then
         if Global_Storage = O_Storage_External then
            return;
         end if;

         --  Define subprograms.
         case Get_Kind (Def) is
            when Iir_Kind_Array_Type_Definition =>
               Create_Array_Type_Builder (Def, Mode_Value);
               if Get_Has_Signal_Flag (Def) then
                  Create_Array_Type_Builder (Def, Mode_Signal);
               end if;
            when Iir_Kind_Record_Type_Definition =>
               Create_Record_Type_Builder (Def, Mode_Value);
               if Get_Has_Signal_Flag (Def) then
                  Create_Record_Type_Builder (Def, Mode_Signal);
               end if;
            when others =>
               Error_Kind ("translate_type_subprograms", Def);
         end case;
      end if;
   end Translate_Type_Subprograms;

   --  Initialize the objects related to a type (type range and type
   --  descriptor).
   procedure Elab_Type_Definition (Def : Iir);
   procedure Elab_Type_Definition_Depend is new Handle_Anonymous_Subtypes
     (Handle_A_Subtype => Elab_Type_Definition);
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

      Create_Type_Definition_Type_Range (Def);
      Create_Type_Definition_Size_Var (Def);
   end Elab_Type_Definition;

   procedure Translate_Subtype_Indication (Def : Iir; With_Vars : Boolean)
   is
      Parent_Type : Iir;
   begin
      Parent_Type := Get_Subtype_Type_Mark (Def);
      pragma Assert (Parent_Type /= Null_Iir);
      Parent_Type := Get_Type (Get_Named_Entity (Parent_Type));
      Translate_Subtype_Definition (Def, Parent_Type, With_Vars);
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
      Chap3.Translate_Subtype_Definition
        (Def, Get_Base_Type (Def), With_Vars);
      Pop_Identifier_Prefix (Mark);
   end Translate_Anonymous_Subtype_Definition;

   procedure Translate_Object_Subtype (Decl      : Iir;
                                       With_Vars : Boolean := True)
   is
      Def : constant Iir := Get_Type (Decl);
      Parent_Type : Iir;
      Mark  : Id_Mark_Type;
      Mark2 : Id_Mark_Type;
   begin
      if Is_Anonymous_Type_Definition (Def) then
         Push_Identifier_Prefix (Mark, Get_Identifier (Decl));
         Push_Identifier_Prefix (Mark2, "OT");
         Parent_Type := Get_Subtype_Type_Mark (Def);
         if Parent_Type /= Null_Iir then
            Parent_Type := Get_Type (Get_Named_Entity (Parent_Type));
         else
            Parent_Type := Get_Base_Type (Def);
            --  Parent_Type should be integer_type_definition for iterators,
            --  or the subtype indication for constant (in the case the
            --  default value constrains the subtype indication), or an
            --  object alias, or anywhere because of 'Subtype applied on one
            --  of the above object...
         end if;
         Chap3.Translate_Subtype_Definition (Def, Parent_Type, With_Vars);
         Pop_Identifier_Prefix (Mark2);
         Pop_Identifier_Prefix (Mark);
      end if;
   end Translate_Object_Subtype;

   procedure Elab_Object_Subtype (Def : Iir) is
   begin
      if Is_Anonymous_Type_Definition (Def) then
         Elab_Type_Definition (Def);
      end if;
   end Elab_Object_Subtype;

   procedure Elab_Type_Declaration (Decl : Iir) is
   begin
      Elab_Type_Definition (Get_Type_Definition (Decl));
   end Elab_Type_Declaration;

   procedure Elab_Subtype_Declaration (Decl : Iir_Subtype_Declaration) is
   begin
      Elab_Type_Definition (Get_Type (Decl));
   end Elab_Subtype_Declaration;

   function Get_Thin_Array_Length (Atype : Iir) return O_Cnode
   is
      Indexes_List : constant Iir_Flist := Get_Index_Subtype_List (Atype);
      Nbr_Dim      : constant Natural := Get_Nbr_Elements (Indexes_List);
      Index        : Iir;
      Val          : Iir_Int64;
      Rng          : Iir;
   begin
      Val := 1;
      for I in 0 .. Nbr_Dim - 1 loop
         Index := Get_Index_Type (Indexes_List, I);
         Rng := Get_Range_Constraint (Index);
         Val := Val * Eval_Discrete_Range_Length (Rng);
      end loop;
      return New_Unsigned_Literal (Ghdl_Index_Type, Unsigned_64 (Val));
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

   function Bounds_To_Element_Bounds (B : Mnode; El : Iir) return Mnode
   is
      El_Type : constant Iir := Get_Type (El);
      El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
      Base_El : constant Iir := Get_Base_Element_Declaration (El);
   begin
      return Lv2M
        (New_Selected_Element (M2Lv (B),
                               Get_Info (Base_El).Field_Bound),
         El_Tinfo, Mode_Value,
         El_Tinfo.B.Bounds_Type, El_Tinfo.B.Bounds_Ptr_Type);
   end Bounds_To_Element_Bounds;

   function Array_Bounds_To_Element_Bounds (B : Mnode; Atype : Iir)
                                           return Mnode
   is
      Arr_Tinfo : constant Type_Info_Acc := Get_Info (Atype);
      El_Type : constant Iir := Get_Element_Subtype (Atype);
      El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
   begin
      return Lv2M
        (New_Selected_Element (M2Lv (B), Arr_Tinfo.B.El_Bounds),
         El_Tinfo, Mode_Value,
         El_Tinfo.B.Bounds_Type, El_Tinfo.B.Bounds_Ptr_Type);
   end Array_Bounds_To_Element_Bounds;

   function Array_Bounds_To_Element_Size (B : Mnode; Atype : Iir)
                                           return O_Lnode
   is
      Arr_Tinfo : constant Type_Info_Acc := Get_Info (Atype);
      Sizes : O_Lnode;
      Field : O_Fnode;
   begin
      Sizes := New_Selected_Element (M2Lv (B), Arr_Tinfo.B.El_Size);
      case Get_Object_Kind (B) is
         when Mode_Value =>
            Field := Ghdl_Sizes_Val;
         when Mode_Signal =>
            Field := Ghdl_Sizes_Sig;
      end case;
      Sizes := New_Selected_Element (Sizes, Field);
      return Sizes;
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

   function Get_Array_Type_Bounds (Info : Type_Info_Acc) return Mnode
   is
   begin
      case Info.Type_Mode is
         when Type_Mode_Unbounded =>
            raise Internal_Error;
         when Type_Mode_Bounded_Arrays
           | Type_Mode_Bounded_Records =>
            return Varv2M (Info.S.Composite_Bounds,
                           Info, Mode_Value,
                           Info.B.Bounds_Type,
                           Info.B.Bounds_Ptr_Type);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Array_Type_Bounds;

   function Get_Array_Type_Bounds (Atype : Iir) return Mnode is
   begin
      return Get_Array_Type_Bounds (Get_Info (Atype));
   end Get_Array_Type_Bounds;

   function Get_Composite_Bounds (Arr : Mnode) return Mnode
   is
      Info : constant Type_Info_Acc := Get_Type_Info (Arr);
   begin
      case Info.Type_Mode is
         when Type_Mode_Unbounded_Array
           | Type_Mode_Unbounded_Record =>
            declare
               Kind : constant Object_Kind_Type := Get_Object_Kind (Arr);
            begin
               return Lp2M
                 (New_Selected_Element (M2Lv (Arr),
                                        Info.B.Bounds_Field (Kind)),
                  Info,
                  Mode_Value,
                  Info.B.Bounds_Type,
                  Info.B.Bounds_Ptr_Type);
            end;
         when Type_Mode_Bounded_Arrays
           | Type_Mode_Bounded_Records =>
            return Get_Array_Type_Bounds (Info);
         when Type_Mode_Bounds_Acc =>
            return Lp2M (M2Lv (Arr), Info, Mode_Value);
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
         return Get_Bounds_Length (Get_Array_Type_Bounds (Atype), Atype);
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

   function Get_Composite_Base (Arr : Mnode) return Mnode
   is
      Info : constant Type_Info_Acc := Get_Type_Info (Arr);
   begin
      case Info.Type_Mode is
         when Type_Mode_Unbounded_Array
           | Type_Mode_Unbounded_Record =>
            declare
               Kind : constant Object_Kind_Type := Get_Object_Kind (Arr);
            begin
               return Lp2M
                 (New_Selected_Element (M2Lv (Arr),
                                        Info.B.Base_Field (Kind)),
                  Info, Kind,
                  Info.B.Base_Type (Kind), Info.B.Base_Ptr_Type (Kind));
            end;
         when Type_Mode_Bounded_Arrays =>
            return Arr;
         when Type_Mode_Bounded_Records =>
            return Unbox_Record (Arr);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Composite_Base;

   function Unbox_Record (Arr : Mnode) return Mnode
   is
      Info : constant Type_Info_Acc := Get_Type_Info (Arr);
   begin
      case Info.Type_Mode is
         when Type_Mode_Arrays =>
            return Arr;
         when Type_Mode_Unbounded_Record =>
            return Arr;
         when Type_Mode_Bounded_Records =>
            declare
               Kind : constant Object_Kind_Type := Get_Object_Kind (Arr);
               Box_Field : constant O_Fnode := Info.S.Box_Field (Kind);
            begin
               if Box_Field /= O_Fnode_Null then
                  --  Unbox the record.
                  return Lv2M (New_Selected_Element (M2Lv (Arr), Box_Field),
                               Info, Kind,
                               Info.B.Base_Type (Kind),
                               Info.B.Base_Ptr_Type (Kind));
               else
                  return Arr;
               end if;
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Unbox_Record;

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

   function Reindex_Array
     (Base : Mnode; Atype : Iir; Index : O_Enode; Stride : O_Enode)
     return O_Enode
   is
      El_Type  : constant Iir := Get_Element_Subtype (Atype);
      El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
      Kind     : constant Object_Kind_Type := Get_Object_Kind (Base);
   begin
      return Add_Pointer (M2E (Base),
                          New_Dyadic_Op (ON_Mul_Ov, Stride, Index),
                          El_Tinfo.Ortho_Ptr_Type (Kind));
   end Reindex_Array;

   function Reindex_Complex_Array
     (Base : Mnode; Atype : Iir; Index : O_Enode; Res_Info : Type_Info_Acc)
      return Mnode
   is
      El_Type  : constant Iir := Get_Element_Subtype (Atype);
      El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
      Kind     : constant Object_Kind_Type := Get_Object_Kind (Base);
   begin
      return E2M (Reindex_Array
                    (Base, Atype,
                     Index,
                     New_Value (Get_Var (El_Tinfo.C (Kind).Size_Var))),
                  Res_Info, Kind);
   end Reindex_Complex_Array;

   function Index_Base (Base : Mnode; Atype : Iir; Index : O_Enode)
                        return Mnode
   is
      El_Type  : constant Iir := Get_Element_Subtype (Atype);
      El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
      Kind     : constant Object_Kind_Type := Get_Object_Kind (Base);
   begin
      if Is_Unbounded_Type (El_Tinfo) then
         --  return Reindex_Unbounded_Array (Base, Atype, Index, El_Tinfo);
         --  TODO
         raise Internal_Error;
      elsif Is_Complex_Type (El_Tinfo) then
         return Reindex_Complex_Array (Base, Atype, Index, El_Tinfo);
      else
         return Lv2M (New_Indexed_Element (M2Lv (Base), Index),
                      El_Tinfo, Kind);
      end if;
   end Index_Base;

   function Index_Array (Arr : Mnode; Atype : Iir; Index : O_Enode)
                        return Mnode
   is
      El_Type  : constant Iir := Get_Element_Subtype (Atype);
      El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
      Kind     : constant Object_Kind_Type := Get_Object_Kind (Arr);
   begin
      if Is_Unbounded_Type (El_Tinfo) then
         return E2M
           (Add_Pointer
              (M2E (Get_Composite_Base (Arr)),
               New_Dyadic_Op
                 (ON_Mul_Ov,
                  Index,
                  New_Value (Array_Bounds_To_Element_Size
                               (Get_Composite_Bounds (Arr), Atype))),
               El_Tinfo.B.Base_Ptr_Type (Kind)),
            El_Tinfo, Kind,
            El_Tinfo.B.Base_Type (Kind),
            El_Tinfo.B.Base_Ptr_Type (Kind));
      elsif Is_Complex_Type (El_Tinfo) then
         return Reindex_Complex_Array
           (Get_Composite_Base (Arr), Atype, Index, El_Tinfo);
      else
         return Lv2M
           (New_Indexed_Element (M2Lv (Get_Composite_Base (Arr)), Index),
            El_Tinfo, Kind);
      end if;
   end Index_Array;

   function Slice_Base (Base : Mnode; Atype : Iir; Index : O_Enode)
                        return Mnode
   is
      T_Info   : constant Type_Info_Acc :=  Get_Info (Atype);
      El_Type  : constant Iir := Get_Element_Subtype (Atype);
      El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
      Kind     : constant Object_Kind_Type := Get_Object_Kind (Base);
   begin
      if Is_Complex_Type (El_Tinfo) then
         return Reindex_Complex_Array (Base, Atype, Index, T_Info);
      else
         return Lv2M (New_Slice (M2Lv (Base),
                                 T_Info.B.Base_Type (Kind),
                                 Index),
                      T_Info, Kind,
                      T_Info.B.Base_Type (Kind),
                      T_Info.B.Base_Ptr_Type (Kind));
      end if;
   end Slice_Base;

   procedure Maybe_Call_Type_Builder (Obj : Mnode; Obj_Type : Iir)
   is
      Dinfo  : constant Type_Info_Acc :=
        Get_Info (Get_Base_Type (Obj_Type));
      Kind   : constant Object_Kind_Type := Get_Object_Kind (Obj);
   begin
      if Is_Complex_Type (Dinfo)
        and then Dinfo.C (Kind).Builder_Need_Func
      then
         Open_Temp;
         --  Build the type.
         Chap3.Gen_Call_Type_Builder (Obj, Obj_Type);
         Close_Temp;
      end if;
   end Maybe_Call_Type_Builder;

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

      Maybe_Call_Type_Builder (Res, Arr_Type);
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

   procedure Create_Array_Subtype (Sub_Type : Iir)
   is
      Mark : Id_Mark_Type;
   begin
      Push_Identifier_Prefix_Uniq (Mark);
      if Get_Info (Sub_Type) = null then
         --  Minimal subtype creation.
         Translate_Subtype_Definition
           (Sub_Type, Get_Base_Type (Sub_Type), False);
      end if;
      --  Force creation of variables.
      Chap3.Create_Composite_Subtype_Bounds_Var (Sub_Type, True);
      Chap3.Create_Type_Definition_Size_Var (Sub_Type);
      Pop_Identifier_Prefix (Mark);
   end Create_Array_Subtype;

   --  Copy SRC to DEST.
   --  Both have the same type, OTYPE.
   procedure Translate_Object_Copy (Dest     : Mnode;
                                    Src      : O_Enode;
                                    Obj_Type : Iir)
   is
      Info : constant Type_Info_Acc := Get_Info (Obj_Type);
      Kind : constant Object_Kind_Type := Get_Object_Kind (Dest);
      D    : Mnode;
   begin
      case Info.Type_Mode is
         when Type_Mode_Scalar
           | Type_Mode_Acc
           | Type_Mode_Bounds_Acc
           | Type_Mode_File =>
            --  Scalar or thin pointer.
            New_Assign_Stmt (M2Lv (Dest), Src);
         when Type_Mode_Unbounded_Array
           | Type_Mode_Unbounded_Record =>
            --  a fat array.
            D := Stabilize (Dest);
            Gen_Memcpy (M2Addr (Get_Composite_Base (D)),
                        M2Addr (Get_Composite_Base (E2M (Src, Info, Kind))),
                        Get_Object_Size (D, Obj_Type));
         when Type_Mode_Bounded_Arrays
            | Type_Mode_Bounded_Records =>
            D := Stabilize (Dest);
            Gen_Memcpy (M2Addr (D), Src, Get_Object_Size (D, Obj_Type));
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
         when Type_Mode_Complex_Array
           | Type_Mode_Complex_Record =>
            --  The length is pre-computed for a complex bounded type.
            if Type_Info.C (Kind).Size_Var /= Null_Var then
               return New_Value (Get_Var (Type_Info.C (Kind).Size_Var));
            else
               raise Internal_Error;
            end if;
         when Type_Mode_Non_Composite
            | Type_Mode_Static_Array
            | Type_Mode_Static_Record =>
            return New_Lit (New_Sizeof (Type_Info.Ortho_Type (Kind),
                                        Ghdl_Index_Type));
         when Type_Mode_Unbounded_Array =>
            declare
               El_Type  : constant Iir := Get_Element_Subtype (Atype);
               El_Sz    : O_Enode;
            begin
               --  See create_array_size_var.
               El_Sz := Get_Subtype_Size (El_Type, Mnode_Null, Kind);
               return New_Dyadic_Op
                 (ON_Mul_Ov, Chap3.Get_Bounds_Length (Bounds, Atype), El_Sz);
            end;
         when Type_Mode_Unbounded_Record =>
            declare
               El_List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Atype);
               El : Iir;
               El_Type : Iir;
               El_Type_Info : Type_Info_Acc;
               El_Bounds : Mnode;
               Stable_Bounds : Mnode;
               Res : O_Enode;
            begin
               Stable_Bounds := Stabilize (Bounds);

               --  Size of base type
               Res := New_Lit (New_Sizeof (Type_Info.B.Base_Type (Kind),
                                           Ghdl_Index_Type));
               for I in Flist_First .. Flist_Last (El_List) loop
                  El := Get_Nth_Element (El_List, I);
                  El_Type := Get_Type (El);
                  El_Type_Info := Get_Info (El_Type);
                  if El_Type_Info.Type_Mode in Type_Mode_Unbounded then
                     --  Recurse
                     Res := Realign (Res, El_Type);
                     El_Bounds := Bounds_To_Element_Bounds (Stable_Bounds, El);
                     Res := New_Dyadic_Op
                       (ON_Add_Ov,
                        Res, Get_Subtype_Size (El_Type, El_Bounds, Kind));
                  elsif Is_Complex_Type (El_Type_Info) then
                     --  Add supplement
                     Res := Realign (Res, El_Type);
                     Res := New_Dyadic_Op
                       (ON_Add_Ov,
                        Res, Get_Subtype_Size (El_Type, Mnode_Null, Kind));
                  end if;
               end loop;
               return Res;
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Subtype_Size;

   function Get_Object_Size (Obj : Mnode; Obj_Type : Iir)
                            return O_Enode
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
         Allocate_Unbounded_Composite_Base
           (Alloc_Kind, Res, Obj_Type);
      else
         New_Assign_Stmt
           (M2Lp (Res),
            Gen_Alloc (Alloc_Kind,
                       Chap3.Get_Object_Size (T2M (Obj_Type, Kind), Obj_Type),
                       Tinfo.Ortho_Ptr_Type (Kind)));

         Maybe_Call_Type_Builder (Res, Obj_Type);
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
            when Iir_To =>
               return Gen_Compare_To;
            when Iir_Downto =>
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

   function Locally_Array_Match (L_Type, R_Type : Iir) return Boolean
   is
      L_Indexes : constant Iir_Flist := Get_Index_Subtype_List (L_Type);
      R_Indexes : constant Iir_Flist := Get_Index_Subtype_List (R_Type);
      L_El      : Iir;
      R_El      : Iir;
   begin
      for I in Flist_First .. Flist_Last (L_Indexes) loop
         L_El := Get_Index_Type (L_Indexes, I);
         R_El := Get_Index_Type (R_Indexes, I);
         if Eval_Discrete_Type_Length (L_El)
           /= Eval_Discrete_Type_Length (R_El)
         then
            return False;
         end if;
      end loop;
      return True;
   end Locally_Array_Match;

   procedure Check_Array_Match (L_Type : Iir;
                                L_Node : Mnode;
                                R_Type : Iir;
                                R_Node : Mnode;
                                Loc    : Iir)
   is
      L_Tinfo : constant Type_Info_Acc := Get_Info (L_Type);
      R_Tinfo : constant Type_Info_Acc := Get_Info (R_Type);
   begin
      --  FIXME: optimize for a statically bounded array of a complex type.
      if L_Tinfo.Type_Mode in Type_Mode_Arrays
        and then L_Tinfo.Type_Locally_Constrained
        and then R_Tinfo.Type_Mode in Type_Mode_Arrays
        and then R_Tinfo.Type_Locally_Constrained
      then
         --  Both left and right are thin array.
         --  Check here the length are the same.
         if not Locally_Array_Match (L_Type, R_Type) then
            Chap6.Gen_Bound_Error (Loc);
         end if;
      else
         --  Check length match.
         declare
            Index_List : constant Iir_Flist :=
              Get_Index_Subtype_List (L_Type);
            Cond       : O_Enode;
            Sub_Cond   : O_Enode;
         begin
            for I in 1 .. Get_Nbr_Elements (Index_List) loop
               Sub_Cond := New_Compare_Op
                 (ON_Neq,
                  M2E (Range_To_Length
                    (Get_Array_Range (L_Node, L_Type, I))),
                  M2E (Range_To_Length
                    (Get_Array_Range (R_Node, R_Type, I))),
                  Ghdl_Bool_Type);
               if I = 1 then
                  Cond := Sub_Cond;
               else
                  Cond := New_Dyadic_Op (ON_Or, Cond, Sub_Cond);
               end if;
            end loop;
            Chap6.Check_Bound_Error (Cond, Loc, 0);
         end;
      end if;
   end Check_Array_Match;

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
         when Iir_To =>
            Op := ON_Add_Ov;
         when Iir_Downto =>
            Op := ON_Sub_Ov;
      end case;

      Start_If_Stmt
        (If_Blk,
         New_Compare_Op (ON_Eq,
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
