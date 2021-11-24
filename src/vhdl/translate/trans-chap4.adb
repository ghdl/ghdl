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

with Vhdl.Errors; use Vhdl.Errors;
with Files_Map;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package; use Vhdl.Std_Package;
with Vhdl.Canon;
with Translation; use Translation;
with Trans.Chap2;
with Trans.Chap3;
with Trans.Chap5;
with Trans.Chap6;
with Trans.Chap7;
with Trans.Chap8;
with Trans.Chap9;
with Trans.Chap14;
with Trans.Rtis;
with Trans.Helpers2; use Trans.Helpers2;
with Trans_Decls; use Trans_Decls;
with Trans.Foreach_Non_Composite;

package body Trans.Chap4 is
   use Trans.Helpers;

   --  Get the ortho type for an object of mode MODE.
   function Get_Object_Type (Tinfo : Type_Info_Acc; Kind : Object_Kind_Type)
                                return O_Tnode is
   begin
      case Tinfo.Type_Mode is
         when Type_Mode_Complex_Record
           | Type_Mode_Complex_Array
           | Type_Mode_Protected =>
            --  For a complex type, use a pointer.
            return Tinfo.Ortho_Ptr_Type (Kind);
         when others =>
            return Tinfo.Ortho_Type (Kind);
      end case;
   end Get_Object_Type;

   --  Return the pointer type for Tinfo.
   --  For a fat array, this is the fat pointer to slightly optimize accesses.
   function Get_Object_Ptr_Type
     (Tinfo : Type_Info_Acc; Kind : Object_Kind_Type) return O_Tnode is
   begin
      if Tinfo.Type_Mode in Type_Mode_Unbounded then
         --  Fat pointers are already pointers, no need to create an
         --  additional indirection.
         return Tinfo.Ortho_Type (Kind);
      else
         if Kind = Mode_Signal
           and then Tinfo.Type_Mode in Type_Mode_Scalar
         then
            --  A scalar signal is already a pointer.
            return Tinfo.Ortho_Type (Kind);
         else
            return Tinfo.Ortho_Ptr_Type (Kind);
         end if;
      end if;
   end Get_Object_Ptr_Type;

   function Lop2M
     (Obj_Ptr : O_Lnode; Tinfo : Type_Info_Acc; Mode : Object_Kind_Type)
     return Mnode is
   begin
      if (Mode = Mode_Signal
            and then Tinfo.Type_Mode in Type_Mode_Scalar)
        or else Tinfo.Type_Mode in Type_Mode_Unbounded
      then
         return Lv2M (Obj_Ptr, Tinfo, Mode);
      else
         return Lp2M (Obj_Ptr, Tinfo, Mode);
      end if;
   end Lop2M;

   procedure Assign_Obj_Ptr (Dest : Mnode; Src : Mnode)
   is
      Mode : constant Object_Kind_Type := Get_Object_Kind (Dest);
      Tinfo : constant Type_Info_Acc := Get_Type_Info (Dest);
   begin
      pragma Assert (Mode = Get_Object_Kind (Src));
      pragma Assert (Tinfo.Type_Mode = Get_Type_Info (Src).Type_Mode);
      if Tinfo.Type_Mode in Type_Mode_Unbounded then
         Copy_Fat_Pointer (Stabilize (Dest), Stabilize (Src));
      else
         if Mode = Mode_Signal
           and then Tinfo.Type_Mode in Type_Mode_Scalar
         then
            New_Assign_Stmt (M2Lv (Dest), M2E (Src));
         else
            New_Assign_Stmt (M2Lp (Dest), M2Addr (Src));
         end if;
      end if;
   end Assign_Obj_Ptr;

   procedure Create_Object (El : Iir)
   is
      Obj_Type : O_Tnode;
      Info     : Object_Info_Acc;
      Tinfo    : Type_Info_Acc;
      Def      : Iir;
      Val      : constant Iir := Get_Default_Value (El);
      Storage  : O_Storage;
      Deferred : Iir;
   begin
      --  Be sure the object type was translated.
      if Get_Kind (El) = Iir_Kind_Constant_Declaration
        and then Get_Deferred_Declaration_Flag (El) = False
        and then Get_Deferred_Declaration (El) /= Null_Iir
      then
         --  This is a full constant declaration which completes a previous
         --  incomplete constant declaration.
         --
         --  Do not create the subtype of this full constant declaration,
         --  since it was already created by the deferred declaration.
         --  Use the type of the deferred declaration.
         Deferred := Get_Deferred_Declaration (El);
         Def := Get_Type (Deferred);
         Info := Get_Info (Deferred);
         Set_Info (El, Info);
      else
         Chap3.Translate_Object_Subtype_Indication (El);
         Info := Add_Info (El, Kind_Object);
         Def := Get_Type (El);
      end if;

      Tinfo := Get_Info (Def);
      Obj_Type := Get_Object_Type (Tinfo, Mode_Value);

      case Get_Kind (El) is
         when Iir_Kind_Variable_Declaration
            | Iir_Kind_Interface_Constant_Declaration =>
            Info.Object_Var :=
              Create_Var (Create_Var_Identifier (El), Obj_Type);
         when Iir_Kind_Constant_Declaration =>
            if Get_Deferred_Declaration (El) /= Null_Iir then
               --  This is a full constant declaration (in a body) of a
               --  deferred constant declaration (in a package).
               Storage := O_Storage_Public;
            else
               Storage := Global_Storage;
            end if;
            if Info.Object_Var = Null_Var then
               --  Not a full constant declaration (ie a value for an
               --   already declared constant).
               --  Must create the declaration.
               if Chap7.Is_Static_Constant (El) then
                  Info.Object_Static := True;
                  Info.Object_Var := Create_Global_Const
                    (Create_Identifier (El), Obj_Type, Global_Storage,
                     O_Cnode_Null);
               else
                  Info.Object_Static := False;
                  Info.Object_Var := Create_Var
                    (Create_Var_Identifier (El), Obj_Type, Global_Storage);
               end if;
            end if;
            if Get_Deferred_Declaration (El) = Null_Iir
              and then Info.Object_Static
              and then Storage /= O_Storage_External
            then
               --  Deferred constant are never considered as locally static.
               --  FIXME: to be improved ?

               --  open_temp/close_temp only required for transient types.
               Open_Temp;
               Define_Global_Const
                 (Info.Object_Var,
                  Chap7.Translate_Static_Expression (Val, Def));
               Close_Temp;
            end if;
         when others =>
            Error_Kind ("create_objet", El);
      end case;
   end Create_Object;

   procedure Create_Signal (Decl : Iir)
   is
      Sig_Type_Def : constant Iir := Get_Type (Decl);
      Type_Info    : Type_Info_Acc;
      Info         : Signal_Info_Acc;
   begin
      Chap3.Translate_Object_Subtype_Indication (Decl);

      Type_Info := Get_Info (Sig_Type_Def);
      Info := Add_Info (Decl, Kind_Signal);

      Info.Signal_Sig := Create_Var
        (Create_Var_Identifier (Decl, "_SIG", 0),
         Get_Object_Type (Type_Info, Mode_Signal));

      if Get_Kind (Decl) = Iir_Kind_Interface_Signal_Declaration then
         --  For interfaces, create a pointer so that there is no need to
         --  update a copy if the association is collapsed.
         Info.Signal_Valp := Create_Var
           (Create_Var_Identifier (Decl, "_VALP", 0),
            Get_Object_Ptr_Type (Type_Info, Mode_Value));

         if Get_Default_Value (Decl) /= Null_Iir then
            --  Default value for ports.
            Info.Signal_Val := Create_Var
              (Create_Var_Identifier (Decl, "_INIT", 0),
               Get_Object_Type (Type_Info, Mode_Value));
         end if;
      else
         Info.Signal_Val := Create_Var
           (Create_Var_Identifier (Decl, "_VAL", 0),
            Get_Object_Type (Type_Info, Mode_Value));
      end if;

      case Get_Kind (Decl) is
         when Iir_Kind_Signal_Declaration
            | Iir_Kind_Interface_Signal_Declaration =>
            Rtis.Generate_Signal_Rti (Decl);
         when Iir_Kind_Guard_Signal_Declaration =>
            --  No name created for guard signal.
            null;
         when others =>
            Error_Kind ("create_signal", Decl);
      end case;
   end Create_Signal;

   procedure Create_Implicit_Signal (Decl : Iir)
   is
      Sig_Type_Def : constant Iir := Get_Type (Decl);
      Type_Info    : constant Type_Info_Acc := Get_Info (Sig_Type_Def);
      Info         : Signal_Info_Acc;
   begin
      --  The type of DECL is already known: either bit, or boolean or the
      --  type of the prefix.

      Info := Add_Info (Decl, Kind_Signal);

      Info.Signal_Sig := Create_Var
        (Create_Uniq_Identifier,
         Get_Object_Type (Type_Info, Mode_Signal));
      Info.Signal_Val := Create_Var
        (Create_Uniq_Identifier,
         Get_Object_Type (Type_Info, Mode_Value));
   end Create_Implicit_Signal;

   procedure Create_File_Object (El : Iir_File_Declaration)
   is
      Obj_Type_Def : constant Iir := Get_Type (El);
      Obj_Type     : constant O_Tnode :=
        Get_Ortho_Type (Obj_Type_Def, Mode_Value);
      Info         : Ortho_Info_Acc;
   begin
      Info := Add_Info (El, Kind_Object);

      Info.Object_Var := Create_Var (Create_Var_Identifier (El), Obj_Type);
   end Create_File_Object;

   procedure Create_Package_Interface (Inter : Iir)
   is
      Pkg      : constant Iir := Get_Uninstantiated_Package_Decl (Inter);
      Pkg_Info : constant Ortho_Info_Acc := Get_Info (Pkg);
      Info     : Ortho_Info_Acc;
   begin
      Info := Add_Info (Inter, Kind_Package_Instance);

      --  The spec
      Info.Package_Instance_Spec_Var :=
        Create_Var (Create_Var_Identifier (Inter, "SPEC", 0),
                    Pkg_Info.Package_Spec_Ptr_Type);
      Set_Scope_Via_Var_Ptr
        (Info.Package_Instance_Spec_Scope,
         Info.Package_Instance_Spec_Var);

      --  The body
      Info.Package_Instance_Body_Var :=
        Create_Var (Create_Var_Identifier (Inter, "BODY", 0),
                    Pkg_Info.Package_Body_Ptr_Type);
      Set_Scope_Via_Var_Ptr
        (Info.Package_Instance_Body_Scope,
         Info.Package_Instance_Body_Var);

      Chap2.Instantiate_Info_Package (Inter);
   end Create_Package_Interface;

   procedure Allocate_Complex_Object (Obj_Type   : Iir;
                                      Alloc_Kind : Allocation_Kind;
                                      Var        : in out Mnode)
   is
      Type_Info : constant Type_Info_Acc := Get_Type_Info (Var);
      Kind      : constant Object_Kind_Type := Get_Object_Kind (Var);
   begin
      --  Cannot allocate unconstrained object (since size is unknown).
      pragma Assert (Type_Info.Type_Mode not in Type_Mode_Unbounded);

      if not Is_Complex_Type (Type_Info) then
         --  Object is not complex.
         return;
      end if;

      --  Allocate variable.
      New_Assign_Stmt
        (M2Lp (Var),
         Gen_Alloc (Alloc_Kind,
                    Chap3.Get_Subtype_Size (Obj_Type, Mnode_Null, Kind),
                    Type_Info.Ortho_Ptr_Type (Kind)));
   end Allocate_Complex_Object;

   --  Note : OBJ can be a tree.
   --  FIXME: should use translate_aggregate_others.
   procedure Init_Array_Object (Obj : Mnode; Obj_Type : Iir)
   is
      --  Type of the object.
      Type_Info : constant Type_Info_Acc := Get_Info (Obj_Type);

      Sobj : Mnode;

      --  Iterator for the elements.
      Index : O_Dnode;

      Upper_Limit : O_Enode;
      Upper_Var   : O_Dnode;

      Label : O_Snode;
   begin
      --  Iterate on all elements of the object.
      Open_Temp;

      if Type_Info.Type_Mode = Type_Mode_Unbounded_Array then
         Sobj := Stabilize (Obj);
      else
         Sobj := Obj;
      end if;

      Upper_Limit := Chap3.Get_Array_Length (Sobj, Obj_Type);
      if Type_Info.Type_Mode = Type_Mode_Static_Array then
         Upper_Var := O_Dnode_Null;
      else
         --  Hoist the computation of the limit before the loop.
         Upper_Var := Create_Temp_Init (Ghdl_Index_Type, Upper_Limit);
      end if;

      Index := Create_Temp (Ghdl_Index_Type);
      Init_Var (Index);
      Start_Loop_Stmt (Label);
      if Upper_Var /= O_Dnode_Null then
         Upper_Limit := New_Obj_Value (Upper_Var);
      end if;
      Gen_Exit_When (Label,
                     New_Compare_Op (ON_Eq,
                                     New_Obj_Value (Index), Upper_Limit,
                                     Ghdl_Bool_Type));
      Init_Object
        (Chap6.Translate_Indexed_Name_By_Offset (Sobj, Obj_Type, Index),
         Get_Element_Subtype (Obj_Type));
      Inc_Var (Index);
      Finish_Loop_Stmt (Label);

      Close_Temp;
   end Init_Array_Object;

   procedure Init_Protected_Object (Obj : Mnode; Obj_Type : Iir)
   is
      Info  : constant Type_Info_Acc := Get_Info (Obj_Type);
      Assoc : O_Assoc_List;
   begin
      --  Call the initializer.
      Start_Association (Assoc, Info.B.Prot_Init_Subprg);
      Subprgs.Add_Subprg_Instance_Assoc (Assoc, Info.B.Prot_Init_Instance);
      --  Use of M2Lp is a little bit fragile (not sure we get the
      --  variable, but should work: we didn't stabilize it).
      New_Assign_Stmt (M2Lp (Obj), New_Function_Call (Assoc));
   end Init_Protected_Object;

   procedure Fini_Protected_Object (Decl : Iir)
   is
      Info  : constant Type_Info_Acc := Get_Info (Get_Type (Decl));
      Obj   : Mnode;
      Assoc : O_Assoc_List;
   begin
      Obj := Chap6.Translate_Name (Decl, Mode_Value);
      --  Call the Finalizator.
      Start_Association (Assoc, Info.B.Prot_Final_Subprg);
      New_Association (Assoc, M2E (Obj));
      New_Procedure_Call (Assoc);
   end Fini_Protected_Object;

   function Get_Scalar_Initial_Value (Atype : Iir) return O_Enode
   is
      Tinfo : constant Type_Info_Acc := Get_Info (Atype);
   begin
      case Tinfo.Type_Mode is
         when Type_Mode_Scalar =>
            return Chap14.Translate_Left_Type_Attribute (Atype);
         when Type_Mode_Acc
           | Type_Mode_Bounds_Acc =>
            return New_Lit (New_Null_Access (Tinfo.Ortho_Type (Mode_Value)));
         when others =>
            Error_Kind ("get_scalar_initial_value", Atype);
      end case;
   end Get_Scalar_Initial_Value;

   procedure Init_Object (Obj : Mnode; Obj_Type : Iir)
   is
      Tinfo : constant Type_Info_Acc := Get_Type_Info (Obj);
   begin
      case Tinfo.Type_Mode is
         when Type_Mode_Scalar
           | Type_Mode_Acc
           | Type_Mode_Bounds_Acc =>
            New_Assign_Stmt (M2Lv (Obj), Get_Scalar_Initial_Value (Obj_Type));
         when Type_Mode_Arrays =>
            Init_Array_Object (Obj, Obj_Type);
         when Type_Mode_Records =>
            declare
               List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Obj_Type);
               Sobj : Mnode;
               El   : Iir_Element_Declaration;
            begin
               Open_Temp;
               Sobj := Stabilize (Obj);
               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  Init_Object (Chap6.Translate_Selected_Element (Sobj, El),
                               Get_Type (El));
               end loop;
               Close_Temp;
            end;
         when Type_Mode_Protected =>
            Init_Protected_Object (Obj, Obj_Type);
         when Type_Mode_Unknown
            | Type_Mode_File =>
            raise Internal_Error;
      end case;
   end Init_Object;

   --  Return True iff subtype indication of DECL is a subtype attribute.
   function Is_Object_Subtype_Attribute (Decl : Iir) return Boolean
   is
      Ind : constant Iir := Get_Subtype_Indication (Decl);
   begin
      return Ind /= Null_Iir
        and then Get_Kind (Ind) = Iir_Kind_Subtype_Attribute;
   end Is_Object_Subtype_Attribute;

   procedure Elab_Subtype_Attribute
     (Decl : Iir; Name_Val : Mnode; Name_Sig : Mnode)
   is
      Ind : constant Iir := Get_Subtype_Indication (Decl);
      Name : Mnode;
      Bnd : Mnode;
   begin
      Name := Chap6.Translate_Name (Get_Prefix (Ind), Mode_Value);
      Bnd := Chap3.Get_Composite_Bounds (Name);

      if Name_Sig /= Mnode_Null then
         Stabilize (Bnd);
         New_Assign_Stmt (M2Lp (Chap3.Get_Composite_Bounds (Name_Sig)),
                          M2Addr (Bnd));
      end if;
      New_Assign_Stmt (M2Lp (Chap3.Get_Composite_Bounds (Name_Val)),
                       M2Addr (Bnd));
   end Elab_Subtype_Attribute;

   procedure Elab_Maybe_Subtype_Attribute
     (Decl : Iir; Name_Val : Mnode; Name_Sig : Mnode) is
   begin
      if not Is_Object_Subtype_Attribute (Decl) then
         return;
      end if;

      Elab_Subtype_Attribute (Decl, Name_Val, Name_Sig);
   end Elab_Maybe_Subtype_Attribute;

   --  If SIZE is larger than the threshold, call __ghdl_check_stack_allocation
   --  to raise an error if the size is too large.  There are two threshold:
   --  one set at compile time (Check_Stack_Allocation_Threshold) and one set
   --  at run-time.
   --
   --  Right now, this function is called only for allocation of a complex
   --  object on the stack (constant or variable).  But there are more sources
   --  of stack allocation (temporary aggregate, unbounded objects, individual
   --  assocs...)
   function Maybe_Check_Stack_Allocation (Size : O_Enode) return O_Enode
   is
      Val : O_Dnode;
      If_Blk : O_If_Block;
      Assoc : O_Assoc_List;
   begin
      if Flag_Check_Stack_Allocation = 0 then
         return Size;
      end if;

      Val := Create_Temp_Init (Ghdl_Index_Type, Size);
      Start_If_Stmt
        (If_Blk,
         New_Compare_Op (ON_Ge,
                         New_Obj_Value (Val),
                         New_Lit (Check_Stack_Allocation_Threshold),
                         Ghdl_Bool_Type));
      Start_Association (Assoc, Ghdl_Check_Stack_Allocation);
      New_Association (Assoc, New_Obj_Value (Val));
      New_Procedure_Call (Assoc);
      Finish_If_Stmt (If_Blk);

      return New_Obj_Value (Val);
   end Maybe_Check_Stack_Allocation;

   procedure Elab_Object_Storage (Obj : Iir)
   is
      Obj_Type : constant Iir := Get_Type (Obj);
      Obj_Info : constant Object_Info_Acc := Get_Info (Obj);

      Name_Node : Mnode;

      Type_Info  : Type_Info_Acc;
      Alloc_Kind : Allocation_Kind;
      Size : O_Enode;
   begin
      --  Elaborate subtype.
      case Get_Kind (Obj) is
         when Iir_Kind_Attribute_Value =>
            null;
         when others =>
            if Is_Object_Subtype_Attribute (Obj) then
               Type_Info := Get_Info (Obj_Type);
               if Type_Info.Type_Mode in Type_Mode_Unbounded then
                  --  Copy bounds and allocate base.
                  Name_Node :=
                    Get_Var (Obj_Info.Object_Var, Type_Info, Mode_Value);
                  Stabilize (Name_Node);
                  Elab_Maybe_Subtype_Attribute (Obj, Name_Node, Mnode_Null);
                  Alloc_Kind := Get_Alloc_Kind_For_Var (Obj_Info.Object_Var);
                  Chap3.Allocate_Unbounded_Composite_Base
                    (Alloc_Kind, Name_Node, Get_Base_Type (Obj_Type));
               return;
               end if;
            else
               Chap3.Elab_Object_Subtype_Indication (Obj);
            end if;
      end case;

      --  Now the subtype is elaborated, its info is defined.
      Type_Info := Get_Info (Obj_Type);

      --  FIXME: the object type may be a fat array!
      --  FIXME: fat array + aggregate ?

      if Type_Info.Type_Mode = Type_Mode_Protected then
         --  Protected object will be created by its INIT function.
         null;
      elsif Is_Unbounded_Type (Type_Info) then
         --  Allocated during initialization.
         null;
      elsif Is_Complex_Type (Type_Info) then
         --  FIXME: avoid allocation if the value is a string and
         --  the object is a constant
         Name_Node := Get_Var (Obj_Info.Object_Var, Type_Info, Mode_Value);
         Alloc_Kind := Get_Alloc_Kind_For_Var (Obj_Info.Object_Var);
         Size := Chap3.Get_Subtype_Size (Obj_Type, Mnode_Null, Mode_Value);
         if Alloc_Kind = Alloc_Stack then
            Size := Maybe_Check_Stack_Allocation (Size);
         end if;
         --  Was: Allocate_Complex_Object.
         New_Assign_Stmt
           (M2Lp (Name_Node),
            Gen_Alloc (Alloc_Kind,
                       Size,
                       Type_Info.Ortho_Ptr_Type (Mode_Value)));
      end if;
   end Elab_Object_Storage;

   --  Generate code to create object OBJ and initialize it with value VAL.
   procedure Elab_Object_Init
     (Name : Mnode; Obj : Iir; Value : Iir; Alloc_Kind : Allocation_Kind)
   is
      Obj_Type  : constant Iir := Get_Type (Obj);
      Type_Info : constant Type_Info_Acc := Get_Info (Obj_Type);

      Name_Node  : Mnode;
      Value_Node : O_Enode;
   begin
      --  Note: no temporary variable region is created, as the allocation
      --  may be performed on the stack.

      if Value = Null_Iir then
         --  Performs default initialization.
         Open_Temp;
         Init_Object (Name, Obj_Type);
         Close_Temp;
      elsif Get_Kind (Value) = Iir_Kind_Aggregate then
         if Type_Info.Type_Mode in Type_Mode_Unbounded
           and then not Is_Object_Subtype_Attribute (Obj)
         then
            --  Allocate.
            declare
               Aggr_Type : constant Iir := Get_Type (Value);
               Aggr_Base_Type : constant Iir := Get_Base_Type (Aggr_Type);
            begin
               Name_Node := Stabilize (Name);
               pragma Assert (Get_Object_Kind (Name_Node) = Mode_Value);
               if Get_Constraint_State (Aggr_Type) /= Fully_Constrained then
                  --  Allocate bounds
                  Chap3.Allocate_Unbounded_Composite_Bounds
                    (Alloc_Kind, Name_Node, Aggr_Base_Type);
                  --  Translate bounds
                  Chap7.Translate_Aggregate_Bounds
                    (Stabilize (Chap3.Get_Composite_Bounds (Name_Node)),
                     Value, Mode_Value);
                  --  Allocate base
                  Chap3.Allocate_Unbounded_Composite_Base
                    (Alloc_Kind, Name_Node, Aggr_Base_Type);
               else
                  Chap3.Create_Composite_Subtype (Aggr_Type);
                  if Alloc_Kind = Alloc_Stack then
                     --  Short-cut: don't allocate bounds.
                     New_Assign_Stmt
                       (M2Lp (Chap3.Get_Composite_Bounds (Name_Node)),
                        M2Addr (Chap3.Get_Composite_Type_Bounds (Aggr_Type)));
                     Chap3.Allocate_Unbounded_Composite_Base
                       (Alloc_Kind, Name_Node, Aggr_Base_Type);
                  else
                     Chap3.Translate_Object_Allocation
                       (Name_Node, Alloc_Kind, Aggr_Base_Type,
                        Chap3.Get_Composite_Type_Bounds (Aggr_Type));
                  end if;
               end if;
            end;
         else
            Name_Node := Name;
         end if;
         Chap7.Translate_Aggregate (Name_Node, Obj_Type, Value);
      else
         Value_Node := Chap7.Translate_Expression (Value, Obj_Type);

         if Type_Info.Type_Mode in Type_Mode_Unbounded then
            declare
               S : Mnode;
            begin
               Name_Node := Stabilize (Name);
               S := Stabilize (E2M (Value_Node, Type_Info, Mode_Value));

               if Get_Kind (Value) = Iir_Kind_String_Literal8
                 and then Get_Kind (Obj) = Iir_Kind_Constant_Declaration
               then
                  --  No need to allocate space for the object.
                  Copy_Fat_Pointer (Name_Node, S);
               else
                  Chap3.Translate_Object_Allocation
                    (Name_Node, Alloc_Kind, Obj_Type,
                     Chap3.Get_Composite_Bounds (S));
                  Chap3.Translate_Object_Copy (Name_Node, S, Obj_Type);
               end if;
            end;
         else
            Chap7.Translate_Assign (Name, Value_Node, Value, Obj_Type, Obj);
         end if;
      end if;
   end Elab_Object_Init;

   --  Generate code to create object OBJ and initialize it with value VAL.
   procedure Elab_Object_Value (Obj : Iir; Value : Iir)
   is
      Obj_Info  : constant Object_Info_Acc := Get_Info (Obj);
      Alloc_Kind : constant Allocation_Kind :=
        Get_Alloc_Kind_For_Var (Obj_Info.Object_Var);
      Name : constant Mnode :=
        Get_Var (Obj_Info.Object_Var, Get_Info (Get_Type (Obj)), Mode_Value);
   begin
      Elab_Object_Storage (Obj);
      Elab_Object_Init (Name, Obj, Value, Alloc_Kind);

      if Alloc_Kind = Alloc_Return then
         --  If the object is allocated on the return stack, avoid
         --  deallocation.  Deallocation must be done manually (this concerns
         --  procedures with suspension).
         Disable_Stack2_Release;
      end if;
   end Elab_Object_Value;

   --  Create code to elaborate OBJ.
   procedure Elab_Object (Obj : Iir)
   is
      Value : constant Iir := Get_Default_Value (Obj);
      Obj1  : Iir;
   begin
      --  Set default value.
      if Get_Kind (Obj) = Iir_Kind_Constant_Declaration then
         if Get_Deferred_Declaration_Flag (Obj) then
            --  No code generation for a deferred constant.
            return;
         end if;

         if Get_Kind (Value) = Iir_Kind_Overflow_Literal then
            --  An overflow can be static, but must still generate an error
            --  at run time.
            Chap6.Gen_Bound_Error (Obj);
            return;
         end if;

         if Get_Info (Obj).Object_Static then
            --  A static object is pre-initialized.
            return;
         end if;

         Obj1 := Get_Deferred_Declaration (Obj);
         if Obj1 = Null_Iir then
            Obj1 := Obj;
         end if;
      else
         Obj1 := Obj;
      end if;

      New_Debug_Line_Stmt (Get_Line_Number (Obj));

      --  Still use the default value of the not deferred constant.
      --  FIXME: what about composite types.
      Elab_Object_Value (Obj1, Value);
   end Elab_Object;

   procedure Fini_Object (Obj : Iir)
   is
      Obj_Type  : constant Iir := Get_Type (Obj);
      Type_Info : constant Type_Info_Acc := Get_Info (Obj_Type);
   begin
      case Type_Mode_Valid (Type_Info.Type_Mode) is
         when Type_Mode_Unbounded =>
            declare
               V : Mnode;
            begin
               Open_Temp;
               V := Chap6.Translate_Name (Obj, Mode_Value);
               Stabilize (V);
               Chap3.Gen_Deallocate
                 (New_Value (M2Lp (Chap3.Get_Composite_Bounds (V))));
               Chap3.Gen_Deallocate
                 (New_Value (M2Lp (Chap3.Get_Composite_Base (V))));
               Close_Temp;
            end;
         when Type_Mode_Complex_Array
           | Type_Mode_Complex_Record
           | Type_Mode_Protected =>
            Chap3.Gen_Deallocate
              (New_Value (M2Lp (Chap6.Translate_Name (Obj, Mode_Value))));
         when Type_Mode_Scalar
           | Type_Mode_Static_Record
           | Type_Mode_Static_Array
           | Type_Mode_Acc
           | Type_Mode_Bounds_Acc =>
            null;
         when Type_Mode_File =>
            --  FIXME: free file ?
            null;
      end case;
   end Fini_Object;

   function Get_Nbr_Signals (Sig : Mnode; Sig_Type : Iir) return O_Enode
   is
      Info : constant Type_Info_Acc := Get_Info (Sig_Type);
   begin
      case Info.Type_Mode is
         when Type_Mode_Scalar =>
            --  Note: here we discard SIG...
            return New_Lit (Ghdl_Index_1);
         when Type_Mode_Arrays =>
            declare
               Len    : O_Dnode;
               If_Blk : O_If_Block;
               Ssig   : Mnode;
            begin
               Ssig := Stabilize (Sig);
               Len := Create_Temp_Init
                 (Ghdl_Index_Type,
                  Chap3.Get_Array_Length (Ssig, Sig_Type));
               --  Can dereference the first index only if the array is not a
               --  null array.
               Start_If_Stmt (If_Blk,
                              New_Compare_Op (ON_Neq,
                                              New_Obj_Value (Len),
                                              New_Lit (Ghdl_Index_0),
                                              Ghdl_Bool_Type));
               New_Assign_Stmt
                 (New_Obj (Len),
                  New_Dyadic_Op
                    (ON_Mul_Ov,
                     New_Obj_Value (Len),
                     Get_Nbr_Signals
                       (Chap3.Index_Base
                            (Chap3.Get_Composite_Base (Ssig), Sig_Type,
                             New_Lit (Ghdl_Index_0)),
                        Get_Element_Subtype (Sig_Type))));
               Finish_If_Stmt (If_Blk);

               return New_Obj_Value (Len);
            end;
         when Type_Mode_Records =>
            declare
               List   : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Get_Base_Type (Sig_Type));
               El     : Iir;
               Res    : O_Enode;
               E      : O_Enode;
               Sig_El : Mnode;
               Ssig   : Mnode;
            begin
               Ssig := Stabilize (Sig);
               Res := O_Enode_Null;
               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  Sig_El := Chap6.Translate_Selected_Element (Ssig, El);
                  E := Get_Nbr_Signals (Sig_El, Get_Type (El));
                  if Res /= O_Enode_Null then
                     Res := New_Dyadic_Op (ON_Add_Ov, Res, E);
                  else
                     Res := E;
                  end if;
               end loop;
               if Res = O_Enode_Null then
                  --  Empty records.
                  Res := New_Lit (Ghdl_Index_0);
               end if;
               return Res;
            end;
         when Type_Mode_Unknown
            | Type_Mode_File
            | Type_Mode_Acc
            | Type_Mode_Bounds_Acc
            | Type_Mode_Protected =>
            raise Internal_Error;
      end case;
   end Get_Nbr_Signals;

   --  Get the leftest signal of SIG.
   --  The leftest signal of
   --   a scalar signal is itself,
   --   an array signal is the leftest,
   --   a record signal is the first element.
   function Get_Leftest_Signal (Sig: Mnode; Sig_Type : Iir)
                                   return Mnode
   is
      Res      : Mnode;
      Res_Type : Iir;
      Info     : Type_Info_Acc;
   begin
      Res := Sig;
      Res_Type := Sig_Type;
      loop
         Info := Get_Type_Info (Res);
         case Info.Type_Mode is
            when Type_Mode_Scalar =>
               return Res;
            when Type_Mode_Arrays =>
               Res := Chap3.Index_Base
                 (Chap3.Get_Composite_Base (Res), Res_Type,
                  New_Lit (Ghdl_Index_0));
               Res_Type := Get_Element_Subtype (Res_Type);
            when Type_Mode_Records =>
               declare
                  El_List : constant Iir_Flist :=
                    Get_Elements_Declaration_List (Get_Base_Type (Res_Type));
                  Element : constant Iir := Get_Nth_Element (El_List, 0);
               begin
                  Res := Chap6.Translate_Selected_Element (Res, Element);
                  Res_Type := Get_Type (Element);
               end;
            when Type_Mode_Unknown
               | Type_Mode_File
               | Type_Mode_Acc
               | Type_Mode_Bounds_Acc
               | Type_Mode_Protected =>
               raise Internal_Error;
         end case;
      end loop;
   end Get_Leftest_Signal;

   --  Add func and instance.
   procedure Add_Associations_For_Resolver
     (Assoc : in out O_Assoc_List; Func_Decl : Iir)
   is
      Func_Info   : constant Subprg_Info_Acc := Get_Info (Func_Decl);
      Resolv_Info : constant Subprg_Resolv_Info_Acc :=
        Func_Info.Subprg_Resolv;
      Val         : O_Enode;
   begin
      New_Association
        (Assoc, New_Lit (New_Subprogram_Address (Resolv_Info.Resolv_Func,
         Ghdl_Ptr_Type)));
      if Subprgs.Has_Subprg_Instance (Resolv_Info.Var_Instance) then
         Val := New_Convert_Ov
           (Subprgs.Get_Subprg_Instance (Resolv_Info.Var_Instance),
            Ghdl_Ptr_Type);
      else
         Val := New_Lit (New_Null_Access (Ghdl_Ptr_Type));
      end if;
      New_Association (Assoc, Val);
   end Add_Associations_For_Resolver;

   type O_If_Block_Acc is access O_If_Block;

   type Elab_Signal_Data is record
      Value : Mnode;
      --  Default value of the signal.
      Init_Val         : Mnode;
      --  If statement for a block of signals.
      If_Stmt          : O_If_Block_Acc;
      --  True if the default value is set.
      Has_Val          : Boolean;
      --  True if a resolution function was already attached.
      Already_Resolved : Boolean;
      --  True if the signal may already have been created.
      Check_Null       : Boolean;
   end record;

   procedure Elab_Signal_Non_Composite (Targ      : Mnode;
                                        Targ_Type : Iir;
                                        Data      : Elab_Signal_Data)
   is
      Type_Info     : constant Type_Info_Acc := Get_Info (Targ_Type);
      Create_Subprg : O_Dnode;
      Res           : O_Enode;
      Assoc         : O_Assoc_List;
      Init_Val      : O_Enode;
      --  For the resolution function (if any).
      Func          : Iir;
      If_Stmt       : O_If_Block;
      Targ_Ptr      : O_Dnode;
      Value         : Mnode;
   begin
      if Data.Check_Null then
         Targ_Ptr := Create_Temp_Init
           (Ghdl_Signal_Ptr_Ptr,
            New_Unchecked_Address (M2Lv (Targ), Ghdl_Signal_Ptr_Ptr));
         Start_If_Stmt
           (If_Stmt,
            New_Compare_Op (ON_Eq,
                            New_Value (New_Acc_Value (New_Obj (Targ_Ptr))),
                            New_Lit (New_Null_Access (Ghdl_Signal_Ptr)),
                            Ghdl_Bool_Type));
      end if;

      --  Set the value.
      Value := Stabilize (Data.Value);
      if Data.Has_Val then
         Init_Val := M2E (Data.Init_Val);
      else
         Init_Val := Get_Scalar_Initial_Value (Targ_Type);
      end if;
      New_Assign_Stmt (M2Lv (Value), Init_Val);

      --  Create the signal.
      case Type_Mode_Scalar (Type_Info.Type_Mode) is
         when Type_Mode_Scalar (Type_Mode_B1) =>
            Create_Subprg := Ghdl_Create_Signal_B1;
         when Type_Mode_E8 =>
            Create_Subprg := Ghdl_Create_Signal_E8;
         when Type_Mode_E32 =>
            Create_Subprg := Ghdl_Create_Signal_E32;
         when Type_Mode_I32
            | Type_Mode_P32 =>
            Create_Subprg := Ghdl_Create_Signal_I32;
         when Type_Mode_P64
            | Type_Mode_I64 =>
            Create_Subprg := Ghdl_Create_Signal_I64;
         when Type_Mode_F64 =>
            Create_Subprg := Ghdl_Create_Signal_F64;
      end case;

      Start_Association (Assoc, Create_Subprg);
      New_Association
        (Assoc, New_Unchecked_Address (M2Lv (Value), Ghdl_Ptr_Type));

      if Get_Kind (Targ_Type) in Iir_Kinds_Subtype_Definition then
         Func := Has_Resolution_Function (Targ_Type);
      else
         Func := Null_Iir;
      end if;
      if Func /= Null_Iir and then not Data.Already_Resolved then
         Add_Associations_For_Resolver (Assoc, Func);
      else
         New_Association (Assoc, New_Lit (New_Null_Access (Ghdl_Ptr_Type)));
         New_Association (Assoc, New_Lit (New_Null_Access (Ghdl_Ptr_Type)));
      end if;

      Res := New_Function_Call (Assoc);

      if Data.Check_Null then
         New_Assign_Stmt (New_Acc_Value (New_Obj (Targ_Ptr)), Res);
         Finish_If_Stmt (If_Stmt);
      else
         New_Assign_Stmt
           (M2Lv (Targ),
            New_Convert_Ov (Res, Type_Info.Ortho_Type (Mode_Signal)));
      end if;
   end Elab_Signal_Non_Composite;

   function Elab_Signal_Prepare_Composite
     (Targ : Mnode; Targ_Type : Iir; Data : Elab_Signal_Data)
         return Elab_Signal_Data
   is
      Assoc : O_Assoc_List;
      Func  : Iir;
      Res   : Elab_Signal_Data;
   begin
      Res := Data;
      if Get_Kind (Targ_Type) in Iir_Kinds_Subtype_Definition then
         Func := Has_Resolution_Function (Targ_Type);
         if Func /= Null_Iir and then not Data.Already_Resolved then
            if Data.Check_Null then
               Res.If_Stmt := new O_If_Block;
               Start_If_Stmt
                 (Res.If_Stmt.all,
                  New_Compare_Op
                    (ON_Eq,
                     New_Convert_Ov (M2E (Get_Leftest_Signal (Targ,
                                                              Targ_Type)),
                                     Ghdl_Signal_Ptr),
                     New_Lit (New_Null_Access (Ghdl_Signal_Ptr)),
                     Ghdl_Bool_Type));
               --Res.Check_Null := False;
            end if;
            --  Add resolver.
            Start_Association (Assoc, Ghdl_Signal_Create_Resolution);
            Add_Associations_For_Resolver (Assoc, Func);
            New_Association
              (Assoc, New_Convert_Ov (M2Addr (Targ), Ghdl_Ptr_Type));
            New_Association (Assoc, Get_Nbr_Signals (Targ, Targ_Type));
            New_Procedure_Call (Assoc);
            Res.Already_Resolved := True;
         end if;
      end if;
      case Get_Info (Targ_Type).Type_Mode is
         when Type_Mode_Records =>
            Res.Value := Stabilize (Data.Value);
            if Data.Has_Val then
               Res.Init_Val := Stabilize (Data.Init_Val);
            end if;
         when Type_Mode_Arrays =>
            Res.Value := Chap3.Get_Composite_Base (Data.Value);
            if Data.Has_Val then
               Res.Init_Val := Chap3.Get_Composite_Base (Data.Init_Val);
            end if;
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Elab_Signal_Prepare_Composite;

   procedure Elab_Signal_Finish_Composite (Data : in out Elab_Signal_Data)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => O_If_Block, Name => O_If_Block_Acc);
   begin
      if Data.If_Stmt /= null then
         Finish_If_Stmt (Data.If_Stmt.all);
         Free (Data.If_Stmt);
      end if;
   end Elab_Signal_Finish_Composite;

   function Elab_Signal_Update_Array
     (Data : Elab_Signal_Data; Targ_Type : Iir; Index : O_Dnode)
     return Elab_Signal_Data
   is
      N_Init_Val : Mnode;
   begin
      if Data.Has_Val then
         N_Init_Val := Chap3.Index_Base (Data.Init_Val, Targ_Type,
                                         New_Obj_Value (Index));
      else
         N_Init_Val := Mnode_Null;
      end if;
      return Elab_Signal_Data'
        (Value => Chap3.Index_Base (Data.Value, Targ_Type,
                                    New_Obj_Value (Index)),
         Init_Val => N_Init_Val,
         Has_Val => Data.Has_Val,
         If_Stmt => null,
         Already_Resolved => Data.Already_Resolved,
         Check_Null => Data.Check_Null);
   end Elab_Signal_Update_Array;

   function Elab_Signal_Update_Record
     (Data : Elab_Signal_Data; Targ_Type : Iir; El : Iir_Element_Declaration)
     return Elab_Signal_Data
   is
      pragma Unreferenced (Targ_Type);
      N_Init_Val : Mnode;
   begin
      if Data.Has_Val then
         N_Init_Val := Chap6.Translate_Selected_Element (Data.Init_Val, El);
      else
         N_Init_Val := Mnode_Null;
      end if;
      return Elab_Signal_Data'
        (Value => Chap6.Translate_Selected_Element (Data.Value, El),
         Init_Val => N_Init_Val,
         Has_Val => Data.Has_Val,
         If_Stmt => null,
         Already_Resolved => Data.Already_Resolved,
         Check_Null => Data.Check_Null);
   end Elab_Signal_Update_Record;

   procedure Elab_Signal is new Foreach_Non_Composite
     (Data_Type => Elab_Signal_Data,
      Composite_Data_Type => Elab_Signal_Data,
      Do_Non_Composite => Elab_Signal_Non_Composite,
      Prepare_Data_Array => Elab_Signal_Prepare_Composite,
      Update_Data_Array => Elab_Signal_Update_Array,
      Finish_Data_Array => Elab_Signal_Finish_Composite,
      Prepare_Data_Record => Elab_Signal_Prepare_Composite,
      Update_Data_Record => Elab_Signal_Update_Record,
      Finish_Data_Record => Elab_Signal_Finish_Composite);

   --  Elaborate signal subtypes and allocate the storage for the object.
   procedure Elab_Signal_Declaration_Storage (Decl : Iir; Has_Copy : Boolean)
   is
      Is_Port : constant Boolean :=
        Get_Kind (Decl) = Iir_Kind_Interface_Signal_Declaration;
      Sig_Type  : constant Iir := Get_Type (Decl);
      Type_Info : Type_Info_Acc;
      Name_Sig : Mnode;
      Name_Val : Mnode;
   begin
      New_Debug_Line_Stmt (Get_Line_Number (Decl));

      Open_Temp;

      Chap3.Elab_Object_Subtype_Indication (Decl);

      Type_Info := Get_Info (Sig_Type);

      if Type_Info.Type_Mode in Type_Mode_Unbounded then
         --  Allocate storage.
         if Has_Copy then
            Name_Sig := Chap6.Translate_Name (Decl, Mode_Signal);
            Name_Val := Mnode_Null;
         else
            Chap6.Translate_Signal_Name (Decl, Name_Sig, Name_Val);
         end if;

         Name_Sig := Stabilize (Name_Sig);

         if Name_Val /= Mnode_Null then
            Name_Val := Stabilize (Name_Val);
            Elab_Maybe_Subtype_Attribute (Decl, Name_Val, Name_Sig);
            Chap3.Allocate_Unbounded_Composite_Base
              (Alloc_System, Name_Val, Sig_Type);
         else
            Elab_Maybe_Subtype_Attribute (Decl, Name_Sig, Mnode_Null);
         end if;

         Chap3.Allocate_Unbounded_Composite_Base
           (Alloc_System, Name_Sig, Sig_Type);

         if Is_Port and then Get_Default_Value (Decl) /= Null_Iir then
            Name_Val := Chap6.Get_Port_Init_Value (Decl);
            Name_Val := Stabilize (Name_Val);
            Chap3.Allocate_Unbounded_Composite_Base
              (Alloc_System, Name_Val, Sig_Type);
         end if;
      elsif Is_Complex_Type (Type_Info) then
         if Has_Copy then
            Name_Sig := Chap6.Translate_Name (Decl, Mode_Signal);
            Name_Val := Mnode_Null;
         else
            Chap6.Translate_Signal_Name (Decl, Name_Sig, Name_Val);
         end if;
         Allocate_Complex_Object (Sig_Type, Alloc_System, Name_Sig);
         if Name_Val /= Mnode_Null then
            Allocate_Complex_Object (Sig_Type, Alloc_System, Name_Val);
         end if;
         if Is_Port and then Get_Default_Value (Decl) /= Null_Iir then
            Name_Val := Chap6.Get_Port_Init_Value (Decl);
            Allocate_Complex_Object (Sig_Type, Alloc_System, Name_Val);
         end if;
      elsif Is_Port then
         if not Has_Copy then
            --  A port that isn't collapsed.  Allocate value.
            Name_Val := Chap6.Translate_Name (Decl, Mode_Value);
            New_Assign_Stmt
              (M2Lp (Name_Val),
               Gen_Alloc (Alloc_System,
                          Chap3.Get_Object_Size (Name_Val, Sig_Type),
                          Type_Info.Ortho_Ptr_Type (Mode_Value)));
         end if;
      end if;

      Close_Temp;
   end Elab_Signal_Declaration_Storage;

   function Has_Direct_Driver (Sig : Iir) return Boolean
   is
      Info : constant Ortho_Info_Acc := Get_Info (Get_Object_Prefix (Sig));
   begin
      --  Can be an alias ?
      return Info.Kind = Kind_Signal
        and then Info.Signal_Driver /= Null_Var;
   end Has_Direct_Driver;

   procedure Elab_Direct_Driver_Declaration_Storage (Decl : Iir)
   is
      Sig_Type  : constant Iir := Get_Type (Decl);
      Sig_Info  : constant Ortho_Info_Acc := Get_Info (Decl);
      Type_Info : constant Type_Info_Acc := Get_Info (Sig_Type);
      Name_Node : Mnode;
   begin
      Open_Temp;

      if Type_Info.Type_Mode in Type_Mode_Unbounded then
         Name_Node := Get_Var (Sig_Info.Signal_Driver, Type_Info, Mode_Value);
         Name_Node := Stabilize (Name_Node);
         --  Copy bounds from signal.
         New_Assign_Stmt
           (M2Lp (Chap3.Get_Composite_Bounds (Name_Node)),
            M2Addr (Chap3.Get_Composite_Bounds
                      (Chap6.Translate_Name (Decl, Mode_Signal))));
         --  Allocate base.
         Chap3.Allocate_Unbounded_Composite_Base
           (Alloc_System, Name_Node, Sig_Type);
      elsif Is_Complex_Type (Type_Info) then
         Name_Node := Get_Var (Sig_Info.Signal_Driver, Type_Info, Mode_Value);
         Allocate_Complex_Object (Sig_Type, Alloc_System, Name_Node);
      end if;

      Close_Temp;
   end Elab_Direct_Driver_Declaration_Storage;

   --  Create signal object.
   --  Note: SIG can be a signal sub-element (used when signals are
   --   collapsed).
   --  If CHECK_NULL is TRUE, create the signal only if it was not yet
   --  created.
   procedure Elab_Signal_Declaration_Object
     (Sig : Iir; Parent : Iir; Check_Null : Boolean)
   is
      Decl      : constant Iir := Strip_Denoting_Name (Sig);
      Sig_Type  : constant Iir := Get_Type (Sig);
      Base_Decl : constant Iir := Get_Object_Prefix (Sig);
      Val_Type  : Iir;
      Name_Sig  : Mnode;
      Name_Val  : Mnode;
      Value     : Iir;
      Data      : Elab_Signal_Data;
   begin
      New_Debug_Line_Stmt (Get_Line_Number (Sig));

      Open_Temp;

      --  Set the name of the signal.
      declare
         Assoc : O_Assoc_List;
      begin
         Start_Association (Assoc, Ghdl_Signal_Name_Rti);
         New_Association
           (Assoc,
            New_Unchecked_Address (New_Obj (Get_Info (Base_Decl).Signal_Rti),
                                   Rtis.Ghdl_Rti_Access));
         Rtis.Associate_Rti_Context (Assoc, Parent);
         New_Procedure_Call (Assoc);
      end;

      Chap6.Translate_Signal_Name (Decl, Name_Sig, Name_Val);
      --  Consistency check: a signal name is a signal.
      pragma Assert (Get_Object_Kind (Name_Sig) = Mode_Signal);

      Data.Value := Name_Val;
      if Decl = Base_Decl then
         Data.Already_Resolved := False;
         Data.Check_Null := Check_Null;
         Value := Get_Default_Value (Base_Decl);
         if Value = Null_Iir then
            Data.Has_Val := False;
         else
            Data.Has_Val := True;
            Val_Type := Get_Type (Value);

            if Get_Kind (Value) = Iir_Kind_Aggregate
              and then Get_Constraint_State (Sig_Type) /= Fully_Constrained
              and then Get_Constraint_State (Val_Type) /= Fully_Constrained
            then
               --  Both the signal type and the value type are not fully
               --  constrained.  This can happend when the signal subtype
               --  indication is 'subtype and the default value is an
               --  aggregate.  The signal was created with bounds, so use
               --  those bounds.
               declare
                  Tinfo : constant Type_Info_Acc := Get_Info (Sig_Type);
                  V : Mnode;
               begin
                  Stabilize (Data.Value);
                  V := Create_Temp (Tinfo);
                  New_Assign_Stmt
                    (M2Lp (Chap3.Get_Composite_Bounds (V)),
                     M2Addr (Chap3.Get_Composite_Bounds (Data.Value)));
                  pragma Assert (Val_Type = Sig_Type);
                  Chap3.Allocate_Unbounded_Composite_Base
                    (Alloc_Stack, V, Sig_Type);
                  Chap7.Translate_Aggregate (V, Val_Type, Value);
                  Data.Init_Val := V;
               end;
            else
               Data.Init_Val := Chap7.Translate_Expression (Value, Sig_Type);
            end if;
         end if;
      else
         --  Sub signal.
         --  Do not add resolver.
         --  Do not use default value.
         Data.Already_Resolved := True;
         Data.Has_Val := False;
         Data.Check_Null := False;
         Value := Null_Iir;
      end if;
      Elab_Signal (Name_Sig, Sig_Type, Data);

      Close_Temp;

      if Value /= Null_Iir then
         Chap9.Destroy_Types (Value);
      end if;
   end Elab_Signal_Declaration_Object;

   procedure Elab_Signal_Declaration
     (Decl : Iir; Parent : Iir; Check_Null : Boolean)
   is
   begin
      Elab_Signal_Declaration_Storage (Decl, False);
      Elab_Signal_Declaration_Object (Decl, Parent, Check_Null);
   end Elab_Signal_Declaration;

   procedure Elab_Signal_Attribute (Decl : Iir)
   is
      Info        : constant Signal_Info_Acc := Get_Info (Decl);
      Dtype       : constant Iir := Get_Type (Decl);
      Type_Info   : constant Type_Info_Acc := Get_Info (Dtype);
      Assoc       : O_Assoc_List;
      Prefix      : Iir;
      Prefix_Node : Mnode;
      Res         : O_Enode;
      Val         : O_Enode;
      Param       : Iir;
      Subprg      : O_Dnode;
   begin
      New_Debug_Line_Stmt (Get_Line_Number (Decl));

      --  Create the signal (with the time)
      case Get_Kind (Decl) is
         when Iir_Kind_Stable_Attribute =>
            Subprg := Ghdl_Create_Stable_Signal;
         when Iir_Kind_Quiet_Attribute =>
            Subprg := Ghdl_Create_Quiet_Signal;
         when Iir_Kind_Transaction_Attribute =>
            Subprg := Ghdl_Create_Transaction_Signal;
         when others =>
            Error_Kind ("elab_signal_attribute", Decl);
      end case;
      Start_Association (Assoc, Subprg);
      New_Association (Assoc,
                       New_Unchecked_Address (Get_Var (Info.Signal_Val),
                                              Ghdl_Ptr_Type));
      case Get_Kind (Decl) is
         when Iir_Kind_Stable_Attribute
            | Iir_Kind_Quiet_Attribute =>
            Param := Get_Parameter (Decl);
            if Param = Null_Iir then
               Val := New_Lit (New_Signed_Literal (Std_Time_Otype, 0));
            else
               Val := Chap7.Translate_Expression (Param);
            end if;
            New_Association (Assoc, Val);
         when others =>
            null;
      end case;
      Res := New_Convert_Ov (New_Function_Call (Assoc),
                             Type_Info.Ortho_Type (Mode_Signal));
      New_Assign_Stmt (Get_Var (Info.Signal_Sig), Res);

      --  Register all signals this depends on.
      Prefix := Get_Prefix (Decl);
      Prefix_Node := Chap6.Translate_Name (Prefix, Mode_Signal);
      Register_Signal (Prefix_Node, Get_Type (Prefix),
                       Ghdl_Signal_Attribute_Register_Prefix);
   end Elab_Signal_Attribute;

   type Delayed_Signal_Data is record
      --  Value part of the signal.  The signal itself is passed by a
      --  parameter.
      Targ_Val : Mnode;

      --  Prefix signal.
      Pfx   : Mnode;

      --  Delay time.
      Param : Iir;
   end record;

   procedure Create_Delayed_Signal_Noncomposite
     (Targ : Mnode; Targ_Type : Iir; Data : Delayed_Signal_Data)
   is
      pragma Unreferenced (Targ_Type);
      Assoc     : O_Assoc_List;
      Type_Info : Type_Info_Acc;
      Val       : O_Enode;
   begin
      Start_Association (Assoc, Ghdl_Create_Delayed_Signal);
      New_Association
        (Assoc,
         New_Convert_Ov (New_Value (M2Lv (Data.Pfx)), Ghdl_Signal_Ptr));
      New_Association
        (Assoc,
         New_Unchecked_Address (M2Lv (Data.Targ_Val), Ghdl_Ptr_Type));
      if Data.Param = Null_Iir then
         Val := New_Lit (New_Signed_Literal (Std_Time_Otype, 0));
      else
         Val := Chap7.Translate_Expression (Data.Param);
      end if;
      New_Association (Assoc, Val);
      Type_Info := Get_Type_Info (Targ);
      New_Assign_Stmt
        (M2Lv (Targ),
         New_Convert_Ov (New_Function_Call (Assoc),
                         Type_Info.Ortho_Type (Mode_Signal)));
   end Create_Delayed_Signal_Noncomposite;

   function Create_Delayed_Signal_Prepare_Composite
     (Targ : Mnode; Targ_Type : Iir; Data : Delayed_Signal_Data)
         return Delayed_Signal_Data
   is
      pragma Unreferenced (Targ_Type);
      Res : Delayed_Signal_Data;
   begin
      Res.Param := Data.Param;
      if Get_Type_Info (Targ).Type_Mode in Type_Mode_Bounded_Records then
         Res.Targ_Val := Stabilize (Data.Targ_Val);
         Res.Pfx := Stabilize (Data.Pfx);
      else
         Res.Targ_Val := Chap3.Get_Composite_Base (Data.Targ_Val);
         Res.Pfx := Chap3.Get_Composite_Base (Data.Pfx);
      end if;
      return Res;
   end Create_Delayed_Signal_Prepare_Composite;

   function Create_Delayed_Signal_Update_Data_Array
     (Data : Delayed_Signal_Data; Targ_Type : Iir; Index : O_Dnode)
         return Delayed_Signal_Data
   is
   begin
      return Delayed_Signal_Data'
        (Targ_Val => Chap3.Index_Base (Data.Targ_Val, Targ_Type,
                                       New_Obj_Value (Index)),
         Pfx => Chap3.Index_Base (Data.Pfx, Targ_Type,
                                  New_Obj_Value (Index)),
         Param => Data.Param);
   end Create_Delayed_Signal_Update_Data_Array;

   function Create_Delayed_Signal_Update_Data_Record
     (Data      : Delayed_Signal_Data;
      Targ_Type : Iir;
      El        : Iir_Element_Declaration)
     return Delayed_Signal_Data
   is
      pragma Unreferenced (Targ_Type);
   begin
      return Delayed_Signal_Data'
        (Targ_Val => Chap6.Translate_Selected_Element (Data.Targ_Val, El),
         Pfx => Chap6.Translate_Selected_Element (Data.Pfx, El),
         Param => Data.Param);
   end Create_Delayed_Signal_Update_Data_Record;

   procedure Create_Delayed_Signal is new Foreach_Non_Composite
     (Data_Type => Delayed_Signal_Data,
      Composite_Data_Type => Delayed_Signal_Data,
      Do_Non_Composite => Create_Delayed_Signal_Noncomposite,
      Prepare_Data_Array => Create_Delayed_Signal_Prepare_Composite,
      Update_Data_Array => Create_Delayed_Signal_Update_Data_Array,
      Prepare_Data_Record => Create_Delayed_Signal_Prepare_Composite,
      Update_Data_Record => Create_Delayed_Signal_Update_Data_Record);

   procedure Elab_Signal_Delayed_Attribute (Decl : Iir)
   is
      Sig_Type  : constant Iir := Get_Type (Decl);
      Type_Info : constant Type_Info_Acc := Get_Info (Sig_Type);
      Name_Sig, Name_Val : Mnode;
      Pfx_Node  : Mnode;
      Data      : Delayed_Signal_Data;
   begin
      Chap6.Translate_Signal_Name (Decl, Name_Sig, Name_Val);

      if Is_Complex_Type (Type_Info) then
         Allocate_Complex_Object (Sig_Type, Alloc_System, Name_Sig);
         Allocate_Complex_Object (Sig_Type, Alloc_System, Name_Val);
         --  We cannot stabilize NAME_NODE, since Allocate_Complex_Object
         --  assign it.
         Chap6.Translate_Signal_Name (Decl, Name_Sig, Name_Val);
      end if;

      Pfx_Node := Chap6.Translate_Name (Get_Prefix (Decl), Mode_Signal);
      Data := Delayed_Signal_Data'(Targ_Val => Name_Val,
                                   Pfx => Pfx_Node,
                                   Param => Get_Parameter (Decl));

      Create_Delayed_Signal (Name_Sig, Sig_Type, Data);
   end Elab_Signal_Delayed_Attribute;

   procedure Elab_File_Declaration (Decl : Iir_File_Declaration)
   is
      Is_Text   : constant Boolean := Get_Text_File_Flag (Get_Type (Decl));
      File_Name : constant Iir := Get_File_Logical_Name (Decl);
      Constr    : O_Assoc_List;
      Name      : Mnode;
      Open_Kind : Iir;
      Mode_Val  : O_Enode;
      Str       : O_Enode;
      Info      : Type_Info_Acc;
   begin
      --  Elaborate the file.
      Name := Chap6.Translate_Name (Decl, Mode_Value);

      if Is_Text then
         Start_Association (Constr, Ghdl_Text_File_Elaborate);
      else
         Start_Association (Constr, Ghdl_File_Elaborate);
         Info := Get_Info (Get_Type (Decl));
         if Info.B.File_Signature /= O_Dnode_Null then
            New_Association
              (Constr, New_Address (New_Obj (Info.B.File_Signature),
               Char_Ptr_Type));
         else
            New_Association (Constr,
                             New_Lit (New_Null_Access (Char_Ptr_Type)));
         end if;
      end if;
      New_Assign_Stmt (M2Lv (Name), New_Function_Call (Constr));

      --  If file_open_information is present, open the file.
      if File_Name = Null_Iir then
         return;
      end if;
      Open_Temp;
      Name := Chap6.Translate_Name (Decl, Mode_Value);
      Open_Kind := Get_File_Open_Kind (Decl);
      if Open_Kind /= Null_Iir then
         --  VHDL 93 and later.
         Mode_Val := New_Convert_Ov
           (Chap7.Translate_Expression (Open_Kind), Ghdl_I32_Type);
      else
         --  VHDL 87.
         case Get_Mode (Decl) is
            when Iir_In_Mode =>
               Mode_Val := New_Lit (New_Signed_Literal (Ghdl_I32_Type, 0));
            when Iir_Out_Mode =>
               Mode_Val := New_Lit (New_Signed_Literal (Ghdl_I32_Type, 1));
            when others =>
               raise Internal_Error;
         end case;
      end if;
      Str := Chap7.Translate_Expression (File_Name, String_Type_Definition);

      if Is_Text then
         Start_Association (Constr, Ghdl_Text_File_Open);
      else
         Start_Association (Constr, Ghdl_File_Open);
      end if;
      New_Association (Constr, M2E (Name));
      New_Association (Constr, Mode_Val);
      New_Association (Constr, Str);
      New_Procedure_Call (Constr);
      Close_Temp;
   end Elab_File_Declaration;

   procedure Final_File_Declaration (Decl : Iir_File_Declaration)
   is
      Is_Text : constant Boolean := Get_Text_File_Flag (Get_Type (Decl));
      Constr  : O_Assoc_List;
      Name    : Mnode;
   begin
      Open_Temp;
      Name := Chap6.Translate_Name (Decl, Mode_Value);
      Stabilize (Name);

      --  LRM 3.4.1 File Operations
      --  An implicit call to FILE_CLOSE exists in a subprogram body for
      --  every file object declared in the corresponding subprogram
      --  declarative part.  Each such call associates a unique file object
      --  with the formal parameter F and is called whenever the
      --  corresponding subprogram completes its execution.
      if Is_Text then
         Start_Association (Constr, Ghdl_Text_File_Close);
      else
         Start_Association (Constr, Ghdl_File_Close);
      end if;
      New_Association (Constr, M2E (Name));
      New_Procedure_Call (Constr);

      if Is_Text then
         Start_Association (Constr, Ghdl_Text_File_Finalize);
      else
         Start_Association (Constr, Ghdl_File_Finalize);
      end if;
      New_Association (Constr, M2E (Name));
      New_Procedure_Call (Constr);

      Close_Temp;
   end Final_File_Declaration;

   procedure Translate_Type_Declaration (Decl : Iir)
   is
      Def : constant Iir := Get_Type_Definition (Decl);
      Mark  : Id_Mark_Type;
   begin
      Push_Identifier_Prefix (Mark, Get_Identifier (Decl));
      case Get_Kind (Def) is
         when Iir_Kinds_Subtype_Definition =>
            Chap3.Translate_Subtype_Indication (Def, True);
            raise Internal_Error;
         when others =>
            Chap3.Translate_Type_Definition (Def);
      end case;
      Pop_Identifier_Prefix (Mark);
   end Translate_Type_Declaration;

   procedure Translate_Anonymous_Type_Declaration (Decl : Iir)
   is
      Mark  : Id_Mark_Type;
      Mark1 : Id_Mark_Type;
   begin
      Push_Identifier_Prefix (Mark, Get_Identifier (Decl));
      Push_Identifier_Prefix (Mark1, "BT");
      Chap3.Translate_Type_Definition (Get_Type_Definition (Decl));
      Pop_Identifier_Prefix (Mark1);
      Pop_Identifier_Prefix (Mark);
   end Translate_Anonymous_Type_Declaration;

   procedure Translate_Subtype_Declaration (Decl : Iir_Subtype_Declaration)
   is
      Def : constant Iir := Get_Type (Decl);
      Mark  : Id_Mark_Type;
   begin
      Push_Identifier_Prefix (Mark, Get_Identifier (Decl));
      Chap3.Translate_Subtype_Definition (Def, True);
      Pop_Identifier_Prefix (Mark);
   end Translate_Subtype_Declaration;

   procedure Translate_Bool_Type_Declaration (Decl : Iir_Type_Declaration)
   is
      Mark : Id_Mark_Type;
   begin
      Push_Identifier_Prefix (Mark, Get_Identifier (Decl));
      Chap3.Translate_Bool_Type_Definition (Get_Type_Definition (Decl));
      Pop_Identifier_Prefix (Mark);
   end Translate_Bool_Type_Declaration;

   procedure Translate_Object_Alias_Declaration
     (Decl : Iir_Object_Alias_Declaration)
   is
      Decl_Type : constant Iir := Get_Type (Decl);
      Name      : constant Iir := Get_Name (Decl);
      Info      : Alias_Info_Acc;
      Tinfo     : Type_Info_Acc;
      Atype     : O_Tnode;
      Id        : Var_Ident_Type;
   begin
      Chap3.Translate_Object_Subtype_Indication (Decl, True);

      Info := Add_Info (Decl, Kind_Alias);
      if Is_Signal_Name (Decl) then
         Info.Alias_Kind := Mode_Signal;
      else
         Info.Alias_Kind := Mode_Value;
      end if;

      Tinfo := Get_Info (Decl_Type);
      for Mode in Mode_Value .. Info.Alias_Kind loop
         case Tinfo.Type_Mode is
            when Type_Mode_Unbounded =>
               --  create an object.
               --  At elaboration: copy base from name, copy bounds from type,
               --   check for matching bounds.
               Atype := Get_Ortho_Type (Decl_Type, Mode);
            when Type_Mode_Bounded_Arrays
              | Type_Mode_Bounded_Records
              | Type_Mode_Acc
              | Type_Mode_Bounds_Acc
              | Type_Mode_Protected =>
               --  Create an object pointer.
               --  At elaboration: copy base from name.
               Atype := Tinfo.Ortho_Ptr_Type (Mode);
            when Type_Mode_Scalar =>
               case Mode is
                  when Mode_Signal =>
                     Atype := Tinfo.Ortho_Type (Mode_Signal);
                  when Mode_Value =>
                     Atype := Tinfo.Ortho_Ptr_Type (Mode_Value);
               end case;
            when others =>
               raise Internal_Error;
         end case;
         if Mode = Mode_Signal then
            Id := Create_Var_Identifier (Decl, "_SIG", 0);
         else
            Id := Create_Var_Identifier (Decl);
         end if;
         Info.Alias_Var (Mode) := Create_Var (Id, Atype);
      end loop;

      if Get_Kind (Name) = Iir_Kind_Slice_Name
        and then Info.Alias_Kind = Mode_Signal
      then
         --  The name subtype will be evaluated once at elaboration, as it is
         --  needed when direct drivers are used (in that case, the name is
         --  evaluated once again).
         declare
            Name_Type : constant Iir := Get_Type (Name);
            Mark1, Mark2 : Id_Mark_Type;
         begin
            Push_Identifier_Prefix (Mark1, Get_Identifier (Decl));
            Push_Identifier_Prefix (Mark2, "AT");
            Chap3.Translate_Array_Subtype (Name_Type);
            Pop_Identifier_Prefix (Mark2);
            Pop_Identifier_Prefix (Mark1);
         end;
      end if;
   end Translate_Object_Alias_Declaration;

   procedure Elab_Object_Alias_Declaration
     (Decl : Iir_Object_Alias_Declaration)
   is
      Decl_Type  : constant Iir := Get_Type (Decl);
      Tinfo      : constant Type_Info_Acc := Get_Info (Decl_Type);
      Name       : constant Iir := Get_Name (Decl);
      Name_Type  : constant Iir := Get_Type (Name);
      Alias_Info : constant Alias_Info_Acc := Get_Info (Decl);
      Name_Node  : Mnode_Array;
   begin
      New_Debug_Line_Stmt (Get_Line_Number (Decl));

      Chap3.Elab_Object_Subtype_Indication (Decl);

      Open_Temp;

      if Get_Kind (Name) = Iir_Kind_Slice_Name
        and then Alias_Info.Alias_Kind = Mode_Signal
      then
         --  See Translate_Object_Alias_Declaration.
         Chap3.Elab_Array_Subtype (Name_Type);
      end if;

      case Alias_Info.Alias_Kind is
         when Mode_Value =>
            Name_Node (Mode_Value) := Chap6.Translate_Name (Name, Mode_Value);
         when Mode_Signal =>
            Chap6.Translate_Signal_Name
              (Name, Name_Node (Mode_Signal), Name_Node (Mode_Value));
      end case;

      for Mode in Mode_Value .. Alias_Info.Alias_Kind loop
         declare
            N : Mnode renames Name_Node (Mode);
            A : Var_Type renames Alias_Info.Alias_Var (Mode);
            Alias_Node : Mnode;
         begin
            --  FIXME: use subtype conversion ?
            case Tinfo.Type_Mode is
               when Type_Mode_Unbounded =>
                  Stabilize (N);
                  Alias_Node := Stabilize (Get_Var (A, Tinfo, Mode));
                  Chap7.Convert_Constrained_To_Unconstrained (Alias_Node, N);
               when Type_Mode_Bounded_Arrays =>
                  Stabilize (N);
                  New_Assign_Stmt
                    (Get_Var (A),
                     New_Convert_Ov (M2E (Chap3.Get_Composite_Base (N)),
                                     Tinfo.Ortho_Ptr_Type (Mode)));
                  Chap3.Check_Composite_Match
                    (Decl_Type, T2M (Decl_Type, Mode),
                     Name_Type, N, Decl);
               when Type_Mode_Acc
                 | Type_Mode_Bounds_Acc
                 | Type_Mode_Protected =>
                  New_Assign_Stmt (Get_Var (A), M2Addr (N));
               when Type_Mode_Scalar =>
                  case Mode is
                     when Mode_Value =>
                        New_Assign_Stmt (Get_Var (A), M2Addr (N));
                     when Mode_Signal =>
                        New_Assign_Stmt (Get_Var (A), M2E (N));
                  end case;
               when Type_Mode_Bounded_Records =>
                  Stabilize (N);
                  --  FIXME: Check ?
                  New_Assign_Stmt (Get_Var (A), M2Addr (N));
               when others =>
                  raise Internal_Error;
            end case;
         end;
      end loop;
      Close_Temp;
   end Elab_Object_Alias_Declaration;

   procedure Translate_Port_Chain (Parent : Iir)
   is
      Port : Iir;
   begin
      Port := Get_Port_Chain (Parent);
      while Port /= Null_Iir loop
         Create_Signal (Port);
         Port := Get_Chain (Port);
      end loop;
   end Translate_Port_Chain;

   procedure Translate_Generic_Chain (Parent : Iir)
   is
      Decl : Iir;
   begin
      Decl := Get_Generic_Chain (Parent);
      while Decl /= Null_Iir loop
         case Get_Kind (Decl) is
            when Iir_Kinds_Interface_Object_Declaration =>
               Create_Object (Decl);
            when Iir_Kind_Interface_Package_Declaration =>
               if Get_Generic_Map_Aspect_Chain (Decl) = Null_Iir then
                  --  Need a formal
                  Create_Package_Interface (Decl);
               else
                  --  Instantiated.
                  Chap2.Translate_Package_Instantiation_Declaration (Decl);
               end if;
            when Iir_Kind_Interface_Type_Declaration
              | Iir_Kinds_Interface_Subprogram_Declaration =>
               null;
            when others =>
               Error_Kind ("translate_generic_chain", Decl);
         end case;
         Decl := Get_Chain (Decl);
      end loop;
   end Translate_Generic_Chain;

   --  Create instance record for a component.
   procedure Translate_Component_Declaration (Decl : Iir)
   is
      Mark : Id_Mark_Type;
      Info : Ortho_Info_Acc;
   begin
      Info := Add_Info (Decl, Kind_Component);
      Push_Identifier_Prefix (Mark, Get_Identifier (Decl));
      Push_Instance_Factory (Info.Comp_Scope'Access);

      Info.Comp_Link := Add_Instance_Factory_Field
        (Wki_Instance, Rtis.Ghdl_Component_Link_Type);

      --  Generic and ports.
      Translate_Generic_Chain (Decl);
      Translate_Port_Chain (Decl);

      Pop_Instance_Factory (Info.Comp_Scope'Access);
      New_Type_Decl (Create_Identifier ("_COMPTYPE"),
                     Get_Scope_Type (Info.Comp_Scope));
      Info.Comp_Ptr_Type := New_Access_Type
        (Get_Scope_Type (Info.Comp_Scope));
      New_Type_Decl (Create_Identifier ("_COMPPTR"), Info.Comp_Ptr_Type);
      Pop_Identifier_Prefix (Mark);
   end Translate_Component_Declaration;

   procedure Translate_Declaration (Decl : Iir)
   is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Use_Clause =>
            null;
         when Iir_Kind_Configuration_Specification =>
            null;
         when Iir_Kind_Disconnection_Specification =>
            null;

         when Iir_Kind_Psl_Default_Clock =>
            null;
         when Iir_Kind_Psl_Declaration =>
            null;

         when Iir_Kind_Component_Declaration =>
            Chap4.Translate_Component_Declaration (Decl);
         when Iir_Kind_Type_Declaration =>
            --  A type declaration can be in fact a subtype declaration.
            Chap4.Translate_Type_Declaration (Decl);
         when Iir_Kind_Anonymous_Type_Declaration =>
            Chap4.Translate_Anonymous_Type_Declaration (Decl);
         when Iir_Kind_Subtype_Declaration =>
            Chap4.Translate_Subtype_Declaration (Decl);

         when Iir_Kind_Function_Declaration
            | Iir_Kind_Procedure_Declaration =>
            raise Internal_Error;
         when Iir_Kind_Function_Body
            | Iir_Kind_Procedure_Body =>
            null;

         when Iir_Kind_Protected_Type_Body =>
            null;

            --when Iir_Kind_Implicit_Function_Declaration =>
            --when Iir_Kind_Signal_Declaration
            --  | Iir_Kind_Interface_Signal_Declaration =>
            --   Chap4.Create_Object (Decl);

         when Iir_Kind_Variable_Declaration
            | Iir_Kind_Constant_Declaration =>
            Create_Object (Decl);

         when Iir_Kind_Signal_Declaration =>
            Create_Signal (Decl);

         when Iir_Kind_Object_Alias_Declaration =>
            Translate_Object_Alias_Declaration (Decl);

         when Iir_Kind_Non_Object_Alias_Declaration =>
            null;

         when Iir_Kind_File_Declaration =>
            Create_File_Object (Decl);

         when Iir_Kind_Attribute_Declaration =>
            --  Attribute declarations have a type mark.
            null;

         when Iir_Kind_Attribute_Specification =>
            Chap5.Translate_Attribute_Specification (Decl);

         when Iir_Kind_Signal_Attribute_Declaration =>
            declare
               Sig : Iir;
            begin
               Sig := Get_Signal_Attribute_Chain (Decl);
               while Is_Valid (Sig) loop
                  Chap4.Create_Implicit_Signal (Sig);
                  Sig := Get_Attr_Chain (Sig);
               end loop;
            end;

         when Iir_Kind_Guard_Signal_Declaration =>
            Create_Signal (Decl);

         when Iir_Kind_Package_Declaration =>
            Chap2.Translate_Package_Declaration (Decl);
         when Iir_Kind_Package_Body =>
            Chap2.Translate_Package_Body (Decl);
         when Iir_Kind_Package_Instantiation_Declaration =>
            Chap2.Translate_Package_Instantiation_Declaration (Decl);

         when Iir_Kind_Group_Template_Declaration =>
            null;
         when Iir_Kind_Group_Declaration =>
            null;

         when others =>
            Error_Kind ("translate_declaration", Decl);
      end case;
   end Translate_Declaration;

   procedure Translate_Resolution_Function (Func : Iir)
   is
      Finfo           : constant Subprg_Info_Acc := Get_Info (Func);
      Rinfo           : constant Subprg_Resolv_Info_Acc := Finfo.Subprg_Resolv;
      --  Type of the resolution function parameter.
      El_Type         : Iir;
      El_Info         : Type_Info_Acc;
      Interface_List  : O_Inter_List;
      Id              : O_Ident;
      Itype           : O_Tnode;
      Unused_Instance : O_Dnode;
   begin
      if Rinfo = null then
         --  Not a resolution function
         return;
      end if;

      --  Declare the procedure.
      Id := Create_Identifier (Func, Get_Overload_Number (Func), "_RESOLV");
      Start_Procedure_Decl (Interface_List, Id, Global_Storage);

      --  The instance.
      if Subprgs.Has_Current_Subprg_Instance then
         Subprgs.Add_Subprg_Instance_Interfaces (Interface_List,
                                                 Rinfo.Var_Instance);
      else
         --  Create a dummy instance parameter
         New_Interface_Decl (Interface_List, Unused_Instance,
                             Wki_Instance, Ghdl_Ptr_Type);
         Rinfo.Var_Instance := Subprgs.Null_Subprg_Instance;
      end if;

      --  The signal.
      El_Type := Get_Type (Get_Interface_Declaration_Chain (Func));
      El_Type := Get_Element_Subtype (El_Type);
      El_Info := Get_Info (El_Type);
      --  FIXME: create a function for getting the type of an interface.
      case El_Info.Type_Mode is
         when Type_Mode_Thin =>
            Itype := El_Info.Ortho_Type (Mode_Signal);
         when Type_Mode_Fat =>
            Itype := El_Info.Ortho_Ptr_Type (Mode_Signal);
         when Type_Mode_Unknown =>
            raise Internal_Error;
      end case;
      New_Interface_Decl
        (Interface_List, Rinfo.Var_Vals, Get_Identifier ("VALS"), Itype);

      New_Interface_Decl
        (Interface_List, Rinfo.Var_Vec, Get_Identifier ("bool_vec"),
         Ghdl_Bool_Array_Ptr);
      New_Interface_Decl
        (Interface_List, Rinfo.Var_Vlen, Get_Identifier ("vec_len"),
         Ghdl_Index_Type);
      New_Interface_Decl
        (Interface_List, Rinfo.Var_Nbr_Drv, Get_Identifier ("nbr_drv"),
         Ghdl_Index_Type);
      New_Interface_Decl
        (Interface_List, Rinfo.Var_Nbr_Ports, Get_Identifier ("nbr_ports"),
         Ghdl_Index_Type);

      Finish_Subprogram_Decl (Interface_List, Rinfo.Resolv_Func);
   end Translate_Resolution_Function;

   type Read_Source_Kind is (Read_Port, Read_Driver);
   type Read_Source_Data is record
      Sig       : Mnode;
      Drv_Index : O_Dnode;
      Kind      : Read_Source_Kind;
   end record;

   procedure Read_Source_Non_Composite
     (Targ : Mnode; Targ_Type : Iir; Data : Read_Source_Data)
   is
      Targ_Info : constant Type_Info_Acc := Get_Info (Targ_Type);
      Assoc     : O_Assoc_List;
      E         : O_Enode;
   begin
      case Data.Kind is
         when Read_Port =>
            Start_Association (Assoc, Ghdl_Signal_Read_Port);
         when Read_Driver =>
            Start_Association (Assoc, Ghdl_Signal_Read_Driver);
      end case;

      New_Association
        (Assoc, New_Convert_Ov (M2E (Data.Sig), Ghdl_Signal_Ptr));
      New_Association (Assoc, New_Obj_Value (Data.Drv_Index));
      E := New_Convert_Ov (New_Function_Call (Assoc),
                           Targ_Info.Ortho_Ptr_Type (Mode_Value));
      New_Assign_Stmt (M2Lv (Targ),
                       New_Value (New_Access_Element (E)));
   end Read_Source_Non_Composite;

   function Read_Source_Prepare_Data_Array
     (Targ: Mnode; Targ_Type : Iir; Data : Read_Source_Data)
         return Read_Source_Data
   is
      pragma Unreferenced (Targ, Targ_Type);
   begin
      return Data;
   end Read_Source_Prepare_Data_Array;

   function Read_Source_Prepare_Data_Record
     (Targ : Mnode; Targ_Type : Iir; Data : Read_Source_Data)
         return Read_Source_Data
   is
      pragma Unreferenced (Targ, Targ_Type);
   begin
      return Read_Source_Data'(Sig => Stabilize (Data.Sig),
                               Drv_Index => Data.Drv_Index,
                               Kind => Data.Kind);
   end Read_Source_Prepare_Data_Record;

   function Read_Source_Update_Data_Array
     (Data : Read_Source_Data; Targ_Type : Iir; Index : O_Dnode)
     return Read_Source_Data is
   begin
      return Read_Source_Data'
        (Sig => Chap3.Index_Base (Data.Sig, Targ_Type,
         New_Obj_Value (Index)),
         Drv_Index => Data.Drv_Index,
         Kind => Data.Kind);
   end Read_Source_Update_Data_Array;

   function Read_Source_Update_Data_Record
     (Data      : Read_Source_Data;
      Targ_Type : Iir;
      El        : Iir_Element_Declaration)
     return Read_Source_Data
   is
      pragma Unreferenced (Targ_Type);
   begin
      return Read_Source_Data'
        (Sig => Chap6.Translate_Selected_Element (Data.Sig, El),
         Drv_Index => Data.Drv_Index,
         Kind => Data.Kind);
   end Read_Source_Update_Data_Record;

   procedure Read_Signal_Source is new Foreach_Non_Composite
     (Data_Type => Read_Source_Data,
      Composite_Data_Type => Read_Source_Data,
      Do_Non_Composite => Read_Source_Non_Composite,
      Prepare_Data_Array => Read_Source_Prepare_Data_Array,
      Update_Data_Array => Read_Source_Update_Data_Array,
      Prepare_Data_Record => Read_Source_Prepare_Data_Record,
      Update_Data_Record => Read_Source_Update_Data_Record);

   procedure Translate_Resolution_Function_Body (Func : Iir)
   is
      --  Type of the resolution function parameter.
      Arr_Type   : Iir;
      Base_Type  : Iir;
      Base_Info  : Type_Info_Acc;
      Index_Info : Index_Info_Acc;

      --  Type of parameter element.
      El_Type : Iir;
      El_Info : Type_Info_Acc;

      --  Type of the function return value.
      Ret_Type : Iir;
      Ret_Info : Type_Info_Acc;

      --  Type and info of the array index.
      Index_Type  : Iir;
      Index_Tinfo : Type_Info_Acc;

      --  Local variables.
      Var_I      : O_Dnode;
      Var_J      : O_Dnode;
      Var_Length : O_Dnode;
      Var_Res    : O_Dnode;

      Vals : Mnode;
      Res  : Mnode;

      If_Blk : O_If_Block;
      Label  : O_Snode;

      V : Mnode;

      Var_Bound : O_Dnode;
      Range_Ptr : Mnode;
      Var_Array : O_Dnode;
      Finfo     : constant Subprg_Info_Acc := Get_Info (Func);
      Rinfo     : constant Subprg_Resolv_Info_Acc := Finfo.Subprg_Resolv;
      Assoc     : O_Assoc_List;

      Data : Read_Source_Data;
   begin
      if Rinfo = null then
         --  No resolver for this function
         return;
      end if;

      Ret_Type := Get_Return_Type (Func);
      Ret_Info := Get_Info (Ret_Type);

      Arr_Type := Get_Type (Get_Interface_Declaration_Chain (Func));
      Base_Type := Get_Base_Type (Arr_Type);
      Index_Info := Get_Info
        (Get_Nth_Element (Get_Index_Subtype_Definition_List (Base_Type), 0));
      Base_Info := Get_Info (Base_Type);

      El_Type := Get_Element_Subtype (Arr_Type);
      El_Info := Get_Info (El_Type);

      Index_Type := Get_Index_Type (Arr_Type, 0);
      Index_Tinfo := Get_Info (Index_Type);

      Start_Subprogram_Body (Rinfo.Resolv_Func);
      if Subprgs.Has_Subprg_Instance (Rinfo.Var_Instance) then
         Subprgs.Start_Subprg_Instance_Use (Rinfo.Var_Instance);
      end if;
      Push_Local_Factory;

      --  A signal.

      New_Var_Decl
        (Var_Res, Get_Identifier ("res"),
         O_Storage_Local, Get_Object_Type (Ret_Info, Mode_Value));

      --  I, J.
      New_Var_Decl (Var_I, Wki_I, O_Storage_Local, Ghdl_Index_Type);
      New_Var_Decl (Var_J, Get_Identifier ("J"),
                    O_Storage_Local, Ghdl_Index_Type);

      --  Length.
      New_Var_Decl
        (Var_Length, Wki_Length, O_Storage_Local, Ghdl_Index_Type);

      New_Var_Decl (Var_Bound, Get_Identifier ("BOUND"), O_Storage_Local,
                    Base_Info.B.Bounds_Type);
      New_Var_Decl (Var_Array, Get_Identifier ("VARRAY"), O_Storage_Local,
                    Base_Info.Ortho_Type (Mode_Value));

      Open_Temp;

      case El_Info.Type_Mode is
         when Type_Mode_Thin =>
            Vals := Dv2M (Rinfo.Var_Vals, El_Info, Mode_Signal);
         when Type_Mode_Fat =>
            Vals := Dp2M (Rinfo.Var_Vals, El_Info, Mode_Signal);
         when Type_Mode_Unknown =>
            raise Internal_Error;
      end case;

      -- * length := vec_len + nports;
      New_Assign_Stmt (New_Obj (Var_Length),
                       New_Dyadic_Op (ON_Add_Ov,
                         New_Obj_Value (Rinfo.Var_Vlen),
                         New_Obj_Value (Rinfo.Var_Nbr_Ports)));

      --  Create range from length
      Range_Ptr := Lv2M (New_Selected_Element (New_Obj (Var_Bound),
                                               Index_Info.Index_Field),
                         Index_Tinfo, Mode_Value,
                         Index_Tinfo.B.Range_Type,
                         Index_Tinfo.B.Range_Ptr_Type);
      Chap3.Create_Range_From_Length (Index_Type, Var_Length, Range_Ptr, Func);

      New_Assign_Stmt
        (New_Selected_Element (New_Obj (Var_Array),
         Base_Info.B.Bounds_Field (Mode_Value)),
         New_Address (New_Obj (Var_Bound), Base_Info.B.Bounds_Ptr_Type));

      --  Allocate the array.
      Chap3.Allocate_Unbounded_Composite_Base
        (Alloc_Stack, Dv2M (Var_Array, Base_Info, Mode_Value), Base_Type);

      --  Fill the array
      --  1. From ports.
      --  * I := 0;
      Init_Var (Var_I);
      --  * loop
      Start_Loop_Stmt (Label);
      --  *   exit when I = nports;
      Gen_Exit_When (Label,
                     New_Compare_Op (ON_Eq,
                       New_Obj_Value (Var_I),
                       New_Obj_Value (Rinfo.Var_Nbr_Ports),
                       Ghdl_Bool_Type));
      --      fill array[i]
      V := Chap3.Index_Base
        (Chap3.Get_Composite_Base (Dv2M (Var_Array, Base_Info, Mode_Value)),
         Base_Type, New_Obj_Value (Var_I));
      Data := Read_Source_Data'(Vals, Var_I, Read_Port);
      Read_Signal_Source (V, El_Type, Data);

      --  *   I := I + 1;
      Inc_Var (Var_I);
      --  * end loop;
      Finish_Loop_Stmt (Label);

      --  2. From drivers.
      --  * J := 0;
      --  * loop
      --  *   exit when j = var_max;
      --  *   if vec[j] then
      --
      --  *     ptr := get_signal_driver (sig, j);
      --  *     array[i].XXX := *ptr
      --
      --  *     i := i + 1;
      --  *   end if;
      --  *   J := J + 1;
      --  * end loop;
      Init_Var (Var_J);
      Start_Loop_Stmt (Label);
      Gen_Exit_When (Label,
                     New_Compare_Op (ON_Eq,
                       New_Obj_Value (Var_J),
                       New_Obj_Value (Rinfo.Var_Nbr_Drv),
                       Ghdl_Bool_Type));
      Start_If_Stmt
        (If_Blk,
         New_Value (New_Indexed_Acc_Value (New_Obj (Rinfo.Var_Vec),
           New_Obj_Value (Var_J))));

      V := Chap3.Index_Base
        (Chap3.Get_Composite_Base (Dv2M (Var_Array, Base_Info, Mode_Value)),
         Base_Type, New_Obj_Value (Var_I));
      Data := Read_Source_Data'(Vals, Var_J, Read_Driver);
      Read_Signal_Source (V, El_Type, Data);

      Inc_Var (Var_I);
      Finish_If_Stmt (If_Blk);

      Inc_Var (Var_J);
      Finish_Loop_Stmt (Label);

      if Finfo.Res_Interface /= O_Dnode_Null then
         Res := Lo2M (Var_Res, Ret_Info, Mode_Value);
         if Ret_Info.Type_Mode /= Type_Mode_Fat_Array then
            Allocate_Complex_Object (Ret_Type, Alloc_Stack, Res);
         end if;
      end if;

      --  Call the resolution function.
      if Finfo.Use_Stack2 then
         Create_Temp_Stack2_Mark;
      end if;

      Start_Association (Assoc, Finfo.Subprg_Node);
      if Finfo.Res_Interface /= O_Dnode_Null then
         New_Association (Assoc, M2E (Res));
      end if;
      Subprgs.Add_Subprg_Instance_Assoc (Assoc, Finfo.Subprg_Instance);
      New_Association
        (Assoc, New_Address (New_Obj (Var_Array),
         Base_Info.Ortho_Ptr_Type (Mode_Value)));

      if Finfo.Res_Interface = O_Dnode_Null then
         Res := E2M (New_Function_Call (Assoc), Ret_Info, Mode_Value);
      else
         New_Procedure_Call (Assoc);
      end if;

      if El_Type /= Ret_Type then
         Res := E2M
           (Chap7.Translate_Implicit_Conv (M2E (Res), Ret_Type, El_Type,
            Mode_Value, Func),
            El_Info, Mode_Value);
      end if;
      Chap7.Set_Driving_Value (Vals, El_Type, Res);

      Close_Temp;
      Pop_Local_Factory;
      if Subprgs.Has_Subprg_Instance (Rinfo.Var_Instance) then
         Subprgs.Finish_Subprg_Instance_Use (Rinfo.Var_Instance);
      end if;
      Finish_Subprogram_Body;
   end Translate_Resolution_Function_Body;

   procedure Translate_Declaration_Chain (Parent : Iir)
   is
      Info : Subprg_Info_Acc;
      El   : Iir;
   begin
      El := Get_Declaration_Chain (Parent);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Procedure_Declaration
              | Iir_Kind_Function_Declaration =>
               --  Translate interfaces.
               if not Is_Implicit_Subprogram (El)
                 and then (not Flag_Discard_Unused or else Get_Use_Flag (El))
                 and then not Is_Second_Subprogram_Specification (El)
               then
                  Info := Add_Info (El, Kind_Subprg);
                  Chap2.Translate_Subprogram_Interfaces (El);
                  if Get_Kind (El) = Iir_Kind_Function_Declaration then
                     if Get_Resolution_Function_Flag (El) then
                        Info.Subprg_Resolv := new Subprg_Resolv_Info;
                     end if;
                  end if;
               end if;
            when Iir_Kind_Function_Body
               | Iir_Kind_Procedure_Body =>
               null;
            when others =>
               Translate_Declaration (El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Translate_Declaration_Chain;

   procedure Translate_Statements_Chain_State_Declaration
     (Stmts : Iir; State_Scope : Var_Scope_Acc)
   is
      Num : Nat32;
      Mark : Id_Mark_Type;
      Locvar_Id : O_Ident;
      Els : O_Element_List;

      procedure Push_Prefix (Really_Push : Boolean := True)
      is
         Num_Img : String := Nat32'Image (Num);
      begin
         Num_Img (Num_Img'First) := 'S';
         Locvar_Id := Get_Identifier (Num_Img);
         Num := Num + 1;
         if Really_Push then
            Push_Identifier_Prefix (Mark, Num_Img);
         end if;
      end Push_Prefix;

      procedure Pop_Prefix (Scope : in out Var_Scope_Type;
                            Really_Push : Boolean := True)
      is
         Locvar_Field : O_Fnode;
      begin
         if Really_Push then
            Pop_Identifier_Prefix (Mark);
         end if;

         New_Union_Field
           (Els, Locvar_Field, Locvar_Id, Get_Scope_Type (Scope));
         Set_Scope_Via_Field (Scope, Locvar_Field, State_Scope);
      end Pop_Prefix;

      Info : Ortho_Info_Acc;
      Stmt : Iir;
      Chain : Iir;
      Scope_Type : O_Tnode;
   begin
      Stmt := Stmts;

      Start_Union_Type (Els);
      Num := 0;

      while Stmt /= Null_Iir loop
         case Get_Kind (Stmt) is
            when Iir_Kind_If_Statement =>
               if Get_Suspend_Flag (Stmt) then
                  Chain := Stmt;
                  while Chain /= Null_Iir loop
                     Push_Prefix;

                     Info := Add_Info (Chain, Kind_Locvar_State);

                     Translate_Statements_Chain_State_Declaration
                       (Get_Sequential_Statement_Chain (Chain),
                        Info.Locvar_Scope'Access);

                     Pop_Prefix (Info.Locvar_Scope);

                     Chain := Get_Else_Clause (Chain);
                  end loop;
               end if;

            when Iir_Kind_Case_Statement =>
               if Get_Suspend_Flag (Stmt) then
                  Chain := Get_Case_Statement_Alternative_Chain (Stmt);
                  while Chain /= Null_Iir loop
                     if not Get_Same_Alternative_Flag (Chain) then
                        Push_Prefix;

                        Info := Add_Info (Chain, Kind_Locvar_State);

                        Translate_Statements_Chain_State_Declaration
                          (Get_Associated_Chain (Chain),
                           Info.Locvar_Scope'Access);

                        Pop_Prefix (Info.Locvar_Scope);
                     end if;
                     Chain := Get_Chain (Chain);
                  end loop;
               end if;

            when Iir_Kind_While_Loop_Statement =>
               if Get_Suspend_Flag (Stmt) then
                  Push_Prefix;

                  Info := Add_Info (Stmt, Kind_Loop_State);

                  Translate_Statements_Chain_State_Declaration
                    (Get_Sequential_Statement_Chain (Stmt),
                     Info.Loop_Locvar_Scope'Access);

                  Pop_Prefix (Info.Loop_Locvar_Scope);
               end if;

            when Iir_Kind_For_Loop_Statement =>
               if Get_Suspend_Flag (Stmt) then
                  Push_Prefix;

                  Info := Add_Info (Stmt, Kind_Loop_State);

                  Push_Instance_Factory (Info.Loop_State_Scope'Access);

                  Chap8.Translate_For_Loop_Statement_Declaration (Stmt);

                  Translate_Statements_Chain_State_Declaration
                    (Get_Sequential_Statement_Chain (Stmt),
                     Info.Loop_Locvar_Scope'Access);

                  Add_Scope_Field (Wki_Locvars, Info.Loop_Locvar_Scope);

                  Pop_Instance_Factory (Info.Loop_State_Scope'Access);

                  New_Type_Decl (Create_Identifier ("FORTYPE"),
                                 Get_Scope_Type (Info.Loop_State_Scope));

                  Pop_Prefix (Info.Loop_State_Scope);
               end if;

            when Iir_Kind_Procedure_Call_Statement =>
               declare
                  Call : constant Iir := Get_Procedure_Call (Stmt);
                  Imp : constant Iir := Get_Implementation (Call);
               begin
                  Vhdl.Canon.Canon_Subprogram_Call (Call);
                  Update_Node_Infos;

                  if Get_Suspend_Flag (Imp) then
                     Push_Prefix;

                     Info := Add_Info (Call, Kind_Call);

                     Chap8.Translate_Procedure_Call_State (Call);

                     Pop_Prefix (Info.Call_State_Scope);
                  end if;
               end;
            when others =>
               null;
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;

      Finish_Union_Type (Els, Scope_Type);

      New_Type_Decl
        (Create_Identifier ("LOCVARTYPE"), Scope_Type);
      Create_Union_Scope (State_Scope.all, Scope_Type);
   end Translate_Statements_Chain_State_Declaration;

   procedure Translate_Declaration_Chain_Subprograms
     (Parent : Iir; What : Subprg_Translate_Kind)
   is
      --  True iff specs must be translated.
      Do_Specs : constant Boolean := What in Subprg_Translate_Spec;

      --  True iff bodies must be translated.
      Do_Bodies : constant Boolean :=
        (What in Subprg_Translate_Body
           and then Global_Storage /= O_Storage_External);

      El     : Iir;
      Infos  : Chap7.Implicit_Subprogram_Infos;
   begin
      El := Get_Declaration_Chain (Parent);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Procedure_Declaration
              | Iir_Kind_Function_Declaration =>
               if Is_Implicit_Subprogram (El) then
                  if Flag_Discard_Unused_Implicit
                    and then not Get_Use_Flag (El)
                  then
                     case Get_Implicit_Definition (El) is
                        when Iir_Predefined_Array_Equality
                          | Iir_Predefined_Array_Greater
                          | Iir_Predefined_Record_Equality =>
                           --  Used implicitly in case statement or other
                           --  predefined equality.
                           if Do_Specs then
                              Chap7.Translate_Implicit_Subprogram_Spec
                                (El, Infos);
                           end if;
                           if Do_Bodies then
                              Chap7.Translate_Implicit_Subprogram_Body (El);
                           end if;
                        when others =>
                           null;
                     end case;
                  else
                     if Do_Specs then
                        Chap7.Translate_Implicit_Subprogram_Spec
                          (El, Infos);
                     end if;
                     if Do_Bodies then
                        Chap7.Translate_Implicit_Subprogram_Body (El);
                     end if;
                  end if;
               else
                  --  Translate only if used.
                  if Do_Specs and then Get_Info (El) /= null then
                     Chap2.Translate_Subprogram_Declaration (El);
                     Translate_Resolution_Function (El);
                  end if;
               end if;
            when Iir_Kind_Function_Body
              | Iir_Kind_Procedure_Body =>
               if Do_Bodies then
                  --  Do not translate body if generating only specs (for
                  --  subprograms in an entity).
                  if not Flag_Discard_Unused
                    or else
                    Get_Use_Flag (Get_Subprogram_Specification (El))
                  then
                     Chap2.Translate_Subprogram_Body (El);
                     Translate_Resolution_Function_Body
                       (Get_Subprogram_Specification (El));
                  end if;
               end if;
            when Iir_Kind_Type_Declaration
               | Iir_Kind_Anonymous_Type_Declaration =>
               Chap3.Translate_Type_Subprograms (El, What);
               Chap7.Init_Implicit_Subprogram_Infos (Infos);
            when Iir_Kind_Protected_Type_Body =>
               if Do_Specs then
                  Chap3.Translate_Protected_Type_Body (El);
               end if;
               if Do_Bodies then
                  Chap3.Translate_Protected_Type_Body_Subprograms_Spec (El);
                  Chap3.Translate_Protected_Type_Body_Subprograms_Body (El);
               end if;
            when Iir_Kind_Package_Declaration
              | Iir_Kind_Package_Body =>
               declare
                  Mark  : Id_Mark_Type;
               begin
                  Push_Identifier_Prefix (Mark, Get_Identifier (El));
                  Translate_Declaration_Chain_Subprograms (El, What);
                  Pop_Identifier_Prefix (Mark);
               end;
            when Iir_Kind_Package_Instantiation_Declaration =>
               if Get_Macro_Expanded_Flag
                 (Get_Uninstantiated_Package_Decl (El))
               then
                  declare
                     Bod : constant Iir := Get_Instance_Package_Body (El);
                     Mark  : Id_Mark_Type;
                  begin
                     Push_Identifier_Prefix (Mark, Get_Identifier (El));
                     Translate_Declaration_Chain_Subprograms (El, What);
                     if Is_Valid (Bod)
                       and then Global_Storage /= O_Storage_External
                     then
                        Translate_Declaration_Chain_Subprograms (Bod, What);
                     end if;
                     Pop_Identifier_Prefix (Mark);
                  end;
               end if;
            when others =>
               null;
         end case;
         El := Get_Chain (El);
      end loop;
   end Translate_Declaration_Chain_Subprograms;

   procedure Elab_Declaration_Chain (Parent : Iir; Need_Final : out Boolean)
   is
      Decl : Iir;
   begin
      Decl := Get_Declaration_Chain (Parent);
      Need_Final := False;
      while Decl /= Null_Iir loop
         case Get_Kind (Decl) is
            when Iir_Kind_Use_Clause =>
               null;
            when Iir_Kind_Component_Declaration =>
               null;
            when Iir_Kind_Configuration_Specification =>
               null;
            when Iir_Kind_Disconnection_Specification =>
               Chap5.Elab_Disconnection_Specification (Decl);

            when Iir_Kind_Type_Declaration
               | Iir_Kind_Anonymous_Type_Declaration =>
               Chap3.Elab_Type_Declaration (Decl);
            when Iir_Kind_Subtype_Declaration =>
               Chap3.Elab_Subtype_Declaration (Decl);

            when Iir_Kind_Protected_Type_Body =>
               null;

               --when Iir_Kind_Signal_Declaration =>
               --   Chap1.Elab_Signal (Decl);
            when Iir_Kind_Variable_Declaration
              | Iir_Kind_Constant_Declaration =>
               --  Do not call Open_Temp/Close_Temp, as objects may be created
               --  using alloca (which has a scope life) or on the secondary
               --  stack.
               Elab_Object (Decl);
               if Get_Kind (Get_Type (Decl))
                 = Iir_Kind_Protected_Type_Declaration
               then
                  Need_Final := True;
               end if;

            when Iir_Kind_Signal_Declaration =>
               Elab_Signal_Declaration (Decl, Parent, False);

            when Iir_Kind_Object_Alias_Declaration =>
               Elab_Object_Alias_Declaration (Decl);

            when Iir_Kind_Non_Object_Alias_Declaration =>
               null;

            when Iir_Kind_File_Declaration =>
               Elab_File_Declaration (Decl);
               Need_Final := True;

            when Iir_Kind_Attribute_Declaration =>
               --  An attribute declaration can only have a type mark.
               null;

            when Iir_Kind_Attribute_Specification =>
               Chap5.Elab_Attribute_Specification (Decl);

            when Iir_Kind_Function_Declaration
               | Iir_Kind_Procedure_Declaration =>
               if not Is_Implicit_Subprogram (Decl)
                 and then Get_Info (Decl) /= null
               then
                  Chap2.Elab_Subprogram_Interfaces (Decl);
               end if;
            when Iir_Kind_Function_Body
               | Iir_Kind_Procedure_Body =>
               null;

            when Iir_Kind_Signal_Attribute_Declaration =>
               declare
                  Sig : Iir;
               begin
                  Sig := Get_Signal_Attribute_Chain (Decl);
                  while Is_Valid (Sig) loop
                     case Iir_Kinds_Signal_Attribute (Get_Kind (Sig)) is
                        when Iir_Kind_Stable_Attribute
                          | Iir_Kind_Quiet_Attribute
                          | Iir_Kind_Transaction_Attribute =>
                           Elab_Signal_Attribute (Sig);
                        when Iir_Kind_Delayed_Attribute =>
                           Elab_Signal_Delayed_Attribute (Sig);
                     end case;
                     Sig := Get_Attr_Chain (Sig);
                  end loop;
               end;

            when Iir_Kind_Group_Template_Declaration
               | Iir_Kind_Group_Declaration =>
               null;

            when Iir_Kind_Package_Declaration =>
               Chap2.Elab_Package (Decl, Get_Package_Header (Decl));
               --  FIXME: finalizer
            when Iir_Kind_Package_Body =>
               declare
                  Nested_Final : Boolean;
               begin
                  Elab_Declaration_Chain (Decl, Nested_Final);
                  Need_Final := Need_Final or Nested_Final;
               end;

            when Iir_Kind_Package_Instantiation_Declaration =>
               --  FIXME: finalizers ?
               Chap2.Elab_Package_Instantiation_Declaration (Decl);

            when Iir_Kind_Psl_Default_Clock =>
               null;
            when Iir_Kind_Psl_Declaration =>
               null;

            when others =>
               Error_Kind ("elab_declaration_chain", Decl);
         end case;

         Decl := Get_Chain (Decl);
      end loop;
   end Elab_Declaration_Chain;

   procedure Final_Declaration_Chain (Parent : Iir; Deallocate : Boolean)
   is
      Decl : Iir;
   begin
      Decl := Get_Declaration_Chain (Parent);
      while Decl /= Null_Iir loop
         case Get_Kind (Decl) is
            when Iir_Kind_File_Declaration =>
               Final_File_Declaration (Decl);
            when Iir_Kind_Variable_Declaration =>
               if Get_Kind (Get_Type (Decl))
                 = Iir_Kind_Protected_Type_Declaration
               then
                  Fini_Protected_Object (Decl);
               end if;
               if Deallocate then
                  Fini_Object (Decl);
               end if;
            when Iir_Kind_Constant_Declaration =>
               if Deallocate then
                  Fini_Object (Decl);
               end if;
            when others =>
               null;
         end case;

         Decl := Get_Chain (Decl);
      end loop;
   end Final_Declaration_Chain;

   type Conv_Mode is (Conv_Mode_In, Conv_Mode_Out);

   --  Create subprogram for an association conversion.
   --  STMT is the statement/block_header containing the association.
   --  BLOCK is the architecture/block containing the instance.
   --  ASSOC is the association and MODE the conversion to work on.
   --  CONV_INFO is the result place holder.
   --  BASE_BLOCK is the base architecture/block containing the instance.
   --  ENTITY is the entity/component instantiated (null for block_stmt)
   procedure Translate_Association_Subprogram
     (Stmt       : Iir;
      Block      : Iir;
      Assoc      : Iir;
      Inter      : Iir;
      Mode       : Conv_Mode;
      Conv_Info  : in out Assoc_Conv_Info;
      Num        : Iir_Int32;
      Base_Block : Iir;
      Entity     : Iir)
   is
      Formal     : constant Iir := Get_Association_Formal (Assoc, Inter);
      Actual     : constant Iir := Get_Actual (Assoc);
      Block_Info : constant Block_Info_Acc := Get_Info (Base_Block);

      Mark2, Mark3      : Id_Mark_Type;
      Inter_List        : O_Inter_List;
      In_Type, Out_Type : Iir;
      In_Info, Out_Info : Type_Info_Acc;
      El_List           : O_Element_List;
      Stmt_Info         : Block_Info_Acc;
      Entity_Info       : Ortho_Info_Acc;
      Var_Data          : O_Dnode;

      --  Variables for body.
      E           : O_Enode;
      V           : O_Dnode;
      V1          : O_Lnode;
      V_Out       : Mnode;
      R           : O_Enode;
      Constr      : O_Assoc_List;
      Subprg_Info : Subprg_Info_Acc;
      Res         : Mnode;
      M1          : Mnode;
      Imp         : Iir;
      Func        : Iir;
      Obj         : Iir;  --  Method object for function conversion
      Obj_Type    : Iir;  --  Valid only if OBJ is valid
   begin
      case Mode is
         when Conv_Mode_In =>
            --  IN: from actual to formal.
            Push_Identifier_Prefix (Mark2, "CONVIN");
            Out_Type := Get_Type (Formal);
            In_Type := Get_Type (Actual);
            Imp := Get_Actual_Conversion (Assoc);

         when Conv_Mode_Out =>
            --  OUT: from formal to actual.
            Push_Identifier_Prefix (Mark2, "CONVOUT");
            In_Type := Get_Type (Formal);
            Out_Type := Get_Type (Actual);
            Imp := Get_Formal_Conversion (Assoc);

      end case;
      --  Add interface name and a unique number in case of individual assoc.
      Push_Identifier_Prefix (Mark3, Get_Identifier (Inter), Num);

      --  Handle anonymous subtypes.
      Chap3.Translate_Anonymous_Subtype_Definition (Out_Type, False);
      Chap3.Translate_Anonymous_Subtype_Definition (In_Type, False);
      Out_Info := Get_Info (Out_Type);
      In_Info := Get_Info (In_Type);

      if Get_Kind (Imp) = Iir_Kind_Function_Call then
         Obj := Get_Method_Object (Imp);
         if Is_Valid (Obj) then
            Obj_Type := Get_Type (Obj);
         end if;
      else
         Obj := Null_Iir;
      end if;

      --  Start record containing data for the conversion function.
      Start_Record_Type (El_List);

      --  Add instance field.
      Conv_Info.Instance_Block := Base_Block;
      New_Record_Field
        (El_List, Conv_Info.Instance_Field, Wki_Instance,
         Block_Info.Block_Decls_Ptr_Type);

      --  Add instance field for the entity in case of direct instantiation.
      if Entity /= Null_Iir then
         Conv_Info.Instantiated_Entity := Entity;
         Entity_Info := Get_Info (Entity);
         declare
            Ptr : O_Tnode;
         begin
            if Entity_Info.Kind = Kind_Component then
               Ptr := Entity_Info.Comp_Ptr_Type;
            else
               Ptr := Entity_Info.Block_Decls_Ptr_Type;
            end if;
            New_Record_Field
              (El_List, Conv_Info.Instantiated_Field,
               Get_Identifier ("instantiated"), Ptr);
         end;
      else
         Conv_Info.Instantiated_Entity := Null_Iir;
         Conv_Info.Instantiated_Field := O_Fnode_Null;
      end if;

      if Obj /= Null_Iir then
         New_Record_Field
           (El_List, Conv_Info.Method_Object, Get_Identifier ("obj"),
            Get_Info (Obj_Type).Ortho_Ptr_Type (Mode_Value));
      else
         Conv_Info.Method_Object := O_Fnode_Null;
      end if;

      --  Add inputs, which is a pointer to the signal.
      New_Record_Field
        (El_List, Conv_Info.In_Sig_Field, Get_Identifier ("sig_in"),
         Get_Object_Ptr_Type (In_Info, Mode_Signal));
      New_Record_Field
        (El_List, Conv_Info.In_Val_Field, Get_Identifier ("val_in"),
         Get_Object_Ptr_Type (In_Info, Mode_Value));

      --  Add output.
      New_Record_Field
        (El_List, Conv_Info.Out_Sig_Field, Get_Identifier ("sig_out"),
         Get_Object_Type (Out_Info, Mode_Signal));
      New_Record_Field
        (El_List, Conv_Info.Out_Val_Field, Get_Identifier ("val_out"),
         Get_Object_Type (Out_Info, Mode_Value));

      Finish_Record_Type (El_List, Conv_Info.Record_Type);
      New_Type_Decl (Create_Identifier ("DTYPE"), Conv_Info.Record_Type);
      Conv_Info.Record_Ptr_Type := New_Access_Type (Conv_Info.Record_Type);
      New_Type_Decl (Create_Identifier ("DPTR"), Conv_Info.Record_Ptr_Type);

      --  Declare the subprogram.
      Start_Procedure_Decl
        (Inter_List, Create_Identifier, O_Storage_Private);
      New_Interface_Decl
        (Inter_List, Var_Data, Get_Identifier ("data"),
         Conv_Info.Record_Ptr_Type);
      Finish_Subprogram_Decl (Inter_List, Conv_Info.Subprg);

      Start_Subprogram_Body (Conv_Info.Subprg);
      Push_Local_Factory;
      Open_Temp;

      --  Add an access to local block.
      V := Create_Temp_Init
        (Block_Info.Block_Decls_Ptr_Type,
         New_Value_Selected_Acc_Value (New_Obj (Var_Data),
                                       Conv_Info.Instance_Field));
      Set_Scope_Via_Param_Ptr (Block_Info.Block_Scope, V);

      --  Add an access to instantiated entity.
      --  This may be used to do some type checks.
      if Conv_Info.Instantiated_Entity /= Null_Iir then
         declare
            Ptr_Type : O_Tnode;
         begin
            if Entity_Info.Kind = Kind_Component then
               Ptr_Type := Entity_Info.Comp_Ptr_Type;
            else
               Ptr_Type := Entity_Info.Block_Decls_Ptr_Type;
            end if;
            V := Create_Temp_Init
              (Ptr_Type,
               New_Value_Selected_Acc_Value (New_Obj (Var_Data),
                 Conv_Info.Instantiated_Field));
            if Entity_Info.Kind = Kind_Component then
               Set_Scope_Via_Param_Ptr (Entity_Info.Comp_Scope, V);
            else
               Set_Scope_Via_Param_Ptr (Entity_Info.Block_Scope, V);
            end if;
         end;
      end if;

      --  Add access to the instantiation-specific data.
      --  This is used only for anonymous subtype variables.
      --  FIXME: what if STMT is a binding_indication ?
      Stmt_Info := Get_Info (Stmt);
      if Stmt_Info /= null
        and then Has_Scope_Type (Stmt_Info.Block_Scope)
      then
         Set_Scope_Via_Field (Stmt_Info.Block_Scope,
                              Stmt_Info.Block_Parent_Field,
                              Get_Info (Block).Block_Scope'Access);
      end if;

      --  Read signal value.
      case Mode is
         when Conv_Mode_In =>
            V1 := New_Selected_Acc_Value (New_Obj (Var_Data),
                                          Conv_Info.In_Val_Field);
            R := M2E (Lop2M (V1, In_Info, Mode_Value));
         when Conv_Mode_Out =>
            V1 := New_Selected_Acc_Value (New_Obj (Var_Data),
                                          Conv_Info.In_Sig_Field);
            M1 := Lop2M (V1, In_Info, Mode_Signal);
            M1 := Chap7.Translate_Signal_Driving_Value (M1, In_Type);
            R := M2E (M1);
      end case;

      case Get_Kind (Imp) is
         when Iir_Kind_Function_Call =>
            Func := Get_Implementation (Imp);
            R := Chap7.Translate_Implicit_Conv
              (R, In_Type,
               Get_Type (Get_Interface_Declaration_Chain (Func)),
               Mode_Value, Assoc);

            --  Create result value.
            Subprg_Info := Get_Info (Func);

            if Subprg_Info.Use_Stack2 then
               Create_Temp_Stack2_Mark;
            end if;

            if Subprg_Info.Res_Interface /= O_Dnode_Null then
               --  Composite result.
               --  If we need to allocate, do it before starting the call!
               declare
                  Res_Type : constant Iir := Get_Return_Type (Func);
                  Res_Info : constant Type_Info_Acc := Get_Info (Res_Type);
               begin
                  Res := Create_Temp (Res_Info);
                  if Res_Info.Type_Mode /= Type_Mode_Fat_Array then
                     Chap4.Allocate_Complex_Object
                       (Res_Type, Alloc_Stack, Res);
                  end if;
               end;
            end if;

            --  Call conversion function.
            Start_Association (Constr, Subprg_Info.Subprg_Node);

            if Subprg_Info.Res_Interface /= O_Dnode_Null then
               --  Composite result.
               New_Association (Constr, M2E (Res));
            end if;

            if Obj /= Null_Iir then
               --  Protected object.
               New_Association
                 (Constr,
                  New_Value (New_Selected_Acc_Value
                               (New_Obj (Var_Data),
                                Conv_Info.Method_Object)));
            else
               Subprgs.Add_Subprg_Instance_Assoc
                 (Constr, Subprg_Info.Subprg_Instance);
            end if;

            New_Association (Constr, R);

            if Subprg_Info.Res_Interface /= O_Dnode_Null then
               --  Composite result.
               New_Procedure_Call (Constr);
               E := M2E (Res);
            else
               E := New_Function_Call (Constr);
            end if;
            Res := E2M
              (Chap7.Translate_Implicit_Conv (E, Get_Return_Type (Func),
                                              Out_Type, Mode_Value, Imp),
               Get_Info (Out_Type), Mode_Value);

         when Iir_Kind_Type_Conversion =>
            declare
               Conv_Type : constant Iir := Get_Type (Imp);
            begin
               E := Chap7.Translate_Type_Conversion
                 (R, In_Type, Conv_Type, Assoc);
               E := Chap7.Translate_Implicit_Conv
                 (E, Conv_Type, Out_Type, Mode_Value, Imp);
               Res := E2M (E, Get_Info (Out_Type), Mode_Value);
            end;

         when others =>
            Error_Kind ("Translate_Association_Subprogram", Imp);
      end case;

      --  Assign signals.
      case Mode is
         when Conv_Mode_In =>
            V1 := New_Selected_Acc_Value (New_Obj (Var_Data),
                                          Conv_Info.Out_Val_Field);
            V_Out := Lo2M (V1, Out_Info, Mode_Value);
            Chap7.Translate_Assign (V_Out, M2E (Res), Formal, Out_Type, Assoc);
         when Conv_Mode_Out =>
            V1 := New_Selected_Acc_Value (New_Obj (Var_Data),
                                          Conv_Info.Out_Sig_Field);
            V_Out := Lo2M (V1, Out_Info, Mode_Signal);
            Chap7.Set_Driving_Value (V_Out, Out_Type, Res);
      end case;

      Close_Temp;
      if Stmt_Info /= null
        and then Has_Scope_Type (Stmt_Info.Block_Scope)
      then
         Clear_Scope (Stmt_Info.Block_Scope);
      end if;
      if Conv_Info.Instantiated_Entity /= Null_Iir then
         if Entity_Info.Kind = Kind_Component then
            Clear_Scope (Entity_Info.Comp_Scope);
         else
            Clear_Scope (Entity_Info.Block_Scope);
         end if;
      end if;
      Clear_Scope (Block_Info.Block_Scope);

      Pop_Local_Factory;
      Finish_Subprogram_Body;

      Pop_Identifier_Prefix (Mark3);
      Pop_Identifier_Prefix (Mark2);
   end Translate_Association_Subprogram;

   procedure Translate_Inertial_Subprogram
     (Stmt       : Iir;
      Block      : Iir;
      Assoc      : Iir;
      Inter      : Iir;
      Num        : Iir_Int32;
      Base_Block : Iir;
      Entity     : Iir)
   is
      pragma Unreferenced (Num);
      Formal     : constant Iir := Get_Association_Formal (Assoc, Inter);
      Actual     : constant Iir := Get_Actual (Assoc);
      Block_Info : constant Block_Info_Acc := Get_Info (Base_Block);
      Assoc_Info  : Inertial_Info_Acc;
      Inter_List  : O_Inter_List;
      Entity_Info : Ortho_Info_Acc;
      Targ : Mnode;
      Val : Mnode;
   begin
      --  Declare the subprogram.
      Assoc_Info := Add_Info (Assoc, Kind_Inertial_Assoc);
      Start_Procedure_Decl
        (Inter_List, Create_Identifier (Inter, "INERTIAL"),
         O_Storage_Private);
      New_Interface_Decl (Inter_List, Assoc_Info.Inertial_Inst,
                          Wki_Instance, Block_Info.Block_Decls_Ptr_Type);
      Finish_Subprogram_Decl (Inter_List, Assoc_Info.Inertial_Proc);

      --  The body.
      New_Debug_Line_Decl (Get_Line_Number (Assoc));
      Start_Subprogram_Body (Assoc_Info.Inertial_Proc);
      Push_Local_Factory;
      --  Access for actual.
      Assoc_Info.Inertial_Block := Base_Block;
      Set_Scope_Via_Param_Ptr (Block_Info.Block_Scope,
                               Assoc_Info.Inertial_Inst);

      Open_Temp;

      --  Access for formals.
      if Entity /= Null_Iir then
         Entity_Info := Get_Info (Entity);
         declare
            Inst_Info  : constant Block_Info_Acc := Get_Info (Stmt);
            V : O_Dnode;
         begin
            if Entity_Info.Kind = Kind_Component then
               Set_Scope_Via_Field (Entity_Info.Comp_Scope,
                                    Inst_Info.Block_Link_Field,
                                    Block_Info.Block_Scope'Access);
            else
               --  Get access to the directly instantiated entity through
               --  the link.  The link is a __ghdl_component_link_type which
               --  points to the __ghdl_entity_link_type of the entity.
               V := Create_Temp_Init
                 (Entity_Info.Block_Decls_Ptr_Type,
                  New_Convert_Ov
                    (New_Value
                       (New_Selected_Element
                          (New_Selected_Element
                             (New_Access_Element (Get_Instance_Access (Block)),
                              Inst_Info.Block_Link_Field),
                           Rtis.Ghdl_Component_Link_Instance)),
                     Entity_Info.Block_Decls_Ptr_Type));
               Set_Scope_Via_Param_Ptr (Entity_Info.Block_Scope, V);
            end if;

         end;
      end if;

      --  Access for formal.
      --  1. Translate target (translate_name)
      Targ := Chap6.Translate_Name (Formal, Mode_Signal);

      --  2. Translate expression
      Val := Chap7.Translate_Expression (Actual, Get_Type (Formal));

      --  3. Check bounds match
      --  TODO

      --  4. Call Gen_Simple_Signal_Assign
      Chap8.Translate_Inertial_Assignment
        (Targ, Get_Type (Formal), Val, Assoc);

      --  Set_Map_Env (Formal_Env);

      if Entity /= Null_Iir then
         if Entity_Info.Kind = Kind_Component then
            Clear_Scope (Entity_Info.Comp_Scope);
         else
            Clear_Scope (Entity_Info.Block_Scope);
         end if;
      end if;

      Close_Temp;

      Clear_Scope (Block_Info.Block_Scope);
      Pop_Local_Factory;
      Finish_Subprogram_Body;
   end Translate_Inertial_Subprogram;

   --  Create subprograms for associations: conversions and inertial assocs.
   --  ENTITY is null for block_statement.
   procedure Translate_Association_Subprograms
     (Stmt : Iir; Block : Iir; Base_Block : Iir; Entity : Iir)
   is
      Assoc : Iir;
      Assoc_Inter : Iir;
      Inter : Iir;
      Info  : Assoc_Info_Acc;
      Num : Iir_Int32;
   begin
      Assoc := Get_Port_Map_Aspect_Chain (Stmt);
      Num := 0;
      if Is_Null (Entity) then
         Assoc_Inter := Get_Port_Chain (Stmt);
      else
         Assoc_Inter := Get_Port_Chain (Entity);
      end if;
      while Assoc /= Null_Iir loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_By_Name =>
               Info := null;
               if Get_Actual_Conversion (Assoc) /= Null_Iir then
                  Info := Add_Info (Assoc, Kind_Assoc);
                  Translate_Association_Subprogram
                    (Stmt, Block, Assoc, Inter, Conv_Mode_In, Info.Assoc_In,
                     Num, Base_Block, Entity);
                  Num := Num + 1;
               end if;
               if Get_Formal_Conversion (Assoc) /= Null_Iir then
                  if Info = null then
                     Info := Add_Info (Assoc, Kind_Assoc);
                  end if;
                  Translate_Association_Subprogram
                    (Stmt, Block, Assoc, Inter, Conv_Mode_Out, Info.Assoc_Out,
                     Num, Base_Block, Entity);
                  Num := Num + 1;
               end if;
            when Iir_Kind_Association_Element_By_Expression =>
               if Get_Expr_Staticness (Get_Actual (Assoc)) = None then
                  Translate_Inertial_Subprogram
                    (Stmt, Block, Assoc, Inter, Num, Base_Block, Entity);
               end if;
            when Iir_Kind_Association_Element_By_Individual
              | Iir_Kind_Association_Element_Open =>
               null;
            when others =>
               Error_Kind ("translate_association_subprograms", Assoc);
         end case;
         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Translate_Association_Subprograms;

   --  Register conversion CONV in association between SIG_IN and SIG_OUT.
   --  This procedure allocates a record data (described by INFO), fill it
   --   with addresses of signals and register it to REG_SUBPRG.
   procedure Elab_Conversion (Sig_In     : Iir;
                              Sig_Out    : Iir;
                              Conv       : Iir;
                              Reg_Subprg : O_Dnode;
                              Info       : Assoc_Conv_Info;
                              Dest_Sig   : out Mnode)
   is
      Out_Type : constant Iir := Get_Type (Sig_Out);
      Out_Info : constant Type_Info_Acc := Get_Info (Out_Type);
      In_Type : constant Iir := Get_Type (Sig_In);
      In_Info : constant Type_Info_Acc := Get_Info (In_Type);
      Src_Sig  : Mnode;
      Src_Val  : Mnode;
      Dest_Val : Mnode;
      Constr   : O_Assoc_List;
      Var_Data : O_Dnode;
      Data     : Elab_Signal_Data;
   begin
      --  Allocate data for the subprogram.
      Var_Data := Create_Temp (Info.Record_Ptr_Type);
      New_Assign_Stmt
        (New_Obj (Var_Data),
         Gen_Alloc (Alloc_System,
                    New_Lit (New_Sizeof (Info.Record_Type, Ghdl_Index_Type)),
                    Info.Record_Ptr_Type));

      --  Set instance.
      New_Assign_Stmt
        (New_Selected_Acc_Value (New_Obj (Var_Data), Info.Instance_Field),
         Get_Instance_Access (Info.Instance_Block));

      --  Set instantiated unit instance (if any).
      if Info.Instantiated_Entity /= Null_Iir then
         declare
            Inst_Addr : O_Enode;
            Inst_Info : Ortho_Info_Acc;
         begin
            if Get_Kind (Info.Instantiated_Entity)
              = Iir_Kind_Component_Declaration
            then
               Inst_Info := Get_Info (Info.Instantiated_Entity);
               Inst_Addr := New_Address
                 (Get_Instance_Ref (Inst_Info.Comp_Scope),
                  Inst_Info.Comp_Ptr_Type);
            else
               Inst_Addr := Get_Instance_Access (Info.Instantiated_Entity);
            end if;
            New_Assign_Stmt
              (New_Selected_Acc_Value (New_Obj (Var_Data),
                                       Info.Instantiated_Field),
               Inst_Addr);
         end;
      end if;

      if Info.Method_Object /= O_Fnode_Null then
         New_Assign_Stmt
           (New_Selected_Acc_Value (New_Obj (Var_Data),
                                    Info.Method_Object),
            M2E (Chap6.Translate_Name
                   (Get_Method_Object (Conv), Mode_Value)));
      end if;

      --  Set input.
      Chap6.Translate_Signal_Name (Sig_In, Src_Sig, Src_Val);
      Src_Sig := Stabilize (Src_Sig, True);

      Assign_Obj_Ptr (Lop2M (New_Selected_Acc_Value (New_Obj (Var_Data),
                                                     Info.In_Sig_Field),
                             In_Info, Mode_Signal),
                      Src_Sig);
      Assign_Obj_Ptr (Lop2M (New_Selected_Acc_Value (New_Obj (Var_Data),
                                                     Info.In_Val_Field),
                             In_Info, Mode_Value),
                      Src_Val);

      --  Create a copy of SIG_OUT.
      Dest_Sig := Lo2M (New_Selected_Acc_Value (New_Obj (Var_Data),
                                                Info.Out_Sig_Field),
                        Out_Info, Mode_Signal);
      Chap4.Allocate_Complex_Object (Out_Type, Alloc_System, Dest_Sig);
      Dest_Val := Lo2M (New_Selected_Acc_Value (New_Obj (Var_Data),
                                                Info.Out_Val_Field),
                        Out_Info, Mode_Value);
      Chap4.Allocate_Complex_Object (Out_Type, Alloc_System, Dest_Val);
      --  Note: NDEST will be assigned by ELAB_SIGNAL.
      Dest_Sig := Lo2M (New_Selected_Acc_Value (New_Obj (Var_Data),
                                                Info.Out_Sig_Field),
                        Out_Info, Mode_Signal);
      Dest_Val := Lo2M (New_Selected_Acc_Value (New_Obj (Var_Data),
                                                Info.Out_Val_Field),
                        Out_Info, Mode_Value);
      Data := Elab_Signal_Data'(Value => Dest_Val,
                                Has_Val => False,
                                Already_Resolved => True,
                                Init_Val => Mnode_Null,
                                Check_Null => False,
                                If_Stmt => null);
      Elab_Signal (Dest_Sig, Out_Type, Data);

      Dest_Sig := Lo2M (New_Selected_Acc_Value (New_Obj (Var_Data),
                                                Info.Out_Sig_Field),
                        Out_Info, Mode_Signal);
      Dest_Sig := Stabilize (Dest_Sig, True);

      --  Register.
      Start_Association (Constr, Reg_Subprg);
      New_Association
        (Constr, New_Lit (New_Subprogram_Address (Info.Subprg,
                                                  Ghdl_Ptr_Type)));
      New_Association
        (Constr, New_Convert_Ov (New_Obj_Value (Var_Data), Ghdl_Ptr_Type));

      New_Association
        (Constr,
         New_Convert_Ov (M2E (Get_Leftest_Signal (Src_Sig, Get_Type (Sig_In))),
                         Ghdl_Signal_Ptr));
      New_Association
        (Constr, Get_Nbr_Signals (Src_Sig, Get_Type (Sig_In)));

      New_Association
        (Constr,
         New_Convert_Ov (M2E (Get_Leftest_Signal (Dest_Sig,
                                                  Get_Type (Sig_Out))),
                         Ghdl_Signal_Ptr));
      New_Association
        (Constr, Get_Nbr_Signals (Dest_Sig, Get_Type (Sig_Out)));

      New_Procedure_Call (Constr);
   end Elab_Conversion;

   --  In conversion: from actual to formal.
   procedure Elab_In_Conversion (Assoc : Iir; Formal : Iir; Ndest : out Mnode)
   is
      Assoc_Info : constant Assoc_Info_Acc := Get_Info (Assoc);
   begin
      Elab_Conversion
        (Get_Actual (Assoc), Formal, Get_Actual_Conversion (Assoc),
         Ghdl_Signal_In_Conversion, Assoc_Info.Assoc_In, Ndest);
   end Elab_In_Conversion;

   --  Out conversion: from formal to actual.
   procedure Elab_Out_Conversion (Assoc : Iir; Formal : Iir; Ndest : out Mnode)
   is
      --  Note: because it's an out conversion, the formal of ASSOC is set.
      --  Still pass INTER for coherence with Elab_In_Conversion.
      Assoc_Info : constant Assoc_Info_Acc := Get_Info (Assoc);
   begin
      Elab_Conversion
        (Formal, Get_Actual (Assoc), Get_Formal_Conversion (Assoc),
         Ghdl_Signal_Out_Conversion, Assoc_Info.Assoc_Out, Ndest);
   end Elab_Out_Conversion;

   --  Create a record that describe thes location of an IIR node and
   --  returns the address of it.
   function Get_Location (N : Iir) return O_Dnode
   is
      Constr : O_Record_Aggr_List;
      Aggr   : O_Cnode;
      Name   : Name_Id;
      Line   : Natural;
      Col    : Natural;
      C      : O_Dnode;
   begin
      Files_Map.Location_To_Position (Get_Location (N), Name, Line, Col);

      New_Const_Decl (C, Create_Uniq_Identifier, O_Storage_Private,
                      Ghdl_Location_Type_Node);
      Start_Init_Value (C);
      Start_Record_Aggr (Constr, Ghdl_Location_Type_Node);
      New_Record_Aggr_El
        (Constr, New_Global_Address (New_Global (Current_Filename_Node),
                                     Char_Ptr_Type));
      New_Record_Aggr_El (Constr, New_Signed_Literal (Ghdl_I32_Type,
                          Integer_64 (Line)));
      New_Record_Aggr_El (Constr, New_Signed_Literal (Ghdl_I32_Type,
                          Integer_64 (Col)));
      Finish_Record_Aggr (Constr, Aggr);
      Finish_Init_Value (C, Aggr);

      return C;
      --return New_Global_Address (C, Ghdl_Location_Ptr_Node);
   end Get_Location;
end Trans.Chap4;
