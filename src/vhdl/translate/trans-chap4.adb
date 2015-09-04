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

with Errorout; use Errorout;
with Files_Map;
with Iirs_Utils; use Iirs_Utils;
with Std_Package; use Std_Package;
with Canon;
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
      if Is_Complex_Type (Tinfo) then
         case Tinfo.Type_Mode is
            when Type_Mode_Fat_Array =>
               return Tinfo.Ortho_Type (Kind);
            when Type_Mode_Record
               | Type_Mode_Array
               | Type_Mode_Protected =>
               --  For a complex type, use a pointer.
               return Tinfo.Ortho_Ptr_Type (Kind);
            when others =>
               raise Internal_Error;
         end case;
      else
         return Tinfo.Ortho_Type (Kind);
      end if;
   end Get_Object_Type;

   procedure Create_Object (El : Iir)
   is
      Obj_Type : O_Tnode;
      Info     : Object_Info_Acc;
      Tinfo    : Type_Info_Acc;
      Def      : Iir;
      Val      : Iir;
      Storage  : O_Storage;
      Deferred : Iir;
   begin
      Def := Get_Type (El);
      Val := Get_Default_Value (El);

      --  Be sure the object type was translated.
      if Get_Kind (El) = Iir_Kind_Constant_Declaration
        and then Get_Deferred_Declaration_Flag (El) = False
        and then Get_Deferred_Declaration (El) /= Null_Iir
      then
         --  This is a full constant declaration which complete a previous
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
         Chap3.Translate_Object_Subtype (El);
         Info := Add_Info (El, Kind_Object);
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
                    (Create_Var_Identifier (El),
                     Obj_Type, Global_Storage);
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
      Sig_Type     : O_Tnode;
      Type_Info    : Type_Info_Acc;
      Info         : Ortho_Info_Acc;
   begin
      Chap3.Translate_Object_Subtype (Decl);

      Type_Info := Get_Info (Sig_Type_Def);
      Sig_Type := Get_Object_Type (Type_Info, Mode_Signal);
      pragma Assert (Sig_Type /= O_Tnode_Null);

      Info := Add_Info (Decl, Kind_Signal);

      Info.Signal_Sig := Create_Var (Create_Var_Identifier (Decl), Sig_Type);

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
      Sig_Type     : constant O_Tnode := Type_Info.Ortho_Type (Mode_Signal);
      Info         : Ortho_Info_Acc;
   begin
      --  This has been disabled since DECL can have an anonymous subtype,
      --  and DECL has no identifiers, which causes translate_object_subtype
      --  to crash.
      --  Note: DECL can only be a iir_kind_delayed_attribute.
      --Chap3.Translate_Object_Subtype (Decl);
      pragma Assert (Sig_Type /= O_Tnode_Null);

      Info := Add_Info (Decl, Kind_Signal);

      Info.Signal_Sig := Create_Var (Create_Uniq_Identifier, Sig_Type);
   end Create_Implicit_Signal;

   procedure Create_File_Object (El : Iir_File_Declaration)
   is
      Obj_Type     : O_Tnode;
      Info         : Ortho_Info_Acc;
      Obj_Type_Def : Iir;
   begin
      Obj_Type_Def := Get_Type (El);
      Obj_Type := Get_Ortho_Type (Obj_Type_Def, Mode_Value);

      Info := Add_Info (El, Kind_Object);

      Info.Object_Var := Create_Var (Create_Var_Identifier (El), Obj_Type);
   end Create_File_Object;

   procedure Create_Package_Interface (Inter : Iir)
   is
      Info     : Ortho_Info_Acc;
      Pkg      : constant Iir := Get_Named_Entity
        (Get_Uninstantiated_Package_Name (Inter));
      Pkg_Info : constant Ortho_Info_Acc := Get_Info (Pkg);
   begin
      Chap2.Instantiate_Info_Package (Inter);
      Info := Get_Info (Inter);

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
   end Create_Package_Interface;

   procedure Allocate_Complex_Object (Obj_Type   : Iir;
                                      Alloc_Kind : Allocation_Kind;
                                      Var        : in out Mnode)
   is
      Type_Info : constant Type_Info_Acc := Get_Type_Info (Var);
      Kind      : constant Object_Kind_Type := Get_Object_Kind (Var);
      Targ      : Mnode;
   begin
      --  Cannot allocate unconstrained object (since size is unknown).
      pragma Assert (Type_Info.Type_Mode /= Type_Mode_Fat_Array);

      if not Is_Complex_Type (Type_Info) then
         --  Object is not complex.
         return;
      end if;

      if Type_Info.C (Kind).Builder_Need_Func
        and then not Is_Stable (Var)
      then
         Targ := Create_Temp (Type_Info, Kind);
      else
         Targ := Var;
      end if;

      --  Allocate variable.
      New_Assign_Stmt (M2Lp (Targ),
                       Gen_Alloc (Alloc_Kind,
                                  Chap3.Get_Object_Size (Var, Obj_Type),
                                  Type_Info.Ortho_Ptr_Type (Kind)));

      if Type_Info.C (Kind).Builder_Need_Func then
         --  Build the type.
         Chap3.Gen_Call_Type_Builder (Targ, Obj_Type);
         if not Is_Stable (Var) then
            New_Assign_Stmt (M2Lp (Var), M2Addr (Targ));
            Var := Targ;
         end if;
      end if;
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

      if Type_Info.Type_Mode = Type_Mode_Fat_Array then
         Sobj := Stabilize (Obj);
      else
         Sobj := Obj;
      end if;
      Upper_Limit := Chap3.Get_Array_Length (Sobj, Obj_Type);

      if Type_Info.Type_Mode /= Type_Mode_Array then
         Upper_Var := Create_Temp_Init (Ghdl_Index_Type, Upper_Limit);
      else
         Upper_Var := O_Dnode_Null;
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
      Init_Object (Chap3.Index_Base (Chap3.Get_Array_Base (Sobj),
                   Obj_Type,
                   New_Obj_Value (Index)),
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
      Start_Association (Assoc, Info.T.Prot_Init_Subprg);
      Subprgs.Add_Subprg_Instance_Assoc (Assoc, Info.T.Prot_Init_Instance);
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
      Obj := Chap6.Translate_Name (Decl);
      --  Call the Finalizator.
      Start_Association (Assoc, Info.T.Prot_Final_Subprg);
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
         when Type_Mode_Record =>
            declare
               Sobj : Mnode;
               El   : Iir_Element_Declaration;
               List : Iir_List;
            begin
               Open_Temp;
               Sobj := Stabilize (Obj);
               List := Get_Elements_Declaration_List
                 (Get_Base_Type (Obj_Type));
               for I in Natural loop
                  El := Get_Nth_Element (List, I);
                  exit when El = Null_Iir;
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

   procedure Elab_Object_Storage (Obj : Iir)
   is
      Obj_Type : constant Iir := Get_Type (Obj);
      Obj_Info : constant Object_Info_Acc := Get_Info (Obj);

      Name_Node : Mnode;

      Type_Info  : Type_Info_Acc;
      Alloc_Kind : Allocation_Kind;
   begin
      --  Elaborate subtype.
      Chap3.Elab_Object_Subtype (Obj_Type);

      Type_Info := Get_Info (Obj_Type);

      --  FIXME: the object type may be a fat array!
      --  FIXME: fat array + aggregate ?

      if Type_Info.Type_Mode = Type_Mode_Protected then
         --  Protected object will be created by its INIT function.
         return;
      end if;

      if Is_Complex_Type (Type_Info)
        and then Type_Info.Type_Mode /= Type_Mode_Fat_Array
      then
         --  FIXME: avoid allocation if the value is a string and
         --  the object is a constant
         Name_Node := Get_Var (Obj_Info.Object_Var, Type_Info, Mode_Value);
         Alloc_Kind := Get_Alloc_Kind_For_Var (Obj_Info.Object_Var);
         Allocate_Complex_Object (Obj_Type, Alloc_Kind, Name_Node);
      end if;
   end Elab_Object_Storage;

   --  Generate code to create object OBJ and initialize it with value VAL.
   procedure Elab_Object_Init (Name : Mnode; Obj : Iir; Value : Iir)
   is
      Obj_Type  : constant Iir := Get_Type (Obj);
      Type_Info : constant Type_Info_Acc := Get_Info (Obj_Type);
      Obj_Info  : constant Object_Info_Acc := Get_Info (Obj);

      Name_Node  : Mnode;
      Value_Node : O_Enode;

      Alloc_Kind : Allocation_Kind;
   begin
      --  Elaborate subtype.
      Alloc_Kind := Get_Alloc_Kind_For_Var (Obj_Info.Object_Var);

      --  Note: no temporary variable region is created, as the allocation
      --  may be performed on the stack.

      if Value = Null_Iir then
         --  Performs default initialization.
         Open_Temp;
         Init_Object (Name, Obj_Type);
         Close_Temp;
      elsif Get_Kind (Value) = Iir_Kind_Aggregate then
         if Type_Info.Type_Mode = Type_Mode_Fat_Array then
            --  Allocate.
            declare
               Aggr_Type : constant Iir := Get_Type (Value);
            begin
               Chap3.Create_Array_Subtype (Aggr_Type);
               Name_Node := Stabilize (Name);
               New_Assign_Stmt
                 (M2Lp (Chap3.Get_Array_Bounds (Name_Node)),
                  M2Addr (Chap3.Get_Array_Type_Bounds (Aggr_Type)));
               Chap3.Allocate_Fat_Array_Base
                 (Alloc_Kind, Name_Node, Get_Base_Type (Aggr_Type));
            end;
         else
            Name_Node := Name;
         end if;
         Chap7.Translate_Aggregate (Name_Node, Obj_Type, Value);
      else
         Value_Node := Chap7.Translate_Expression (Value, Obj_Type);

         if Type_Info.Type_Mode = Type_Mode_Fat_Array then
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
                     Chap3.Get_Array_Bounds (S));
                  Chap3.Translate_Object_Copy
                    (Name_Node, M2Addr (S), Obj_Type);
               end if;
            end;
         else
            Chap3.Translate_Object_Copy (Name, Value_Node, Obj_Type);
         end if;
      end if;
   end Elab_Object_Init;

   --  Generate code to create object OBJ and initialize it with value VAL.
   procedure Elab_Object_Value (Obj : Iir; Value : Iir)
   is
      Name : Mnode;
   begin
      Elab_Object_Storage (Obj);
      Name := Get_Var (Get_Info (Obj).Object_Var,
                       Get_Info (Get_Type (Obj)), Mode_Value);
      Elab_Object_Init (Name, Obj, Value);
   end Elab_Object_Value;

   --  Create code to elaborate OBJ.
   procedure Elab_Object (Obj : Iir)
   is
      Value : constant Iir := Get_Default_Value (Obj);
      Obj1  : Iir;
   begin
      --  A locally static constant is pre-elaborated.
      --  (only constant can be locally static).
      if Get_Expr_Staticness (Obj) = Locally
        and then Get_Deferred_Declaration (Obj) = Null_Iir
      then
         if Get_Kind (Value) = Iir_Kind_Overflow_Literal then
            Chap6.Gen_Bound_Error (Obj);
         end if;
         return;
      end if;

      --  Set default value.
      if Get_Kind (Obj) = Iir_Kind_Constant_Declaration then
         if Get_Info (Obj).Object_Static then
            return;
         end if;
         if Get_Deferred_Declaration_Flag (Obj) then
            --  No code generation for a deferred constant.
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
      if Type_Info.Type_Mode = Type_Mode_Fat_Array then
         declare
            V : Mnode;
         begin
            Open_Temp;
            V := Chap6.Translate_Name (Obj);
            Stabilize (V);
            Chap3.Gen_Deallocate
              (New_Value (M2Lp (Chap3.Get_Array_Bounds (V))));
            Chap3.Gen_Deallocate
              (New_Value (M2Lp (Chap3.Get_Array_Base (V))));
            Close_Temp;
         end;
      elsif Is_Complex_Type (Type_Info) then
         Chap3.Gen_Deallocate
           (New_Value (M2Lp (Chap6.Translate_Name (Obj))));
      end if;
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
                            (Chap3.Get_Array_Base (Ssig), Sig_Type,
                             New_Lit (Ghdl_Index_0)),
                        Get_Element_Subtype (Sig_Type))));
               Finish_If_Stmt (If_Blk);

               return New_Obj_Value (Len);
            end;
         when Type_Mode_Record =>
            declare
               List   : constant Iir_List :=
                 Get_Elements_Declaration_List (Get_Base_Type (Sig_Type));
               El     : Iir;
               Res    : O_Enode;
               E      : O_Enode;
               Sig_El : Mnode;
               Ssig   : Mnode;
            begin
               Ssig := Stabilize (Sig);
               Res := O_Enode_Null;
               for I in Natural loop
                  El := Get_Nth_Element (List, I);
                  exit when El = Null_Iir;
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
                 (Chap3.Get_Array_Base (Res), Res_Type,
                  New_Lit (Ghdl_Index_0));
               Res_Type := Get_Element_Subtype (Res_Type);
            when Type_Mode_Record =>
               declare
                  Element : Iir;
               begin
                  Element := Get_First_Element
                    (Get_Elements_Declaration_List
                       (Get_Base_Type (Res_Type)));
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
      --  Default value of the signal.
      Val              : Mnode;
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
      Conv          : O_Tnode;
      Res           : O_Enode;
      Assoc         : O_Assoc_List;
      Init_Val      : O_Enode;
      --  For the resolution function (if any).
      Func          : Iir;
      If_Stmt       : O_If_Block;
      Targ_Ptr      : O_Dnode;
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

      case Type_Info.Type_Mode is
         when Type_Mode_B1 =>
            Create_Subprg := Ghdl_Create_Signal_B1;
            Conv := Ghdl_Bool_Type;
         when Type_Mode_E8 =>
            Create_Subprg := Ghdl_Create_Signal_E8;
            Conv := Ghdl_I32_Type;
         when Type_Mode_E32 =>
            Create_Subprg := Ghdl_Create_Signal_E32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_I32
            | Type_Mode_P32 =>
            Create_Subprg := Ghdl_Create_Signal_I32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_P64
            | Type_Mode_I64 =>
            Create_Subprg := Ghdl_Create_Signal_I64;
            Conv := Ghdl_I64_Type;
         when Type_Mode_F64 =>
            Create_Subprg := Ghdl_Create_Signal_F64;
            Conv := Ghdl_Real_Type;
         when others =>
            Error_Kind ("elab_signal_non_composite", Targ_Type);
      end case;

      if Data.Has_Val then
         Init_Val := M2E (Data.Val);
      else
         Init_Val := Get_Scalar_Initial_Value (Targ_Type);
      end if;

      Start_Association (Assoc, Create_Subprg);
      New_Association (Assoc, New_Convert_Ov (Init_Val, Conv));

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
      if Data.Has_Val then
         if Get_Type_Info (Data.Val).Type_Mode = Type_Mode_Record then
            Res.Val := Stabilize (Data.Val);
         else
            Res.Val := Chap3.Get_Array_Base (Data.Val);
         end if;
      end if;
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

   function Elab_Signal_Update_Array (Data      : Elab_Signal_Data;
                                      Targ_Type : Iir;
                                      Index     : O_Dnode)
                                         return Elab_Signal_Data
   is
   begin
      if not Data.Has_Val then
         return Data;
      else
         return Elab_Signal_Data'
           (Val => Chap3.Index_Base (Data.Val, Targ_Type,
            New_Obj_Value (Index)),
            Has_Val => True,
            If_Stmt => null,
            Already_Resolved => Data.Already_Resolved,
            Check_Null => Data.Check_Null);
      end if;
   end Elab_Signal_Update_Array;

   function Elab_Signal_Update_Record (Data      : Elab_Signal_Data;
                                       Targ_Type : Iir;
                                       El        : Iir_Element_Declaration)
                                          return Elab_Signal_Data
   is
      pragma Unreferenced (Targ_Type);
   begin
      if not Data.Has_Val then
         return Data;
      else
         return Elab_Signal_Data'
           (Val => Chap6.Translate_Selected_Element (Data.Val, El),
            Has_Val => True,
            If_Stmt => null,
            Already_Resolved => Data.Already_Resolved,
            Check_Null => Data.Check_Null);
      end if;
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
   procedure Elab_Signal_Declaration_Storage (Decl : Iir)
   is
      Sig_Type  : constant Iir := Get_Type (Decl);
      Type_Info : Type_Info_Acc;
      Name_Node : Mnode;
   begin
      New_Debug_Line_Stmt (Get_Line_Number (Decl));

      Open_Temp;

      Chap3.Elab_Object_Subtype (Sig_Type);
      Type_Info := Get_Info (Sig_Type);

      if Type_Info.Type_Mode = Type_Mode_Fat_Array then
         Name_Node := Chap6.Translate_Name (Decl);
         Name_Node := Stabilize (Name_Node);
         Chap3.Allocate_Fat_Array_Base (Alloc_System, Name_Node, Sig_Type);
      elsif Is_Complex_Type (Type_Info) then
         Name_Node := Chap6.Translate_Name (Decl);
         Allocate_Complex_Object (Sig_Type, Alloc_System, Name_Node);
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

      if Type_Info.Type_Mode = Type_Mode_Fat_Array then
         Name_Node := Get_Var (Sig_Info.Signal_Driver, Type_Info, Mode_Value);
         Name_Node := Stabilize (Name_Node);
         --  Copy bounds from signal.
         New_Assign_Stmt
           (M2Lp (Chap3.Get_Array_Bounds (Name_Node)),
            M2Addr (Chap3.Get_Array_Bounds (Chap6.Translate_Name (Decl))));
         --  Allocate base.
         Chap3.Allocate_Fat_Array_Base (Alloc_System, Name_Node, Sig_Type);
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
      Name_Node : Mnode;
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
            New_Lit (New_Global_Unchecked_Address
                       (Get_Info (Base_Decl).Signal_Rti,
                        Rtis.Ghdl_Rti_Access)));
         Rtis.Associate_Rti_Context (Assoc, Parent);
         New_Procedure_Call (Assoc);
      end;

      Name_Node := Chap6.Translate_Name (Decl);
      --  Consistency check: a signal name is a signal.
      pragma Assert (Get_Object_Kind (Name_Node) = Mode_Signal);

      if Decl = Base_Decl then
         Data.Already_Resolved := False;
         Data.Check_Null := Check_Null;
         Value := Get_Default_Value (Base_Decl);
         if Value = Null_Iir then
            Data.Has_Val := False;
         else
            Data.Has_Val := True;
            Data.Val := E2M (Chap7.Translate_Expression (Value, Sig_Type),
                             Get_Info (Sig_Type),
                             Mode_Value);
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
      Elab_Signal (Name_Node, Sig_Type, Data);

      Close_Temp;

      if Value /= Null_Iir then
         Chap9.Destroy_Types (Value);
      end if;
   end Elab_Signal_Declaration_Object;

   procedure Elab_Signal_Declaration
     (Decl : Iir; Parent : Iir; Check_Null : Boolean)
   is
   begin
      Elab_Signal_Declaration_Storage (Decl);
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
      Prefix_Node := Chap6.Translate_Name (Prefix);
      Register_Signal (Prefix_Node, Get_Type (Prefix),
                       Ghdl_Signal_Attribute_Register_Prefix);
   end Elab_Signal_Attribute;

   type Delayed_Signal_Data is record
      Pfx   : Mnode;
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
      if Get_Type_Info (Targ).Type_Mode = Type_Mode_Record then
         Res.Pfx := Stabilize (Data.Pfx);
      else
         Res.Pfx := Chap3.Get_Array_Base (Data.Pfx);
      end if;
      return Res;
   end Create_Delayed_Signal_Prepare_Composite;

   function Create_Delayed_Signal_Update_Data_Array
     (Data : Delayed_Signal_Data; Targ_Type : Iir; Index : O_Dnode)
         return Delayed_Signal_Data
   is
   begin
      return Delayed_Signal_Data'
        (Pfx => Chap3.Index_Base (Data.Pfx, Targ_Type,
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
        (Pfx => Chap6.Translate_Selected_Element (Data.Pfx, El),
         Param => Data.Param);
   end Create_Delayed_Signal_Update_Data_Record;

   procedure Create_Delayed_Signal_Finish_Data_Composite
     (Data : in out Delayed_Signal_Data)
   is
      pragma Unreferenced (Data);
   begin
      null;
   end Create_Delayed_Signal_Finish_Data_Composite;

   procedure Create_Delayed_Signal is new Foreach_Non_Composite
     (Data_Type => Delayed_Signal_Data,
      Composite_Data_Type => Delayed_Signal_Data,
      Do_Non_Composite => Create_Delayed_Signal_Noncomposite,
      Prepare_Data_Array => Create_Delayed_Signal_Prepare_Composite,
      Update_Data_Array => Create_Delayed_Signal_Update_Data_Array,
      Finish_Data_Array => Create_Delayed_Signal_Finish_Data_Composite,
      Prepare_Data_Record => Create_Delayed_Signal_Prepare_Composite,
      Update_Data_Record => Create_Delayed_Signal_Update_Data_Record,
      Finish_Data_Record => Create_Delayed_Signal_Finish_Data_Composite);

   procedure Elab_Signal_Delayed_Attribute (Decl : Iir)
   is
      Sig_Type  : constant Iir := Get_Type (Decl);
      Type_Info : constant Type_Info_Acc := Get_Info (Sig_Type);
      Name_Node : Mnode;
      Pfx_Node  : Mnode;
      Data      : Delayed_Signal_Data;
   begin
      Name_Node := Chap6.Translate_Name (Decl);

      if Is_Complex_Type (Type_Info) then
         Allocate_Complex_Object (Sig_Type, Alloc_System, Name_Node);
         --  We cannot stabilize NAME_NODE, since Allocate_Complex_Object
         --  assign it.
         Name_Node := Chap6.Translate_Name (Decl);
      end if;

      Pfx_Node := Chap6.Translate_Name (Get_Prefix (Decl));
      Data := Delayed_Signal_Data'(Pfx => Pfx_Node,
                                   Param => Get_Parameter (Decl));

      Create_Delayed_Signal (Name_Node, Get_Type (Decl), Data);
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
      Name := Chap6.Translate_Name (Decl);
      pragma Assert (Get_Object_Kind (Name) = Mode_Value);

      if Is_Text then
         Start_Association (Constr, Ghdl_Text_File_Elaborate);
      else
         Start_Association (Constr, Ghdl_File_Elaborate);
         Info := Get_Info (Get_Type (Decl));
         if Info.T.File_Signature /= O_Dnode_Null then
            New_Association
              (Constr, New_Address (New_Obj (Info.T.File_Signature),
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
      Name := Chap6.Translate_Name (Decl);
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
      Name := Chap6.Translate_Name (Decl);
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

   procedure Translate_Type_Declaration (Decl : Iir) is
   begin
      Chap3.Translate_Named_Type_Definition (Get_Type_Definition (Decl),
                                             Get_Identifier (Decl));
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
   begin
      Chap3.Translate_Named_Type_Definition (Get_Type (Decl),
                                             Get_Identifier (Decl));
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
      Info      : Alias_Info_Acc;
      Tinfo     : Type_Info_Acc;
      Atype     : O_Tnode;
   begin
      Chap3.Translate_Named_Type_Definition (Decl_Type, Get_Identifier (Decl));

      Info := Add_Info (Decl, Kind_Alias);
      case Get_Kind (Get_Object_Prefix (Decl)) is
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kinds_Signal_Attribute =>
            Info.Alias_Kind := Mode_Signal;
         when others =>
            Info.Alias_Kind := Mode_Value;
      end case;

      Tinfo := Get_Info (Decl_Type);
      case Tinfo.Type_Mode is
         when Type_Mode_Fat_Array =>
            --  create an object.
            --  At elaboration: copy base from name, copy bounds from type,
            --   check for matching bounds.
            Atype := Get_Ortho_Type (Decl_Type, Info.Alias_Kind);
         when Type_Mode_Array
            | Type_Mode_Acc
            | Type_Mode_Bounds_Acc =>
            --  Create an object pointer.
            --  At elaboration: copy base from name.
            Atype := Tinfo.Ortho_Ptr_Type (Info.Alias_Kind);
         when Type_Mode_Scalar =>
            case Info.Alias_Kind is
               when Mode_Signal =>
                  Atype := Tinfo.Ortho_Type (Mode_Signal);
               when Mode_Value =>
                  Atype := Tinfo.Ortho_Ptr_Type (Mode_Value);
            end case;
         when Type_Mode_Record =>
            --  Create an object pointer.
            --  At elaboration: copy base from name.
            Atype := Tinfo.Ortho_Ptr_Type (Info.Alias_Kind);
         when others =>
            raise Internal_Error;
      end case;
      Info.Alias_Var := Create_Var (Create_Var_Identifier (Decl), Atype);
   end Translate_Object_Alias_Declaration;

   procedure Elab_Object_Alias_Declaration
     (Decl : Iir_Object_Alias_Declaration)
   is
      Decl_Type  : constant Iir := Get_Type (Decl);
      Tinfo      : constant Type_Info_Acc := Get_Info (Decl_Type);
      Name       : constant Iir := Get_Name (Decl);
      Name_Type  : constant Iir := Get_Type (Name);
      Alias_Info : constant Alias_Info_Acc := Get_Info (Decl);
      Name_Node  : Mnode;
      Alias_Node : Mnode;
      Kind       : Object_Kind_Type;
   begin
      New_Debug_Line_Stmt (Get_Line_Number (Decl));

      Chap3.Elab_Object_Subtype (Decl_Type);
      Name_Node := Chap6.Translate_Name (Name);
      Kind := Get_Object_Kind (Name_Node);

      case Tinfo.Type_Mode is
         when Type_Mode_Fat_Array =>
            Open_Temp;
            Stabilize (Name_Node);
            Alias_Node := Stabilize
              (Get_Var (Alias_Info.Alias_Var,
               Tinfo, Alias_Info.Alias_Kind));
            Copy_Fat_Pointer (Alias_Node, Name_Node);
            Close_Temp;
         when Type_Mode_Array =>
            Open_Temp;
            Stabilize (Name_Node);
            New_Assign_Stmt
              (Get_Var (Alias_Info.Alias_Var),
               M2E (Chap3.Get_Array_Base (Name_Node)));
            Chap3.Check_Array_Match (Decl_Type, T2M (Decl_Type, Kind),
                                     Name_Type, Name_Node,
                                     Decl);
            Close_Temp;
         when Type_Mode_Acc
            | Type_Mode_Bounds_Acc =>
            New_Assign_Stmt (Get_Var (Alias_Info.Alias_Var),
                             M2Addr (Name_Node));
         when Type_Mode_Scalar =>
            case Alias_Info.Alias_Kind is
               when Mode_Value =>
                  New_Assign_Stmt (Get_Var (Alias_Info.Alias_Var),
                                   M2Addr (Name_Node));
               when Mode_Signal =>
                  New_Assign_Stmt (Get_Var (Alias_Info.Alias_Var),
                                   M2E (Name_Node));
            end case;
         when Type_Mode_Record =>
            Open_Temp;
            Stabilize (Name_Node);
            New_Assign_Stmt (Get_Var (Alias_Info.Alias_Var),
                             M2Addr (Name_Node));
            Close_Temp;
         when others =>
            raise Internal_Error;
      end case;
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
               Create_Package_Interface (Decl);
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

         when Iir_Kind_Component_Declaration =>
            Chap4.Translate_Component_Declaration (Decl);
         when Iir_Kind_Type_Declaration =>
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
            --  Useless as attribute declarations have a type mark.
            Chap3.Translate_Object_Subtype (Decl);

         when Iir_Kind_Attribute_Specification =>
            Chap5.Translate_Attribute_Specification (Decl);

         when Iir_Kinds_Signal_Attribute =>
            Chap4.Create_Implicit_Signal (Decl);

         when Iir_Kind_Guard_Signal_Declaration =>
            Create_Signal (Decl);

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

   procedure Read_Source_Finish_Data_Composite
     (Data : in out Read_Source_Data)
   is
      pragma Unreferenced (Data);
   begin
      null;
   end Read_Source_Finish_Data_Composite;

   procedure Read_Signal_Source is new Foreach_Non_Composite
     (Data_Type => Read_Source_Data,
      Composite_Data_Type => Read_Source_Data,
      Do_Non_Composite => Read_Source_Non_Composite,
      Prepare_Data_Array => Read_Source_Prepare_Data_Array,
      Update_Data_Array => Read_Source_Update_Data_Array,
      Finish_Data_Array => Read_Source_Finish_Data_Composite,
      Prepare_Data_Record => Read_Source_Prepare_Data_Record,
      Update_Data_Record => Read_Source_Update_Data_Record,
      Finish_Data_Record => Read_Source_Finish_Data_Composite);

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
        (Get_First_Element (Get_Index_Subtype_Definition_List (Base_Type)));
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
                    Base_Info.T.Bounds_Type);
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
                         Index_Tinfo.T.Range_Type,
                         Index_Tinfo.T.Range_Ptr_Type);
      Chap3.Create_Range_From_Length (Index_Type, Var_Length, Range_Ptr, Func);

      New_Assign_Stmt
        (New_Selected_Element (New_Obj (Var_Array),
         Base_Info.T.Bounds_Field (Mode_Value)),
         New_Address (New_Obj (Var_Bound), Base_Info.T.Bounds_Ptr_Type));

      --  Allocate the array.
      Chap3.Allocate_Fat_Array_Base
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
        (Chap3.Get_Array_Base (Dv2M (Var_Array, Base_Info, Mode_Value)),
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
        (Chap3.Get_Array_Base (Dv2M (Var_Array, Base_Info, Mode_Value)),
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

      Start_Association (Assoc, Finfo.Ortho_Func);
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
                  Canon.Canon_Subprogram_Call (Call);
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

   procedure Translate_Declaration_Chain_Subprograms (Parent : Iir)
   is
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
                           Chap7.Translate_Implicit_Subprogram (El, Infos);
                        when others =>
                           null;
                     end case;
                  else
                     Chap7.Translate_Implicit_Subprogram (El, Infos);
                  end if;
               else
                  --  Translate only if used.
                  if Get_Info (El) /= null then
                     Chap2.Translate_Subprogram_Declaration (El);
                     Translate_Resolution_Function (El);
                  end if;
               end if;
            when Iir_Kind_Function_Body
               | Iir_Kind_Procedure_Body =>
               --  Do not translate body if generating only specs (for
               --  subprograms in an entity).
               if Global_Storage /= O_Storage_External
                 and then
                   (not Flag_Discard_Unused
                    or else
                    Get_Use_Flag (Get_Subprogram_Specification (El)))
               then
                  Chap2.Translate_Subprogram_Body (El);
                  Translate_Resolution_Function_Body
                    (Get_Subprogram_Specification (El));
               end if;
            when Iir_Kind_Type_Declaration
               | Iir_Kind_Anonymous_Type_Declaration =>
               Chap3.Translate_Type_Subprograms (El);
               Chap7.Init_Implicit_Subprogram_Infos (Infos);
            when Iir_Kind_Protected_Type_Body =>
               Chap3.Translate_Protected_Type_Body (El);
               Chap3.Translate_Protected_Type_Body_Subprograms (El);
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
               Chap3.Elab_Object_Subtype (Get_Type (Decl));

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

            when Iir_Kind_Stable_Attribute
               | Iir_Kind_Quiet_Attribute
               | Iir_Kind_Transaction_Attribute =>
               Elab_Signal_Attribute (Decl);

            when Iir_Kind_Delayed_Attribute =>
               Elab_Signal_Delayed_Attribute (Decl);

            when Iir_Kind_Group_Template_Declaration
               | Iir_Kind_Group_Declaration =>
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
      Mode       : Conv_Mode;
      Conv_Info  : in out Assoc_Conv_Info;
      Base_Block : Iir;
      Entity     : Iir)
   is
      Formal : constant Iir := Get_Formal (Assoc);
      Actual : constant Iir := Get_Actual (Assoc);

      Mark2, Mark3      : Id_Mark_Type;
      Inter_List        : O_Inter_List;
      In_Type, Out_Type : Iir;
      In_Info, Out_Info : Type_Info_Acc;
      Itype             : O_Tnode;
      El_List           : O_Element_List;
      Block_Info        : constant Block_Info_Acc := Get_Info (Base_Block);
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
      Imp         : Iir;
      Func        : Iir;
   begin
      case Mode is
         when Conv_Mode_In =>
            --  IN: from actual to formal.
            Push_Identifier_Prefix (Mark2, "CONVIN");
            Out_Type := Get_Type (Formal);
            In_Type := Get_Type (Actual);
            Imp := Get_In_Conversion (Assoc);

         when Conv_Mode_Out =>
            --  OUT: from formal to actual.
            Push_Identifier_Prefix (Mark2, "CONVOUT");
            In_Type := Get_Type (Formal);
            Out_Type := Get_Type (Actual);
            Imp := Get_Out_Conversion (Assoc);

      end case;
      --  FIXME: individual assoc -> overload.
      Push_Identifier_Prefix
        (Mark3, Get_Identifier (Get_Association_Interface (Assoc)));

      --  Handle anonymous subtypes.
      Chap3.Translate_Anonymous_Type_Definition (Out_Type);
      Chap3.Translate_Anonymous_Type_Definition (In_Type);
      Out_Info := Get_Info (Out_Type);
      In_Info := Get_Info (In_Type);

      --  Start record containing data for the conversion function.
      Start_Record_Type (El_List);

      --  Add instance field.
      Conv_Info.Instance_Block := Base_Block;
      New_Record_Field
        (El_List, Conv_Info.Instance_Field, Wki_Instance,
         Block_Info.Block_Decls_Ptr_Type);

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

      --  Add input.
      case In_Info.Type_Mode is
         when Type_Mode_Thin =>
            Itype := In_Info.Ortho_Type (Mode_Signal);
         when Type_Mode_Fat =>
            Itype := In_Info.Ortho_Ptr_Type (Mode_Signal);
         when Type_Mode_Unknown =>
            raise Internal_Error;
      end case;
      New_Record_Field
        (El_List, Conv_Info.In_Field, Get_Identifier ("val_in"), Itype);

      --  Add output.
      New_Record_Field
        (El_List, Conv_Info.Out_Field, Get_Identifier ("val_out"),
         Get_Object_Type (Out_Info, Mode_Signal));
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
      E := New_Value_Selected_Acc_Value (New_Obj (Var_Data),
                                         Conv_Info.In_Field);
      case Mode is
         when Conv_Mode_In =>
            R := Chap7.Translate_Signal_Effective_Value (E, In_Type);
         when Conv_Mode_Out =>
            R := Chap7.Translate_Signal_Driving_Value (E, In_Type);
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
            Start_Association (Constr, Subprg_Info.Ortho_Func);

            if Subprg_Info.Res_Interface /= O_Dnode_Null then
               --  Composite result.
               New_Association (Constr, M2E (Res));
            end if;

            Subprgs.Add_Subprg_Instance_Assoc
              (Constr, Subprg_Info.Subprg_Instance);

            New_Association (Constr, R);

            if Subprg_Info.Res_Interface /= O_Dnode_Null then
               --  Composite result.
               New_Procedure_Call (Constr);
               E := M2E (Res);
            else
               E := New_Function_Call (Constr);
            end if;
            Res := E2M
              (Chap7.Translate_Implicit_Conv
                 (E, Get_Return_Type (Func),
                  Out_Type, Mode_Value, Imp),
               Get_Info (Out_Type), Mode_Value);

         when Iir_Kind_Type_Conversion =>
            declare
               Conv_Type : Iir;
            begin
               Conv_Type := Get_Type (Imp);
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
      V1 := New_Selected_Acc_Value (New_Obj (Var_Data),
                                    Conv_Info.Out_Field);
      V_Out := Lo2M (V1, Out_Info, Mode_Signal);

      case Mode is
         when Conv_Mode_In =>
            Chap7.Set_Effective_Value (V_Out, Out_Type, Res);
         when Conv_Mode_Out =>
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

   --  ENTITY is null for block_statement.
   procedure Translate_Association_Subprograms
     (Stmt : Iir; Block : Iir; Base_Block : Iir; Entity : Iir)
   is
      Assoc : Iir;
      Info  : Assoc_Info_Acc;
   begin
      Assoc := Get_Port_Map_Aspect_Chain (Stmt);
      while Assoc /= Null_Iir loop
         if Get_Kind (Assoc) = Iir_Kind_Association_Element_By_Expression
         then
            Info := null;
            if Get_In_Conversion (Assoc) /= Null_Iir then
               Info := Add_Info (Assoc, Kind_Assoc);
               Translate_Association_Subprogram
                 (Stmt, Block, Assoc, Conv_Mode_In, Info.Assoc_In,
                  Base_Block, Entity);
            end if;
            if Get_Out_Conversion (Assoc) /= Null_Iir then
               if Info = null then
                  Info := Add_Info (Assoc, Kind_Assoc);
               end if;
               Translate_Association_Subprogram
                 (Stmt, Block, Assoc, Conv_Mode_Out, Info.Assoc_Out,
                  Base_Block, Entity);
            end if;
         end if;
         Assoc := Get_Chain (Assoc);
      end loop;
   end Translate_Association_Subprograms;

   procedure Elab_Conversion (Sig_In     : Iir;
                              Sig_Out    : Iir;
                              Reg_Subprg : O_Dnode;
                              Info       : Assoc_Conv_Info;
                              Ndest      : out Mnode)
   is
      Out_Type : Iir;
      Out_Info : Type_Info_Acc;
      Ssig     : Mnode;
      Constr   : O_Assoc_List;
      Var_Data : O_Dnode;
      Data     : Elab_Signal_Data;
   begin
      Out_Type := Get_Type (Sig_Out);
      Out_Info := Get_Info (Out_Type);

      --  Allocate data for the subprogram.
      Var_Data := Create_Temp (Info.Record_Ptr_Type);
      New_Assign_Stmt
        (New_Obj (Var_Data),
         Gen_Alloc (Alloc_System,
           New_Lit (New_Sizeof (Info.Record_Type,
             Ghdl_Index_Type)),
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

      --  Set input.
      Ssig := Chap6.Translate_Name (Sig_In);
      Ssig := Stabilize (Ssig, True);

      New_Assign_Stmt
        (New_Selected_Acc_Value (New_Obj (Var_Data), Info.In_Field),
         M2E (Ssig));

      --  Create a copy of SIG_OUT.
      Ndest := Lo2M (New_Selected_Acc_Value (New_Obj (Var_Data),
                     Info.Out_Field),
                     Out_Info, Mode_Signal);
      Chap4.Allocate_Complex_Object (Out_Type, Alloc_System, Ndest);
      --  Note: NDEST will be assigned by ELAB_SIGNAL.
      Ndest := Lo2M (New_Selected_Acc_Value (New_Obj (Var_Data),
                     Info.Out_Field),
                     Out_Info, Mode_Signal);
      Data := Elab_Signal_Data'(Has_Val => False,
                                Already_Resolved => True,
                                Val => Mnode_Null,
                                Check_Null => False,
                                If_Stmt => null);
      Elab_Signal (Ndest, Out_Type, Data);

      Ndest := Lo2M (New_Selected_Acc_Value (New_Obj (Var_Data),
                     Info.Out_Field),
                     Out_Info, Mode_Signal);
      Ndest := Stabilize (Ndest, True);

      --  Register.
      Start_Association (Constr, Reg_Subprg);
      New_Association
        (Constr, New_Lit (New_Subprogram_Address (Info.Subprg,
         Ghdl_Ptr_Type)));
      New_Association
        (Constr, New_Convert_Ov (New_Obj_Value (Var_Data), Ghdl_Ptr_Type));

      New_Association
        (Constr,
         New_Convert_Ov (M2E (Get_Leftest_Signal (Ssig, Get_Type (Sig_In))),
           Ghdl_Signal_Ptr));
      New_Association (Constr, Get_Nbr_Signals (Ssig, Get_Type (Sig_In)));

      New_Association
        (Constr,
         New_Convert_Ov
           (M2E (Get_Leftest_Signal (Ndest, Get_Type (Sig_Out))),
            Ghdl_Signal_Ptr));
      New_Association (Constr, Get_Nbr_Signals (Ndest, Get_Type (Sig_Out)));

      New_Procedure_Call (Constr);
   end Elab_Conversion;

   --  In conversion: from actual to formal.
   procedure Elab_In_Conversion (Assoc : Iir; Ndest : out Mnode)
   is
      Assoc_Info : Assoc_Info_Acc;
   begin
      Assoc_Info := Get_Info (Assoc);

      Elab_Conversion
        (Get_Actual (Assoc), Get_Formal (Assoc),
         Ghdl_Signal_In_Conversion, Assoc_Info.Assoc_In, Ndest);
   end Elab_In_Conversion;

   --  Out conversion: from formal to actual.
   procedure Elab_Out_Conversion (Assoc : Iir; Ndest : out Mnode)
   is
      Assoc_Info : Assoc_Info_Acc;
   begin
      Assoc_Info := Get_Info (Assoc);

      Elab_Conversion
        (Get_Formal (Assoc), Get_Actual (Assoc),
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
      Start_Const_Value (C);
      Start_Record_Aggr (Constr, Ghdl_Location_Type_Node);
      New_Record_Aggr_El
        (Constr, New_Global_Address (Current_Filename_Node, Char_Ptr_Type));
      New_Record_Aggr_El (Constr, New_Signed_Literal (Ghdl_I32_Type,
                          Integer_64 (Line)));
      New_Record_Aggr_El (Constr, New_Signed_Literal (Ghdl_I32_Type,
                          Integer_64 (Col)));
      Finish_Record_Aggr (Constr, Aggr);
      Finish_Const_Value (C, Aggr);

      return C;
      --return New_Global_Address (C, Ghdl_Location_Ptr_Node);
   end Get_Location;
end Trans.Chap4;
