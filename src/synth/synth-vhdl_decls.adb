--  Create declarations for synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
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

with Types; use Types;
with Std_Names;

with Netlists.Builders; use Netlists.Builders;
with Netlists.Folds; use Netlists.Folds;
with Netlists.Utils; use Netlists.Utils;
with Netlists.Gates;

with Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package;

with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Types; use Elab.Vhdl_Types;
with Elab.Vhdl_Decls; use Elab.Vhdl_Decls;
with Elab.Vhdl_Files;

with Synth.Vhdl_Environment; use Synth.Vhdl_Environment.Env;
with Synth.Vhdl_Expr; use Synth.Vhdl_Expr;
with Synth.Vhdl_Stmts;
with Synth.Source; use Synth.Source;
with Synth.Errors; use Synth.Errors;
with Synth.Vhdl_Context; use Synth.Vhdl_Context;

package body Synth.Vhdl_Decls is
   function Create_Var_Wire (Syn_Inst : Synth_Instance_Acc;
                             Decl : Node;
                             Kind : Wire_Kind;
                             Init : Valtyp) return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Value : Net;
      Ival : Net;
      W : Width;
      Name : Sname;
      Wid : Wire_Id;
   begin
      Wid := Alloc_Wire (Kind, (Decl, Init.Typ));

      --  FIXME: get the width directly from the wire ?
      W := Get_Type_Width (Init.Typ);
      Name := New_Sname_User (Get_Identifier (Decl),
                              Get_Sname (Syn_Inst));
      if Init.Val /= null then
         Ival := Get_Net (Ctxt, Init);
         pragma Assert (Get_Width (Ival) = W);
         Value := Build_Isignal (Ctxt, Name, Ival);
      else
         Value := Build_Signal (Ctxt, Name, W);
      end if;
      Set_Location (Value, Decl);

      Set_Wire_Gate (Wid, Value);
      return Create_Value_Wire (Wid, Init.Typ);
   end Create_Var_Wire;

   function Type_To_Param_Type (Atype : Node) return Param_Type
   is
      use Vhdl.Std_Package;
      Btype : constant Node := Get_Base_Type (Atype);
   begin
      if Btype = String_Type_Definition then
         return Param_Pval_String;
      elsif Btype = Time_Type_Definition then
         return Param_Pval_Time_Ps;
      else
         case Get_Kind (Btype) is
            when Iir_Kind_Integer_Type_Definition =>
               return Param_Pval_Integer;
            when Iir_Kind_Floating_Type_Definition =>
               return Param_Pval_Real;
            when others =>
               return Param_Pval_Vector;
         end case;
      end if;
   end Type_To_Param_Type;

   function Memtyp_To_Pval (Mt : Memtyp) return Pval
   is
      Len    : constant Uns32 := (Mt.Typ.W + 31) / 32;
      Vec    : Logvec_Array_Acc;
      Off    : Uns32;
      Has_Zx : Boolean;
      Pv     : Pval;
   begin
      if Len = 0 then
         return Create_Pval2 (0);
      end if;

      Vec := new Logvec_Array'(0 .. Digit_Index (Len - 1) => (0, 0));
      Off := 0;
      Has_Zx := False;
      Value2logvec (Mt, 0, Mt.Typ.W, Vec.all, Off, Has_Zx);
      pragma Assert (Off = Mt.Typ.W);
      if Has_Zx then
         Pv := Create_Pval4 (Mt.Typ.W);
      else
         Pv := Create_Pval2 (Mt.Typ.W);
      end if;
      for I in 0 .. Len - 1 loop
         Write_Pval (Pv, I, Vec (Digit_Index (I)));
      end loop;
      Free_Logvec_Array (Vec);
      return Pv;
   end Memtyp_To_Pval;

   procedure Synth_Constant_Declaration (Syn_Inst : Synth_Instance_Acc;
                                         Decl : Node;
                                         Is_Subprg : Boolean;
                                         Last_Type : in out Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Deferred_Decl : constant Node := Get_Deferred_Declaration (Decl);
      First_Decl : Node;
      Decl_Type : Node;
      Val : Valtyp;
      Cst : Valtyp;
      Obj_Type : Type_Acc;
   begin
      Elab_Declaration_Type (Syn_Inst, Decl);
      if Deferred_Decl = Null_Node
        or else Get_Deferred_Declaration_Flag (Decl)
      then
         --  Create the object (except for full declaration of a
         --  deferred constant).
         Create_Object (Syn_Inst, Decl, No_Valtyp);
      end if;
      --  Initialize the value (except for a deferred declaration).
      if Get_Deferred_Declaration_Flag (Decl) then
         return;
      end if;
      if Deferred_Decl = Null_Node then
         --  A normal constant declaration
         First_Decl := Decl;
      else
         --  The full declaration of a deferred constant.
         First_Decl := Deferred_Decl;
      end if;
      pragma Assert (First_Decl /= Null_Node);

      --  Use the type of the declaration.  The type of the constant may
      --  be derived from the value.
      --  FIXME: what about multiple declarations ?
      Decl_Type := Get_Subtype_Indication (Decl);
      if Decl_Type = Null_Node then
         Decl_Type := Last_Type;
      else
         if Get_Kind (Decl_Type) in Iir_Kinds_Denoting_Name then
            --  Type mark.
            Decl_Type := Get_Type (Get_Named_Entity (Decl_Type));
         end if;
         Last_Type := Decl_Type;
      end if;
      Obj_Type := Get_Subtype_Object (Syn_Inst, Decl_Type);
      Val := Synth_Expression_With_Type
        (Syn_Inst, Get_Default_Value (Decl), Obj_Type);
      if Val = No_Valtyp then
         Set_Error (Syn_Inst);
         return;
      end if;
      Val := Synth_Subtype_Conversion (Ctxt, Val, Obj_Type, True, Decl);
      --  For constant functions, the value must be constant.
      pragma Assert (not Get_Instance_Const (Syn_Inst)
                     or else Is_Static (Val.Val));
      case Val.Val.Kind is
         when Value_Const
            | Value_Alias =>
            Cst := Val;
         when others =>
            if Is_Static (Val.Val) then
               Cst := Create_Value_Const (Val, Decl);
            else
               if not Is_Subprg then
                  Error_Msg_Synth
                    (+Decl, "signals cannot be used in default value "
                     & "of this constant");
               end if;
               Cst := Val;
            end if;
      end case;
      Create_Object_Force (Syn_Inst, First_Decl, Cst);
   end Synth_Constant_Declaration;

   procedure Synth_Attribute_Object (Syn_Inst : Synth_Instance_Acc;
                                     Attr_Value : Node;
                                     Attr_Decl  : Node;
                                     Val        : Valtyp)
   is
      Obj   : constant Node := Get_Designated_Entity (Attr_Value);
      Id    : constant Name_Id := Get_Identifier (Attr_Decl);
      Inst  : Instance;
      V     : Valtyp;
      Ptype : Param_Type;
      Pv    : Pval;
   begin
      if Id = Std_Names.Name_Foreign then
         --  Not for synthesis.
         return;
      end if;

      case Get_Kind (Obj) is
         when Iir_Kind_Signal_Declaration
            | Iir_Kind_Variable_Declaration
            | Iir_Kind_Interface_Signal_Declaration =>
            V := Get_Value (Syn_Inst, Obj);
            pragma Assert (V.Val.Kind = Value_Wire);
            Inst := Get_Net_Parent (Get_Wire_Gate (Get_Value_Wire (V.Val)));
         when Iir_Kind_Component_Instantiation_Statement =>
            --  TODO
            return;
         when others =>
            --  TODO: components ?
            --  TODO: Interface_Signal ?  But no instance for them.
            Warning_Msg_Synth
              (+Attr_Value, "attribute %i for %n is not kept in the netlist",
               (+Attr_Decl, +Obj));
            return;
      end case;

      Ptype := Type_To_Param_Type (Get_Type (Attr_Decl));
      Pv := Memtyp_To_Pval (Get_Memtyp (Val));

      Set_Attribute (Inst, Id, Ptype, Pv);
   end Synth_Attribute_Object;

   procedure Synth_Attribute_Specification
     (Syn_Inst : Synth_Instance_Acc; Spec : Node)
   is
      Attr_Decl : constant Node :=
        Get_Named_Entity (Get_Attribute_Designator (Spec));
      Value : Node;
      Val : Valtyp;
      Val_Type : Type_Acc;
   begin
      Val_Type := Get_Subtype_Object (Syn_Inst, Get_Type (Attr_Decl));
      Value := Get_Attribute_Value_Spec_Chain (Spec);
      while Value /= Null_Iir loop
         --  2. The expression is evaluated to determine the value
         --     of the attribute.
         --     It is an error if the value of the expression does not
         --     belong to the subtype of the attribute; if the
         --     attribute is of an array type, then an implicit
         --     subtype conversion is first performed on the value,
         --     unless the attribute's subtype indication denotes an
         --     unconstrained array type.
         Val := Synth_Expression_With_Type
           (Syn_Inst, Get_Expression (Spec), Val_Type);
         --  Check_Constraints (Instance, Val, Attr_Type, Decl);

         --  3. A new instance of the designated attribute is created
         --     and associated with each of the affected items.
         --
         --  4. Each new attribute instance is assigned the value of
         --     the expression.
         Create_Object (Syn_Inst, Value, Val);
         --  Unshare (Val, Instance_Pool);

         Value := Get_Spec_Chain (Value);
      end loop;
   end Synth_Attribute_Specification;

   procedure Synth_Concurrent_Attribute_Specification
     (Syn_Inst : Synth_Instance_Acc; Spec : Node)
   is
      Attr_Decl : constant Node :=
        Get_Named_Entity (Get_Attribute_Designator (Spec));
      Value : Node;
      Val : Valtyp;
   begin
      if Get_Instance_Const (Syn_Inst) then
         return;
      end if;

      Value := Get_Attribute_Value_Spec_Chain (Spec);
      while Value /= Null_Iir loop
         Val := Get_Value (Syn_Inst, Value);
         Synth_Attribute_Object (Syn_Inst, Value, Attr_Decl, Val);

         Value := Get_Spec_Chain (Value);
      end loop;
   end Synth_Concurrent_Attribute_Specification;

   procedure Synth_Package_Declaration
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node)
   is
      Syn_Inst : Synth_Instance_Acc;
   begin
      if Is_Uninstantiated_Package (Pkg) then
         --  Nothing to do (yet) for uninstantiated packages.
         return;
      end if;

      Syn_Inst := Get_Package_Object (Parent_Inst, Pkg);
      Set_Extra (Syn_Inst, Parent_Inst, No_Sname);

      Synth_Concurrent_Declarations (Syn_Inst, Get_Declaration_Chain (Pkg));
   end Synth_Package_Declaration;

   procedure Synth_Package_Body
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node; Bod : Node)
   is
      Pkg_Inst : Synth_Instance_Acc;
   begin
      if Is_Uninstantiated_Package (Pkg) then
         --  Nothing to do (yet) for uninstantiated packages.
         return;
      end if;

      Pkg_Inst := Get_Package_Object (Parent_Inst, Pkg);

      Synth_Concurrent_Declarations (Pkg_Inst, Get_Declaration_Chain (Bod));
   end Synth_Package_Body;

   procedure Synth_Package_Instantiation
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node)
   is
      Bod : constant Node := Get_Instance_Package_Body (Pkg);
      Sub_Inst : Synth_Instance_Acc;
   begin
      Sub_Inst := Get_Package_Object (Parent_Inst, Pkg);

      Synth_Concurrent_Declarations (Sub_Inst, Get_Declaration_Chain (Pkg));

      if Bod /= Null_Node then
         --  Macro expanded package instantiation.
         raise Internal_Error;
      else
         --  Shared body
         declare
            Uninst : constant Node := Get_Uninstantiated_Package_Decl (Pkg);
            Uninst_Bod : constant Node := Get_Package_Body (Uninst);
         begin
            Set_Uninstantiated_Scope (Sub_Inst, Uninst);
            --  Synth declarations of (optional) body.
            if Uninst_Bod /= Null_Node then
               Synth_Concurrent_Declarations
                 (Sub_Inst, Get_Declaration_Chain (Uninst_Bod));
            end if;
         end;
      end if;
   end Synth_Package_Instantiation;

   procedure Synth_Variable_Declaration (Syn_Inst : Synth_Instance_Acc;
                                         Decl : Node;
                                         Is_Subprg : Boolean)
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Def : constant Node := Get_Default_Value (Decl);
      Decl_Type : constant Node := Get_Type (Decl);
      Init : Valtyp;
      Val : Valtyp;
      Obj_Typ : Type_Acc;
      Wid : Wire_Id;
   begin
      Elab_Declaration_Type (Syn_Inst, Decl);
      if Get_Kind (Decl_Type) = Iir_Kind_Protected_Type_Declaration then
         Error_Msg_Synth
           (+Decl, "protected type variable is not synthesizable");
         Set_Error (Syn_Inst);
         Create_Object (Syn_Inst, Decl, No_Valtyp);
         return;
      end if;

      Obj_Typ := Get_Subtype_Object (Syn_Inst, Decl_Type);
      if not Obj_Typ.Is_Synth
        and then not Get_Instance_Const (Syn_Inst)
      then
         Error_Msg_Synth
           (+Decl, "variable with access type is not synthesizable");
         --  FIXME: use a poison value ?
         Create_Object (Syn_Inst, Decl, Create_Value_Default (Obj_Typ));
      else
         if Is_Valid (Def) then
            Init := Synth_Expression_With_Type (Syn_Inst, Def, Obj_Typ);
            Init := Synth_Subtype_Conversion
              (Ctxt, Init, Obj_Typ, False, Decl);
            if not Is_Subprg
              and then not Is_Static (Init.Val)
            then
               Error_Msg_Synth
                 (+Decl, "signals cannot be used in default value of "
                    & "this variable");
            end if;
         else
            Init := Create_Value_Default (Obj_Typ);
         end if;
         if Get_Instance_Const (Syn_Inst) then
            Init := Strip_Alias_Const (Init);
            Init := Unshare (Init, Current_Pool);
            Create_Object (Syn_Inst, Decl, Init);
         else
            Val := Create_Var_Wire (Syn_Inst, Decl, Wire_Variable, Init);
            Create_Object (Syn_Inst, Decl, Val);
            Wid := Get_Value_Wire (Val.Val);
            if Is_Subprg then
               if Is_Static (Init.Val) then
                  Phi_Assign_Static (Wid, Get_Memtyp (Init));
               else
                  Phi_Assign_Net (Ctxt, Wid, Get_Net (Ctxt, Init), 0);
               end if;
            end if;
         end if;
      end if;
   end Synth_Variable_Declaration;

   procedure Synth_Shared_Variable_Declaration (Syn_Inst : Synth_Instance_Acc;
                                                Decl : Node)
   is
      Init : Valtyp;
      Val : Valtyp;
   begin
      Init := Get_Value (Syn_Inst, Decl);

      Val := Create_Var_Wire (Syn_Inst, Decl, Wire_Variable, Init);
      Mutate_Object (Syn_Inst, Decl, Val);
   end Synth_Shared_Variable_Declaration;

   procedure Synth_Signal_Declaration (Syn_Inst : Synth_Instance_Acc;
                                       Decl : Node)
   is
      Prev : Valtyp;
      Init : Valtyp;
      Val : Valtyp;
   begin
      if Get_Kind (Get_Parent (Decl)) = Iir_Kind_Package_Declaration then
         Error_Msg_Synth (+Decl, "signals in packages are not supported");
         return;
      end if;

      Prev := Get_Value (Syn_Inst, Decl);
      if Prev.Val.Init = null then
         Init := (Prev.Typ, null);
      else
         Init := (Prev.Typ, Prev.Val.Init);
      end if;

      Val := Create_Var_Wire (Syn_Inst, Decl, Wire_Signal, Init);
      Replace_Signal (Syn_Inst, Decl, Val);
   end Synth_Signal_Declaration;

   procedure Synth_Object_Alias_Declaration
     (Syn_Inst : Synth_Instance_Acc; Decl : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Atype : constant Node := Get_Declaration_Type (Decl);
      Off : Value_Offsets;
      Dyn : Vhdl_Stmts.Dyn_Name;
      Res : Valtyp;
      Obj_Typ : Type_Acc;
      Base : Valtyp;
      Typ : Type_Acc;
   begin
      --  Subtype indication may not be present.
      if Atype /= Null_Node then
         Synth_Subtype_Indication (Syn_Inst, Atype);
         Obj_Typ := Get_Subtype_Object (Syn_Inst, Atype);
      else
         Obj_Typ := null;
      end if;

      Vhdl_Stmts.Synth_Assignment_Prefix (Syn_Inst, Get_Name (Decl),
                                          Base, Typ, Off, Dyn);
      pragma Assert (Dyn.Voff = No_Net);
      if Base.Val.Kind = Value_Net then
         --  Object is a net if it is not writable.  Extract the
         --  bits for the alias.
         Res := Create_Value_Net
           (Build2_Extract (Ctxt,
                            Get_Value_Net (Base.Val), Off.Net_Off, Typ.W),
            Typ);
      else
         Res := Create_Value_Alias (Base, Off, Typ);
      end if;
      if Obj_Typ /= null then
         Res := Synth_Subtype_Conversion (Ctxt, Res, Obj_Typ, True, Decl);
      end if;
      Create_Object (Syn_Inst, Decl, Res);
   end Synth_Object_Alias_Declaration;

   procedure Synth_Concurrent_Object_Alias_Declaration
     (Syn_Inst : Synth_Instance_Acc; Decl : Node)
   is
      Val : Valtyp;
      Aval : Valtyp;
      Obj : Value_Acc;
      Base : Node;
   begin
      Val := Get_Value (Syn_Inst, Decl);
      pragma Assert (Val.Val.Kind = Value_Alias);
      Obj := Val.Val.A_Obj;
      if Obj.Kind = Value_Signal then
         --  A signal must have been changed to a wire or a net, but the
         --  aliases have not been updated.  Update here.
         Base := Get_Base_Name (Get_Name (Decl));
         Aval := Synth_Expression (Syn_Inst, Base);

         if Aval.Val.Kind = Value_Net then
            --  Object is a net if it is not writable.  Extract the
            --  bits for the alias.
            Aval := Create_Value_Net
              (Build2_Extract (Get_Build (Syn_Inst), Get_Value_Net (Aval.Val),
                               Val.Val.A_Off.Net_Off, Val.Typ.W),
               Val.Typ);
            Val.Val.A_Off := (0, 0);
         end if;
         Val.Val.A_Obj := Aval.Val;
      end if;
   end Synth_Concurrent_Object_Alias_Declaration;

   procedure Synth_Declaration (Syn_Inst : Synth_Instance_Acc;
                                Decl : Node;
                                Is_Subprg : Boolean;
                                Last_Type : in out Node) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Variable_Declaration =>
            Synth_Variable_Declaration (Syn_Inst, Decl, Is_Subprg);
         when Iir_Kind_Interface_Variable_Declaration =>
            --  Ignore default value.
            declare
               Val : Valtyp;
               Obj_Typ : Type_Acc;
            begin
               Obj_Typ := Get_Subtype_Object (Syn_Inst, Get_Type (Decl));
               Val := Create_Var_Wire
                 (Syn_Inst, Decl, Wire_Variable, (Obj_Typ, null));
               Create_Object (Syn_Inst, Decl, Val);
            end;
         when Iir_Kind_Constant_Declaration =>
            Synth_Constant_Declaration (Syn_Inst, Decl, Is_Subprg, Last_Type);
         when Iir_Kind_Signal_Declaration =>
            pragma Assert (not Is_Subprg);
            Synth_Signal_Declaration (Syn_Inst, Decl);
         when Iir_Kind_Object_Alias_Declaration =>
            Synth_Object_Alias_Declaration (Syn_Inst, Decl);
         when Iir_Kind_Procedure_Declaration
            | Iir_Kind_Function_Declaration =>
            Elab_Subprogram_Declaration (Syn_Inst, Decl);
         when Iir_Kind_Procedure_Body
            | Iir_Kind_Function_Body =>
            null;
         when Iir_Kind_Non_Object_Alias_Declaration =>
            null;
         when Iir_Kind_Attribute_Declaration =>
            --  Nothing to do: the type is a type_mark, not a subtype
            --  indication.
            null;
         when Iir_Kind_Attribute_Specification =>
            Synth_Attribute_Specification (Syn_Inst, Decl);
         when Iir_Kind_Type_Declaration =>
            Elab_Type_Definition (Syn_Inst, Get_Type_Definition (Decl));
         when Iir_Kind_Anonymous_Type_Declaration =>
            Elab_Anonymous_Type_Definition
              (Syn_Inst, Get_Type_Definition (Decl),
               Get_Subtype_Definition (Decl));
         when Iir_Kind_Subtype_Declaration =>
            Elab_Declaration_Type (Syn_Inst, Decl);
         when Iir_Kind_Component_Declaration =>
            null;
         when Iir_Kind_File_Declaration =>
            declare
               F : File_Index;
               Res : Valtyp;
               Obj_Typ : Type_Acc;
            begin
               F := Elab.Vhdl_Files.Elaborate_File_Declaration
                 (Syn_Inst, Decl);
               Obj_Typ := Get_Subtype_Object (Syn_Inst, Get_Type (Decl));
               Res := Create_Value_File (Obj_Typ, F);
               Create_Object (Syn_Inst, Decl, Res);
            end;
         when Iir_Kind_Protected_Type_Body =>
            null;
         when Iir_Kind_Psl_Default_Clock =>
            --  Ignored; directly used by PSL directives.
            null;
         when Iir_Kind_Use_Clause =>
            null;
         when Iir_Kind_Configuration_Specification =>
            null;
         when Iir_Kind_Signal_Attribute_Declaration =>
            --  Not supported by synthesis.
            null;
         when others =>
            Vhdl.Errors.Error_Kind ("synth_declaration", Decl);
      end case;
   end Synth_Declaration;

   procedure Synth_Declarations (Syn_Inst : Synth_Instance_Acc;
                                 Decls : Node;
                                 Is_Subprg : Boolean := False)
   is
      Decl : Node;
      Last_Type : Node;
   begin
      Last_Type := Null_Node;
      Decl := Decls;
      while Is_Valid (Decl) loop
         Synth_Declaration (Syn_Inst, Decl, Is_Subprg, Last_Type);

         exit when Is_Error (Syn_Inst);

         Decl := Get_Chain (Decl);
      end loop;
   end Synth_Declarations;

   --  Finalize a variable or a signal.
   procedure Finalize_Signal (Syn_Inst : Synth_Instance_Acc; Decl : Node)
   is
      use Netlists.Gates;
      Vt : Valtyp;
      Gate_Net : Net;
      Gate : Instance;
      Drv : Net;
      Def_Val : Net;
      W : Wire_Id;
   begin
      Vt := Get_Value (Syn_Inst, Decl);
      if Vt = No_Valtyp then
         pragma Assert (Is_Error (Syn_Inst));
         return;
      end if;
      if Vt.Val.Kind /= Value_Wire then
         --  Could be a net for in ports.
         --  Could be a static value for a variable of type file.
         return;
      end if;

      W := Get_Value_Wire (Vt.Val);

      Finalize_Assignment (Get_Build (Syn_Inst), W);

      Gate_Net := Get_Wire_Gate (W);
      Gate := Get_Net_Parent (Gate_Net);
      case Get_Id (Gate) is
         when Id_Signal
            | Id_Output
            | Id_Inout =>
            Drv := Get_Input_Net (Gate, 0);
            Def_Val := No_Net;
         when Id_Isignal
            | Id_Ioutput
            | Id_Iinout =>
            Drv := Get_Input_Net (Gate, 0);
            Def_Val := Get_Input_Net (Gate, 1);
         when others =>
            --  Todo: output ?
            raise Internal_Error;
      end case;
      if Drv = No_Net then
         --  Undriven signals.
         if Is_Connected (Get_Output (Gate, 0)) then
            --  No warning if the signal is not used.
            --  TODO: maybe simply remove it.
            if Def_Val = No_Net then
               Warning_Msg_Synth
                 (+Decl, "%n is never assigned and has no default value",
                  (1 => +Decl));
            else
               Warning_Msg_Synth (+Decl, "%n is never assigned", (1 => +Decl));
            end if;
         end if;
         if Def_Val = No_Net then
            --  The initial value of an undriven signal is X.
            Def_Val := Build_Const_X (Get_Build (Syn_Inst),
                                      Get_Width (Gate_Net));
         end if;

         --  The value of an undriven signal is its initial value.
         Connect (Get_Input (Gate, 0), Def_Val);
      end if;

      Free_Wire (W);
   end Finalize_Signal;

   procedure Finalize_Declaration
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Is_Subprg : Boolean) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration =>
            if not Get_Instance_Const (Syn_Inst) then
               Finalize_Signal (Syn_Inst, Decl);
            end if;
         when Iir_Kind_Constant_Declaration =>
            null;
         when Iir_Kind_Signal_Declaration
            | Iir_Kind_Interface_Signal_Declaration =>
            pragma Assert (not Is_Subprg);
            Finalize_Signal (Syn_Inst, Decl);
         when Iir_Kind_Object_Alias_Declaration =>
            null;
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Declaration =>
            null;
         when Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Body =>
            null;
         when Iir_Kind_Non_Object_Alias_Declaration =>
            null;
         when Iir_Kind_Attribute_Declaration =>
            null;
         when Iir_Kind_Attribute_Specification =>
            null;
         when Iir_Kind_Type_Declaration =>
            null;
         when Iir_Kind_Anonymous_Type_Declaration =>
            null;
         when  Iir_Kind_Subtype_Declaration =>
            null;
         when Iir_Kind_Component_Declaration =>
            null;
         when Iir_Kind_File_Declaration =>
            null;
         when Iir_Kind_Configuration_Specification =>
            null;
         when Iir_Kind_Psl_Default_Clock =>
            --  Ignored; directly used by PSL directives.
            null;
         when Iir_Kind_Signal_Attribute_Declaration =>
            --  Not supported by synthesis.
            null;
         when others =>
            Vhdl.Errors.Error_Kind ("finalize_declaration", Decl);
      end case;
   end Finalize_Declaration;

   procedure Finalize_Declarations (Syn_Inst : Synth_Instance_Acc;
                                    Decls : Node;
                                    Is_Subprg : Boolean := False)
   is
      Decl : Node;
   begin
      Decl := Decls;
      while Is_Valid (Decl) loop
         Finalize_Declaration (Syn_Inst, Decl, Is_Subprg);

         Decl := Get_Chain (Decl);
      end loop;
   end Finalize_Declarations;

   procedure Synth_Concurrent_Declaration (Syn_Inst : Synth_Instance_Acc;
                                           Decl : Node) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Signal_Declaration =>
            Synth_Signal_Declaration (Syn_Inst, Decl);
         when Iir_Kind_Variable_Declaration =>
            Synth_Shared_Variable_Declaration (Syn_Inst, Decl);
         when Iir_Kind_Constant_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Configuration_Specification
           | Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Use_Clause =>
            --  Fully handled during elaboration.
            null;
         when Iir_Kind_Object_Alias_Declaration =>
            Synth_Concurrent_Object_Alias_Declaration (Syn_Inst, Decl);
         when Iir_Kind_Attribute_Specification =>
            Synth_Concurrent_Attribute_Specification (Syn_Inst, Decl);
         when others =>
            Vhdl.Errors.Error_Kind ("synth_concurrent_declaration", Decl);
      end case;
   end Synth_Concurrent_Declaration;

   procedure Synth_Concurrent_Declarations (Syn_Inst : Synth_Instance_Acc;
                                            Decls : Node)
   is
      Decl : Node;
   begin
      Decl := Decls;
      while Decl /= Null_Node loop
         Synth_Concurrent_Declaration (Syn_Inst, Decl);
         Decl := Get_Chain (Decl);
      end loop;
   end Synth_Concurrent_Declarations;
end Synth.Vhdl_Decls;
