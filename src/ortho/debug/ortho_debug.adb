--  Ortho debug back-end.
--  Copyright (C) 2005 Tristan Gingold
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

with Ada.Unchecked_Deallocation;

package body Ortho_Debug is
   --  If True, disable some checks so that the output can be generated.
   Disable_Checks : constant Boolean := False;

   type ON_Op_To_OE_Type is array (ON_Op_Kind) of OE_Kind;
   ON_Op_To_OE : constant ON_Op_To_OE_Type :=
     (
      ON_Nil => OE_Nil,

      --  Dyadic operations.
      ON_Add_Ov => OE_Add_Ov,
      ON_Sub_Ov => OE_Sub_Ov,
      ON_Mul_Ov => OE_Mul_Ov,
      ON_Div_Ov => OE_Div_Ov,
      ON_Rem_Ov => OE_Rem_Ov,
      ON_Mod_Ov => OE_Mod_Ov,

      --  Binary operations.
      ON_And => OE_And,
      ON_Or => OE_Or,
      ON_Xor => OE_Xor,

      --  Monadic operations.
      ON_Not => OE_Not,
      ON_Neg_Ov => OE_Neg_Ov,
      ON_Abs_Ov => OE_Abs_Ov,

      --  Comparaisons
      ON_Eq => OE_Eq,
      ON_Neq => OE_Neq,
      ON_Le => OE_Le,
      ON_Lt => OE_Lt,
      ON_Ge => OE_Ge,
      ON_Gt => OE_Gt
      );

   type Decl_Scope_Type is record
      --  Declarations are chained.
      Parent : O_Snode;
      Last_Decl : O_Dnode;
      Last_Stmt : O_Snode;

      --  If this scope corresponds to a function, PREV_FUNCTION contains
      --  the previous function.
      Prev_Function : O_Dnode;

      --  Declaration scopes are chained.
      Prev : Decl_Scope_Acc;
   end record;

   type Stmt_Kind is
     (Stmt_Function, Stmt_Declare, Stmt_If, Stmt_Loop, Stmt_Case);
   type Stmt_Scope_Type (Kind : Stmt_Kind);
   type Stmt_Scope_Acc is access Stmt_Scope_Type;
   type Stmt_Scope_Type (Kind : Stmt_Kind) is record
      --  Statement which created this scope.
      Parent : O_Snode;
      --  Previous (parent) scope.
      Prev : Stmt_Scope_Acc;
      case Kind is
         when Stmt_Function =>
            Prev_Function : Stmt_Scope_Acc;
            --  Declaration for the function.
            Decl : O_Dnode;
         when Stmt_Declare =>
            null;
         when Stmt_If =>
            Last_Elsif : O_Snode;
         when Stmt_Loop =>
            null;
         when Stmt_Case =>
            Last_Branch : O_Snode;
            Last_Choice : O_Choice;
            Case_Type : O_Tnode;
      end case;
   end record;
   subtype Stmt_Function_Scope_Type is Stmt_Scope_Type (Stmt_Function);
   subtype Stmt_Declare_Scope_Type is Stmt_Scope_Type (Stmt_Declare);
   subtype Stmt_If_Scope_Type is Stmt_Scope_Type (Stmt_If);
   subtype Stmt_Loop_Scope_Type is Stmt_Scope_Type (Stmt_Loop);
   subtype Stmt_Case_Scope_Type is Stmt_Scope_Type (Stmt_Case);

   Current_Stmt_Scope : Stmt_Scope_Acc := null;
   Current_Function : Stmt_Scope_Acc := null;
   Current_Decl_Scope : Decl_Scope_Acc := null;
   Current_Loop_Level : Natural := 0;

   procedure Push_Decl_Scope (Parent : O_Snode)
   is
      Res : Decl_Scope_Acc;
   begin
      Res := new Decl_Scope_Type'(Parent => Parent,
                                  Last_Decl => null,
                                  Last_Stmt => null,
                                  Prev_Function => null,
                                  Prev => Current_Decl_Scope);
      Parent.Alive := True;
      Current_Decl_Scope := Res;
   end Push_Decl_Scope;

   procedure Pop_Decl_Scope
   is
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Object => Decl_Scope_Type, Name => Decl_Scope_Acc);
      Old : Decl_Scope_Acc;
   begin
      Old := Current_Decl_Scope;
      Old.Parent.Alive := False;
      Current_Decl_Scope := Old.Prev;
      Unchecked_Deallocation (Old);
   end Pop_Decl_Scope;

   procedure Add_Decl (El : O_Dnode; Check_Dup : Boolean := True) is
   begin
      if Current_Decl_Scope = null then
         --  Not yet initialized, or after compilation.
         raise Program_Error;
      end if;

      --  Note: this requires an hashed ident table.
      --  Use ortho_ident_hash.
      if False and then Check_Dup
        and then not Is_Nul (El.Name)
      then
         --  Check the name is not already defined.
         declare
            E : O_Dnode;
         begin
            E := Current_Decl_Scope.Parent.Decls;
            while E /= O_Dnode_Null loop
               if Is_Equal (E.Name, El.Name) then
                  raise Syntax_Error;
               end if;
               E := E.Next;
            end loop;
         end;
      end if;

      if Current_Decl_Scope.Last_Decl = null then
         if Current_Decl_Scope.Parent.Kind = ON_Declare_Stmt then
            Current_Decl_Scope.Parent.Decls := El;
         else
            raise Type_Error;
         end if;
      else
         Current_Decl_Scope.Last_Decl.Next := El;
      end if;
      El.Next := null;
      Current_Decl_Scope.Last_Decl := El;
   end Add_Decl;

   procedure Add_Stmt (Stmt : O_Snode)
   is
   begin
      if Current_Decl_Scope = null or Current_Function = null then
         --  You are adding a statement at the global level, ie not inside
         --  a function.
         raise Syntax_Error;
      end if;

      Stmt.Next := null;
      if Current_Decl_Scope.Last_Stmt = null then
         if Current_Decl_Scope.Parent.Kind = ON_Declare_Stmt then
            Current_Decl_Scope.Parent.Stmts := Stmt;
         else
            raise Syntax_Error;
         end if;
      else
         Current_Decl_Scope.Last_Stmt.Next := Stmt;
      end if;
      Current_Decl_Scope.Last_Stmt := Stmt;
   end Add_Stmt;

   procedure Push_Stmt_Scope (Scope : Stmt_Scope_Acc)
   is
   begin
      if Scope.Prev /= Current_Stmt_Scope then
         --  SCOPE was badly initialized.
         raise Program_Error;
      end if;
      Current_Stmt_Scope := Scope;
   end Push_Stmt_Scope;

   procedure Pop_Stmt_Scope (Kind : Stmt_Kind)
   is
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Object => Stmt_Scope_Type, Name => Stmt_Scope_Acc);
      Old : Stmt_Scope_Acc;
   begin
      Old := Current_Stmt_Scope;
      if Old.Kind /= Kind then
         raise Syntax_Error;
      end if;
      --Old.Parent.Last_Stmt := Current_Decl_Scope.Last_Stmt;
      Current_Stmt_Scope := Old.Prev;
      Unchecked_Deallocation (Old);
   end Pop_Stmt_Scope;

   --  Check declaration DECL is reachable, ie its scope is in the current
   --  stack of scopes.
   procedure Check_Scope (Decl : O_Dnode)
   is
      Res : Boolean;
   begin
      if Disable_Checks then
         return;
      end if;
      case Decl.Kind is
         when ON_Interface_Decl =>
            Res := Decl.Func_Scope.Alive;
         when others =>
            Res := Decl.Scope.Alive;
      end case;
      if not Res then
         raise Syntax_Error;
      end if;
   end Check_Scope;

   --  Raise SYNTAX_ERROR if OBJ is not at a constant address.
--    procedure Check_Const_Address (Obj : O_Lnode) is
--    begin
--       case Obj.Kind is
--          when OL_Const_Ref
--            | OL_Var_Ref =>
--             case Obj.Decl.Storage is
--                when O_Storage_External
--                  | O_Storage_Public
--                  | O_Storage_Private =>
--                   null;
--                when O_Storage_Local =>
--                   raise Syntax_Error;
--             end case;
--          when others =>
--             --  FIXME: constant indexed element, selected element maybe
--             --   of const address.
--             raise Syntax_Error;
--       end case;
--    end Check_Const_Address;

   procedure Check_Type (T1, T2 : O_Tnode) is
   begin
      if T1 = T2 then
         return;
      end if;
      --  TODO: Two different subtypes with the same constraints are allowed.
      --  Is it needed ?
      if T1.Kind = ON_Array_Subtype and then T2.Kind = ON_Array_Subtype
        and then T1.Arr_Base = T2.Arr_Base
        and then T1.Arr_El_Type = T2.Arr_El_Type
        and then T1.Length.all = T2.Length.all
      then
         return;
      end if;
      if T1.Kind = ON_Record_Subtype and then T2.Kind = ON_Record_Subtype
      then
         --  TODO: check elements.
         return;
      end if;
      if not Disable_Checks then
         raise Type_Error;
      end if;
   end Check_Type;

   procedure Check_Ref (N : O_Enode) is
   begin
      if N.Ref then
         --  Already referenced.
         raise Syntax_Error;
      end if;
      N.Ref := True;
   end Check_Ref;

   procedure Check_Ref (N : O_Lnode) is
   begin
      if N.Ref then
         raise Syntax_Error;
      end if;
      N.Ref := True;
   end Check_Ref;

   procedure Check_Ref (N : O_Gnode) is
   begin
      if N.Ref then
         raise Syntax_Error;
      end if;
      N.Ref := True;
   end Check_Ref;

   procedure Check_Complete_Type (T : O_Tnode) is
   begin
      if not T.Complete then
         --  Uncomplete type cannot be used here (since its size is required,
         --   for example).
         raise Syntax_Error;
      end if;
   end Check_Complete_Type;

   procedure Check_Constrained_Type (T : O_Tnode) is
   begin
      if not T.Constrained then
         --  Unconstrained type cannot be used here (since its size is
         --  required, for example).
         null;
         raise Syntax_Error;
      end if;
   end Check_Constrained_Type;

   function New_Dyadic_Op (Kind : ON_Dyadic_Op_Kind; Left, Right : O_Enode)
     return O_Enode
   is
      K : constant OE_Kind := ON_Op_To_OE (Kind);
      Res : O_Enode;
   begin
      Check_Type (Left.Rtype, Right.Rtype);
      Check_Ref (Left);
      Check_Ref (Right);
      Res := new O_Enode_Type (K);
      Res.Rtype := Left.Rtype;
      Res.Ref := False;
      Res.Left := Left;
      Res.Right := Right;
      return Res;
   end New_Dyadic_Op;

   function New_Monadic_Op (Kind : ON_Monadic_Op_Kind; Operand : O_Enode)
     return O_Enode
   is
      Res : O_Enode;
   begin
      Check_Ref (Operand);
      Res := new O_Enode_Type (ON_Op_To_OE (Kind));
      Res.Ref := False;
      Res.Operand := Operand;
      Res.Rtype := Operand.Rtype;
      return Res;
   end New_Monadic_Op;

   function New_Compare_Op
     (Kind : ON_Compare_Op_Kind; Left, Right : O_Enode; Ntype : O_Tnode)
     return O_Enode
   is
      Res : O_Enode;
   begin
      if Ntype.Kind /= ON_Boolean_Type then
         raise Type_Error;
      end if;
      if Left.Rtype /= Right.Rtype then
         raise Type_Error;
      end if;
      Check_Ref (Left);
      Check_Ref (Right);
      Res := new O_Enode_Type (ON_Op_To_OE (Kind));
      Res.Ref := False;
      Res.Left := Left;
      Res.Right := Right;
      Res.Rtype := Ntype;
      return Res;
   end New_Compare_Op;


   function New_Signed_Literal (Ltype : O_Tnode; Value : Integer_64)
     return O_Cnode
   is
      subtype O_Cnode_Signed_Lit is O_Cnode_Type (OC_Signed_Lit);
   begin
      if Ltype.Kind = ON_Signed_Type then
         return new O_Cnode_Signed_Lit'(Kind => OC_Signed_Lit,
                                        Ctype => Ltype,
                                        Ref => False,
                                        S_Val => Value);
      else
         raise Type_Error;
      end if;
   end New_Signed_Literal;

   function New_Unsigned_Literal (Ltype : O_Tnode; Value : Unsigned_64)
     return O_Cnode
   is
      subtype O_Cnode_Unsigned_Lit is O_Cnode_Type (OC_Unsigned_Lit);
   begin
      if Ltype.Kind = ON_Unsigned_Type then
         return new O_Cnode_Unsigned_Lit'(Kind => OC_Unsigned_Lit,
                                          Ctype => Ltype,
                                          Ref => False,
                                          U_Val => Value);
      else
         raise Type_Error;
      end if;
   end New_Unsigned_Literal;

   function New_Float_Literal (Ltype : O_Tnode; Value : IEEE_Float_64)
     return O_Cnode
   is
      subtype O_Cnode_Float_Lit is O_Cnode_Type (OC_Float_Lit);
   begin
      if Ltype.Kind = ON_Float_Type then
         return new O_Cnode_Float_Lit'(Kind => OC_Float_Lit,
                                       Ctype => Ltype,
                                       Ref => False,
                                       F_Val => Value);
      else
         raise Type_Error;
      end if;
   end New_Float_Literal;

   function New_Null_Access (Ltype : O_Tnode) return O_Cnode
   is
      subtype O_Cnode_Null_Lit_Type is O_Cnode_Type (OC_Null_Lit);
   begin
      if Ltype.Kind /= ON_Access_Type then
         raise Type_Error;
      end if;
      return new O_Cnode_Null_Lit_Type'(Kind => OC_Null_Lit,
                                         Ctype => Ltype,
                                         Ref => False);
   end New_Null_Access;

   function New_Default_Value (Ltype : O_Tnode) return O_Cnode
   is
      subtype O_Cnode_Default_Lit_Type is O_Cnode_Type (OC_Default_Lit);
   begin
      return new O_Cnode_Default_Lit_Type'(Kind => OC_Default_Lit,
                                           Ctype => Ltype,
                                           Ref => False);
   end New_Default_Value;

   function New_Sizeof (Atype : O_Tnode; Rtype : O_Tnode) return O_Cnode is
   begin
      if Rtype.Kind /= ON_Unsigned_Type
        and then Rtype.Kind /= ON_Access_Type
      then
         raise Type_Error;
      end if;
      Check_Complete_Type (Atype);
      Check_Constrained_Type (Atype);
      return new O_Cnode_Type'(Kind => OC_Sizeof_Lit,
                               Ctype => Rtype,
                               Ref => False,
                               S_Type => Atype);
   end New_Sizeof;

   function New_Record_Sizeof
     (Atype : O_Tnode; Rtype : O_Tnode) return O_Cnode is
   begin
      if Rtype.Kind /= ON_Unsigned_Type
        and then Rtype.Kind /= ON_Access_Type
      then
         raise Type_Error;
      end if;
      Check_Complete_Type (Atype);
      if Atype.Kind /= ON_Record_Type then
         raise Type_Error;
      end if;
      return new O_Cnode_Type'(Kind => OC_Record_Sizeof_Lit,
                               Ctype => Rtype,
                               Ref => False,
                               S_Type => Atype);
   end New_Record_Sizeof;

   function New_Alignof (Atype : O_Tnode; Rtype : O_Tnode) return O_Cnode
   is
      subtype O_Cnode_Alignof_Type is O_Cnode_Type (OC_Alignof_Lit);
   begin
      if Rtype.Kind /= ON_Unsigned_Type then
         raise Type_Error;
      end if;
      Check_Complete_Type (Atype);
      return new O_Cnode_Alignof_Type'(Kind => OC_Alignof_Lit,
                                       Ctype => Rtype,
                                       Ref => False,
                                       S_Type => Atype);
   end New_Alignof;

   function New_Offsetof (Atype : O_Tnode; Field : O_Fnode; Rtype : O_Tnode)
                         return O_Cnode
   is
      subtype O_Cnode_Offsetof_Type is O_Cnode_Type (OC_Offsetof_Lit);
   begin
      if Rtype.Kind /= ON_Unsigned_Type
        and then Rtype.Kind /= ON_Access_Type
      then
         raise Type_Error;
      end if;
      if Field.Parent /= Atype then
         raise Type_Error;
      end if;
      return new O_Cnode_Offsetof_Type'(Kind => OC_Offsetof_Lit,
                                        Ctype => Rtype,
                                        Ref => False,
                                        Off_Field => Field);
   end New_Offsetof;

   function New_Alloca (Rtype : O_Tnode; Size : O_Enode) return O_Enode
   is
      subtype O_Enode_Alloca_Type is O_Enode_Type (OE_Alloca);
      Res : O_Enode;
   begin
      if Rtype.Kind /= ON_Access_Type then
         raise Type_Error;
      end if;
      if Size.Rtype.Kind /= ON_Unsigned_Type then
         raise Type_Error;
      end if;
      Res := new O_Enode_Alloca_Type'(Kind => OE_Alloca,
                                      Rtype => Rtype,
                                      Ref => False,
                                      A_Size => Size);
      return Res;
   end New_Alloca;

   function Get_Base_Type (Atype : O_Tnode) return O_Tnode is
   begin
      case Atype.Kind is
         when ON_Array_Subtype =>
            return Atype.Arr_Base;
         when ON_Record_Subtype =>
            return Atype.Subrec_Base;
         when others =>
            return Atype;
      end case;
   end Get_Base_Type;

   procedure New_Completed_Type_Decl (Atype : O_Tnode)
   is
      N : O_Dnode;
   begin
      if Atype.Decl = null then
         --  The uncompleted type must have been declared.
         raise Type_Error;
      end if;
      N := new O_Dnode_Type (ON_Completed_Type_Decl);
      N.Name := Atype.Decl.Name;
      N.Dtype := Atype;
      Add_Decl (N, False);
   end New_Completed_Type_Decl;

   procedure New_Uncomplete_Record_Type (Res : out O_Tnode) is
   begin
      Res := new O_Tnode_Type'(Kind => ON_Record_Type,
                               Decl => O_Dnode_Null,
                               Uncomplete => True,
                               Complete => False,
                               Constrained => True,
                               Rec_Elements => O_Fnode_Null);
   end New_Uncomplete_Record_Type;

   procedure Start_Uncomplete_Record_Type (Res : O_Tnode;
                                           Elements : out O_Element_List) is
   begin
      if not Res.Uncomplete then
         --  RES record type is not an uncomplete record type.
         raise Syntax_Error;
      end if;
      if Res.Rec_Elements /= O_Fnode_Null then
         --  RES record type already has elements...
         raise Syntax_Error;
      end if;
      Elements.Res := Res;
      Elements.Last := null;
   end Start_Uncomplete_Record_Type;

   procedure Start_Record_Type (Elements : out O_Element_List)
   is
      Res : O_Tnode;
   begin
      Res := new O_Tnode_Type'(Kind => ON_Record_Type,
                               Decl => O_Dnode_Null,
                               Uncomplete => False,
                               Complete => False,
                               Constrained => True,
                               Rec_Elements => O_Fnode_Null);
      Elements := (Res => Res,
                   Last => null);
   end Start_Record_Type;

   procedure New_Record_Field
     (Elements : in out O_Element_List;
      El : out O_Fnode;
      Ident : O_Ident; Etype : O_Tnode)
   is
   begin
      Check_Complete_Type (Etype);
      if not Etype.Constrained then
         Elements.Res.Constrained := False;
      end if;
      El := new O_Fnode_Type'(Parent => Elements.Res,
                              Next => null,
                              Ident => Ident,
                              Ftype => Etype);
      --  Append EL.
      if Elements.Last = null then
         Elements.Res.Rec_Elements := El;
      else
         Elements.Last.Next := El;
      end if;
      Elements.Last := El;
   end New_Record_Field;

   procedure Finish_Record_Type
     (Elements : in out O_Element_List; Res : out O_Tnode) is
   begin
      --  Align the structure.
      Res := Elements.Res;
      if Res.Uncomplete then
         New_Completed_Type_Decl (Res);
      end if;
      Res.Complete := True;
   end Finish_Record_Type;

   procedure Start_Record_Subtype
     (Rtype : O_Tnode; Elements : out O_Element_Sublist)
   is
      Res : O_Tnode;
   begin
      if Rtype.Kind /= ON_Record_Type then
         raise Syntax_Error;
      end if;

      Res := new O_Tnode_Type'(Kind => ON_Record_Subtype,
                               Decl => O_Dnode_Null,
                               Uncomplete => False,
                               Complete => False,
                               Constrained => True,
                               Subrec_Elements => O_Fnode_Null,
                               Subrec_Base => Rtype);
      Elements := (Res => Res,
                   Last => null,
                   Base_Field => Rtype.Rec_Elements);
   end Start_Record_Subtype;

   procedure New_Subrecord_Field
     (Elements : in out O_Element_Sublist; El : out O_Fnode; Etype : O_Tnode)
   is
      Base_Field : O_Fnode;
   begin
      Check_Complete_Type (Etype);
      Check_Constrained_Type (Etype);

      Base_Field := Elements.Base_Field;
      if Base_Field = O_Fnode_Null then
         raise Syntax_Error;
      end if;
      if Base_Field.Ftype.Constrained then
         --  For constrained field of the base type, the type must be the
         --  same.
         if Base_Field.Ftype /= Etype then
            raise Syntax_Error;
         end if;
      else
         --  Otherwise, must be a subtype.
         if Get_Base_Type (Etype) /= Base_Field.Ftype then
            raise Syntax_Error;
         end if;
      end if;
      El := new O_Fnode_Type'(Parent => Elements.Res,
                              Next => null,
                              Ident => Base_Field.Ident,
                              Ftype => Etype);

      --  Append EL.
      if Elements.Last = null then
         Elements.Res.Subrec_Elements := El;
      else
         Elements.Last.Next := El;
      end if;
      Elements.Last := El;

      Elements.Base_Field := Base_Field.Next;
   end New_Subrecord_Field;

   procedure Finish_Record_Subtype
     (Elements : in out O_Element_Sublist; Res : out O_Tnode) is
   begin
      Res := Elements.Res;
      Res.Complete := True;
   end Finish_Record_Subtype;

   procedure Start_Union_Type (Elements : out O_Element_List) is
   begin
      Elements.Res := new O_Tnode_Type'(Kind => ON_Union_Type,
                                        Decl => O_Dnode_Null,
                                        Uncomplete => False,
                                        Complete => False,
                                        Constrained => True,
                                        Rec_Elements => O_Fnode_Null);
      Elements.Last := null;
   end Start_Union_Type;

   procedure New_Union_Field
     (Elements : in out O_Element_List;
      El : out O_Fnode;
      Ident : O_Ident; Etype : O_Tnode)
   is
   begin
      New_Record_Field (Elements, El, Ident, Etype);
   end New_Union_Field;

   procedure Finish_Union_Type
     (Elements : in out O_Element_List; Res : out O_Tnode) is
   begin
      Res := Elements.Res;
      Res.Complete := True;
   end Finish_Union_Type;

   function Is_Subtype (T : O_Tnode) return Boolean is
   begin
      case T.Kind is
         when ON_Array_Subtype
            | ON_Record_Subtype =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Subtype;

   function New_Access_Type (Dtype : O_Tnode) return O_Tnode
   is
      subtype O_Tnode_Access is O_Tnode_Type (ON_Access_Type);
      Res : O_Tnode;
   begin
      Res := new O_Tnode_Access'(Kind => ON_Access_Type,
                                 Decl => O_Dnode_Null,
                                 Uncomplete => Dtype = O_Tnode_Null,
                                 Complete => True,
                                 Constrained => True,
                                 D_Type => Dtype);
      return Res;
   end New_Access_Type;

   procedure Finish_Access_Type (Atype : O_Tnode; Dtype : O_Tnode) is
   begin
      if Is_Subtype (Dtype) then
         --  Access to sub array are not allowed, use access to array.
         raise Type_Error;
      end if;
      if Atype.D_Type /= O_Tnode_Null
        or Atype.Uncomplete = False
      then
         --  Type already completed.
         raise Syntax_Error;
      end if;
      Atype.D_Type := Dtype;
      New_Completed_Type_Decl (Atype);
   end Finish_Access_Type;

   function New_Array_Type (El_Type : O_Tnode; Index_Type : O_Tnode)
     return O_Tnode
   is
      subtype O_Tnode_Array is O_Tnode_Type (ON_Array_Type);
   begin
      Check_Complete_Type (El_Type);
      return new O_Tnode_Array'(Kind => ON_Array_Type,
                                Decl => O_Dnode_Null,
                                Uncomplete => False,
                                Complete => True,
                                Constrained => False, --  By definition
                                El_Type => El_Type,
                                Index_Type => Index_Type);
   end New_Array_Type;

   function New_Array_Subtype
     (Atype : O_Tnode; El_Type : O_Tnode; Length : O_Cnode) return O_Tnode
   is
      subtype O_Tnode_Sub_Array is O_Tnode_Type (ON_Array_Subtype);
   begin
      --  Can only constraint an array type.
      if Atype.Kind /= ON_Array_Type then
         raise Type_Error;
      end if;

      --  The element must either be ATYPE element or a constrained subtype
      --  of it.
      if El_Type /= Atype.El_Type then
         if Get_Base_Type (El_Type) /= Atype.El_Type then
            raise Type_Error;
         end if;
      end if;
      Check_Constrained_Type (El_Type);

      return new O_Tnode_Sub_Array'(Kind => ON_Array_Subtype,
                                    Decl => O_Dnode_Null,
                                    Uncomplete => False,
                                    Complete => True,
                                    Constrained => True,
                                    Arr_Base => Atype,
                                    Arr_El_Type => El_Type,
                                    Length => Length);
   end New_Array_Subtype;

   function New_Unsigned_Type (Size : Natural) return O_Tnode
   is
      subtype O_Tnode_Unsigned is O_Tnode_Type (ON_Unsigned_Type);
   begin
      return new O_Tnode_Unsigned'(Kind => ON_Unsigned_Type,
                                   Decl => O_Dnode_Null,
                                   Uncomplete => False,
                                   Complete => True,
                                   Constrained => True,
                                   Int_Size => Size);
   end New_Unsigned_Type;

   function New_Signed_Type (Size : Natural) return O_Tnode
   is
      subtype O_Tnode_Signed is O_Tnode_Type (ON_Signed_Type);
   begin
      return new O_Tnode_Signed'(Kind => ON_Signed_Type,
                                 Decl => O_Dnode_Null,
                                 Uncomplete => False,
                                 Complete => True,
                                 Constrained => True,
                                 Int_Size => Size);
   end New_Signed_Type;

   function New_Float_Type return O_Tnode
   is
      subtype O_Tnode_Float is O_Tnode_Type (ON_Float_Type);
   begin
      return new O_Tnode_Float'(Kind => ON_Float_Type,
                                Decl => O_Dnode_Null,
                                Uncomplete => False,
                                Complete => True,
                                Constrained => True);
   end New_Float_Type;

   procedure New_Boolean_Type (Res : out O_Tnode;
                               False_Id : O_Ident;
                               False_E : out O_Cnode;
                               True_Id : O_Ident;
                               True_E : out O_Cnode)
   is
      subtype O_Tnode_Boolean is O_Tnode_Type (ON_Boolean_Type);
      subtype O_Cnode_Boolean_Lit is O_Cnode_Type (OC_Boolean_Lit);
   begin
      Res := new O_Tnode_Boolean'(Kind => ON_Boolean_Type,
                                  Decl => O_Dnode_Null,
                                  Uncomplete => False,
                                  Complete => True,
                                  Constrained => True,
                                  True_N => O_Cnode_Null,
                                  False_N => O_Cnode_Null);
      True_E := new O_Cnode_Boolean_Lit'(Kind => OC_Boolean_Lit,
                                         Ctype => Res,
                                         Ref => False,
                                         B_Val => True,
                                         B_Id => True_Id);
      False_E := new O_Cnode_Boolean_Lit'(Kind => OC_Boolean_Lit,
                                          Ctype => Res,
                                          Ref => False,
                                          B_Val => False,
                                          B_Id => False_Id);
      Res.True_N := True_E;
      Res.False_N := False_E;
   end New_Boolean_Type;

   procedure Start_Enum_Type (List : out O_Enum_List; Size : Natural)
   is
      pragma Unreferenced (Size);
      subtype O_Tnode_Enum is O_Tnode_Type (ON_Enum_Type);
      Res : O_Tnode;
   begin
      Res := new O_Tnode_Enum'(Kind => ON_Enum_Type,
                               Decl => O_Dnode_Null,
                               Uncomplete => False,
                               Complete => False,
                               Constrained => True,
                               Nbr => 0,
                               Literals => O_Cnode_Null);
      List.Res := Res;
      List.Last := O_Cnode_Null;
   end Start_Enum_Type;

   procedure New_Enum_Literal (List : in out O_Enum_List;
                               Ident : O_Ident;
                               Res : out O_Cnode)
   is
      subtype O_Cnode_Enum_Lit is O_Cnode_Type (OC_Enum_Lit);
   begin
      Res := new O_Cnode_Enum_Lit'(Kind => OC_Enum_Lit,
                                   Ctype => List.Res,
                                   Ref => False,
                                   E_Val => List.Res.Nbr,
                                   E_Name => Ident,
                                   E_Next => O_Cnode_Null);
      --  Link it.
      if List.Last = O_Cnode_Null then
         List.Res.Literals := Res;
      else
         List.Last.E_Next := Res;
      end if;
      List.Last := Res;

      List.Res.Nbr := List.Res.Nbr + 1;
   end New_Enum_Literal;

   procedure Finish_Enum_Type (List : in out O_Enum_List; Res : out O_Tnode) is
   begin
      Res := List.Res;
      Res.Complete := True;
   end Finish_Enum_Type;

   function Get_Array_El_Type (Atype : O_Tnode) return O_Tnode is
   begin
      case Atype.Kind is
         when ON_Array_Subtype =>
            return Atype.Arr_El_Type;
         when ON_Array_Type =>
            return Atype.El_Type;
         when others =>
            raise Syntax_Error;
      end case;
   end Get_Array_El_Type;

   function Get_Record_Elements (Atype : O_Tnode) return O_Fnode is
   begin
      case Atype.Kind is
         when ON_Record_Subtype =>
            return Atype.Subrec_Elements;
         when ON_Record_Type =>
            return Atype.Rec_Elements;
         when others =>
            raise Syntax_Error;
      end case;
   end Get_Record_Elements;

   procedure Start_Record_Aggr (List : out O_Record_Aggr_List; Atype : O_Tnode)
   is
      subtype O_Cnode_Aggregate is O_Cnode_Type (OC_Record_Aggregate);
      Res : O_Cnode;
      Els : O_Fnode;
   begin
      Els := Get_Record_Elements (Atype);
      Check_Complete_Type (Atype);
      Res := new O_Cnode_Aggregate'(Kind => OC_Record_Aggregate,
                                    Ctype => Atype,
                                    Ref => False,
                                    Rec_Els => null);
      List.Res := Res;
      List.Last := null;
      List.Field := Els;
   end Start_Record_Aggr;

   procedure New_Record_Aggr_El (List : in out O_Record_Aggr_List;
                                 Value : O_Cnode)
   is
      subtype O_Cnode_Aggrel_Type is O_Cnode_Type (OC_Aggr_Element);
      El : O_Cnode;
   begin
      if List.Field = O_Fnode_Null then
         --  No more element in the aggregate.
         raise Syntax_Error;
      end if;
      Check_Type (Value.Ctype, List.Field.Ftype);
      El := new O_Cnode_Aggrel_Type'(Kind => OC_Aggr_Element,
                                     Ctype => Value.Ctype,
                                     Ref => False,
                                     Aggr_Value => Value,
                                     Aggr_Next => null);
      if List.Last = null then
         List.Res.Rec_Els := El;
      else
         List.Last.Aggr_Next := El;
      end if;
      List.Last := El;
      List.Field := List.Field.Next;
   end New_Record_Aggr_El;

   procedure Finish_Record_Aggr
     (List : in out O_Record_Aggr_List; Res : out O_Cnode)
   is
   begin
      if List.Field /= null then
         --  Not enough elements in aggregate.
         raise Type_Error;
      end if;
      Res := List.Res;
   end Finish_Record_Aggr;

   procedure Start_Array_Aggr
     (List : out O_Array_Aggr_List; Atype : O_Tnode; Len : Unsigned_32)
   is
      subtype O_Cnode_Aggregate is O_Cnode_Type (OC_Array_Aggregate);
      Res : O_Cnode;
   begin
      case Atype.Kind is
         when ON_Array_Subtype =>
            if Atype.Length.U_Val /= Unsigned_64 (Len) then
               raise Type_Error;
            end if;
         when ON_Array_Type =>
            null;
         when others =>
            raise Type_Error;
      end case;
      List.El_Type := Get_Array_El_Type (Atype);
      Check_Complete_Type (Atype);
      Res := new O_Cnode_Aggregate'(Kind => OC_Array_Aggregate,
                                    Ctype => Atype,
                                    Ref => False,
                                    Arr_Len => Len,
                                    Arr_Els => null);
      List.Res := Res;
      List.Last := null;
   end Start_Array_Aggr;

   procedure New_Array_Aggr_El (List : in out O_Array_Aggr_List;
                                Value : O_Cnode)
   is
      subtype O_Cnode_Aggrel_Type is O_Cnode_Type (OC_Aggr_Element);
      El : O_Cnode;
   begin
      Check_Type (Value.Ctype, List.El_Type);
      El := new O_Cnode_Aggrel_Type'(Kind => OC_Aggr_Element,
                                     Ctype => Value.Ctype,
                                     Ref => False,
                                     Aggr_Value => Value,
                                     Aggr_Next => null);
      if List.Last = null then
         List.Res.Arr_Els := El;
      else
         List.Last.Aggr_Next := El;
      end if;
      List.Last := El;
   end New_Array_Aggr_El;

   procedure Finish_Array_Aggr
     (List : in out O_Array_Aggr_List; Res : out O_Cnode) is
   begin
      Res := List.Res;
   end Finish_Array_Aggr;

   function New_Union_Aggr (Atype : O_Tnode; Field : O_Fnode; Value : O_Cnode)
                           return O_Cnode
   is
      subtype O_Cnode_Union_Aggr is O_Cnode_Type (OC_Union_Aggr);
      Res : O_Cnode;
   begin
      if Atype.Kind /= ON_Union_Type then
         raise Type_Error;
      end if;
      Check_Type (Value.Ctype, Field.Ftype);

      Res := new O_Cnode_Union_Aggr'(Kind => OC_Union_Aggr,
                                     Ctype => Atype,
                                     Ref => False,
                                     Uaggr_Field => Field,
                                     Uaggr_Value => Value);
      return Res;
   end New_Union_Aggr;

   function New_Obj (Obj : O_Dnode) return O_Lnode
   is
      subtype O_Lnode_Obj is O_Lnode_Type (OL_Obj);
   begin
      case Obj.Kind is
         when ON_Const_Decl
           | ON_Var_Decl
           | ON_Interface_Decl =>
            null;
         when others =>
            raise Syntax_Error;
      end case;
      Check_Scope (Obj);
      return new O_Lnode_Obj'(Kind => OL_Obj,
                              Rtype => Obj.Dtype,
                              Ref => False,
                              Obj => Obj);
   end New_Obj;

   function New_Global (Decl : O_Dnode) return O_Gnode
   is
      subtype O_Gnode_Decl is O_Gnode_Type (OG_Decl);
   begin
      case Decl.Kind is
         when ON_Const_Decl
           | ON_Var_Decl =>
            null;
         when others =>
            raise Syntax_Error;
      end case;
      if Decl.Storage = O_Storage_Local then
         raise Syntax_Error;
      end if;
      return new O_Gnode_Decl'(Kind => OG_Decl,
                               Rtype => Decl.Dtype,
                               Ref => False,
                               Decl => Decl);
   end New_Global;

   function New_Indexed_Element (Arr : O_Lnode; Index : O_Enode)
                                return O_Lnode
   is
      subtype O_Lnode_Indexed is O_Lnode_Type (OL_Indexed_Element);
      El_Type : O_Tnode;
      Res : O_Lnode;
   begin
      if Arr.Rtype.Kind not in ON_Array_Kinds then
         --  Can only index an array.
         raise Type_Error;
      end if;
      --  The element type of ARR must be constrained.
      El_Type := Get_Array_El_Type (Arr.Rtype);
      Check_Constrained_Type (El_Type);
      Check_Ref (Arr);
      Res := new O_Lnode_Indexed'(Kind => OL_Indexed_Element,
                                  Rtype => El_Type,
                                  Ref => False,
                                  Array_Base => Arr,
                                  Index => Index);
      return Res;
   end New_Indexed_Element;

   function New_Slice (Arr : O_Lnode; Res_Type : O_Tnode; Index : O_Enode)
                      return O_Lnode
   is
      subtype O_Lnode_Slice is O_Lnode_Type (OL_Slice);
      Res : O_Lnode;
   begin
      if Arr.Rtype.Kind not in ON_Array_Kinds then
         --  Can only slice an array.
         raise Type_Error;
      end if;
      --  The element type of ARR must be constrained.
      Check_Constrained_Type (Get_Array_El_Type (Res_Type));
      --  The result is an array.
      if Res_Type.Kind not in ON_Array_Kinds then
         raise Type_Error;
      end if;
      Check_Ref (Arr);
      Check_Ref (Index);
      -- FIXME: check type.
      Res := new O_Lnode_Slice'(Kind => OL_Slice,
                                Rtype => Res_Type,
                                Ref => False,
                                Slice_Base => Arr,
                                Slice_Index => Index);
      return Res;
   end New_Slice;

   function New_Selected_Element (Rec : O_Lnode; El : O_Fnode)
     return O_Lnode
   is
      subtype O_Lnode_Selected_Element is O_Lnode_Type (OL_Selected_Element);
   begin
      case Rec.Rtype.Kind is
         when ON_Record_Type
            | ON_Record_Subtype
            | ON_Union_Type =>
            null;
         when others =>
            raise Type_Error;
      end case;
      if Rec.Rtype /= El.Parent then
         raise Type_Error;
      end if;
      Check_Ref (Rec);
      return new O_Lnode_Selected_Element'(Kind => OL_Selected_Element,
                                           Rtype => El.Ftype,
                                           Ref => False,
                                           Rec_Base => Rec,
                                           Rec_El => El);
   end New_Selected_Element;

   function New_Global_Selected_Element (Rec : O_Gnode; El : O_Fnode)
                                        return O_Gnode
   is
      subtype O_Gnode_Selected_Element is O_Gnode_Type (OG_Selected_Element);
   begin
      if Rec.Rtype.Kind /= ON_Record_Type
        and then Rec.Rtype.Kind /= ON_Union_Type
      then
         raise Type_Error;
      end if;
      if Rec.Rtype /= El.Parent then
         raise Type_Error;
      end if;
      Check_Ref (Rec);
      return new O_Gnode_Selected_Element'(Kind => OG_Selected_Element,
                                           Rtype => El.Ftype,
                                           Ref => False,
                                           Rec_Base => Rec,
                                           Rec_El => El);
   end New_Global_Selected_Element;

   function New_Access_Element (Acc : O_Enode) return O_Lnode
   is
      subtype O_Lnode_Access_Element is O_Lnode_Type (OL_Access_Element);
   begin
      if Acc.Rtype.Kind /= ON_Access_Type then
         raise Type_Error;
      end if;
      Check_Ref (Acc);
      return new O_Lnode_Access_Element'(Kind => OL_Access_Element,
                                         Rtype => Acc.Rtype.D_Type,
                                         Ref => False,
                                         Acc_Base => Acc);
   end New_Access_Element;

   function Check_Conv (Source : ON_Type_Kind; Target : ON_Type_Kind)
     return Boolean
   is
      type Conv_Array is array (ON_Type_Kind, ON_Type_Kind) of Boolean;
      T : constant Boolean := True;
      F : constant Boolean := False;
      Conv_Allowed : constant Conv_Array :=
        --                     B  E  U  S  F  A  a  R  r  U  A
        (ON_Boolean_Type =>   (T, F, T, T, F, F, F, F, F, F, F),
         ON_Enum_Type =>      (F, F, T, T, F, F, F, F, F, F, F),
         ON_Unsigned_Type =>  (T, T, T, T, F, F, F, F, F, F, F),
         ON_Signed_Type =>    (T, T, T, T, T, F, F, F, F, F, F),
         ON_Float_Type =>     (F, F, F, T, T, F, F, F, F, F, F),
         ON_Array_Type =>     (F, F, F, F, F, F, F, F, F, F, F),
         ON_Array_Subtype =>  (F, F, F, F, F, F, F, F, F, F, F),
         ON_Record_Type =>    (F, F, F, F, F, F, F, F, F, F, F),
         ON_Record_Subtype => (F, F, F, F, F, F, F, F, F, F, F),
         ON_Union_Type =>     (F, F, F, F, F, F, F, F, F, F, F),
         ON_Access_Type =>    (F, F, F, F, F, F, F, F, F, F, T));
   begin
      if Source = Target then
         return True;
      else
         return Conv_Allowed (Source, Target);
      end if;
   end Check_Conv;

   function New_Convert_Ov (Val : O_Enode; Rtype : O_Tnode) return O_Enode
   is
      Res : O_Enode;
   begin
      Check_Ref (Val);
      if not Check_Conv (Val.Rtype.Kind, Rtype.Kind) then
         raise Type_Error;
      end if;
      Res := new O_Enode_Type'(Kind => OE_Convert_Ov,
                               Rtype => Rtype,
                               Ref => False,
                               Conv => Val);
      return Res;
   end New_Convert_Ov;

   function New_Convert (Val : O_Enode; Rtype : O_Tnode) return O_Enode
   is
      Res : O_Enode;
   begin
      Check_Ref (Val);
      if not Check_Conv (Val.Rtype.Kind, Rtype.Kind) then
         raise Type_Error;
      end if;
      Res := new O_Enode_Type'(Kind => OE_Convert,
                               Rtype => Rtype,
                               Ref => False,
                               Conv => Val);
      return Res;
   end New_Convert;

   function New_Unchecked_Address (Lvalue : O_Lnode; Atype : O_Tnode)
     return O_Enode
   is
      subtype O_Enode_Address is O_Enode_Type (OE_Unchecked_Address);
   begin
      Check_Ref (Lvalue);
      if Atype.Kind /= ON_Access_Type then
         --  An address is of type access.
         raise Type_Error;
      end if;
      return new O_Enode_Address'(Kind => OE_Unchecked_Address,
                                  Rtype => Atype,
                                  Ref => False,
                                  Lvalue => Lvalue);
   end New_Unchecked_Address;

   function New_Address (Lvalue : O_Lnode; Atype : O_Tnode) return O_Enode
   is
      subtype O_Enode_Address is O_Enode_Type (OE_Address);
   begin
      Check_Ref (Lvalue);
      if Atype.Kind /= ON_Access_Type then
         --  An address is of type access.
         raise Type_Error;
      end if;
      Check_Type (Get_Base_Type (Lvalue.Rtype), Get_Base_Type (Atype.D_Type));
      return new O_Enode_Address'(Kind => OE_Address,
                                  Rtype => Atype,
                                  Ref => False,
                                  Lvalue => Lvalue);
   end New_Address;

   function New_Global_Unchecked_Address (Lvalue : O_Gnode; Atype : O_Tnode)
     return O_Cnode
   is
      subtype O_Cnode_Address is O_Cnode_Type (OC_Unchecked_Address);
   begin
      --  FIXME: check Lvalue is a static object.
      Check_Ref (Lvalue);
      if Atype.Kind /= ON_Access_Type then
         --  An address is of type access.
         raise Type_Error;
      end if;
      return new O_Cnode_Address'(Kind => OC_Unchecked_Address,
                                  Ctype => Atype,
                                  Ref => False,
                                  Addr_Global => Lvalue);
   end New_Global_Unchecked_Address;

   function New_Global_Address (Lvalue : O_Gnode; Atype : O_Tnode)
                               return O_Cnode
   is
      subtype O_Cnode_Address is O_Cnode_Type (OC_Address);
   begin
      --  FIXME: check Lvalue is a static object.
      Check_Ref (Lvalue);
      if Atype.Kind /= ON_Access_Type then
         --  An address is of type access.
         raise Type_Error;
      end if;
      if Get_Base_Type (Lvalue.Rtype) /= Get_Base_Type (Atype.D_Type) then
         raise Type_Error;
      end if;
      return new O_Cnode_Address'(Kind => OC_Address,
                                  Ctype => Atype,
                                  Ref => False,
                                  Addr_Global => Lvalue);
   end New_Global_Address;

   function New_Subprogram_Address (Subprg : O_Dnode; Atype : O_Tnode)
     return O_Cnode
   is
      subtype O_Cnode_Subprg_Address is O_Cnode_Type (OC_Subprogram_Address);
   begin
      if Atype.Kind /= ON_Access_Type then
         --  An address is of type access.
         raise Type_Error;
      end if;
      return new O_Cnode_Subprg_Address'(Kind => OC_Subprogram_Address,
                                         Ctype => Atype,
                                         Ref => False,
                                         Addr_Decl => Subprg);
   end New_Subprogram_Address;

   --  Raise TYPE_ERROR is ATYPE is a composite type.
   procedure Check_Not_Composite (Atype : O_Tnode) is
   begin
      case Atype.Kind is
         when ON_Boolean_Type
           | ON_Unsigned_Type
           | ON_Signed_Type
           | ON_Float_Type
           | ON_Enum_Type
           | ON_Access_Type=>
            return;
         when ON_Array_Type
           | ON_Record_Type
           | ON_Record_Subtype
           | ON_Union_Type
           | ON_Array_Subtype =>
            raise Type_Error;
      end case;
   end Check_Not_Composite;

   function New_Value (Lvalue : O_Lnode) return O_Enode is
      subtype O_Enode_Value is O_Enode_Type (OE_Value);
   begin
      Check_Not_Composite (Lvalue.Rtype);
      Check_Ref (Lvalue);
      return new O_Enode_Value'(Kind => OE_Value,
                                Rtype => Lvalue.Rtype,
                                Ref => False,
                                Value => Lvalue);
   end New_Value;

   function New_Obj_Value (Obj : O_Dnode) return O_Enode is
   begin
      return New_Value (New_Obj (Obj));
   end New_Obj_Value;

   function New_Lit (Lit : O_Cnode) return O_Enode is
      subtype O_Enode_Lit is O_Enode_Type (OE_Lit);
   begin
      Check_Not_Composite (Lit.Ctype);
      return new O_Enode_Lit'(Kind => OE_Lit,
                              Rtype => Lit.Ctype,
                              Ref => False,
                              Lit => Lit);
   end New_Lit;

   ---------------------
   --  Declarations.  --
   ---------------------

   procedure New_Debug_Filename_Decl (Filename : String)
   is
      subtype O_Dnode_Filename_Decl is O_Dnode_Type (ON_Debug_Filename_Decl);
      N : O_Dnode;
   begin
      N := new O_Dnode_Filename_Decl;
      N.Filename := new String'(Filename);
      Add_Decl (N, False);
   end New_Debug_Filename_Decl;

   procedure New_Debug_Line_Decl (Line : Natural)
   is
      subtype O_Dnode_Line_Decl is O_Dnode_Type (ON_Debug_Line_Decl);
      N : O_Dnode;
   begin
      N := new O_Dnode_Line_Decl;
      N.Line := Line;
      Add_Decl (N, False);
   end New_Debug_Line_Decl;

   procedure New_Debug_Comment_Decl (Comment : String)
   is
      subtype O_Dnode_Comment_Decl is O_Dnode_Type (ON_Debug_Comment_Decl);
      N : O_Dnode;
   begin
      N := new O_Dnode_Comment_Decl;
      N.Comment := new String'(Comment);
      Add_Decl (N, False);
   end New_Debug_Comment_Decl;

   procedure New_Type_Decl (Ident : O_Ident; Atype : O_Tnode)
   is
      N : O_Dnode;
   begin
      if Atype.Decl /= null then
         --  Type was already declared.
         raise Type_Error;
      end if;
      N := new O_Dnode_Type (ON_Type_Decl);
      N.Name := Ident;
      N.Dtype := Atype;
      Atype.Decl := N;
      Add_Decl (N);
   end New_Type_Decl;

   procedure Check_Object_Storage (Storage : O_Storage) is
   begin
      if Current_Function /= null then
         --  Inside a subprogram.
         case Storage is
            when O_Storage_Public =>
               --  Cannot create public variables inside a subprogram.
               raise Syntax_Error;
            when O_Storage_Private
              | O_Storage_Local
              | O_Storage_External =>
               null;
         end case;
      else
         --  Global scope.
         case Storage is
            when O_Storage_Public
              | O_Storage_Private
              | O_Storage_External =>
               null;
            when O_Storage_Local =>
               --  Cannot create a local variables outside a subprogram.
               raise Syntax_Error;
         end case;
      end if;
   end Check_Object_Storage;

   procedure New_Const_Decl
     (Res : out O_Dnode;
      Ident : O_Ident;
      Storage : O_Storage;
      Atype : O_Tnode)
   is
      subtype O_Dnode_Const is O_Dnode_Type (ON_Const_Decl);
   begin
      Check_Complete_Type (Atype);
      Check_Constrained_Type (Atype);
      if Storage = O_Storage_Local then
         --  A constant cannot be local.
         raise Syntax_Error;
      end if;
      Check_Object_Storage (Storage);
      Res := new O_Dnode_Const'(Kind => ON_Const_Decl,
                                Name => Ident,
                                Next => null,
                                Dtype => Atype,
                                Storage => Storage,
                                Scope => Current_Decl_Scope.Parent,
                                Lineno => 0,
                                Value_Decl => O_Dnode_Null);
      Add_Decl (Res);
   end New_Const_Decl;

   procedure Start_Init_Value (Decl : in out O_Dnode)
   is
      subtype O_Dnode_Init_Value is O_Dnode_Type (ON_Init_Value);
      N : O_Dnode;
   begin
      if Decl.Value_Decl /= O_Dnode_Null then
         --  Constant already has a value.
         raise Syntax_Error;
      end if;

      if Decl.Storage = O_Storage_External then
         --  An external variable/constant cannot have a value.
         raise Syntax_Error;
      end if;

      --  FIXME: check scope is the same.

      N := new O_Dnode_Init_Value'(Kind => ON_Init_Value,
                                   Name => Decl.Name,
                                   Next => null,
                                   Dtype => Decl.Dtype,
                                   Storage => Decl.Storage,
                                   Scope => Current_Decl_Scope.Parent,
                                   Lineno => 0,
                                   Init_Decl => Decl,
                                   Value => O_Cnode_Null);
      Decl.Value_Decl := N;
      Add_Decl (N, False);
   end Start_Init_Value;

   procedure Finish_Init_Value (Decl : in out O_Dnode; Val : O_Cnode) is
   begin
      if Decl.Value_Decl = O_Dnode_Null then
         --  Start_Init_Value not called.
         raise Syntax_Error;
      end if;
      if Decl.Value_Decl.Value /= O_Cnode_Null then
         --  Finish_Init_Value already called.
         raise Syntax_Error;
      end if;
      if Val = O_Cnode_Null then
         --  No value or bad type.
         raise Type_Error;
      end if;
      Check_Type (Val.Ctype, Decl.Dtype);
      Decl.Value_Decl.Value := Val;
   end Finish_Init_Value;

   procedure New_Var_Decl
     (Res : out O_Dnode;
      Ident : O_Ident;
      Storage : O_Storage;
      Atype : O_Tnode)
   is
      subtype O_Dnode_Var is O_Dnode_Type (ON_Var_Decl);
   begin
      Check_Complete_Type (Atype);
      Check_Constrained_Type (Atype);
      Check_Object_Storage (Storage);
      Res := new O_Dnode_Var'(Kind => ON_Var_Decl,
                              Name => Ident,
                              Next => null,
                              Dtype => Atype,
                              Storage => Storage,
                              Lineno => 0,
                              Scope => Current_Decl_Scope.Parent,
                              Value_Decl => O_Dnode_Null);
      Add_Decl (Res);
   end New_Var_Decl;

   procedure Start_Subprogram_Decl_1
     (Interfaces : out O_Inter_List;
      Ident : O_Ident;
      Storage : O_Storage;
      Rtype : O_Tnode)
   is
      subtype O_Dnode_Function is O_Dnode_Type (ON_Function_Decl);
      N : O_Dnode;
   begin
      N := new O_Dnode_Function'(Kind => ON_Function_Decl,
                                 Next => null,
                                 Name => Ident,
                                 Dtype => Rtype,
                                 Storage => Storage,
                                 Scope => Current_Decl_Scope.Parent,
                                 Lineno => 0,
                                 Interfaces => null,
                                 Func_Body => null,
                                 Alive => False);
      Add_Decl (N);
      Interfaces.Func := N;
      Interfaces.Last := null;
   end Start_Subprogram_Decl_1;

   procedure Start_Function_Decl
     (Interfaces : out O_Inter_List;
      Ident : O_Ident;
      Storage : O_Storage;
      Rtype : O_Tnode)
   is
   begin
      Check_Not_Composite (Rtype);
      Check_Complete_Type (Rtype);
      Start_Subprogram_Decl_1 (Interfaces, Ident, Storage, Rtype);
   end Start_Function_Decl;

   procedure Start_Procedure_Decl
     (Interfaces : out O_Inter_List;
      Ident : O_Ident;
      Storage : O_Storage) is
   begin
      Start_Subprogram_Decl_1 (Interfaces, Ident, Storage, null);
   end Start_Procedure_Decl;

   procedure New_Interface_Decl
     (Interfaces : in out O_Inter_List;
      Res : out O_Dnode;
      Ident : O_Ident;
      Atype : O_Tnode)
   is
      subtype O_Dnode_Interface is O_Dnode_Type (ON_Interface_Decl);
   begin
      Check_Not_Composite (Atype);
      Check_Complete_Type (Atype);
      Res := new O_Dnode_Interface'(Kind => ON_Interface_Decl,
                                    Next => null,
                                    Name => Ident,
                                    Dtype => Atype,
                                    Storage => O_Storage_Private,
                                    Scope => Current_Decl_Scope.Parent,
                                    Lineno => 0,
                                    Func_Scope => Interfaces.Func);
      if Interfaces.Last = null then
         Interfaces.Func.Interfaces := Res;
      else
         Interfaces.Last.Next := Res;
      end if;
      Interfaces.Last := Res;
   end New_Interface_Decl;

   procedure Finish_Subprogram_Decl
     (Interfaces : in out O_Inter_List; Res : out O_Dnode)
   is
   begin
      Res := Interfaces.Func;
   end Finish_Subprogram_Decl;

   procedure Start_Subprogram_Body (Func : O_Dnode)
   is
      B : O_Dnode;
      S : O_Snode;
   begin
      if Func.Func_Body /= null then
         --  Function was already declared.
         raise Syntax_Error;
      end if;
      S := new O_Snode_Type (ON_Declare_Stmt);
      S.all := O_Snode_Type'(Kind => ON_Declare_Stmt,
                             Next => null,
                             Decls => null,
                             Stmts => null,
                             Lineno => 0,
                             Alive => True);
      B := new O_Dnode_Type (ON_Function_Body);
      B.all := O_Dnode_Type'(ON_Function_Body,
                             Name => Func.Name,
                             Dtype => Func.Dtype,
                             Storage => Func.Storage,
                             Scope => Current_Decl_Scope.Parent,
                             Lineno => 0,
                             Func_Decl => Func,
                             Func_Stmt => S,
                             Next => null);
      Add_Decl (B, False);
      Func.Func_Body := B;
      Push_Decl_Scope (S);
      Push_Stmt_Scope
        (new Stmt_Function_Scope_Type'(Kind => Stmt_Function,
                                       Parent => S,
                                       Prev => Current_Stmt_Scope,
                                       Prev_Function => Current_Function,
                                       Decl => Func));
      Current_Function := Current_Stmt_Scope;
      Func.Alive := True;
   end Start_Subprogram_Body;

   procedure Finish_Subprogram_Body is
   begin
      Pop_Decl_Scope;
      if Current_Function.Kind /= Stmt_Function then
         --  Internal error.
         raise Syntax_Error;
      end if;
      Current_Function.Decl.Alive := False;
      Current_Function := Current_Function.Prev_Function;
      Pop_Stmt_Scope (Stmt_Function);
   end Finish_Subprogram_Body;

   -------------------
   --  Statements.  --
   -------------------

   procedure New_Debug_Line_Stmt (Line : Natural)
   is
      subtype O_Snode_Line_Stmt is O_Snode_Type (ON_Debug_Line_Stmt);
   begin
      Add_Stmt (new O_Snode_Line_Stmt'(Kind => ON_Debug_Line_Stmt,
                                       Next => null,
                                       Lineno => 0,
                                       Line => Line));
   end New_Debug_Line_Stmt;

   procedure New_Debug_Comment_Stmt (Comment : String)
   is
      subtype O_Snode_Comment_Stmt is O_Snode_Type (ON_Debug_Comment_Stmt);
   begin
      Add_Stmt (new O_Snode_Comment_Stmt'(Kind => ON_Debug_Comment_Stmt,
                                          Next => null,
                                          Lineno => 0,
                                          Comment => new String'(Comment)));
   end New_Debug_Comment_Stmt;

   procedure Start_Declare_Stmt
   is
      N : O_Snode;
   begin
      N := new O_Snode_Type (ON_Declare_Stmt);
      Add_Stmt (N);
      Push_Decl_Scope (N);
      Push_Stmt_Scope
        (new Stmt_Declare_Scope_Type'(Kind => Stmt_Declare,
                                      Parent => N,
                                      Prev => Current_Stmt_Scope));
   end Start_Declare_Stmt;

   procedure Finish_Declare_Stmt is
   begin
      Pop_Decl_Scope;
      Pop_Stmt_Scope (Stmt_Declare);
   end Finish_Declare_Stmt;

   procedure New_Assign_Stmt (Target : O_Lnode; Value : O_Enode)
   is
      N : O_Snode;
   begin
      Check_Type (Target.Rtype, Value.Rtype);
      Check_Not_Composite (Target.Rtype);
      Check_Ref (Target);
      Check_Ref (Value);
      N := new O_Snode_Type (ON_Assign_Stmt);
      N.all := O_Snode_Type'(Kind => ON_Assign_Stmt,
                             Next => null,
                             Lineno => 0,
                             Target => Target,
                             Value => Value);
      Add_Stmt (N);
   end New_Assign_Stmt;

   procedure New_Return_Stmt_1 (Value : O_Enode)
   is
      subtype O_Snode_Return_Stmt is O_Snode_Type (ON_Return_Stmt);
      N : O_Snode;
   begin
      N := new O_Snode_Return_Stmt'(Kind => ON_Return_Stmt,
                                    Next => null,
                                    Lineno => 0,
                                    Ret_Val => Value);
      Add_Stmt (N);
   end New_Return_Stmt_1;

   procedure New_Return_Stmt (Value : O_Enode)
   is
   begin
      if Current_Function = null
        or else Current_Function.Decl.Dtype = O_Tnode_Null
      then
         -- Either not in a function or in a procedure.
         raise Syntax_Error;
      end if;
      Check_Type (Value.Rtype, Current_Function.Decl.Dtype);
      Check_Ref (Value);
      New_Return_Stmt_1 (Value);
   end New_Return_Stmt;

   procedure New_Return_Stmt is
   begin
      if Current_Function = null
        or else Current_Function.Decl.Dtype /= O_Tnode_Null
      then
         -- Not in a procedure.
         raise Syntax_Error;
      end if;
      New_Return_Stmt_1 (null);
   end New_Return_Stmt;

   procedure Start_Association (Assocs : out O_Assoc_List; Subprg : O_Dnode)
   is
   begin
      Check_Scope (Subprg);
      Assocs.Subprg := Subprg;
      Assocs.Interfaces := Subprg.Interfaces;
      Assocs.First := null;
      Assocs.Last := null;
   end Start_Association;

   procedure New_Association (Assocs : in out O_Assoc_List; Val : O_Enode)
   is
      N : O_Anode;
   begin
      if Assocs.Interfaces = null then
         --  Too many arguments.
         raise Syntax_Error;
      end if;
      Check_Type (Assocs.Interfaces.Dtype, Val.Rtype);
      Check_Ref (Val);
      N := new O_Anode_Type'(Next => null,
                             Formal => Assocs.Interfaces, Actual => Val);
      Assocs.Interfaces := Assocs.Interfaces.Next;
      if Assocs.Last = null then
         Assocs.First := N;
      else
         Assocs.Last.Next := N;
      end if;
      Assocs.Last := N;
   end New_Association;

   function New_Function_Call (Assocs : O_Assoc_List) return O_Enode
   is
      subtype O_Enode_Call is O_Enode_Type (OE_Function_Call);
      Res : O_Enode;
   begin
      if Assocs.Interfaces /= null then
         --  Not enough arguments.
         raise Syntax_Error;
      end if;
      if Assocs.Subprg.Dtype = null then
         --  This is a procedure.
         raise Syntax_Error;
      end if;

      Res := new O_Enode_Call'(Kind => OE_Function_Call,
                               Rtype => Assocs.Subprg.Dtype,
                               Ref => False,
                               Func => Assocs.Subprg,
                               Assoc => Assocs.First);
      return Res;
   end New_Function_Call;

   procedure New_Procedure_Call (Assocs : in out O_Assoc_List)
   is
      N : O_Snode;
   begin
      if Assocs.Interfaces /= null then
         --  Not enough arguments.
         raise Syntax_Error;
      end if;
      if Assocs.Subprg.Dtype /= null then
         --  This is a function.
         raise Syntax_Error;
      end if;
      N := new O_Snode_Type (ON_Call_Stmt);
      N.Proc := Assocs.Subprg;
      N.Assoc := Assocs.First;
      Add_Stmt (N);
   end New_Procedure_Call;

   procedure New_Elsif_Stmt (Block : in out O_If_Block; Cond : O_Enode);

   procedure Start_If_Stmt (Block : in out O_If_Block; Cond : O_Enode)
   is
      subtype O_Snode_If is O_Snode_Type (ON_If_Stmt);
      N : O_Snode;
   begin
      --  Note: no checks are performed here, since they are done in
      --  new_elsif_stmt.
      N := new O_Snode_If'(Kind => ON_If_Stmt,
                           Next => null,
                           Lineno => 0,
                           Elsifs => null,
                           If_Last => null);
      Add_Stmt (N);
      Push_Stmt_Scope (new Stmt_If_Scope_Type'(Kind => Stmt_If,
                                               Parent => N,
                                               Prev => Current_Stmt_Scope,
                                               Last_Elsif => null));
      New_Elsif_Stmt (Block, Cond);
   end Start_If_Stmt;

   procedure New_Elsif_Stmt (Block : in out O_If_Block; Cond : O_Enode)
   is
      pragma Unreferenced (Block);
      N : O_Snode;
   begin
      if Cond /= null then
         if Cond.Rtype.Kind /= ON_Boolean_Type then
            raise Type_Error;
         end if;
         Check_Ref (Cond);
      end if;
      N := new O_Snode_Type (ON_Elsif_Stmt);
      N.all := O_Snode_Type'(Kind => ON_Elsif_Stmt,
                             Next => null,
                             Lineno => 0,
                             Cond => Cond,
                             Next_Elsif => null);
      if Current_Stmt_Scope.Kind /= Stmt_If then
         raise Syntax_Error;
      end if;
      Add_Stmt (N);
      if Current_Stmt_Scope.Last_Elsif = null then
         Current_Stmt_Scope.Parent.Elsifs := N;
      else
         --  Check for double 'else'
         if Current_Stmt_Scope.Last_Elsif.Cond = null then
            raise Syntax_Error;
         end if;
         Current_Stmt_Scope.Last_Elsif.Next_Elsif := N;
      end if;
      Current_Stmt_Scope.Last_Elsif := N;
   end New_Elsif_Stmt;

   procedure New_Else_Stmt (Block : in out O_If_Block) is
   begin
      New_Elsif_Stmt (Block, null);
   end New_Else_Stmt;

   procedure Finish_If_Stmt (Block : in out O_If_Block)
   is
      pragma Unreferenced (Block);
      Parent : O_Snode;
   begin
      Parent := Current_Stmt_Scope.Parent;
      Pop_Stmt_Scope (Stmt_If);
      Parent.If_Last := Current_Decl_Scope.Last_Stmt;
   end Finish_If_Stmt;

   procedure Start_Loop_Stmt (Label : out O_Snode)
   is
      subtype O_Snode_Loop_Type is O_Snode_Type (ON_Loop_Stmt);
   begin
      Current_Loop_Level := Current_Loop_Level + 1;
      Label := new O_Snode_Loop_Type'(Kind => ON_Loop_Stmt,
                                      Next => null,
                                      Lineno => 0,
                                      Loop_Last => null,
                                      Loop_Level => Current_Loop_Level);
      Add_Stmt (Label);
      Push_Stmt_Scope (new Stmt_Loop_Scope_Type'(Kind => Stmt_Loop,
                                                 Parent => Label,
                                                 Prev => Current_Stmt_Scope));
   end Start_Loop_Stmt;

   procedure Finish_Loop_Stmt (Label : in out O_Snode)
   is
      pragma Unreferenced (Label);
      Parent : O_Snode;
   begin
      Parent := Current_Stmt_Scope.Parent;
      Pop_Stmt_Scope (Stmt_Loop);
      Parent.Loop_Last := Current_Decl_Scope.Last_Stmt;
      Current_Loop_Level := Current_Loop_Level - 1;
   end Finish_Loop_Stmt;

   procedure New_Exit_Next_Stmt (Kind : ON_Stmt_Kind; L : O_Snode)
   is
      N : O_Snode;
   begin
      N := new O_Snode_Type (Kind);
      N.Next := null;
      N.Loop_Id := L;
      Add_Stmt (N);
   end New_Exit_Next_Stmt;

   procedure New_Exit_Stmt (L : O_Snode) is
   begin
      New_Exit_Next_Stmt (ON_Exit_Stmt, L);
   end New_Exit_Stmt;

   procedure New_Next_Stmt (L : O_Snode) is
   begin
      New_Exit_Next_Stmt (ON_Next_Stmt, L);
   end New_Next_Stmt;

   procedure Start_Case_Stmt (Block : in out O_Case_Block; Value : O_Enode)
   is
      subtype O_Snode_Case_Type is O_Snode_Type (ON_Case_Stmt);
      N : O_Snode;
   begin
      case Value.Rtype.Kind is
         when ON_Boolean_Type
           | ON_Unsigned_Type
           | ON_Signed_Type
           | ON_Enum_Type =>
            null;
         when others =>
            raise Type_Error;
      end case;
      Check_Ref (Value);
      N := new O_Snode_Case_Type'(Kind => ON_Case_Stmt,
                                  Next => null,
                                  Lineno => 0,
                                  Case_Last => null,
                                  Selector => Value,
                                  Branches => null);
      Block.Case_Stmt := N;
      Add_Stmt (N);
      Push_Stmt_Scope (new Stmt_Case_Scope_Type'(Kind => Stmt_Case,
                                                 Parent => N,
                                                 Prev => Current_Stmt_Scope,
                                                 Last_Branch => null,
                                                 Last_Choice => null,
                                                 Case_Type => Value.Rtype));
   end Start_Case_Stmt;

   procedure Start_Choice (Block : in out O_Case_Block)
   is
      N : O_Snode;
   begin
      if Current_Stmt_Scope.Kind /= Stmt_Case
        or else Current_Stmt_Scope.Parent /= Block.Case_Stmt
      then
         --  You are adding a branch outside a the case statment.
         raise Syntax_Error;
      end if;
      if Current_Stmt_Scope.Last_Choice /= null then
         --  You are creating branch while the previous one was not finished.
         raise Syntax_Error;
      end if;

      N := new O_Snode_Type (ON_When_Stmt);
      N.all := O_Snode_Type'(Kind => ON_When_Stmt,
                             Next => null,
                             Lineno => 0,
                             Branch_Parent => Block.Case_Stmt,
                             Choice_List => null,
                             Next_Branch => null);
      if Current_Stmt_Scope.Last_Branch = null then
         Current_Stmt_Scope.Parent.Branches := N;
      else
         Current_Stmt_Scope.Last_Branch.Next_Branch := N;
      end if;
      Current_Stmt_Scope.Last_Branch := N;
      Current_Stmt_Scope.Last_Choice := null;
      Add_Stmt (N);
   end Start_Choice;

   procedure Add_Choice (Block : in out O_Case_Block; Choice : O_Choice) is
   begin
      if Current_Stmt_Scope.Kind /= Stmt_Case
        or else Current_Stmt_Scope.Parent /= Block.Case_Stmt
      then
         --  You are adding a branch outside a the case statment.
         raise Syntax_Error;
      end if;
      if Current_Stmt_Scope.Last_Branch = null then
         --  You are not inside a branch.
         raise Syntax_Error;
      end if;
      if Current_Stmt_Scope.Last_Choice = null then
         if Current_Stmt_Scope.Last_Branch.Choice_List /= null then
            --  The branch was already closed.
            raise Syntax_Error;
         end if;
         Current_Stmt_Scope.Last_Branch.Choice_List := Choice;
      else
         Current_Stmt_Scope.Last_Choice.Next := Choice;
      end if;
      Current_Stmt_Scope.Last_Choice := Choice;
   end Add_Choice;

   procedure New_Expr_Choice (Block : in out O_Case_Block; Expr : O_Cnode)
   is
      N : O_Choice;
   begin
      if Current_Stmt_Scope.Kind /= Stmt_Case
        or else Current_Stmt_Scope.Parent /= Block.Case_Stmt
      then
         --  You are adding a branch outside a the case statment.
         raise Syntax_Error;
      end if;
      if Current_Stmt_Scope.Case_Type /= Expr.Ctype then
         --  Expr type is not the same as choice type.
         raise Type_Error;
      end if;

      N := new O_Choice_Type (ON_Choice_Expr);
      N.all := O_Choice_Type'(Kind => ON_Choice_Expr,
                              Next => null,
                              Expr => Expr);
      Add_Choice (Block, N);
   end New_Expr_Choice;

   procedure New_Range_Choice (Block : in out O_Case_Block;
                               Low, High : O_Cnode)
   is
      N : O_Choice;
   begin
      if Current_Stmt_Scope.Kind /= Stmt_Case
        or else Current_Stmt_Scope.Parent /= Block.Case_Stmt
      then
         --  You are adding a branch outside a the case statment.
         raise Syntax_Error;
      end if;
      if Current_Stmt_Scope.Case_Type /= Low.Ctype
        or Current_Stmt_Scope.Case_Type /= High.Ctype
      then
         --  Low/High type is not the same as choice type.
         raise Type_Error;
      end if;

      N := new O_Choice_Type (ON_Choice_Range);
      N.all := O_Choice_Type'(Kind => ON_Choice_Range,
                              Next => null,
                              Low => Low,
                              High => High);
      Add_Choice (Block, N);
   end New_Range_Choice;

   procedure New_Default_Choice (Block : in out O_Case_Block)
   is
      N : O_Choice;
   begin
      if Current_Stmt_Scope.Kind /= Stmt_Case
        or else Current_Stmt_Scope.Parent /= Block.Case_Stmt
      then
         --  You are adding a branch outside a the case statment.
         raise Syntax_Error;
      end if;

      N := new O_Choice_Type (ON_Choice_Default);
      N.all := O_Choice_Type'(Kind => ON_Choice_Default,
                             Next => null);
      Add_Choice (Block, N);
   end New_Default_Choice;

   procedure Finish_Choice (Block : in out O_Case_Block) is
   begin
      if Current_Stmt_Scope.Kind /= Stmt_Case
        or else Current_Stmt_Scope.Parent /= Block.Case_Stmt
      then
         --  You are adding a branch outside a the case statment.
         raise Syntax_Error;
      end if;
      if Current_Stmt_Scope.Last_Branch = null then
         --  You are not inside a branch.
         raise Syntax_Error;
      end if;
      if Current_Stmt_Scope.Last_Choice = null then
         --  The branch is empty or you are not inside a branch.
         raise Syntax_Error;
      end if;
      Current_Stmt_Scope.Last_Choice := null;
   end Finish_Choice;

   procedure Finish_Case_Stmt (Block : in out O_Case_Block)
   is
      Parent : O_Snode;
   begin
      if Current_Stmt_Scope.Kind /= Stmt_Case
        or else Current_Stmt_Scope.Parent /= Block.Case_Stmt
      then
         --  You are adding a branch outside a the case statment.
         raise Syntax_Error;
      end if;
      Parent := Current_Stmt_Scope.Parent;
      Pop_Stmt_Scope (Stmt_Case);
      Parent.Case_Last := Current_Decl_Scope.Last_Stmt;
   end Finish_Case_Stmt;

   procedure Init is
   begin
      Top := new O_Snode_Type (ON_Declare_Stmt);
      Push_Decl_Scope (Top);
   end Init;

   procedure Finish is
   begin
      Pop_Decl_Scope;
   end Finish;
end Ortho_Debug;
