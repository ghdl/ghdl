--  Mcode back-end for ortho.
--  Copyright (C) 2006 Tristan Gingold
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
with Ada.Text_IO;
with Ortho_Code.Debug;
with Ortho_Ident;
with Ortho_Code.Abi;
-- with Binary_File;

package body Ortho_Mcode is
   procedure New_Debug_Comment_Stmt (Comment : String)
   is
      pragma Unreferenced (Comment);
   begin
      null;
   end New_Debug_Comment_Stmt;

   procedure Start_Init_Value (Decl : in out O_Dnode)
   is
      pragma Unreferenced (Decl);
   begin
      null;
   end Start_Init_Value;

   procedure Start_Record_Type (Elements : out O_Element_List) is
   begin
      Ortho_Code.Types.Start_Record_Type
        (Ortho_Code.Types.O_Element_List (Elements));
   end Start_Record_Type;

   procedure New_Record_Field
     (Elements : in out O_Element_List;
      El : out O_Fnode;
      Ident : O_Ident; Etype : O_Tnode) is
   begin
      Ortho_Code.Types.New_Record_Field
        (Ortho_Code.Types.O_Element_List (Elements),
         Ortho_Code.O_Fnode (El), Ident, Ortho_Code.O_Tnode (Etype));
   end New_Record_Field;

   procedure Finish_Record_Type
     (Elements : in out O_Element_List; Res : out O_Tnode) is
   begin
      Ortho_Code.Types.Finish_Record_Type
        (Ortho_Code.Types.O_Element_List (Elements),
         Ortho_Code.O_Tnode (Res));
   end Finish_Record_Type;

   procedure Start_Record_Subtype
     (Rtype : O_Tnode; Elements : out O_Element_Sublist) is
   begin
      Ortho_Code.Types.Start_Record_Subtype
        (Ortho_Code.O_Tnode (Rtype),
         Ortho_Code.Types.O_Element_List (Elements));
   end Start_Record_Subtype;

   procedure New_Subrecord_Field
     (Elements : in out O_Element_Sublist; El : out O_Fnode; Etype : O_Tnode)
   is
   begin
      Ortho_Code.Types.New_Subrecord_Field
        (Ortho_Code.Types.O_Element_List (Elements),
         Ortho_Code.O_Fnode (El), Ortho_Code.O_Tnode (Etype));
   end New_Subrecord_Field;

   procedure Finish_Record_Subtype
     (Elements : in out O_Element_Sublist; Res : out O_Tnode) is
   begin
      Ortho_Code.Types.Finish_Record_Subtype
        (Ortho_Code.Types.O_Element_List (Elements),
         Ortho_Code.O_Tnode (Res));
   end Finish_Record_Subtype;

   procedure New_Uncomplete_Record_Type (Res : out O_Tnode) is
   begin
      Ortho_Code.Types.New_Uncomplete_Record_Type (Ortho_Code.O_Tnode (Res));
   end New_Uncomplete_Record_Type;

   procedure Start_Uncomplete_Record_Type (Res : O_Tnode;
                                           Elements : out O_Element_List) is
   begin
      Ortho_Code.Types.Start_Uncomplete_Record_Type
        (Ortho_Code.O_Tnode (Res),
         Ortho_Code.Types.O_Element_List (Elements));
   end Start_Uncomplete_Record_Type;

   procedure Start_Union_Type (Elements : out O_Element_List) is
   begin
      Ortho_Code.Types.Start_Union_Type
        (Ortho_Code.Types.O_Element_List (Elements));
   end Start_Union_Type;

   procedure New_Union_Field
     (Elements : in out O_Element_List;
      El : out O_Fnode;
      Ident : O_Ident;
      Etype : O_Tnode) is
   begin
      Ortho_Code.Types.New_Union_Field
        (Ortho_Code.Types.O_Element_List (Elements),
         Ortho_Code.O_Fnode (El),
         Ident,
         Ortho_Code.O_Tnode (Etype));
   end New_Union_Field;

   procedure Finish_Union_Type
     (Elements : in out O_Element_List; Res : out O_Tnode) is
   begin
      Ortho_Code.Types.Finish_Union_Type
        (Ortho_Code.Types.O_Element_List (Elements),
         Ortho_Code.O_Tnode (Res));
   end Finish_Union_Type;

   function New_Access_Type (Dtype : O_Tnode) return O_Tnode is
   begin
      return O_Tnode
        (Ortho_Code.Types.New_Access_Type (Ortho_Code.O_Tnode (Dtype)));
   end New_Access_Type;

   procedure Finish_Access_Type (Atype : O_Tnode; Dtype : O_Tnode) is
   begin
      Ortho_Code.Types.Finish_Access_Type (Ortho_Code.O_Tnode (Atype),
                                           Ortho_Code.O_Tnode (Dtype));
   end Finish_Access_Type;

   procedure Finish_Init_Value (Decl : in out O_Dnode; Val : O_Cnode)
   is
      pragma Warnings (Off, Decl);
   begin
      New_Init_Value (Ortho_Code.O_Dnode (Decl), Ortho_Code.O_Cnode (Val));
   end Finish_Init_Value;

   function New_Array_Type (El_Type : O_Tnode; Index_Type : O_Tnode)
                           return O_Tnode is
   begin
      return O_Tnode
        (Ortho_Code.Types.New_Array_Type (Ortho_Code.O_Tnode (El_Type),
                                          Ortho_Code.O_Tnode (Index_Type)));
   end New_Array_Type;

   function New_Array_Subtype
     (Atype : O_Tnode; El_Type : O_Tnode; Length : O_Cnode) return O_Tnode
   is
      Len : constant Ortho_Code.O_Cnode := Ortho_Code.O_Cnode (Length);
      L_Type : Ortho_Code.O_Tnode;
   begin
      L_Type := Get_Const_Type (Len);
      if Get_Type_Kind (L_Type) /= OT_Unsigned then
         raise Syntax_Error;
      end if;
      return O_Tnode (New_Array_Subtype (Ortho_Code.O_Tnode (Atype),
                      Ortho_Code.O_Tnode (El_Type),
                      Get_Const_U32 (Len)));
   end New_Array_Subtype;

   function New_Unsigned_Type (Size : Natural) return O_Tnode is
   begin
      return O_Tnode (Ortho_Code.Types.New_Unsigned_Type (Size));
   end New_Unsigned_Type;

   function New_Signed_Type (Size : Natural) return O_Tnode is
   begin
      return O_Tnode (Ortho_Code.Types.New_Signed_Type (Size));
   end New_Signed_Type;

   function New_Float_Type return O_Tnode is
   begin
      return O_Tnode (Ortho_Code.Types.New_Float_Type);
   end New_Float_Type;

   procedure New_Boolean_Type (Res : out O_Tnode;
                               False_Id : O_Ident;
                               False_E : out O_Cnode;
                               True_Id : O_Ident;
                               True_E : out O_Cnode) is
   begin
      Ortho_Code.Types.New_Boolean_Type (Ortho_Code.O_Tnode (Res),
                                         False_Id,
                                         Ortho_Code.O_Cnode (False_E),
                                         True_Id,
                                         Ortho_Code.O_Cnode (True_E));
   end New_Boolean_Type;

   procedure Start_Enum_Type (List : out O_Enum_List; Size : Natural) is
   begin
      Ortho_Code.Types.Start_Enum_Type (Ortho_Code.Types.O_Enum_List (List),
                                        Size);
   end Start_Enum_Type;

   procedure New_Enum_Literal (List : in out O_Enum_List;
                               Ident : O_Ident; Res : out O_Cnode) is
   begin
      Ortho_Code.Types.New_Enum_Literal (Ortho_Code.Types.O_Enum_List (List),
                                         Ident, Ortho_Code.O_Cnode (Res));
   end New_Enum_Literal;

   procedure Finish_Enum_Type (List : in out O_Enum_List; Res : out O_Tnode) is
   begin
      Ortho_Code.Types.Finish_Enum_Type (Ortho_Code.Types.O_Enum_List (List),
                                         Ortho_Code.O_Tnode (Res));
   end Finish_Enum_Type;

   -------------------
   --  Expressions  --
   -------------------

   To_Op : constant array (ON_Op_Kind) of Ortho_Code.ON_Op_Kind :=
     (
      ON_Nil => ON_Nil,

      --  Dyadic operations.
      ON_Add_Ov => ON_Add_Ov,
      ON_Sub_Ov => ON_Sub_Ov,
      ON_Mul_Ov => ON_Mul_Ov,
      ON_Div_Ov => ON_Div_Ov,
      ON_Rem_Ov => ON_Rem_Ov,
      ON_Mod_Ov => ON_Mod_Ov,

      --  Binary operations.
      ON_And => ON_And,
      ON_Or => ON_Or,
      ON_Xor => ON_Xor,

      --  Monadic operations.
      ON_Not => ON_Not,
      ON_Neg_Ov => ON_Neg_Ov,
      ON_Abs_Ov => ON_Abs_Ov,

      --  Comparaisons
      ON_Eq => ON_Eq,
      ON_Neq => ON_Neq,
      ON_Le => ON_Le,
      ON_Lt => ON_Lt,
      ON_Ge => ON_Ge,
      ON_Gt => ON_Gt
     );

   function New_Signed_Literal (Ltype : O_Tnode; Value : Integer_64)
                               return O_Cnode is
   begin
      return O_Cnode
        (Ortho_Code.Consts.New_Signed_Literal (Ortho_Code.O_Tnode (Ltype),
                                               Value));
   end New_Signed_Literal;

   function New_Unsigned_Literal (Ltype : O_Tnode; Value : Unsigned_64)
                                 return O_Cnode is
   begin
      return O_Cnode
        (Ortho_Code.Consts.New_Unsigned_Literal (Ortho_Code.O_Tnode (Ltype),
                                                 Value));
   end New_Unsigned_Literal;

   function New_Float_Literal (Ltype : O_Tnode; Value : IEEE_Float_64)
                              return O_Cnode is
   begin
      return O_Cnode
        (Ortho_Code.Consts.New_Float_Literal (Ortho_Code.O_Tnode (Ltype),
                                              Value));
   end New_Float_Literal;

   function New_Null_Access (Ltype : O_Tnode) return O_Cnode is
   begin
      return O_Cnode
        (Ortho_Code.Consts.New_Null_Access (Ortho_Code.O_Tnode (Ltype)));
   end New_Null_Access;

   function New_Default_Value (Ltype : O_Tnode) return O_Cnode is
   begin
      return O_Cnode
        (Ortho_Code.Consts.New_Default_Value (Ortho_Code.O_Tnode (Ltype)));
   end New_Default_Value;

   procedure Start_Record_Aggr (List : out O_Record_Aggr_List;
                                Atype : O_Tnode) is
   begin
      Ortho_Code.Consts.Start_Record_Aggr
        (Ortho_Code.Consts.O_Record_Aggr_List (List),
         Ortho_Code.O_Tnode (Atype));
   end Start_Record_Aggr;

   procedure New_Record_Aggr_El (List : in out O_Record_Aggr_List;
                                 Value : O_Cnode) is
   begin
      Ortho_Code.Consts.New_Record_Aggr_El
        (Ortho_Code.Consts.O_Record_Aggr_List (List),
         Ortho_Code.O_Cnode (Value));
   end New_Record_Aggr_El;

   procedure Finish_Record_Aggr (List : in out O_Record_Aggr_List;
                                 Res : out O_Cnode) is
   begin
      Ortho_Code.Consts.Finish_Record_Aggr
        (Ortho_Code.Consts.O_Record_Aggr_List (List),
         Ortho_Code.O_Cnode (Res));
   end Finish_Record_Aggr;

   procedure Start_Array_Aggr
     (List : out O_Array_Aggr_List; Atype : O_Tnode; Len : Unsigned_32)
   is
   begin
      Ortho_Code.Consts.Start_Array_Aggr
        (Ortho_Code.Consts.O_Array_Aggr_List (List),
         Ortho_Code.O_Tnode (Atype),
         Len);
   end Start_Array_Aggr;

   procedure New_Array_Aggr_El (List : in out O_Array_Aggr_List;
                                Value : O_Cnode) is
   begin
      Ortho_Code.Consts.New_Array_Aggr_El
        (Ortho_Code.Consts.O_Array_Aggr_List (List),
         Ortho_Code.O_Cnode (Value));
   end New_Array_Aggr_El;

   procedure Finish_Array_Aggr (List : in out O_Array_Aggr_List;
                                Res : out O_Cnode) is
   begin
      Ortho_Code.Consts.Finish_Array_Aggr
        (Ortho_Code.Consts.O_Array_Aggr_List (List),
         Ortho_Code.O_Cnode (Res));
   end Finish_Array_Aggr;

   function New_Union_Aggr (Atype : O_Tnode; Field : O_Fnode; Value : O_Cnode)
                           return O_Cnode is
   begin
      return O_Cnode
        (Ortho_Code.Consts.New_Union_Aggr (Ortho_Code.O_Tnode (Atype),
                                           Ortho_Code.O_Fnode (Field),
                                           Ortho_Code.O_Cnode (Value)));
   end New_Union_Aggr;

   function New_Sizeof (Atype : O_Tnode; Rtype : O_Tnode) return O_Cnode is
   begin
      return O_Cnode
        (Ortho_Code.Consts.New_Sizeof (Ortho_Code.O_Tnode (Atype),
                                       Ortho_Code.O_Tnode (Rtype)));
   end New_Sizeof;

   function New_Record_Sizeof
     (Atype : O_Tnode; Rtype : O_Tnode) return O_Cnode is
   begin
      return O_Cnode
        (Ortho_Code.Consts.New_Record_Sizeof (Ortho_Code.O_Tnode (Atype),
                                              Ortho_Code.O_Tnode (Rtype)));
   end New_Record_Sizeof;

   function New_Alignof (Atype : O_Tnode; Rtype : O_Tnode) return O_Cnode is
   begin
      return O_Cnode
        (Ortho_Code.Consts.New_Alignof (Ortho_Code.O_Tnode (Atype),
                                        Ortho_Code.O_Tnode (Rtype)));
   end New_Alignof;

   function New_Offsetof (Atype : O_Tnode; Field : O_Fnode; Rtype : O_Tnode)
                         return O_Cnode is
   begin
      return O_Cnode
        (Ortho_Code.Consts.New_Offsetof (Ortho_Code.O_Tnode (Atype),
                                         Ortho_Code.O_Fnode (Field),
                                         Ortho_Code.O_Tnode (Rtype)));
   end New_Offsetof;

   function New_Subprogram_Address (Subprg : O_Dnode; Atype : O_Tnode)
                                   return O_Cnode is
   begin
      return O_Cnode
        (Ortho_Code.Consts.New_Subprogram_Address
           (Ortho_Code.O_Dnode (Subprg), Ortho_Code.O_Tnode (Atype)));
   end New_Subprogram_Address;

   function New_Global_Address (Lvalue : O_Gnode; Atype : O_Tnode)
                               return O_Cnode is
   begin
      return O_Cnode
        (Ortho_Code.Consts.New_Global_Address
           (Ortho_Code.O_Gnode (Lvalue), Ortho_Code.O_Tnode (Atype)));
   end New_Global_Address;

   function New_Global_Unchecked_Address (Lvalue : O_Gnode; Atype : O_Tnode)
                                         return O_Cnode is
   begin
      return O_Cnode
        (Ortho_Code.Consts.New_Global_Unchecked_Address
           (Ortho_Code.O_Gnode (Lvalue), Ortho_Code.O_Tnode (Atype)));
   end New_Global_Unchecked_Address;

   function New_Global (Decl : O_Dnode) return O_Gnode is
   begin
      return O_Gnode
        (Ortho_Code.Consts.New_Global (Ortho_Code.O_Dnode (Decl)));
   end New_Global;

   function New_Global_Selected_Element (Rec : O_Gnode; El : O_Fnode)
                                        return O_Gnode is
   begin
      return O_Gnode
        (Ortho_Code.Consts.New_Global_Selected_Element
           (Ortho_Code.O_Gnode (Rec), Ortho_Code.O_Fnode (El)));
   end New_Global_Selected_Element;

   function New_Lit (Lit : O_Cnode) return O_Enode is
   begin
      return O_Enode (Ortho_Code.Exprs.New_Lit (Ortho_Code.O_Cnode (Lit)));
   end New_Lit;

   function New_Dyadic_Op (Kind : ON_Dyadic_Op_Kind; Left, Right : O_Enode)
                          return O_Enode is
   begin
      return O_Enode
        (Ortho_Code.Exprs.New_Dyadic_Op (To_Op (Kind),
                                         Ortho_Code.O_Enode (Left),
                                         Ortho_Code.O_Enode (Right)));
   end New_Dyadic_Op;

   function New_Monadic_Op (Kind : ON_Monadic_Op_Kind; Operand : O_Enode)
                           return O_Enode is
   begin
      return O_Enode
        (Ortho_Code.Exprs.New_Monadic_Op (To_Op (Kind),
                                          Ortho_Code.O_Enode (Operand)));
   end New_Monadic_Op;

   function New_Compare_Op
     (Kind : ON_Compare_Op_Kind; Left, Right : O_Enode; Ntype : O_Tnode)
     return O_Enode is
   begin
      return O_Enode
        (Ortho_Code.Exprs.New_Compare_Op (To_Op (Kind),
                                          Ortho_Code.O_Enode (Left),
                                          Ortho_Code.O_Enode (Right),
                                          Ortho_Code.O_Tnode (Ntype)));
   end New_Compare_Op;

   function New_Indexed_Element (Arr : O_Lnode; Index : O_Enode)
                                return O_Lnode is
   begin
      return O_Lnode
        (Ortho_Code.Exprs.New_Indexed_Element (Ortho_Code.O_Lnode (Arr),
                                               Ortho_Code.O_Enode (Index)));
   end New_Indexed_Element;

   function New_Slice (Arr : O_Lnode; Res_Type : O_Tnode; Index : O_Enode)
                      return O_Lnode is
   begin
      return O_Lnode
        (Ortho_Code.Exprs.New_Slice (Ortho_Code.O_Lnode (Arr),
                                     Ortho_Code.O_Tnode (Res_Type),
                                     Ortho_Code.O_Enode (Index)));
   end New_Slice;

   function New_Selected_Element (Rec : O_Lnode; El : O_Fnode)
                                 return O_Lnode is
   begin
      return O_Lnode
        (Ortho_Code.Exprs.New_Selected_Element (Ortho_Code.O_Lnode (Rec),
                                                Ortho_Code.O_Fnode (El)));
   end New_Selected_Element;

   function New_Access_Element (Acc : O_Enode) return O_Lnode is
   begin
      return O_Lnode
        (Ortho_Code.Exprs.New_Access_Element (Ortho_Code.O_Enode (Acc)));
   end New_Access_Element;

   function New_Convert_Ov (Val : O_Enode; Rtype : O_Tnode) return O_Enode is
   begin
      return O_Enode
        (Ortho_Code.Exprs.New_Convert_Ov (Ortho_Code.O_Enode (Val),
                                          Ortho_Code.O_Tnode (Rtype)));
   end New_Convert_Ov;

   function New_Convert (Val : O_Enode; Rtype : O_Tnode) return O_Enode is
   begin
      return O_Enode
        (Ortho_Code.Exprs.New_Convert (Ortho_Code.O_Enode (Val),
                                       Ortho_Code.O_Tnode (Rtype)));
   end New_Convert;

   function New_Address (Lvalue : O_Lnode; Atype : O_Tnode)
                        return O_Enode is
   begin
      return O_Enode
        (Ortho_Code.Exprs.New_Address (Ortho_Code.O_Lnode (Lvalue),
                                       Ortho_Code.O_Tnode (Atype)));
   end New_Address;

   function New_Unchecked_Address (Lvalue : O_Lnode; Atype : O_Tnode)
                                  return O_Enode is
   begin
      return O_Enode
        (Ortho_Code.Exprs.New_Unchecked_Address (Ortho_Code.O_Lnode (Lvalue),
                                                 Ortho_Code.O_Tnode (Atype)));
   end New_Unchecked_Address;

   function New_Value (Lvalue : O_Lnode) return O_Enode is
   begin
      return O_Enode
        (Ortho_Code.Exprs.New_Value (Ortho_Code.O_Lnode (Lvalue)));
   end New_Value;

   function New_Obj_Value (Obj : O_Dnode) return O_Enode is
   begin
      return New_Value (New_Obj (Obj));
   end New_Obj_Value;

   function New_Alloca (Rtype : O_Tnode; Size : O_Enode) return O_Enode is
   begin
      return O_Enode (Ortho_Code.Exprs.New_Alloca (Ortho_Code.O_Tnode (Rtype),
                                                   Ortho_Code.O_Enode (Size)));
   end New_Alloca;

   ---------------------
   --  Declarations.  --
   ---------------------

   procedure New_Debug_Filename_Decl (Filename : String)
     renames Ortho_Code.Abi.New_Debug_Filename_Decl;

   procedure New_Debug_Line_Decl (Line : Natural)
   is
      pragma Unreferenced (Line);
   begin
      null;
   end New_Debug_Line_Decl;

   procedure New_Type_Decl (Ident : O_Ident; Atype : O_Tnode) is
   begin
      Ortho_Code.Decls.New_Type_Decl (Ident, Ortho_Code.O_Tnode (Atype));
   end New_Type_Decl;

   To_Storage : constant array (O_Storage) of Ortho_Code.O_Storage :=
     (O_Storage_External => O_Storage_External,
      O_Storage_Public => O_Storage_Public,
      O_Storage_Private => O_Storage_Private,
      O_Storage_Local => O_Storage_Local);

   procedure New_Const_Decl
     (Res : out O_Dnode;
      Ident : O_Ident;
      Storage : O_Storage;
      Atype : O_Tnode) is
   begin
      Ortho_Code.Decls.New_Const_Decl
        (Ortho_Code.O_Dnode (Res), Ident, To_Storage (Storage),
         Ortho_Code.O_Tnode (Atype));
   end New_Const_Decl;

   procedure New_Var_Decl
     (Res : out O_Dnode;
      Ident : O_Ident;
      Storage : O_Storage;
      Atype : O_Tnode) is
   begin
      Ortho_Code.Decls.New_Var_Decl
        (Ortho_Code.O_Dnode (Res), Ident, To_Storage (Storage),
         Ortho_Code.O_Tnode (Atype));
   end New_Var_Decl;

   function New_Obj (Obj : O_Dnode) return O_Lnode is
   begin
      return O_Lnode (Ortho_Code.Exprs.New_Obj (Ortho_Code.O_Dnode (Obj)));
   end New_Obj;

   procedure Start_Function_Decl
     (Interfaces : out O_Inter_List;
      Ident : O_Ident;
      Storage : O_Storage;
      Rtype : O_Tnode) is
   begin
      Ortho_Code.Decls.Start_Function_Decl
        (Ortho_Code.Decls.O_Inter_List (Interfaces),
         Ident, To_Storage (Storage), Ortho_Code.O_Tnode (Rtype));
   end Start_Function_Decl;

   procedure Start_Procedure_Decl
     (Interfaces : out O_Inter_List;
      Ident : O_Ident;
      Storage : O_Storage) is
   begin
      Ortho_Code.Decls.Start_Procedure_Decl
        (Ortho_Code.Decls.O_Inter_List (Interfaces),
         Ident, To_Storage (Storage));
   end Start_Procedure_Decl;

   procedure New_Interface_Decl
     (Interfaces : in out O_Inter_List;
      Res : out O_Dnode;
      Ident : O_Ident;
      Atype : O_Tnode) is
   begin
      Ortho_Code.Decls.New_Interface_Decl
        (Ortho_Code.Decls.O_Inter_List (Interfaces),
         Ortho_Code.O_Dnode (Res),
         Ident,
         Ortho_Code.O_Tnode (Atype));
   end New_Interface_Decl;

   procedure Finish_Subprogram_Decl
     (Interfaces : in out O_Inter_List; Res : out O_Dnode) is
   begin
      Ortho_Code.Decls.Finish_Subprogram_Decl
        (Ortho_Code.Decls.O_Inter_List (Interfaces), Ortho_Code.O_Dnode (Res));
   end Finish_Subprogram_Decl;

   procedure Start_Subprogram_Body (Func : O_Dnode) is
   begin
      Ortho_Code.Exprs.Start_Subprogram_Body (Ortho_Code.O_Dnode (Func));
   end Start_Subprogram_Body;

   procedure Finish_Subprogram_Body
     renames Ortho_Code.Exprs.Finish_Subprogram_Body;

   -------------------
   --  Statements.  --
   -------------------

   procedure New_Debug_Line_Stmt (Line : Natural)
     renames Ortho_Code.Exprs.New_Debug_Line_Stmt;

   procedure New_Debug_Comment_Decl (Comment : String)
   is
      pragma Unreferenced (Comment);
   begin
      null;
   end New_Debug_Comment_Decl;

   procedure Start_Declare_Stmt renames
     Ortho_Code.Exprs.Start_Declare_Stmt;
   procedure Finish_Declare_Stmt renames
     Ortho_Code.Exprs.Finish_Declare_Stmt;

   procedure Start_Association (Assocs : out O_Assoc_List; Subprg : O_Dnode) is
   begin
      Ortho_Code.Exprs.Start_Association
        (Ortho_Code.Exprs.O_Assoc_List (Assocs), Ortho_Code.O_Dnode (Subprg));
   end Start_Association;

   procedure New_Association (Assocs : in out O_Assoc_List; Val : O_Enode) is
   begin
      Ortho_Code.Exprs.New_Association
        (Ortho_Code.Exprs.O_Assoc_List (Assocs), Ortho_Code.O_Enode (Val));
   end New_Association;

   function New_Function_Call (Assocs : O_Assoc_List) return O_Enode is
   begin
      return O_Enode (Ortho_Code.Exprs.New_Function_Call
                        (Ortho_Code.Exprs.O_Assoc_List (Assocs)));
   end New_Function_Call;

   procedure New_Procedure_Call (Assocs : in out O_Assoc_List) is
   begin
      Ortho_Code.Exprs.New_Procedure_Call
        (Ortho_Code.Exprs.O_Assoc_List (Assocs));
   end New_Procedure_Call;

   procedure New_Assign_Stmt (Target : O_Lnode; Value : O_Enode) is
   begin
      Ortho_Code.Exprs.New_Assign_Stmt (Ortho_Code.O_Lnode (Target),
                                        Ortho_Code.O_Enode (Value));
   end New_Assign_Stmt;

   procedure New_Return_Stmt (Value : O_Enode) is
   begin
      Ortho_Code.Exprs.New_Return_Stmt (Ortho_Code.O_Enode (Value));
   end New_Return_Stmt;

   procedure New_Return_Stmt
     renames Ortho_Code.Exprs.New_Return_Stmt;

   procedure Start_If_Stmt (Block : in out O_If_Block; Cond : O_Enode) is
   begin
      Ortho_Code.Exprs.Start_If_Stmt (Ortho_Code.Exprs.O_If_Block (Block),
                                      Ortho_Code.O_Enode (Cond));
   end Start_If_Stmt;

   procedure New_Else_Stmt (Block : in out O_If_Block) is
   begin
      Ortho_Code.Exprs.New_Else_Stmt (Ortho_Code.Exprs.O_If_Block (Block));
   end New_Else_Stmt;

   procedure Finish_If_Stmt (Block : in out O_If_Block) is
   begin
      Ortho_Code.Exprs.Finish_If_Stmt (Ortho_Code.Exprs.O_If_Block (Block));
   end Finish_If_Stmt;

   procedure Start_Loop_Stmt (Label : out O_Snode) is
   begin
      Ortho_Code.Exprs.Start_Loop_Stmt (Ortho_Code.Exprs.O_Snode (Label));
   end Start_Loop_Stmt;

   procedure Finish_Loop_Stmt (Label : in out O_Snode) is
   begin
      Ortho_Code.Exprs.Finish_Loop_Stmt (Ortho_Code.Exprs.O_Snode (Label));
   end Finish_Loop_Stmt;

   procedure New_Exit_Stmt (L : O_Snode) is
   begin
      Ortho_Code.Exprs.New_Exit_Stmt (Ortho_Code.Exprs.O_Snode (L));
   end New_Exit_Stmt;

   procedure New_Next_Stmt (L : O_Snode) is
   begin
      Ortho_Code.Exprs.New_Next_Stmt (Ortho_Code.Exprs.O_Snode (L));
   end New_Next_Stmt;

   procedure Start_Case_Stmt (Block : in out O_Case_Block; Value : O_Enode) is
   begin
      Ortho_Code.Exprs.Start_Case_Stmt
        (Ortho_Code.Exprs.O_Case_Block (Block), Ortho_Code.O_Enode (Value));
   end Start_Case_Stmt;

   procedure Start_Choice (Block : in out O_Case_Block) is
   begin
      Ortho_Code.Exprs.Start_Choice (Ortho_Code.Exprs.O_Case_Block (Block));
   end Start_Choice;

   procedure New_Expr_Choice (Block : in out O_Case_Block; Expr : O_Cnode) is
   begin
      Ortho_Code.Exprs.New_Expr_Choice (Ortho_Code.Exprs.O_Case_Block (Block),
                                        Ortho_Code.O_Cnode (Expr));
   end New_Expr_Choice;

   procedure New_Range_Choice (Block : in out O_Case_Block;
                               Low, High : O_Cnode) is
   begin
      Ortho_Code.Exprs.New_Range_Choice
        (Ortho_Code.Exprs.O_Case_Block (Block),
         Ortho_Code.O_Cnode (Low), Ortho_Code.O_Cnode (High));
   end New_Range_Choice;

   procedure New_Default_Choice (Block : in out O_Case_Block) is
   begin
      Ortho_Code.Exprs.New_Default_Choice
        (Ortho_Code.Exprs.O_Case_Block (Block));
   end New_Default_Choice;

   procedure Finish_Choice (Block : in out O_Case_Block) is
   begin
      Ortho_Code.Exprs.Finish_Choice (Ortho_Code.Exprs.O_Case_Block (Block));
   end Finish_Choice;

   procedure Finish_Case_Stmt (Block : in out O_Case_Block) is
   begin
      Ortho_Code.Exprs.Finish_Case_Stmt
        (Ortho_Code.Exprs.O_Case_Block (Block));
   end Finish_Case_Stmt;

   procedure Init is
   begin
      --  Create an anonymous pointer type.
      if New_Access_Type (O_Tnode_Null) /= O_Tnode (O_Tnode_Ptr) then
         raise Program_Error;
      end if;
      --  Do not finish the access, since this creates an infinite recursion
      --  in gdb (at least for GDB 6.3).
      --Finish_Access_Type (O_Tnode_Ptr, O_Tnode_Ptr);
      Ortho_Code.Abi.Init;
   end Init;

   procedure Finish is
   begin
      if False then
         Ortho_Code.Decls.Disp_All_Decls;
         --Ortho_Code.Exprs.Disp_All_Enode;
      end if;
      Ortho_Code.Decls.Alloc_Zero;
      Ortho_Code.Abi.Finish;
      if Debug.Flag_Debug_Stat then
         Ada.Text_IO.Put_Line ("Statistics:");
         Ortho_Code.Exprs.Disp_Stats;
         Ortho_Code.Decls.Disp_Stats;
         Ortho_Code.Types.Disp_Stats;
         Ortho_Code.Consts.Disp_Stats;
         Ortho_Ident.Disp_Stats;
         -- Binary_File.Disp_Stats;
      end if;
   end Finish;

   procedure Free_All is
   begin
      Ortho_Code.Types.Finish;
      Ortho_Code.Exprs.Finish;
      Ortho_Code.Consts.Finish;
      Ortho_Code.Decls.Finish;
      Ortho_Ident.Finish;
   end Free_All;
end Ortho_Mcode;
