--  Mcode back-end for ortho - Internal tree dumper.
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
with Ada.Text_IO; use Ada.Text_IO;
with Ortho_Code.Debug;
with Ortho_Code.Consts;
with Ortho_Code.Decls;
with Ortho_Code.Types;
with Ortho_Code.Flags;
with Ortho_Ident;
with Interfaces;

package body Ortho_Code.Disps is
   procedure Disp_Subprg (Ident : Natural; S_Entry : O_Enode);
   procedure Disp_Expr (Expr : O_Enode);

   procedure Disp_Indent (Indent : Natural)
   is
   begin
      Put ((1 .. 2 * Indent => ' '));
   end Disp_Indent;

   procedure Disp_Ident (Id : O_Ident)
   is
      use Ortho_Ident;
   begin
      Put (Get_String (Id));
   end Disp_Ident;

   procedure Disp_Storage (Storage : O_Storage) is
   begin
      case Storage is
         when O_Storage_External =>
            Put ("external");
         when O_Storage_Public =>
            Put ("public");
         when O_Storage_Private =>
            Put ("private");
         when O_Storage_Local =>
            Put ("local");
      end case;
   end Disp_Storage;

   procedure Disp_Label (Label : O_Enode)
   is
      N : Int32;
   begin
      case Get_Expr_Kind (Label) is
         when OE_Label =>
            Put ("label");
            N := Int32 (Label);
         when OE_Loop =>
            Put ("loop");
            N := Int32 (Label);
         when OE_BB =>
            Put ("BB");
            N := Get_BB_Number (Label);
         when others =>
            raise Program_Error;
      end case;
      Put (Int32'Image (N));
      Put (":");
   end Disp_Label;

   procedure Disp_Call (Call : O_Enode)
   is
      Arg : O_Enode;
   begin
      Decls.Disp_Decl_Name (Get_Call_Subprg (Call));

      Arg := Get_Arg_Link (Call);
      if Arg /= O_Enode_Null then
         Put (" (");
         loop
            Disp_Expr (Get_Expr_Operand (Arg));
            Arg := Get_Arg_Link (Arg);
            exit when Arg = O_Enode_Null;
            Put (", ");
         end loop;
         Put (")");
      end if;
   end Disp_Call;

   procedure Put_Trim (Str : String) is
   begin
      if Str (Str'First) = ' ' then
         Put (Str (Str'First + 1 .. Str'Last));
      else
         Put (Str);
      end if;
   end Put_Trim;

   procedure Disp_Typed_Lit (Lit : O_Cnode; Val : String)
   is
      use Ortho_Code.Consts;
   begin
      Disp_Type (Get_Const_Type (Lit));
      Put ("'[");
      Put_Trim (Val);
      Put (']');
   end Disp_Typed_Lit;

   procedure Disp_Lit (Lit : O_Cnode)
   is
      use Interfaces;
      use Ortho_Code.Consts;
   begin
      case Get_Const_Kind (Lit) is
         when OC_Unsigned =>
            Disp_Typed_Lit (Lit, Unsigned_64'Image (Get_Const_U64 (Lit)));
         when OC_Signed =>
            Disp_Typed_Lit (Lit, Integer_64'Image (Get_Const_I64 (Lit)));
         when OC_Subprg_Address =>
            Disp_Type (Get_Const_Type (Lit));
            Put ("'subprg_addr (");
            Decls.Disp_Decl_Name (Get_Const_Decl (Lit));
            Put (")");
         when OC_Address =>
            Disp_Type (Get_Const_Type (Lit));
            Put ("'address (");
            Decls.Disp_Decl_Name (Get_Const_Decl (Lit));
            Put (")");
         when OC_Sizeof =>
            Disp_Type (Get_Const_Type (Lit));
            Put ("'sizeof (");
            Disp_Type (Get_Sizeof_Type (Lit));
            Put (")");
         when OC_Null =>
            Disp_Type (Get_Const_Type (Lit));
            Put ("'[null]");
         when OC_Lit =>
            declare
               L : O_Cnode;
            begin
               L := Types.Get_Type_Enum_Lit
                 (Get_Const_Type (Lit), Get_Lit_Value (Lit));
               Disp_Typed_Lit
                 (Lit, Ortho_Ident.Get_String (Get_Lit_Ident (L)));
            end;
         when OC_Array =>
            Put ('{');
            for I in 1 .. Get_Const_Aggr_Length (Lit) loop
               if I /= 1 then
                  Put (", ");
               end if;
               Disp_Lit (Get_Const_Aggr_Element (Lit, I - 1));
            end loop;
            Put ('}');
         when OC_Record =>
            declare
               use Ortho_Code.Types;
               F : O_Fnode;
            begin
               F := Get_Type_Record_Fields (Get_Const_Type (Lit));
               Put ('{');
               for I in 1 .. Get_Const_Aggr_Length (Lit) loop
                  if I /= 1 then
                     Put (", ");
                  end if;
                  Put ('.');
                  Disp_Ident (Get_Field_Ident (F));
                  Put (" = ");
                  Disp_Lit (Get_Const_Aggr_Element (Lit, I - 1));
                  F := Get_Field_Chain (F);
               end loop;
               Put ('}');
            end;
         when OC_Union =>
            Put ('{');
            Put ('.');
            Disp_Ident (Types.Get_Field_Ident (Get_Const_Union_Field (Lit)));
            Put ('=');
            Disp_Lit (Get_Const_Union_Value (Lit));
            Put ('}');
         when others =>
            Put ("*lit " & OC_Kind'Image (Get_Const_Kind (Lit)) & '*');
      end case;
   end Disp_Lit;

   procedure Disp_Expr (Expr : O_Enode)
   is
      Kind : OE_Kind;
   begin
      Kind := Get_Expr_Kind (Expr);
      case Kind is
         when OE_Const =>
            case Get_Expr_Mode (Expr) is
               when Mode_I8
                 | Mode_I16
                 | Mode_I32 =>
                  Put_Trim (Int32'Image (To_Int32 (Get_Expr_Low (Expr))));
               when Mode_U8
                 | Mode_U16
                 | Mode_U32 =>
                  Put_Trim (Uns32'Image (Get_Expr_Low (Expr)));
               when others =>
                  Put ("const:");
                  Debug.Disp_Mode (Get_Expr_Mode (Expr));
            end case;
         when OE_Lit =>
            Disp_Lit (Get_Expr_Lit (Expr));
         when OE_Case_Expr =>
            Put ("{case}");
         when OE_Kind_Dyadic
           | OE_Kind_Cmp
           | OE_Add
           | OE_Mul
           | OE_Shl =>
            Put ("(");
            Disp_Expr (Get_Expr_Left (Expr));
            Put (' ');
            case Kind is
               when OE_Eq =>
                  Put ('=');
               when OE_Neq =>
                  Put ("/=");
               when OE_Lt =>
                  Put ("<");
               when OE_Gt =>
                  Put (">");
               when OE_Ge =>
                  Put (">=");
               when OE_Le =>
                  Put ("<=");
               when OE_Add =>
                  Put ('+');
               when OE_Mul =>
                  Put ('*');
               when OE_Add_Ov =>
                  Put ("+#");
               when OE_Sub_Ov =>
                  Put ("-#");
               when OE_Mul_Ov =>
                  Put ("*#");
               when OE_Shl =>
                  Put ("<<");
               when OE_And =>
                  Put ("and");
               when OE_Or =>
                  Put ("or");
               when others =>
                  Put (OE_Kind'Image (Kind));
            end case;
            Put (' ');
            Disp_Expr (Get_Expr_Right (Expr));
            Put (")");
         when OE_Not =>
            Put ("not ");
            Disp_Expr (Get_Expr_Operand (Expr));
         when OE_Neg_Ov =>
            Put ("neg ");
            Disp_Expr (Get_Expr_Operand (Expr));
         when OE_Abs_Ov =>
            Put ("abs ");
            Disp_Expr (Get_Expr_Operand (Expr));
         when OE_Indir =>
            declare
               Op : O_Enode;
            begin
               Op := Get_Expr_Operand (Expr);
               case Get_Expr_Kind (Op) is
                  when OE_Addrd
                    | OE_Addrl =>
                     Decls.Disp_Decl_Name (Get_Addr_Decl (Op));
                  when others =>
                     --Put ("*");
                     Disp_Expr (Op);
               end case;
            end;
         when OE_Addrl
           | OE_Addrd =>
            -- Put ('@');
            Decls.Disp_Decl_Name (Get_Addr_Decl (Expr));
         when OE_Call =>
            Disp_Call (Expr);
         when OE_Alloca =>
            Put ("alloca (");
            Disp_Expr (Get_Expr_Operand (Expr));
            Put (")");
         when OE_Conv_Ov =>
            Disp_Type (Get_Conv_Type (Expr));
            Put ("'conv (");
            Disp_Expr (Get_Expr_Operand (Expr));
            Put (")");
         when OE_Conv_Ptr =>
            Disp_Type (Get_Conv_Type (Expr));
            Put ("'address (");
            Disp_Expr (Get_Expr_Operand (Expr));
            Put (")");
         when OE_Typed =>
            Disp_Type (Get_Conv_Type (Expr));
            Put ("'");
            --  Note: there is always parenthesis around comparison.
            Disp_Expr (Get_Expr_Operand (Expr));
         when OE_Record_Ref =>
            Disp_Expr (Get_Expr_Operand (Expr));
            Put (".");
            Disp_Ident (Types.Get_Field_Ident (Get_Ref_Field (Expr)));
         when OE_Access_Ref =>
            Disp_Expr (Get_Expr_Operand (Expr));
            Put (".all");
         when OE_Index_Ref =>
            Disp_Expr (Get_Expr_Operand (Expr));
            Put ('[');
            Disp_Expr (Get_Ref_Index (Expr));
            Put (']');
         when OE_Slice_Ref =>
            Disp_Expr (Get_Expr_Operand (Expr));
            Put ('[');
            Disp_Expr (Get_Ref_Index (Expr));
            Put ("...]");
         when OE_Get_Stack =>
            Put ("%sp");
         when OE_Get_Frame =>
            Put ("%fp");
         when others =>
            Put_Line (Standard_Error, "disps.disp_expr: unknown expr "
                      & OE_Kind'Image (Kind));
      end case;
   end Disp_Expr;

   procedure Disp_Fields (Indent : Natural; Atype : O_Tnode)
   is
      use Types;
      Nbr : Uns32;
      F : O_Fnode;
   begin
      Nbr := Get_Type_Record_Nbr_Fields (Atype);
      F := Get_Type_Record_Fields (Atype);
      for I in 1 .. Nbr loop
         Disp_Indent (Indent);
         Disp_Ident (Get_Field_Ident (F));
         Put (": ");
         Disp_Type (Get_Field_Type (F));
         Put (";");
         New_Line;
         F := Get_Field_Chain (F);
      end loop;
   end Disp_Fields;

   procedure Disp_Type (Atype : O_Tnode; Force : Boolean := False)
   is
      use Types;
      Kind : OT_Kind;
      Decl : O_Dnode;
   begin
      if not Force then
         Decl := Decls.Get_Type_Decl (Atype);
         if Decl /= O_Dnode_Null then
            Decls.Disp_Decl_Name (Decl);
            return;
         end if;
      end if;

      Kind := Get_Type_Kind (Atype);
      case Kind is
         when OT_Signed =>
            Put ("signed (");
            Put_Trim (Uns32'Image (8 * Get_Type_Size (Atype)));
            Put (")");
         when OT_Unsigned =>
            Put ("unsigned (");
            Put_Trim (Uns32'Image (8 * Get_Type_Size (Atype)));
            Put (")");
         when OT_Float =>
            Put ("float");
         when OT_Access =>
            Put ("access");
            declare
               Acc_Type : O_Tnode;
            begin
               Acc_Type := Get_Type_Access_Type (Atype);
               if Acc_Type /= O_Tnode_Null then
                  Put (' ');
                  Disp_Type (Acc_Type);
               end if;
            end;
         when OT_Ucarray =>
            Put ("array [");
            Disp_Type (Get_Type_Ucarray_Index (Atype));
            Put ("] of ");
            Disp_Type (Get_Type_Ucarray_Element (Atype));
         when OT_Subarray =>
            Put ("subarray ");
            Disp_Type (Get_Type_Subarray_Base (Atype));
            Put ("[");
            Put_Trim (Uns32'Image (Get_Type_Subarray_Length (Atype)));
            Put ("]");
            Put (" ");
            Put ("of");
            Put (" ");
            Disp_Type (Get_Type_Subarray_Element (Atype));
         when OT_Record =>
            Put_Line ("record");
            Disp_Fields (1, Atype);
            Put ("end record");
         when OT_Subrecord =>
            Put_Line ("subrecord");
            Disp_Type (Get_Type_Subrecord_Base (Atype));
            Put ("(");
            Disp_Fields (1, Atype);
            Put (")");
         when OT_Union =>
            Put_Line ("union");
            Disp_Fields (1, Atype);
            Put ("end union");
         when OT_Boolean =>
            declare
               Lit : O_Cnode;
            begin
               Put ("boolean {");
               Lit := Get_Type_Bool_False (Atype);
               Disp_Ident (Consts.Get_Lit_Ident (Lit));
               Put (", ");
               Lit := Get_Type_Bool_True (Atype);
               Disp_Ident (Consts.Get_Lit_Ident (Lit));
               Put ("}");
            end;
         when OT_Enum =>
            declare
               use Consts;
               Lit : O_Cnode;
            begin
               Put ("enum {");
               Lit := Get_Type_Enum_Lits (Atype);
               for I in 1 .. Get_Type_Enum_Nbr_Lits (Atype) loop
                  if I /= 1 then
                     Put (", ");
                  end if;
                  Disp_Ident (Get_Lit_Ident (Lit));
                  Put (" =");
                  Put (Uns32'Image (I - 1));
                  Lit := Get_Lit_Chain (Lit);
               end loop;
               Put ('}');
            end;
         when OT_Complete =>
            Put ("-- complete: ");
            Disp_Type (Get_Type_Complete_Type (Atype));
      end case;
   end Disp_Type;

   procedure Debug_Tnode (Atype : O_Tnode)
   is
      Decl : O_Dnode;
   begin
      Decl := Decls.Get_Type_Decl (Atype);
      if Decl /= O_Dnode_Null then
         Decls.Disp_Decl_Name (Decl);
         Put (": ");
      end if;
      Disp_Type (Atype, True);
      New_Line;
   end Debug_Tnode;
   pragma Unreferenced (Debug_Tnode);

   procedure Debug_Enode (Expr : O_Enode) is
   begin
      Disp_Expr (Expr);
      New_Line;
   end Debug_Enode;
   pragma Unreferenced (Debug_Enode);

   procedure Debug_Lnode (Expr : O_Lnode) is
   begin
      Disp_Expr (O_Enode (Expr));
      New_Line;
   end Debug_Lnode;
   pragma Unreferenced (Debug_Lnode);

   procedure Disp_Decl_Storage (Decl : O_Dnode) is
   begin
      Disp_Storage (Decls.Get_Decl_Storage (Decl));
      Put (' ');
   end Disp_Decl_Storage;

   procedure Disp_Subprg_Decl (Indent : Natural; Decl : O_Dnode)
   is
      use Decls;
      Kind : OD_Kind;
      Inter : O_Dnode;
   begin
      Disp_Decl_Storage (Decl);
      Kind := Get_Decl_Kind (Decl);
      case Kind is
         when OD_Function =>
            Put ("function ");
         when OD_Procedure =>
            Put ("procedure ");
         when others =>
            raise Program_Error;
      end case;

      Disp_Decl_Name (Decl);
      Inter := Get_Subprg_Interfaces (Decl);
      Put (" (");
      New_Line;
      if Inter /= O_Dnode_Null then
         loop
            Disp_Indent (Indent + 1);
            Disp_Decl_Name (Inter);
            Put (": ");
            Disp_Type (Get_Decl_Type (Inter));
            Inter := Get_Interface_Chain (Inter);
            exit when Inter = O_Dnode_Null;
            Put (";");
            New_Line;
         end loop;
      else
         Disp_Indent (Indent + 1);
      end if;
      Put (")");
      if Kind = OD_Function then
         New_Line;
         Disp_Indent (Indent + 1);
         Put ("return ");
         Disp_Type (Get_Decl_Type (Decl));
      end if;
   end Disp_Subprg_Decl;

   procedure Disp_Decl (Indent : Natural;
                        Decl : O_Dnode;
                        Nl : Boolean := False)
   is
      use Decls;
      Kind : OD_Kind;
      Dtype : O_Tnode;
   begin
      Kind := Get_Decl_Kind (Decl);
      if Kind = OD_Interface then
         return;
      end if;
      Disp_Indent (Indent);
      case Kind is
         when OD_Type =>
            Dtype := Get_Decl_Type (Decl);
            Put ("type ");
            Disp_Decl_Name (Decl);
            Put (" is ");
            Disp_Type (Dtype, True);
            Put_Line (";");
         when OD_Local
           | OD_Var =>
            Disp_Decl_Storage (Decl);
            Put ("var ");
            Disp_Decl_Name (Decl);
            Put (" : ");
            Dtype := Get_Decl_Type (Decl);
            Disp_Type (Dtype);
            if True then
               Put (" {size="
                    & Uns32'Image (Types.Get_Type_Size (Dtype)) & "}");
            end if;
            Put_Line (";");
         when OD_Const =>
            Disp_Decl_Storage (Decl);
            Put ("constant ");
            Disp_Decl_Name (Decl);
            Put (" : ");
            Disp_Type (Get_Decl_Type (Decl));
            Put_Line (";");
         when OD_Init_Val =>
            Put ("constant ");
            Disp_Decl_Name (Get_Val_Decl (Decl));
            Put (" := ");
            Disp_Lit (Get_Val_Val (Decl));
            Put_Line (";");
         when OD_Function
           | OD_Procedure =>
            Disp_Subprg_Decl (Indent, Decl);
            Put_Line (";");
         when OD_Interface =>
            null;
         when OD_Body =>
            --  Put ("body ");
            Disp_Subprg_Decl (Indent, Get_Body_Decl (Decl));
            -- Disp_Decl_Name (Get_Body_Decl (Decl));
            New_Line;
            Disp_Subprg (Indent, Get_Body_Stmt (Decl));
         when OD_Block | OD_Subprg_Ext =>
            null;
      end case;
      if Nl then
         New_Line;
      end if;
   end Disp_Decl;

   procedure Disp_Stmt (Indent : in out Natural; Stmt : O_Enode)
   is
      use Decls;
      Expr : O_Enode;
   begin
      case Get_Expr_Kind (Stmt) is
         when OE_Beg =>
            Disp_Indent (Indent);
            Put_Line ("declare");
            declare
               Last : O_Dnode;
               Decl : O_Dnode;
            begin
               Decl := Get_Block_Decls (Stmt);
               Last := Get_Block_Last (Decl);
               Decl := Decl + 1;
               while Decl <= Last loop
                  case Get_Decl_Kind (Decl) is
                     when OD_Block =>
                        Decl := Get_Block_Last (Decl) + 1;
                     when others =>
                        Disp_Decl (Indent + 1, Decl, False);
                        Decl := Decl + 1;
                  end case;
               end loop;
            end;
            Disp_Indent (Indent);
            Put_Line ("begin");
            Indent := Indent + 1;
         when OE_End =>
            Indent := Indent - 1;
            Disp_Indent (Indent);
            Put_Line ("end;");
         when OE_Line =>
            Disp_Indent (Indent);
            Put_Line ("--#" & Int32'Image (Get_Expr_Line_Number (Stmt)));
         when OE_BB =>
            Disp_Indent (Indent);
            Put_Line ("# BB" & Int32'Image (Get_BB_Number (Stmt)));
         when OE_Asgn =>
            Disp_Indent (Indent);
            Disp_Expr (Get_Assign_Target (Stmt));
            Put (" := ");
            Disp_Expr (Get_Expr_Operand (Stmt));
            Put_Line (";");
         when OE_Call =>
            Disp_Indent (Indent);
            Disp_Call (Stmt);
            Put_Line (";");
         when OE_Jump_F =>
            Disp_Indent (Indent);
            Put ("jump ");
            Disp_Label (Get_Jump_Label (Stmt));
            Put (" if not ");
            Disp_Expr (Get_Expr_Operand (Stmt));
            New_Line;
         when OE_Jump_T =>
            Disp_Indent (Indent);
            Put ("jump ");
            Disp_Label (Get_Jump_Label (Stmt));
            Put (" if ");
            Disp_Expr (Get_Expr_Operand (Stmt));
            New_Line;
         when OE_Jump =>
            Disp_Indent (Indent);
            Put ("jump ");
            Disp_Label (Get_Jump_Label (Stmt));
            New_Line;
         when OE_Label =>
            Disp_Indent (Indent);
            Disp_Label (Stmt);
            New_Line;
         when OE_Ret =>
            Disp_Indent (Indent);
            Put ("return");
            Expr := Get_Expr_Operand (Stmt);
            if Expr /= O_Enode_Null then
               Put (" ");
               Disp_Expr (Expr);
            end if;
            Put_Line (";");
         when OE_Set_Stack =>
            Disp_Indent (Indent);
            Put ("%sp := ");
            Disp_Expr (Get_Expr_Operand (Stmt));
            Put_Line (";");
         when OE_Leave =>
            Disp_Indent (Indent);
            Put_Line ("# leave");
         when OE_If =>
            Disp_Indent (Indent);
            Put ("if ");
            Disp_Expr (Get_Expr_Operand (Stmt));
            Put (" then");
            New_Line;
            Indent := Indent + 1;
         when OE_Else =>
            Disp_Indent (Indent - 1);
            Put ("else");
            New_Line;
         when OE_Endif =>
            Indent := Indent - 1;
            Disp_Indent (Indent);
            Put_Line ("end if;");
         when OE_Loop =>
            Disp_Indent (Indent);
            Disp_Label (Stmt);
            New_Line;
            Indent := Indent + 1;
         when OE_Exit =>
            Disp_Indent (Indent);
            Put ("exit ");
            Disp_Label (Get_Jump_Label (Stmt));
            Put (";");
            New_Line;
         when OE_Next =>
            Disp_Indent (Indent);
            Put ("next ");
            Disp_Label (Get_Jump_Label (Stmt));
            Put (";");
            New_Line;
         when OE_Eloop =>
            Indent := Indent - 1;
            Disp_Indent (Indent);
            Put_Line ("end loop;");
         when OE_Case =>
            Disp_Indent (Indent);
            Put ("case ");
            Disp_Expr (Get_Expr_Operand (Stmt));
            Put (" is");
            New_Line;
            if Debug.Flag_Debug_Hli then
               Indent := Indent + 2;
            end if;
         when OE_Case_Branch =>
            Disp_Indent (Indent - 1);
            Put ("when ");
            declare
               C : O_Enode;
               L, H : O_Enode;
            begin
               C := Get_Case_Branch_Choice (Stmt);
               loop
                  L := Get_Expr_Left (C);
                  H := Get_Expr_Right (C);
                  if L = O_Enode_Null then
                     Put ("others");
                  else
                     Disp_Expr (L);
                     if H /= O_Enode_Null then
                        Put (" ... ");
                        Disp_Expr (H);
                     end if;
                  end if;
                  C := Get_Case_Choice_Link (C);
                  exit when C = O_Enode_Null;
                  New_Line;
                  Disp_Indent (Indent - 1);
                  Put ("  | ");
               end loop;
               Put (" =>");
               New_Line;
            end;
         when OE_Case_End =>
            Indent := Indent - 2;
            Disp_Indent (Indent);
            Put ("end case;");
            New_Line;
         when others =>
            Put_Line (Standard_Error, "debug.disp_stmt: unknown statement " &
                      OE_Kind'Image (Get_Expr_Kind (Stmt)));
      end case;
   end Disp_Stmt;

   procedure Disp_Subprg (Ident : Natural; S_Entry : O_Enode)
   is
      Stmt : O_Enode;
      N_Ident : Natural := Ident;
   begin
      Stmt := S_Entry;
      loop
         Stmt := Get_Stmt_Link (Stmt);
         Disp_Stmt (N_Ident, Stmt);
         exit when Get_Expr_Kind (Stmt) = OE_Leave;
      end loop;
   end Disp_Subprg;

   Last_Decl : O_Dnode := O_Dnode_First;

   procedure Disp_Decls_Until (Last : O_Dnode; Nl : Boolean := False) is
   begin
      while Last_Decl <= Last loop
         Disp_Decl (0, Last_Decl, Nl);
         Last_Decl := Last_Decl + 1;
      end loop;
   end Disp_Decls_Until;

   procedure Disp_Subprg (Subprg : Subprogram_Data_Acc)
   is
      use Decls;
   begin
      Disp_Decls_Until (Subprg.D_Body, True);
      if Get_Decl_Kind (Last_Decl) /= OD_Block then
         raise Program_Error;
      end if;
      if Debug.Flag_Debug_Keep then
         --  If nodes are kept, the next declaration to be displayed (at top
         --   level) is the one that follow the subprogram block.
         Last_Decl := Get_Block_Last (Last_Decl) + 1;
      else
         --  If nodes are not kept, this subprogram block will be freed, and
         --  the next declaration is the block itself.
         Last_Decl := Subprg.D_Body;
      end if;
   end Disp_Subprg;

   procedure Init is
   begin
      Flags.Flag_Type_Name := True;
   end Init;

   procedure Finish is
   begin
      Disp_Decls_Until (Decls.Get_Decl_Last, True);
   end Finish;

end Ortho_Code.Disps;
