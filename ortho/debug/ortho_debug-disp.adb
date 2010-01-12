--  Display the code from the ortho debug tree.
--  Copyright (C) 2005 Tristan Gingold
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

package body Ortho_Debug.Disp is
   package Formated_Output is
      use Interfaces.C_Streams;

      type Disp_Context is limited private;

      procedure Init_Context (File : FILEs);

      --  Save the current context, and create a new one.
      procedure Push_Context (File : FILEs; Prev_Ctx : out Disp_Context);

      --  Restore a previous context, saved by Push_Context.
      procedure Pop_Context (Prev_Ctx : Disp_Context);

      procedure Put (Str : String);

      procedure Put_Line (Str : String);

      --  Add a tabulation.
      --  Every new line will start at this tabulation.
      procedure Add_Tab;

      --  Removed a tabulation.
      --  The next new line will start at the previous tabulation.
      procedure Rem_Tab;

      --  Flush the current output.
      procedure Flush;

      --  Return TRUE if the ident level is nul.
      function Is_Top return Boolean;

      procedure Put_Tab;

      procedure New_Line;

      procedure Put (C : Character);

      procedure Put_Trim (Str : String);

      procedure Set_Mark;
   private
      type Disp_Context is record
         --  File where the info are written to.
         File : FILEs;
         --  Line number of the line to be written.
         Lineno : Natural;
         --  Buffer for the current line.
         Line : String (1 .. 256);
         --  Number of characters currently in the line.
         Line_Len : Natural;

         --  Current tabulation.
         Tab : Natural;
         --  Tabulation to be used for the next line.
         Next_Tab : Natural;

         Mark : Natural;
      end record;
   end Formated_Output;

   package body Formated_Output is
      --  The current context.
      Ctx : Disp_Context;

      procedure Init_Context (File : FILEs) is
      begin
         Ctx.File := File;
         Ctx.Lineno := 1;
         Ctx.Line_Len := 0;
         Ctx.Tab := 0;
         Ctx.Next_Tab := 0;
         Ctx.Mark := 0;
      end Init_Context;

      procedure Push_Context (File : FILEs; Prev_Ctx : out Disp_Context)
      is
      begin
         Prev_Ctx := Ctx;
         Init_Context (File);
      end Push_Context;

      --  Restore a previous context, saved by Push_Context.
      procedure Pop_Context (Prev_Ctx : Disp_Context) is
      begin
         Flush;
         Ctx := Prev_Ctx;
      end Pop_Context;

      procedure Flush
      is
         Status : size_t;
         Res : int;
         pragma Unreferenced (Status, Res);
      begin
         if Ctx.Line_Len > 0 then
            Status := fwrite (Ctx.Line'Address, size_t (Ctx.Line_Len), 1,
                              Ctx.File);
            Res := fputc (Character'Pos (ASCII.Lf), Ctx.File);
            Ctx.Line_Len := 0;
         end if;
         Ctx.Mark := 0;
      end Flush;

      function Is_Top return Boolean is
      begin
         return Ctx.Tab = 0;
      end Is_Top;

      procedure Put_Tab
      is
         Tab : Natural := Ctx.Next_Tab;
         Max_Tab : constant Natural := 40;
      begin
         if Tab > Max_Tab then
            --  Limit indentation length, to limit line length.
            Tab := Max_Tab;
         end if;

         Ctx.Line (1 .. Tab) := (others => ' ');
         Ctx.Line_Len := Tab;
         Ctx.Next_Tab := Ctx.Tab + 2;
      end Put_Tab;

      procedure Put (Str : String) is
         Saved : String (1 .. 80);
         Len : Natural;
      begin
         if Ctx.Line_Len + Str'Length >= 80 then
            if Ctx.Mark > 0 then
               Len := Ctx.Line_Len - Ctx.Mark + 1;
               Saved (1 .. Len) := Ctx.Line (Ctx.Mark .. Ctx.Line_Len);
               Ctx.Line_Len := Ctx.Mark - 1;
               Flush;
               Put_Tab;
               Ctx.Line (Ctx.Line_Len + 1 .. Ctx.Line_Len + Len) :=
                 Saved (1 .. Len);
               Ctx.Line_Len := Ctx.Line_Len + Len;
            else
               Flush;
            end if;
         end if;
         if Ctx.Line_Len = 0 then
            Put_Tab;
         end if;
         Ctx.Line (Ctx.Line_Len + 1 .. Ctx.Line_Len + Str'Length) := Str;
         Ctx.Line_Len := Ctx.Line_Len + Str'Length;
      end Put;

      procedure Put_Trim (Str : String) is
      begin
         for I in Str'Range loop
            if Str (I) /= ' ' then
               Put (Str (I .. Str'Last));
               return;
            end if;
         end loop;
      end Put_Trim;

      procedure Put_Line (Str : String) is
      begin
         Put (Str);
         Flush;
         Ctx.Next_Tab := Ctx.Tab;
      end Put_Line;

      procedure New_Line
      is
         Status : int;
         pragma Unreferenced (Status);
      begin
         if Ctx.Line_Len > 0 then
            Flush;
         else
            Status := fputc (Character'Pos (ASCII.LF), Ctx.File);
         end if;
         Ctx.Next_Tab := Ctx.Tab;
      end New_Line;

      procedure Put (C : Character)
      is
         S : constant String (1 .. 1) := (1 => C);
      begin
         Put (S);
      end Put;

      --  Add a tabulation.
      --  Every new line will start at this tabulation.
      procedure Add_Tab is
      begin
         Ctx.Tab := Ctx.Tab + 2;
         Ctx.Next_Tab := Ctx.Tab;
      end Add_Tab;

      --  Removed a tabulation.
      --  The next new line will start at the previous tabulation.
      procedure Rem_Tab is
      begin
         Ctx.Tab := Ctx.Tab - 2;
         Ctx.Next_Tab := Ctx.Tab;
      end Rem_Tab;

      procedure Set_Mark is
      begin
         Ctx.Mark := Ctx.Line_Len;
      end Set_Mark;
   end Formated_Output;

   use Formated_Output;

   procedure Init_Context (File : Interfaces.C_Streams.FILEs) is
   begin
      Formated_Output.Init_Context (File);
   end Init_Context;

   procedure Disp_Enode (E : O_Enode);
   procedure Disp_Lnode (Node : O_Lnode);
   procedure Disp_Snode (First, Last : O_Snode);
   procedure Disp_Dnode (Decl : O_Dnode);
   procedure Disp_Tnode (Atype : O_Tnode; Full : Boolean);

   procedure Disp_Ident (Id : O_Ident) is
   begin
      Put (Get_String (Id));
   end Disp_Ident;

   procedure Disp_Tnode_Name (Atype : O_Tnode) is
   begin
      Disp_Tnode (Atype, False);
   end Disp_Tnode_Name;

   procedure Disp_Dnode_Name (Decl : O_Dnode) is
   begin
      Disp_Ident (Decl.Name);
   end Disp_Dnode_Name;

   procedure Disp_Loop_Name (Stmt : O_Snode) is
   begin
      Put ("loop" & Natural'Image (Stmt.Loop_Level));
   end Disp_Loop_Name;

   function Get_Enode_Name (Kind : OE_Kind) return String
   is
   begin
      case Kind is
--          when OE_Boolean_Lit =>
--             return "boolean_lit";
--          when OE_Unsigned_Lit =>
--             return "unsigned_lit";
--          when OE_Signed_Lit =>
--             return "signed lit";
--          when OE_Float_Lit =>
--             return "float lit";
--          when OE_Null_Lit =>
--             return "null lit";
--          when OE_Enum_Lit =>
--             return "enum lit";

--          when OE_Sizeof_Lit =>
--             return "sizeof lit";
--          when OE_Offsetof_Lit =>
--             return "offsetof lit";
--          when OE_Aggregate =>
--             return "aggregate";
--          when OE_Aggr_Element =>
--             return "aggr_element";
--          when OE_Union_Aggr =>
--             return "union aggr";

         when OE_Lit =>
            return "lit";
         when OE_Add_Ov =>
            return "+#";
         when OE_Sub_Ov =>
            return "-#";
         when OE_Mul_Ov =>
            return "*#";
         when OE_Div_Ov =>
            return "/#";
         when OE_Rem_Ov =>
            return "rem#";
         when OE_Mod_Ov =>
            return "mod#";
         when OE_Exp_Ov =>
            return "**#";

         when OE_And =>
            return "and";
         when OE_Or =>
            return "or";
         when OE_Xor =>
            return "xor";
         when OE_And_Then =>
            return "and_then";
         when OE_Or_Else =>
            return "or_else";

         when OE_Not =>
            return "not";
         when OE_Neg_Ov =>
            return "-";
         when OE_Abs_Ov =>
            return "abs";

         when OE_Eq =>
            return "=";
         when OE_Neq =>
            return "/=";
         when OE_Le =>
            return "<=";
         when OE_Lt =>
            return "<";
         when OE_Ge =>
            return ">=";
         when OE_Gt =>
            return ">";

         when OE_Function_Call =>
            return "function call";
         when OE_Convert_Ov =>
            return "convert_ov";
         when OE_Address =>
            return "address";
         when OE_Unchecked_Address =>
            return "unchecked_address";
--          when OE_Subprogram_Address =>
--             return "subprg_address";
         when OE_Alloca =>
            return "alloca";
         when OE_Value =>
            return "value";
         when OE_Nil =>
            return "??";
      end case;
   end Get_Enode_Name;

   function Get_Lnode_Name (Kind : OL_Kind) return String
   is
   begin
      case Kind is
         when OL_Obj =>
            return "obj";
         when OL_Indexed_Element =>
            return "indexed_element";
         when OL_Slice =>
            return "slice";
         when OL_Selected_Element =>
            return "selected_element";
         when OL_Access_Element =>
            return "access_element";
--          when OL_Param_Ref =>
--             return "param_ref";
--          when OL_Var_Ref =>
--             return "var_ref";
--          when OL_Const_Ref =>
--             return "const_ref";
      end case;
   end Get_Lnode_Name;

   pragma Unreferenced (Get_Lnode_Name);

   procedure Disp_Enode_Name (Kind : OE_Kind) is
   begin
      Put (Get_Enode_Name (Kind));
   end Disp_Enode_Name;

   procedure Disp_Assoc_List (Head : O_Anode)
   is
      El : O_Anode;
   begin
      El := Head;
      Put ("(");
      if El /= null then
         loop
            Disp_Enode (El.Actual);
            El := El.Next;
            exit when El = null;
            Put (", ");
         end loop;
      end if;
      Put (")");
   end Disp_Assoc_List;

   function Image (Lit : Integer) return String
   is
      S : constant String := Integer'Image (Lit);
   begin
      if S (1) = ' ' then
         return S (2 .. S'Length);
      else
         return S;
      end if;
   end Image;

   procedure Disp_Lit (Lit_Type : O_Tnode; Str : String) is
   begin
      if False then
         Put_Trim (Str);
      else
         Disp_Tnode_Name (Lit_Type);
         Put ("'[");
         Put_Trim (Str);
         Put (']');
      end if;
   end Disp_Lit;

   procedure Disp_Cnode (C : O_Cnode) is
   begin
      case C.Kind is
         when OC_Unsigned_Lit =>
            if False and then (C.U_Val >= Character'Pos(' ')
                               and C.U_Val <= Character'Pos ('~'))
            then
               Put (''');
               Put (Character'Val (C.U_Val));
               Put (''');
            else
               Disp_Lit (C.Ctype, Unsigned_64'Image (C.U_Val));
            end if;
         when OC_Signed_Lit =>
            Disp_Lit (C.Ctype, Integer_64'Image (C.S_Val));
         when OC_Float_Lit =>
            Disp_Lit (C.Ctype, IEEE_Float_64'Image (C.F_Val));
         when OC_Boolean_Lit =>
            Disp_Lit (C.Ctype, Get_String (C.B_Id));
         when OC_Null_Lit =>
            Disp_Lit (C.Ctype, "null");
         when OC_Enum_Lit =>
            Disp_Lit (C.Ctype, Get_String (C.E_Name));
         when OC_Sizeof_Lit =>
            Disp_Tnode_Name (C.Ctype);
            Put ("'sizeof (");
            Disp_Tnode_Name (C.S_Type);
            Put (")");
         when OC_Offsetof_Lit =>
            Disp_Tnode_Name (C.Ctype);
            Put ("'offsetof (");
            Disp_Tnode_Name (C.Off_Field.Parent);
            Put (".");
            Disp_Ident (C.Off_Field.Ident);
            Put (")");
         when OC_Aggregate =>
            declare
               El : O_Cnode;
               Field : O_Fnode;
            begin
               Put ('{');
               El := C.Aggr_Els;
               if C.Ctype.Kind = ON_Record_Type then
                  Field := C.Ctype.Elements;
               else
                  Field := null;
               end if;
               if El /= null then
                  loop
                     Set_Mark;
                     if Field /= null then
                        Put ('.');
                        Disp_Ident (Field.Ident);
                        Put (" = ");
                        Field := Field.Next;
                     end if;
                     Disp_Cnode (El.Aggr_Value);
                     El := El.Aggr_Next;
                     exit when El = null;
                     Put (", ");
                  end loop;
               end if;
               Put ('}');
            end;
         when OC_Aggr_Element =>
            Disp_Cnode (C.Aggr_Value);
         when OC_Union_Aggr =>
            Put ('{');
            Put ('.');
            Disp_Ident (C.Uaggr_Field.Ident);
            Put (" = ");
            Disp_Cnode (C.Uaggr_Value);
            Put ('}');
         when OC_Address =>
            Disp_Tnode_Name (C.Ctype);
            Put ("'address (");
            Disp_Dnode_Name (C.Decl);
            Put (")");
         when OC_Unchecked_Address =>
            Disp_Tnode_Name (C.Ctype);
            Put ("'unchecked_address (");
            Disp_Dnode_Name (C.Decl);
            Put (")");
         when OC_Subprogram_Address =>
            Disp_Tnode_Name (C.Ctype);
            Put ("'subprg_addr (");
            Disp_Dnode_Name (C.Decl);
            Put (")");
      end case;
   end Disp_Cnode;

   procedure Disp_Enode (E : O_Enode)
   is
   begin
      case E.Kind is
         when OE_Lit =>
            Disp_Cnode (E.Lit);
         when OE_Dyadic_Expr_Kind =>
            Put ("(");
            Disp_Enode (E.Left);
            Put (' ');
            Disp_Enode_Name (E.Kind);
            Put (' ');
            Disp_Enode (E.Right);
            Put (')');
         when OE_Compare_Expr_Kind =>
            Disp_Tnode_Name (E.Rtype);
            Put ("'(");
            Disp_Enode (E.Left);
            Put (' ');
            Disp_Enode_Name (E.Kind);
            Put (' ');
            Disp_Enode (E.Right);
            Put (')');
         when OE_Monadic_Expr_Kind =>
            Disp_Enode_Name (E.Kind);
            if E.Kind /= OE_Neg_Ov then
               Put (' ');
            end if;
            Disp_Enode (E.Operand);
         when OE_Address =>
            Disp_Tnode_Name (E.Rtype);
            Put ("'address (");
            Disp_Lnode (E.Lvalue);
            Put (")");
         when OE_Unchecked_Address =>
            Disp_Tnode_Name (E.Rtype);
            Put ("'unchecked_address (");
            Disp_Lnode (E.Lvalue);
            Put (")");
         when OE_Convert_Ov =>
            Disp_Tnode_Name (E.Rtype);
            Put ("'conv (");
            Disp_Enode (E.Conv);
            Put (')');
         when OE_Function_Call =>
            Disp_Dnode_Name (E.Func);
            Put (' ');
            Disp_Assoc_List (E.Assoc);
         when OE_Alloca =>
            Disp_Tnode_Name (E.Rtype);
            Put ("'alloca (");
            Disp_Enode (E.A_Size);
            Put (')');
         when OE_Value =>
            Disp_Lnode (E.Value);
         when OE_Nil =>
            null;
      end case;
   end Disp_Enode;

   procedure Disp_Lnode (Node : O_Lnode) is
   begin
      case Node.Kind is
         when OL_Obj =>
            Disp_Dnode_Name (Node.Obj);
         when OL_Access_Element =>
            Disp_Enode (Node.Acc_Base);
            Put (".all");
         when OL_Indexed_Element =>
            Disp_Lnode (Node.Array_Base);
            Put ('[');
            Disp_Enode (Node.Index);
            Put (']');
         when OL_Slice =>
            Disp_Lnode (Node.Slice_Base);
            Put ('[');
            Disp_Enode (Node.Slice_Index);
            Put ("...]");
         when OL_Selected_Element =>
            Disp_Lnode (Node.Rec_Base);
            Put ('.');
            Disp_Ident (Node.Rec_El.Ident);
--          when OL_Var_Ref
--            | OL_Const_Ref
--            | OL_Param_Ref =>
--             Disp_Dnode_Name (Node.Decl);
      end case;
   end Disp_Lnode;

   procedure Disp_Fnodes (First : O_Fnode)
   is
      El : O_Fnode;
   begin
      Add_Tab;
      El := First;
      while El /= null loop
         Disp_Ident (El.Ident);
         Put (": ");
         Disp_Tnode (El.Ftype, False);
         Put_Line ("; ");
         El := El.Next;
      end loop;
      Rem_Tab;
   end Disp_Fnodes;

   procedure Disp_Tnode (Atype : O_Tnode; Full : Boolean) is
   begin
      if not Full and Atype.Decl /= null then
         Disp_Ident (Atype.Decl.Name);
         return;
      end if;
      case Atype.Kind is
         when ON_Boolean_Type =>
            Put ("boolean {");
            Disp_Ident (Atype.False_N.B_Id);
            Put (", ");
            Disp_Ident (Atype.True_N.B_Id);
            Put ("}");
         when ON_Unsigned_Type =>
            Put ("unsigned (");
            Put_Trim (Unsigned_32'Image (8 * Atype.Size));
            Put (")");
         when ON_Signed_Type =>
            Put ("signed (");
            Put_Trim (Unsigned_32'Image (8 * Atype.Size));
            Put (")");
         when ON_Float_Type =>
            Put ("float");
         when ON_Enum_Type =>
            declare
               El : O_Cnode;
            begin
               Put ("enum {");
               El := Atype.Literals;
               while El /= O_Cnode_Null loop
                  Set_Mark;
                  Disp_Ident (El.E_Name);
                  Put (" = ");
                  Put (Image (El.E_Val));
                  El := El.E_Next;
                  exit when El = O_Cnode_Null;
                  Put (", ");
               end loop;
               Put ("}");
            end;
         when ON_Array_Type =>
            Put ("array [");
            Disp_Tnode (Atype.Index_Type, False);
            Put ("] of ");
            Disp_Tnode (Atype.El_Type, False);
         when ON_Access_Type =>
            Put ("access ");
            if Atype.D_Type /= O_Tnode_Null then
               Disp_Tnode (Atype.D_Type, False);
            end if;
         when ON_Record_Type =>
            Put_Line ("record ");
            Disp_Fnodes (Atype.Elements);
            Put ("end record");
         when ON_Union_Type =>
            Put_Line ("union ");
            Disp_Fnodes (Atype.Elements);
            Put ("end union");
         when ON_Array_Sub_Type =>
            Put ("subarray ");
            Disp_Tnode_Name (Atype.Base_Type);
            Put ("[");
            Disp_Cnode (Atype.Length);
            Put ("]");
      end case;
   end Disp_Tnode;

   procedure Disp_Storage_Name (Storage : O_Storage) is
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
   end Disp_Storage_Name;

   procedure Disp_Decls (Decls : O_Dnode)
   is
      El : O_Dnode;
   begin
      El := Decls;
      while El /= null loop
         Disp_Dnode (El);
         El := El.Next;
         if Is_Top then
            -- NOTE: some declaration does not disp anything, so there may be
            -- double new line.
            New_Line;
         end if;
      end loop;
   end Disp_Decls;

   procedure Disp_Function_Decl (Decl : O_Dnode) is
   begin
      Disp_Storage_Name (Decl.Storage);
      Put (" ");
      if Decl.Dtype = null then
         Put ("procedure ");
      else
         Put ("function ");
      end if;
      Disp_Ident (Decl.Name);
      Put_Line (" (");
      Add_Tab;
      declare
         El : O_Dnode;
      begin
         El := Decl.Interfaces;
         if El /= null then
            loop
               Disp_Dnode (El);
               El := El.Next;
               exit when El = null;
               Put_Line (";");
            end loop;
         end if;
         Put (")");
      end;
      if Decl.Dtype /= null then
         New_Line;
         Put ("return ");
         Disp_Tnode (Decl.Dtype, False);
      end if;
      Rem_Tab;
   end Disp_Function_Decl;

   procedure Disp_Dnode (Decl : O_Dnode) is
   begin
      case Decl.Kind is
         when ON_Type_Decl =>
            Put ("type ");
            Disp_Ident (Decl.Name);
            Put (" is ");
            if not Decl.Dtype.Uncomplete then
               Disp_Tnode (Decl.Dtype, True);
            else
               case Decl.Dtype.Kind is
                  when ON_Record_Type =>
                     Put ("record");
                  when ON_Access_Type =>
                     Put ("access");
                  when others =>
                     raise Program_Error;
               end case;
            end if;
            Put_Line (";");
         when ON_Completed_Type_Decl =>
            Put ("type ");
            Disp_Ident (Decl.Name);
            Put (" is ");
            Disp_Tnode (Decl.Dtype, True);
            Put_Line (";");
         when ON_Const_Decl =>
            Disp_Storage_Name (Decl.Storage);
            Put (" ");
            Put ("constant ");
            Disp_Ident (Decl.Name);
            Put (" : ");
            Disp_Tnode_Name (Decl.Dtype);
            Put_Line (";");
         when ON_Const_Value =>
            Put ("constant ");
            Disp_Ident (Decl.Name);
            Put (" := ");
            Disp_Cnode (Decl.Value);
            Put_Line (";");
         when ON_Var_Decl =>
            Disp_Storage_Name (Decl.Storage);
            Put (" ");
            Put ("var ");
            Disp_Ident (Decl.Name);
            Put (" : ");
            Disp_Tnode_Name (Decl.Dtype);
            Put_Line (";");
         when ON_Function_Decl =>
            if Decl.Next = null or Decl.Next /= Decl.Func_Body then
               --  This is a forward/external declaration.
               Disp_Function_Decl (Decl);
               Put_Line (";");
            end if;
         when ON_Function_Body =>
            Disp_Function_Decl (Decl.Func_Decl);
            New_Line;
            Disp_Snode (Decl.Func_Stmt, Decl.Func_Stmt);
         when ON_Interface_Decl =>
            Disp_Ident (Decl.Name);
            Put (": ");
            Disp_Tnode (Decl.Dtype, False);
         when ON_Debug_Line_Decl =>
            Put_Line ("--#" & Natural'Image (Decl.Line));
         when ON_Debug_Comment_Decl =>
            Put_Line ("-- " & Decl.Comment.all);
         when ON_Debug_Filename_Decl =>
            Put_Line ("--F " & Decl.Filename.all);
      end case;
   end Disp_Dnode;

   procedure Disp_Snode (First : O_Snode; Last : O_Snode) is
      Stmt : O_Snode;
   begin
      Stmt := First;
      loop
         --if Stmt.Kind = ON_Elsif_Stmt or Stmt.Kind = ON_When_Stmt then
         --   Put_Indent (Tab - 1);
         --else
         --   Put_Indent (Tab);
         --end if;
         case Stmt.Kind is
            when ON_Declare_Stmt =>
               Put_Line ("declare");
               Add_Tab;
               Disp_Decls (Stmt.Decls);
               Rem_Tab;
               Put_Line ("begin");
               Add_Tab;
               if Stmt.Stmts /= null then
                  Disp_Snode (Stmt.Stmts, null);
               end if;
               Rem_Tab;
               Put_Line ("end;");
            when ON_Assign_Stmt =>
               Disp_Lnode (Stmt.Target);
               Put (" := ");
               Disp_Enode (Stmt.Value);
               Put_Line (";");
            when ON_Return_Stmt =>
               Put ("return ");
               if Stmt.Ret_Val /= null then
                  Disp_Enode (Stmt.Ret_Val);
               end if;
               Put_Line (";");
            when ON_If_Stmt =>
               Add_Tab;
               Disp_Snode (Stmt.Next, Stmt.If_Last);
               Stmt := Stmt.If_Last;
               Rem_Tab;
               Put_Line ("end if;");
            when ON_Elsif_Stmt =>
               Rem_Tab;
               if Stmt.Cond = null then
                  Put_Line ("else");
               else
                  if First = Stmt then
                     Put ("if ");
                  else
                     Put ("elsif ");
                  end if;
                  Disp_Enode (Stmt.Cond);
                  Put_Line (" then");
               end if;
               Add_Tab;
            when ON_Loop_Stmt =>
               Disp_Loop_Name (Stmt);
               Put_Line (":");
               Add_Tab;
               Disp_Snode (Stmt.Next, Stmt.Loop_Last);
               Stmt := Stmt.Loop_Last;
               Rem_Tab;
               Put_Line ("end loop;");
            when ON_Exit_Stmt =>
               Put ("exit ");
               Disp_Loop_Name (Stmt.Loop_Id);
               Put_Line (";");
            when ON_Next_Stmt =>
               Put ("next ");
               Disp_Loop_Name (Stmt.Loop_Id);
               Put_Line (";");
            when ON_Case_Stmt =>
               Put ("case ");
               Disp_Enode (Stmt.Selector);
               Put_Line (" is");
               Add_Tab;
               Disp_Snode (Stmt.Next, Stmt.Case_Last);
               Stmt := Stmt.Case_Last;
               Rem_Tab;
               Put_Line ("end case;");
            when ON_When_Stmt =>
               declare
                  Choice: O_Choice;
               begin
                  Rem_Tab;
                  Choice := Stmt.Choice_List;
                  while Choice /= null loop
                     Put ("when ");
                     case Choice.Kind is
                        when ON_Choice_Expr =>
                           Disp_Cnode (Choice.Expr);
                        when ON_Choice_Range =>
                           Disp_Cnode (Choice.Low);
                           Put (" ... ");
                           Disp_Cnode (Choice.High);
                        when ON_Choice_Default =>
                           Put ("default");
                     end case;
                     Put_Line (" =>");
                     Choice := Choice.Next;
                  end loop;
                  Add_Tab;
               end;
            when ON_Call_Stmt =>
               Disp_Dnode_Name (Stmt.Proc);
               Put (' ');
               Disp_Assoc_List (Stmt.Assoc);
               Put_Line (";");
            when ON_Debug_Line_Stmt =>
               Put_Line ("--#" & Natural'Image (Stmt.Line));
            when ON_Debug_Comment_Stmt =>
               Put_Line ("-- " & Stmt.Comment.all);
         end case;
         exit when Stmt = Last;
         Stmt := Stmt.Next;
         exit when Stmt = null and Last = null;
      end loop;
   end Disp_Snode;

   procedure Disp_Ortho (Decls : O_Snode) is
   begin
      Disp_Decls (Decls.Decls);
      Flush;
   end Disp_Ortho;

   procedure Disp_Tnode_Decl (N : O_Tnode) is
   begin
      Disp_Ident (N.Decl.Name);
      Put (" : ");
      Disp_Tnode (N, True);
   end Disp_Tnode_Decl;

   procedure Debug_Tnode (N : O_Tnode)
   is
      Ctx : Disp_Context;
   begin
      Push_Context (Interfaces.C_Streams.stdout, Ctx);
      Disp_Tnode_Decl (N);
      Pop_Context (Ctx);
   end Debug_Tnode;

   procedure Debug_Enode (N : O_Enode)
   is
      Ctx : Disp_Context;
   begin
      Push_Context (Interfaces.C_Streams.stdout, Ctx);
      Disp_Enode (N);
      Put (" : ");
      Disp_Tnode_Decl (N.Rtype);
      Pop_Context (Ctx);
   end Debug_Enode;

   procedure Debug_Fnode (N : O_Fnode)
   is
      Ctx : Disp_Context;
   begin
      Push_Context (Interfaces.C_Streams.stdout, Ctx);
      Disp_Ident (N.Ident);
      Put (": ");
      Disp_Tnode (N.Ftype, False);
      Pop_Context (Ctx);
   end Debug_Fnode;

   procedure Debug_Dnode (N : O_Dnode)
   is
      Ctx : Disp_Context;
   begin
      Push_Context (Interfaces.C_Streams.stdout, Ctx);
      Disp_Dnode (N);
      Pop_Context (Ctx);
   end Debug_Dnode;

   procedure Debug_Lnode (N : O_Lnode)
   is
      Ctx : Disp_Context;
   begin
      Push_Context (Interfaces.C_Streams.stdout, Ctx);
      Disp_Lnode (N);
      Put (" : ");
      Disp_Tnode_Decl (N.Rtype);
      Pop_Context (Ctx);
   end Debug_Lnode;

   procedure Debug_Snode (N : O_Snode)
   is
      Ctx : Disp_Context;
   begin
      Push_Context (Interfaces.C_Streams.stdout, Ctx);
      Disp_Snode (N, null);
      Pop_Context (Ctx);
   end Debug_Snode;

   pragma Unreferenced (Debug_Tnode, Debug_Enode, Debug_Fnode,
                        Debug_Dnode, Debug_Lnode, Debug_Snode);
end Ortho_Debug.Disp;
