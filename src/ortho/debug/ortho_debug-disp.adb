--  Display the code from the ortho debug tree.
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

package body Ortho_Debug.Disp is
   Disp_All_Types : constant Boolean := False;

   package Formated_Output is
      use Interfaces.C_Streams;

      type Disp_Context is limited private;

      procedure Init_Context (File : FILEs);

      --  Save the current context, and create a new one.
      procedure Push_Context (File : FILEs; Prev_Ctx : out Disp_Context);

      --  Restore a previous context, saved by Push_Context.
      procedure Pop_Context (Prev_Ctx : Disp_Context);

      procedure Put (Str : String);

      procedure Put_Keyword (Str : String);

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

      --  Flush to disk.  Only for debugging in case of crash.
      procedure Flush_File;
      pragma Unreferenced (Flush_File);
   private
      type Disp_Context is record
         --  File where the info are written to.
         File : FILEs;
         --  Line number of the line to be written.
         Lineno : Natural;
         --  Buffer for the current line.
         Line : String (1 .. 2048);
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

      procedure Put_Keyword (Str : String)
      is
         Kw : String (Str'Range);
      begin
         --  Convert to uppercase
         for I in Str'Range loop
            pragma Assert (Str (I) in 'a' .. 'z');
            Kw (I) := Character'Val
              (Character'Pos ('A')
                 + Character'Pos (Str (I)) - Character'Pos ('a'));
         end loop;

         Put (Kw);
      end Put_Keyword;

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

      procedure Flush_File is
         Status : int;
         pragma Unreferenced (Status);
      begin
         Flush;
         Status := fflush (Ctx.File);
      end Flush_File;
   end Formated_Output;

   use Formated_Output;

   procedure Init_Context (File : Interfaces.C_Streams.FILEs) is
   begin
      Formated_Output.Init_Context (File);
   end Init_Context;

   procedure Disp_Enode (E : O_Enode; Etype : O_Tnode);
   procedure Disp_Lnode (Node : O_Lnode);
   procedure Disp_Gnode (Node : O_Gnode);
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
      Put_Keyword ("loop");
      Put (Natural'Image (Stmt.Loop_Level));
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
         when OE_Convert =>
            return "convert";
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
            Disp_Enode (El.Actual, El.Formal.Dtype);
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

   --  Disp STR as a literal for scalar type LIT_TYPE.
   procedure Disp_Lit (Lit_Type : O_Tnode; Known : Boolean; Str : String) is
   begin
      if Known and not Disp_All_Types then
         Put_Trim (Str);
      else
         Disp_Tnode_Name (Lit_Type);
         Put ("'[");
         Put_Trim (Str);
         Put (']');
      end if;
   end Disp_Lit;

   Xdigit : constant array (0 .. 15) of Character := "0123456789abcdef";

   procedure Disp_Float_Lit
     (Lit_Type : O_Tnode; Known : Boolean; Val : IEEE_Float_64)
   is
      pragma Assert (IEEE_Float_64'Machine_Radix = 2);
      pragma Assert (IEEE_Float_64'Machine_Mantissa = 53);
      Exp : Integer;
      Man : Unsigned_64;
      --  Res: sign(1) + 0x(2) + Man(53 / 3 ~= 18) + p(1) + sing(1) + exp(4)
      Str : String (1 .. 1 + 2 + 18 + 1 + 1 + 4);
      P : Natural;
      Neg : Boolean;
   begin
      Exp := IEEE_Float_64'Exponent (Val) - 1;
      Man := Unsigned_64 (abs (IEEE_Float_64'Fraction (Val)) * 2.0 ** 53);

      --  Use decimal representation if there is no digit after the dot.
      if Man = 0 then
         Disp_Lit (Lit_Type, Known, "0.0");
      else
         pragma Assert (Shift_Right (Man, 52) = 1);

         --  Remove hidden 1.
         Man := Man and (2**52 - 1);

         --  Remove trailing hex 0.
         while Man /= 0 and (Man rem 16) = 0 loop
            Man := Man / 16;
         end loop;

         --  Exponent.
         P := Str'Last;
         if Exp < 0 then
            Neg := True;
            Exp := -Exp;
         else
            Neg := False;
         end if;
         loop
            Str (P) := Xdigit (Exp rem 10);
            P := P - 1;
            Exp := Exp / 10;
            exit when Exp = 0;
         end loop;
         if Neg then
            Str (P) := '-';
            P := P - 1;
         end if;
         Str (P) := 'p';
         P := P - 1;

         --  Mantissa.
         loop
            Str (P) := Xdigit (Natural (Man and 15));
            P := P - 1;
            Man := Man / 16;
            exit when Man = 0;
         end loop;

         P := P - 4;
         Str (P + 1) := '0';
         Str (P + 2) := 'x';
         Str (P + 3) := '1';
         Str (P + 4) := '.';

         if Val < 0.0 then
            Str (P) := '-';
            P := P - 1;
         end if;

         Disp_Lit (Lit_Type, Known, Str (P + 1 .. Str'Last));
      end if;
   end Disp_Float_Lit;

   --  Display C. If CTYPE is set, this is the known type of C.
   procedure Disp_Cnode (C : O_Cnode; Ctype : O_Tnode)
   is
      Known : constant Boolean := Ctype /= O_Tnode_Null;
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
               Disp_Lit (C.Ctype, Known, Unsigned_64'Image (C.U_Val));
            end if;
         when OC_Signed_Lit =>
            Disp_Lit (C.Ctype, Known, Integer_64'Image (C.S_Val));
         when OC_Float_Lit =>
            Disp_Float_Lit (C.Ctype, Known, C.F_Val);
         when OC_Boolean_Lit =>
            --  Always disp the type of boolean literals.
            Disp_Lit (C.Ctype, False, Get_String (C.B_Id));
         when OC_Null_Lit =>
            --  Always disp the type of null literals.
            Disp_Tnode_Name (C.Ctype);
            Put ("'[");
            Put_Keyword ("null");
            Put (']');
         when OC_Default_Lit =>
            --  Always disp the type of default literals.
            Disp_Tnode_Name (C.Ctype);
            Put ("'[");
            Put_Keyword ("default");
            Put (']');
         when OC_Enum_Lit =>
            --  Always disp the type of enum literals.
            Disp_Lit (C.Ctype, False, Get_String (C.E_Name));
         when OC_Sizeof_Lit =>
            Disp_Tnode_Name (C.Ctype);
            Put ("'sizeof (");
            Disp_Tnode_Name (C.S_Type);
            Put (")");
         when OC_Record_Sizeof_Lit =>
            Disp_Tnode_Name (C.Ctype);
            Put ("'record_sizeof (");
            Disp_Tnode_Name (C.S_Type);
            Put (")");
         when OC_Alignof_Lit =>
            Disp_Tnode_Name (C.Ctype);
            Put ("'alignof (");
            Disp_Tnode_Name (C.S_Type);
            Put (")");
         when OC_Offsetof_Lit =>
            Disp_Tnode_Name (C.Ctype);
            Put ("'offsetof (");
            Disp_Tnode_Name (C.Off_Field.Parent);
            Put (".");
            Disp_Ident (C.Off_Field.Ident);
            Put (")");
         when OC_Array_Aggregate =>
            declare
               El : O_Cnode;
               El_Type : O_Tnode;
            begin
               El := C.Arr_Els;
               El_Type := Get_Array_El_Type (C.Ctype);
               Put ('[');
               Put_Trim (Unsigned_32'Image (C.Arr_Len));
               Put (']');
               Put ('{');
               if El /= null then
                  loop
                     Set_Mark;
                     Disp_Cnode (El.Aggr_Value, El_Type);
                     El := El.Aggr_Next;
                     exit when El = null;
                     Put (", ");
                  end loop;
               end if;
               Put ('}');
            end;
         when OC_Record_Aggregate =>
            declare
               El : O_Cnode;
               El_Type : O_Tnode;
               Field : O_Fnode;
            begin
               Put ('{');
               El := C.Rec_Els;
               Field := Get_Record_Elements (C.Ctype);
               if El /= null then
                  loop
                     Set_Mark;
                     if Disp_All_Types then
                        Put ('.');
                        Disp_Ident (Field.Ident);
                        Put (" = ");
                     end if;
                     El_Type := Field.Ftype;
                     Field := Field.Next;
                     Disp_Cnode (El.Aggr_Value, El_Type);
                     El := El.Aggr_Next;
                     exit when El = null;
                     Put (", ");
                  end loop;
               end if;
               Put ('}');
            end;
         when OC_Aggr_Element =>
            Disp_Cnode (C.Aggr_Value, Ctype);
         when OC_Union_Aggr =>
            Put ('{');
            Put ('.');
            Disp_Ident (C.Uaggr_Field.Ident);
            Put (" = ");
            Disp_Cnode (C.Uaggr_Value, C.Uaggr_Field.Ftype);
            Put ('}');
         when OC_Address =>
            Disp_Tnode_Name (C.Ctype);
            Put ("'address (");
            Disp_Gnode (C.Addr_Global);
            Put (")");
         when OC_Unchecked_Address =>
            Disp_Tnode_Name (C.Ctype);
            Put ("'unchecked_address (");
            Disp_Gnode (C.Addr_Global);
            Put (")");
         when OC_Subprogram_Address =>
            Disp_Tnode_Name (C.Ctype);
            Put ("'subprg_addr (");
            Disp_Dnode_Name (C.Addr_Decl);
            Put (")");
      end case;
   end Disp_Cnode;

   function Is_Neg_Neg (E : O_Enode) return Boolean
   is
      Lit : O_Cnode;
   begin
      pragma Assert (E.Kind = OE_Neg_Ov);
      case E.Operand.Kind is
         when OE_Neg_Ov =>
            return True;
         when OE_Lit =>
            Lit := E.Operand.Lit;
            case Lit.Kind is
               when OC_Signed_Lit =>
                  return Lit.S_Val < 0;
               when OC_Float_Lit =>
                  return Lit.F_Val < 0.0;
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
      return False;
   end Is_Neg_Neg;

   --  Disp E whose expected type is ETYPE (may not be set).
   procedure Disp_Enode (E : O_Enode; Etype : O_Tnode) is
   begin
      case E.Kind is
         when OE_Lit =>
            Disp_Cnode (E.Lit, Etype);
         when OE_Dyadic_Expr_Kind =>
            Put ("(");
            Disp_Enode (E.Left, O_Tnode_Null);
            Put (' ');
            case E.Kind is
               when OE_Rem_Ov =>
                  Put_Keyword ("rem");
                  Put ('#');
               when OE_Mod_Ov =>
                  Put_Keyword ("mod");
                  Put ('#');
               when OE_And =>
                  Put_Keyword ("and");
               when OE_Or =>
                  Put_Keyword ("or");
               when OE_Xor =>
                  Put_Keyword ("xor");
               when others =>
                  Disp_Enode_Name (E.Kind);
            end case;
            Put (' ');
            Disp_Enode (E.Right, E.Left.Rtype);
            Put (')');
         when OE_Compare_Expr_Kind =>
            Disp_Tnode_Name (E.Rtype);
            Put ("'(");
            Disp_Enode (E.Left, O_Tnode_Null);
            Put (' ');
            Disp_Enode_Name (E.Kind);
            Put (' ');
            Disp_Enode (E.Right, E.Left.Rtype);
            Put (')');
         when OE_Monadic_Expr_Kind =>
            case E.Kind is
               when OE_Not =>
                  Put_Keyword ("not");
               when OE_Abs_Ov =>
                  Put_Keyword ("abs");
               when others =>
                  Disp_Enode_Name (E.Kind);
            end case;
            --  Don't print space after '-' unless the operand is also '-'.
            --  (avoid to print --, which is a comment).
            if E.Kind /= OE_Neg_Ov or else Is_Neg_Neg (E) then
               Put (' ');
            end if;
            Disp_Enode (E.Operand, Etype);
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
            Put ("'conv# (");
            Disp_Enode (E.Conv, O_Tnode_Null);
            Put (')');
         when OE_Convert =>
            Disp_Tnode_Name (E.Rtype);
            Put ("'conv (");
            Disp_Enode (E.Conv, O_Tnode_Null);
            Put (')');
         when OE_Function_Call =>
            Disp_Dnode_Name (E.Func);
            Put (' ');
            Disp_Assoc_List (E.Assoc);
         when OE_Alloca =>
            Disp_Tnode_Name (E.Rtype);
            Put ("'alloca (");
            Disp_Enode (E.A_Size, O_Tnode_Null);
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
            Disp_Enode (Node.Acc_Base, O_Tnode_Null);
            Put (".");
            Put_Keyword ("all");
         when OL_Indexed_Element =>
            Disp_Lnode (Node.Array_Base);
            Put ('[');
            Disp_Enode (Node.Index, O_Tnode_Null);
            Put (']');
         when OL_Slice =>
            Disp_Lnode (Node.Slice_Base);
            Put ('[');
            Disp_Enode (Node.Slice_Index, O_Tnode_Null);
            Put ("...]");
         when OL_Selected_Element =>
            Disp_Lnode (Node.Rec_Base);
            Put ('.');
            Disp_Ident (Node.Rec_El.Ident);
      end case;
   end Disp_Lnode;

   procedure Disp_Gnode (Node : O_Gnode) is
   begin
      case Node.Kind is
         when OG_Decl =>
            Disp_Dnode_Name (Node.Decl);
         when OG_Selected_Element =>
            Disp_Gnode (Node.Rec_Base);
            Put ('.');
            Disp_Ident (Node.Rec_El.Ident);
      end case;
   end Disp_Gnode;

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
         Put_Line (";");
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
            Put_Keyword ("boolean");
            Put (" {");
            Disp_Ident (Atype.False_N.B_Id);
            Put (", ");
            Disp_Ident (Atype.True_N.B_Id);
            Put ("}");
         when ON_Unsigned_Type =>
            Put_Keyword ("unsigned");
            Put (" (");
            Put_Trim (Natural'Image (Atype.Int_Size));
            Put (")");
         when ON_Signed_Type =>
            Put_Keyword ("signed");
            Put (" (");
            Put_Trim (Natural'Image (Atype.Int_Size));
            Put (")");
         when ON_Float_Type =>
            Put_Keyword ("float");
         when ON_Enum_Type =>
            declare
               El : O_Cnode;
            begin
               Put_Keyword ("enum");
               Put (" {");
               El := Atype.Literals;
               while El /= O_Cnode_Null loop
                  Set_Mark;
                  Disp_Ident (El.E_Name);
                  if False then
                     Put (" = ");
                     Put (Image (El.E_Val));
                  end if;
                  El := El.E_Next;
                  exit when El = O_Cnode_Null;
                  Put (", ");
               end loop;
               Put ("}");
            end;
         when ON_Array_Type =>
            Put_Keyword ("array");
            Put (" [");
            Disp_Tnode (Atype.Index_Type, False);
            Put ("] ");
            Put_Keyword ("of");
            Put (" ");
            Disp_Tnode (Atype.El_Type, False);
         when ON_Access_Type =>
            Put_Keyword ("access");
            Put (" ");
            if Atype.D_Type /= O_Tnode_Null then
               Disp_Tnode (Atype.D_Type, False);
            end if;
         when ON_Record_Type =>
            Put_Keyword ("record");
            New_Line;
            Disp_Fnodes (Atype.Rec_Elements);
            Put_Keyword ("end");
            Put (" ");
            Put_Keyword ("record");
         when ON_Record_Subtype =>
            Put_Keyword ("subrecord");
            Put (" ");
            Disp_Tnode_Name (Atype.Subrec_Base);
            Put ("(");
            Disp_Fnodes (Atype.Subrec_Elements);
            Put (")");
         when ON_Union_Type =>
            Put_Keyword ("union");
            New_Line;
            Disp_Fnodes (Atype.Rec_Elements);
            Put_Keyword ("end");
            Put (" ");
            Put_Keyword ("union");
         when ON_Array_Subtype =>
            declare
               Base : constant O_Tnode := Atype.Arr_Base;
            begin
               Put_Keyword ("subarray");
               Put (" ");
               Disp_Tnode_Name (Base);
               Put ("[");
               Disp_Cnode (Atype.Length, Base.Index_Type);
               Put ("]");
               if Atype.Arr_El_Type /= Base.El_Type then
                  Put (" ");
                  Put_Keyword ("of");
                  Put (" ");
                  Disp_Tnode (Atype.Arr_El_Type, False);
               end if;
            end;
      end case;
   end Disp_Tnode;

   procedure Disp_Storage_Name (Storage : O_Storage) is
   begin
      case Storage is
         when O_Storage_External =>
            Put_Keyword ("external");
         when O_Storage_Public =>
            Put_Keyword ("public");
         when O_Storage_Private =>
            Put_Keyword ("private");
         when O_Storage_Local =>
            Put_Keyword ("local");
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
         Put_Keyword ("procedure");
      else
         Put_Keyword ("function");
      end if;
      Put (" ");
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
         Put_Keyword ("return");
         Put (" ");
         Disp_Tnode (Decl.Dtype, False);
      end if;
      Rem_Tab;
   end Disp_Function_Decl;

   procedure Disp_Dnode (Decl : O_Dnode) is
   begin
      case Decl.Kind is
         when ON_Type_Decl =>
            Put_Keyword ("type");
            Put (" ");
            Disp_Ident (Decl.Name);
            Put (" ");
            Put_Keyword ("is");
            Put (" ");
            if not Decl.Dtype.Uncomplete then
               Disp_Tnode (Decl.Dtype, True);
            else
               case Decl.Dtype.Kind is
                  when ON_Record_Type =>
                     Put_Keyword ("record");
                  when ON_Access_Type =>
                     Put_Keyword ("access");
                  when others =>
                     raise Program_Error;
               end case;
            end if;
            Put_Line (";");
         when ON_Completed_Type_Decl =>
            Put_Keyword ("type");
            Put (" ");
            Disp_Ident (Decl.Name);
            Put (" ");
            Put_Keyword ("is");
            Put (" ");
            Disp_Tnode (Decl.Dtype, True);
            Put_Line (";");
         when ON_Const_Decl =>
            Disp_Storage_Name (Decl.Storage);
            Put (" ");
            Put_Keyword ("constant");
            Put (" ");
            Disp_Ident (Decl.Name);
            Put (" : ");
            Disp_Tnode_Name (Decl.Dtype);
            Put_Line (";");
         when ON_Init_Value =>
            Put_Keyword ("constant");
            Put (" ");
            Disp_Ident (Decl.Name);
            Put (" := ");
            Disp_Cnode (Decl.Value, Decl.Dtype);
            Put_Line (";");
         when ON_Var_Decl =>
            Disp_Storage_Name (Decl.Storage);
            Put (" ");
            Put_Keyword ("var");
            Put (" ");
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
               Put_Keyword ("declare");
               New_Line;
               Add_Tab;
               Disp_Decls (Stmt.Decls);
               Rem_Tab;
               Put_Keyword ("begin");
               New_Line;
               Add_Tab;
               if Stmt.Stmts /= null then
                  Disp_Snode (Stmt.Stmts, null);
               end if;
               Rem_Tab;
               Put_Keyword ("end");
               Put_Line (";");
            when ON_Assign_Stmt =>
               Disp_Lnode (Stmt.Target);
               Put (" := ");
               Disp_Enode (Stmt.Value, Stmt.Target.Rtype);
               Put_Line (";");
            when ON_Return_Stmt =>
               Put_Keyword ("return");
               Put (" ");
               if Stmt.Ret_Val /= null then
                  Disp_Enode (Stmt.Ret_Val, O_Tnode_Null);
               end if;
               Put_Line (";");
            when ON_If_Stmt =>
               Add_Tab;
               Disp_Snode (Stmt.Next, Stmt.If_Last);
               Stmt := Stmt.If_Last;
               Rem_Tab;
               Put_Keyword ("end");
               Put (" ");
               Put_Keyword ("if");
               Put_Line (";");
            when ON_Elsif_Stmt =>
               Rem_Tab;
               if Stmt.Cond = null then
                  Put_Keyword ("else");
                  New_Line;
               else
                  if First = Stmt then
                     Put_Keyword ("if");
                  else
                     Put_Keyword ("elsif");
                  end if;
                  Put (" ");
                  Disp_Enode (Stmt.Cond, O_Tnode_Null);
                  Put (" ");
                  Put_Keyword ("then");
                  New_Line;
               end if;
               Add_Tab;
            when ON_Loop_Stmt =>
               Disp_Loop_Name (Stmt);
               Put_Line (":");
               Add_Tab;
               if Stmt.Loop_Last /= Stmt then
                  --  Only if the loop is not empty.
                  Disp_Snode (Stmt.Next, Stmt.Loop_Last);
               end if;
               Stmt := Stmt.Loop_Last;
               Rem_Tab;
               Put_Keyword ("end");
               Put (" ");
               Put_Keyword ("loop");
               Put_Line (";");
            when ON_Exit_Stmt =>
               Put_Keyword ("exit");
               Put (" ");
               Disp_Loop_Name (Stmt.Loop_Id);
               Put_Line (";");
            when ON_Next_Stmt =>
               Put_Keyword ("next");
               Put (" ");
               Disp_Loop_Name (Stmt.Loop_Id);
               Put_Line (";");
            when ON_Case_Stmt =>
               Put_Keyword ("case");
               Put (" ");
               Disp_Enode (Stmt.Selector, O_Tnode_Null);
               Put (" ");
               Put_Keyword ("is");
               Put_Line ("");
               Add_Tab;
               Disp_Snode (Stmt.Next, Stmt.Case_Last);
               Stmt := Stmt.Case_Last;
               Rem_Tab;
               Put_Keyword ("end");
               Put (" ");
               Put_Keyword ("case");
               Put_Line (";");
            when ON_When_Stmt =>
               declare
                  Choice: O_Choice;
                  Choice_Type : constant O_Tnode :=
                    Stmt.Branch_Parent.Selector.Rtype;
               begin
                  Rem_Tab;
                  Choice := Stmt.Choice_List;
                  Put_Keyword ("when");
                  Put (" ");
                  loop
                     case Choice.Kind is
                        when ON_Choice_Expr =>
                           Disp_Cnode (Choice.Expr, Choice_Type);
                        when ON_Choice_Range =>
                           Disp_Cnode (Choice.Low, Choice_Type);
                           Put (" ... ");
                           Disp_Cnode (Choice.High, Choice_Type);
                        when ON_Choice_Default =>
                           Put_Keyword ("default");
                     end case;
                     Choice := Choice.Next;
                     exit when Choice = null;
                     Put_Line (",");
                     Put ("     ");
                  end loop;
                  Put_Line (" =>");
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
      if N.Decl /= O_Dnode_Null then
         Disp_Ident (N.Decl.Name);
         Put (" : ");
      end if;
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
      Disp_Enode (N, O_Tnode_Null);
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
