--  Verilog disp source
--  Copyright (C) 2023 Tristan Gingold
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
with Name_Table;
with Simple_IO; use Simple_IO;
with Utils_IO; use Utils_IO;

with Verilog.Types; use Verilog.Types;
with Verilog.Flags; use Verilog.Flags;
with Verilog.Tokens; use Verilog.Tokens;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Bignums; use Verilog.Bignums;
with Verilog.Standard;
with Verilog.Disp_Tree;
with Str_Table;

package body Verilog.Disp_Verilog is
   procedure Disp (Id : Name_Id) is
   begin
      Put (Name_Table.Image (Id));
   end Disp;

   procedure Disp_Identifier (N : Node) is
   begin
      Disp (Get_Identifier (N));
   end Disp_Identifier;

   procedure Put (Tok : Token_Type) is
   begin
      Put (Image (Tok));
   end Put;

   procedure Disp_Item (Indent : Natural; Item : in out Node);
   procedure Disp_Item_Chain (Indent : Natural; Head : Node);
   procedure Disp_Statement (Indent : Natural; Stmt : Node);
   procedure Disp_Arguments (Args : Node; Has_Parenthesis : Boolean);
   procedure Disp_List_Of_Ports (Indent : Natural; Header : Node);
   procedure Disp_Data_Type (Indent : Natural; Dt : Node);
   procedure Disp_Class_Instance (N : Node);
   procedure Disp_Constraint_Expression (Indent : Natural; Item : Node);
   procedure Disp_Constraint_Block (Indent : Natural; Blk : Node);

   procedure Disp_Int32 (V : Int32)
   is
      S : constant String := Int32'Image (V);
   begin
      if S (1) = ' ' then
         Put (S (2 .. S'Last));
      else
         Put (S);
      end if;
   end Disp_Int32;

   procedure Disp_Uns32 (V : Uns32)
   is
      S : constant String := Uns32'Image (V);
   begin
      pragma Assert (S (1) = ' ');
      Put (S (2 .. S'Last));
   end Disp_Uns32;

   procedure Disp_Fp64 (V : Fp64)
   is
      S : constant String := Fp64'Image (V);
   begin
      if S (1) = ' ' then
         Put (S (2 .. S'Last));
      else
         Put (S);
      end if;
   end Disp_Fp64;

   function Is_Set (V : Uns32; Bit : Natural) return Boolean is
   begin
      return (Shift_Right (V, Bit) and 1) = 1;
   end Is_Set;

   Hexdigits : constant array (Uns32 range 0 .. 15) of Character :=
     "0123456789abcdef";

   procedure Disp_Hexa (V : Logvec_Array; Size : Width_Type)
   is
      D : Uns32;
      N : Integer;
      I : Digit_Index;
   begin
      N := Natural (Size - 1) / 4;
      N := N * 4;

      I := Digit_Index (N / Digit_Width);
      N := N mod Digit_Width;
      loop
         D := Shift_Right (V (I).Zx, N) and 16#F#;
         if D /= 0 then
            D := Shift_Right (V (I).Val, N) and 16#F#;
            if D = 15 then
               Put ('x');
            else
               Put ('z');
            end if;
         else
            D := Shift_Right (V (I).Val, N) and 16#F#;
            Put (Hexdigits (D));
         end if;
         if N = 0 then
            exit when I = 0;
            I := I - 1;
            N := Digit_Width - 4;
         else
            N := N - 4;
         end if;
      end loop;
   end Disp_Hexa;

   procedure Disp_Number (N : Node)
   is
      Size : Width_Type;
      Base : Base_Type;
   begin
      Size := Get_Number_Size (N);
      if Size /= 0 then
         Disp_Uns32 (Uns32 (Size));
      end if;
      Base := Get_Number_Base (N);
      if Size /= 0 or Base /= Base_Decimal then
         Put (''');
         if Get_Signed_Flag (N) then
            Put ('s');
         end if;
         case Base is
            when Base_Binary =>
               Put ('b');
            when Base_Octal =>
               Put ('o');
            when Base_Decimal =>
               Put ('d');
            when Base_Hexa =>
               Put ('h');
         end case;
      end if;
      if Size = 0 then
         Size := 32;
      end if;
      declare
         V : Logvec_Array (0 .. To_Last (Size));
      begin
         pragma Assert (Digit_Width = 32);
         V (0) := (Get_Number_Lo_Val (N),
                   Get_Number_Lo_Zx (N));
         if Size > 32 then
            V (1) := (Get_Number_Hi_Val (N),
                      Get_Number_Hi_Zx (N));
         end if;
         if Size > 64 then
            raise Program_Error;
         end if;

         case Base is
            when Base_Binary =>
               declare
                  N : Natural;
                  I : Digit_Index;
               begin
                  N := Natural (Size - 1);
                  I := Digit_Index (N / Digit_Width);
                  N := N mod Digit_Width;

                  loop
                     if Is_Set (V (I).Zx, N) then
                        if Is_Set (V (I).Val, N) then
                           Put ('x');
                        else
                           Put ('z');
                        end if;
                     else
                        if Is_Set (V (I).Val, N) then
                           Put ('1');
                        else
                           Put ('0');
                        end if;
                     end if;

                     if N = 0 then
                        exit when I = 0;
                        I := I - 1;
                        N := Digit_Width - 1;
                     else
                        N := N - 1;
                     end if;
                  end loop;
               end;
            when Base_Octal =>
               Put ("?");
            when Base_Decimal =>
               if V (0).Zx /= 0 then
                  if V (0).Val /= 0 then
                     Put ('x');
                  else
                     Put ('z');
                  end if;
               else
                  declare
                     Res : String (1 .. Natural (Size / 3) + 1);
                     L : Natural;
                     R : Uns32;
                  begin
                     L := Res'Last;
                     loop
                        Compute_Div_Clean
                          (To_Logvec_Ptr (V'Address), Size, 10, R);
                        Res (L) := Character'Val (48 + R);
                        exit when Is_Zero_Clean (To_Logvec_Ptr (V'Address),
                                                 Size);
                        L := L - 1;
                     end loop;
                     Put (Res (L .. Res'Last));
                  end;
               end if;
            when Base_Hexa =>
               Disp_Hexa (V, Size);
         end case;
      end;
   end Disp_Number;

   procedure Disp_Computed_Number (N : Node)
   is
      Typ : constant Node := Get_Expr_Type (N);
      Size : constant Width_Type := Get_Type_Width (Typ);
      V : Logvec_Array (0 .. 1);
   begin
      V (0) := (Get_Number_Lo_Val (N),
                Get_Number_Lo_Zx (N));
      if Size > 32 then
         V (1) := (Get_Number_Hi_Val (N),
                   Get_Number_Hi_Zx (N));
      end if;
      Disp_Hexa (V, Size);
   end Disp_Computed_Number;

   procedure Disp_End_Name (Decl : Node) is
   begin
      if Get_Has_End_Name (Decl) then
         Put (" : ");
         Disp_Identifier (Decl);
      end if;
   end Disp_End_Name;

   procedure Disp_Binary_Operator (Op : Binary_Ops) is
   begin
      Put (Verilog.Disp_Tree.Image_Binary_Ops (Op));
   end Disp_Binary_Operator;

   procedure Disp_Binary_Expression (Expr : Node) is
   begin
      if not Flag_Keep_Parentheses then
         Put ('(');
      end if;

      Disp_Expression (Get_Left (Expr));
      Put (' ');
      Disp_Binary_Operator (Get_Binary_Op (Expr));
      Put (' ');
      Disp_Expression (Get_Right (Expr));

      if not Flag_Keep_Parentheses then
         Put (')');
      end if;
   end Disp_Binary_Expression;

   procedure Disp_Unary_Expression (Expr : Node) is
   begin
      if not Flag_Keep_Parentheses then
         Put ('(');
      end if;

      case Get_Unary_Op (Expr) is
         when Unop_Plus =>
            Put ('+');
         when Unop_Minus =>
            Put ('-');
         when Unop_Red_Or =>
            Put ('|');
         when Unop_Red_Nor =>
            Put ("~|");
         when Unop_Red_And =>
            Put ('&');
         when Unop_Red_Nand =>
            Put ("~&");
         when Unop_Red_Xor =>
            Put ('^');
         when Unop_Red_Nxor =>
            Put ("~^");
         when Unop_Red_Xnor =>
            Put ("^~");
         when Unop_Bit_Neg =>
            Put ('~');
         when Unop_Logic_Neg =>
            Put ('!');
      end case;
      Disp_Expression (Get_Expression (Expr));

      if not Flag_Keep_Parentheses then
         Put (')');
      end if;
   end Disp_Unary_Expression;

   procedure Disp_Expressions (Head : Node)
   is
      Val : Node;
   begin
      Val := Head;
      loop
         Disp_Expression (Get_Expression (Val));
         Val := Get_Chain (Val);
         exit when Val = Null_Node;
         Put (", ");
      end loop;
   end Disp_Expressions;

   procedure Disp_Concatenation (Expr : Node)
   is
      Rep : Node;
   begin
      Put ('{');
      Rep := Get_Replication (Expr);
      if Rep /= Null_Node then
         Disp_Expression (Rep);
         Put (" {");
      end if;
      Disp_Expressions (Get_Expressions (Expr));
      if Rep /= Null_Node then
         Put ('}');
      end if;
      Put ('}');
   end Disp_Concatenation;

   procedure Disp_Streaming_Concatenation (Expr : Node)
   is
      Kind : constant Nkinds_Streaming := Get_Kind (Expr);
   begin
      Put ('{');
      case Kind is
         when N_Left_Streaming_Expr
           | N_Left_Streaming_Type =>
            Put ("<<");
         when N_Right_Streaming_Expr
           | N_Right_Streaming_Type =>
            Put (">>");
      end case;
      Put (' ');
      case Kind is
         when N_Left_Streaming_Expr
           | N_Right_Streaming_Expr =>
            declare
               Sz : constant Node := Get_Expression (Expr);
            begin
               if Sz /= Null_Node then
                  Disp_Expression (Sz);
                  Put (' ');
               end if;
            end;
         when N_Left_Streaming_Type
           | N_Right_Streaming_Type =>
            declare
               Sz : constant Node := Get_Slice_Size_Type (Expr);
            begin
               Disp_Data_Type (0, Sz);
               Put (' ');
            end;
      end case;
      Put ('{');
      Disp_Expressions (Get_Expressions (Expr));
      Put ('}');
      Put ('}');
   end Disp_Streaming_Concatenation;

   procedure Disp_Pattern_Inner (Expr : Node)
   is
      Key : Node;
      Val : Node;
   begin
      Val := Get_Elements (Expr);
      loop
         Key := Get_Pattern_Key (Val);
         if Key /= Null_Node then
            Disp_Expression (Key);
            Put (": ");
         end if;
         Disp_Expression (Get_Expression (Val));
         Val := Get_Chain (Val);
         exit when Val = Null_Node;
         Put (", ");
      end loop;
   end Disp_Pattern_Inner;

   procedure Disp_Pattern (Expr : Node)
   is
      Rep : Node;
   begin
      Put ("'{");
      Rep := Get_Replication (Expr);
      if Rep /= Null_Node then
         Disp_Expression (Rep);
         Put (" {");
      end if;
      Disp_Pattern_Inner (Expr);
      if Rep /= Null_Node then
         Put ('}');
      end if;
      Put ('}');
   end Disp_Pattern;

   procedure Disp_Pattern_Cst (Expr : Node) is
   begin
      Put ("'{");
      Disp_Int32 (Get_Replication_Cst (Expr));
      Put (" {");
      Disp_Pattern_Inner (Expr);
      Put ("}}");
   end Disp_Pattern_Cst;

   procedure Disp_Implicit_Cast (Expr : Node; Name : String)
   is
      Expr_Type : Node;
   begin
      if Flag_Disp_Implicit_Cast then
         Expr_Type := Get_Expr_Type (Expr);
         Put ('`');
         Put (Name);
         Put ('#');
         if Get_Signed_Flag (Expr_Type) then
            Put ('s');
         else
            Put ('u');
         end if;
         Disp_Uns32 (Uns32 (Get_Type_Width (Expr_Type)));
         Put ('(');
      end if;
      Disp_Expression (Get_Expression (Expr));
      if Flag_Disp_Implicit_Cast then
         Put (')');
      end if;
   end Disp_Implicit_Cast;

   procedure Disp_Time_Literal (Expr : Node) is
   begin
      Disp_Fp64 (Get_Real_Number (Expr));
      case Get_Time_Unit (Expr) is
         when -15 =>
            Put ("fs");
         when -12 =>
            Put ("ps");
         when -9 =>
            Put ("ns");
         when -6 =>
            Put ("us");
         when -3 =>
            Put ("ms");
         when 0 =>
            Put ("s");
         when others =>
            Put ("??");
            Disp_Int32 (Get_Time_Unit (Expr));
            raise Internal_Error;
      end case;
   end Disp_Time_Literal;

   procedure Disp_Range (L : Node; R : Node; Op : Token_Type) is
   begin
      Put ('[');
      Disp_Expression (L);
      Put (Op);
      Disp_Expression (R);
      Put (']');
   end Disp_Range;

   procedure Disp_Range (Expr : Node; Op : Token_Type) is
   begin
      Disp_Range (Get_Msb (Expr), Get_Lsb (Expr), Op);
   end Disp_Range;

   procedure Disp_Range_Cst (Rng : Node) is
   begin
      Put ('[');
      Disp_Int32 (Get_Msb_Cst (Rng));
      Put (':');
      Disp_Int32 (Get_Lsb_Cst (Rng));
      Put (']');
   end Disp_Range_Cst;

   procedure Disp_Indexed_Range_Cst (Expr : Node; Op : Token_Type) is
   begin
      Put ('[');
      Disp_Expression (Get_Base_Expr (Expr));
      Put (Op);
      Disp_Int32 (Get_Width_Cst (Expr));
      Put (']');
   end Disp_Indexed_Range_Cst;

   procedure Disp_Expression (Expr : Node) is
   begin
      case Get_Kind (Expr) is
         when N_Number =>
            Disp_Number (Expr);
         when N_Computed_Number =>
            Disp_Computed_Number (Expr);
         when N_Real_Number =>
            Put (Fp64'Image (Get_Real_Number (Expr)));
         when N_Unbased_Literal =>
            if Get_Number_Lo_Zx (Expr) = 0 then
               if Get_Number_Lo_Val (Expr) = 0 then
                  Put ("'0");
               else
                  Put ("'1");
               end if;
            else
               if Get_Number_Lo_Val (Expr) = 0 then
                  Put ("'z");
               else
                  Put ("'x");
               end if;
            end if;
         when N_Time_Literal =>
            Disp_Time_Literal (Expr);
         when N_String_Literal =>
            declare
               use Str_Table;
               Id : constant String8_Id := Get_String_Id (Expr);
               C : Character;
            begin
               Put ('"');
               for I in 1 .. Nat32 (Get_String_Size (Expr)) loop
                  C := Char_String8 (Id, I);
                  case C is
                     when '"' =>
                        Put ("\""");
                     when '\' =>
                        Put ("\\");
                     when ASCII.HT =>
                        Put ("\t");
                     when ASCII.LF =>
                        Put ("\n");
                     when ' ' | '!'
                       | '#' .. '['
                       | ']' .. '~' =>
                        Put (C);
                     when others =>
                        Put ("\xxx");
                  end case;
               end loop;
               Put ('"');
            end;
         when N_Mintypmax =>
            Disp_Expression (Get_Min_Expr (Expr));
            Put (':');
            Disp_Expression (Get_Typ_Expr (Expr));
            Put (':');
            Disp_Expression (Get_Max_Expr (Expr));
         when N_This =>
            Put ("this");
         when N_Super =>
            Put ("super");
         when N_Null =>
            Put ("null");
         when N_Infinity =>
            Put ("$");
         when N_Default =>
            Put ("default");
         when N_Parenthesis_Expr =>
            Put ('(');
            Disp_Expression (Get_Expression (Expr));
            Put (')');

         when N_Name
           | N_This_Name =>
            Disp_Identifier (Expr);
         when N_Class_Instance =>
            Disp_Class_Instance (Expr);
         when N_Hierarchical
           | N_Dotted_Name
           | N_Member_Name
           | N_Interface_Item
           | N_Modport_Item =>
            Disp_Expression (Get_Name (Expr));
            Put ('.');
            Disp_Identifier (Expr);
         when N_Property_Name
           | N_Method_Name =>
            declare
               Name : constant Node := Get_Name (Expr);
            begin
               if Name /= Null_Node
                 and then Get_Kind (Name) /= N_This_Var
               then
                  Disp_Expression (Name);
                  Put ('.');
               end if;
               Disp_Identifier (Expr);
            end;
         when N_Scoped_Name =>
            Disp_Expression (Get_Name (Expr));
            Put ("::");
            Disp_Identifier (Expr);
         when N_Wildcard_Name =>
            Disp_Expression (Get_Name (Expr));
            Put ("::");
            Put ("*");
         when N_Bit_Select
           | N_Indexed_Name
           | N_String_Index =>
            Disp_Expression (Get_Name (Expr));
            Put ('[');
            Disp_Expression (Get_Expression (Expr));
            Put (']');
         when N_Part_Select =>
            Disp_Expression (Get_Name (Expr));
            Disp_Range (Expr, Tok_Colon);
         when N_Part_Select_Cst =>
            Disp_Expression (Get_Name (Expr));
            Disp_Range_Cst (Expr);
         when N_Plus_Part_Select =>
            Disp_Expression (Get_Name (Expr));
            Disp_Range (Get_Base_Expr (Expr), Get_Width_Expr (Expr),
                        Tok_Plus_Colon);
         when N_Minus_Part_Select =>
            Disp_Expression (Get_Name (Expr));
            Disp_Range (Get_Base_Expr (Expr), Get_Width_Expr (Expr),
                        Tok_Minus_Colon);
         when N_Plus_Part_Select_Cst =>
            Disp_Expression (Get_Name (Expr));
            Disp_Indexed_Range_Cst (Expr, Tok_Plus_Colon);
         when N_Minus_Part_Select_Cst =>
            Disp_Expression (Get_Name (Expr));
            Disp_Indexed_Range_Cst (Expr, Tok_Minus_Colon);
         when N_Value_Range =>
            Disp_Range (Expr, Tok_Colon);

         when N_Or =>
            Disp_Expression (Get_Left (Expr));
            Put (" or ");
            Disp_Expression (Get_Right (Expr));
         when N_Posedge =>
            Put ("posedge ");
            Disp_Expression (Get_Expression (Expr));
         when N_Negedge =>
            Put ("negedge ");
            Disp_Expression (Get_Expression (Expr));

         when N_Cond_Op =>
            if not Flag_Keep_Parentheses then
               Put ('(');
            end if;
            Disp_Expression (Get_Condition (Expr));
            Put (" ? ");
            Disp_Expression (Get_Cond_True (Expr));
            Put (" : ");
            Disp_Expression (Get_Cond_False (Expr));
            if not Flag_Keep_Parentheses then
               Put (')');
            end if;

         when N_Concatenation =>
            Disp_Concatenation (Expr);
         when N_Replication_Cst =>
            Put ('{');
            Disp_Int32 (Get_Replication_Cst (Expr));
            Put (" {");
            Disp_Expressions (Get_Expressions (Expr));
            Put ('}');
            Put ('}');
         when Nkinds_Streaming =>
            Disp_Streaming_Concatenation (Expr);
         when N_Aggregate_Literal =>
            Disp_Pattern (Expr);
         when N_Aggregate_Literal_Cst =>
            Disp_Pattern_Cst (Expr);
         when N_Unary_Op =>
            Disp_Unary_Expression (Expr);
         when N_Binary_Op
           | N_Short_Circuit_Op =>
            Disp_Binary_Expression (Expr);

         when N_Type_Cast =>
            Disp_Data_Type (0, Get_Cast_Data_Type (Expr));
            Put ("'(");
            Disp_Expression (Get_Expression (Expr));
            Put (')');
         when N_Bits_Type =>
            Put ("$bits(");
            Disp_Data_Type (0, Get_Expr_Type (Expr));
            Put (')');
         when N_Bits_Expr =>
            Put ("$bits(");
            Disp_Expression (Get_Expression (Expr));
            Put (')');
         when N_Call =>
            Disp_Expression (Get_Subroutine (Expr));
            Disp_Arguments (Get_Arguments (Expr), Get_Has_Parenthesis (Expr));
         when N_Randomize_Call =>
            Disp_Expression (Get_Subroutine (Expr));
            Disp_Arguments (Get_Arguments (Expr), Get_Has_Parenthesis (Expr));
            Put (" with ");
            Disp_Constraint_Block
              (1, Get_Constraint_Block_Chain (Expr));
         when N_Array_Method_Call =>
            declare
               Arg : constant Node := Get_Iterator_Argument (Expr);
            begin
               Disp_Expression (Get_Subroutine (Expr));
               if Arg /= Null_Node then
                  Put ('(');
                  Disp_Identifier (Arg);
                  Put (')');
               end if;
               Put (" with (");
               Disp_Expression (Get_With_Expression (Expr));
               Put (")");
            end;
         when N_System_Call =>
            Put ('$');
            Disp_Identifier (Expr);
            Disp_Arguments (Get_Arguments (Expr), Get_Has_Parenthesis (Expr));
         when N_New_Call =>
            Put ("new");
            Disp_Arguments (Get_Arguments (Expr), Get_Has_Parenthesis (Expr));
         when N_Dynamic_Array_New =>
            Put ("new");
            Put ('[');
            Disp_Expression (Get_Size_Expression (Expr));
            Put (']');
            declare
               Init : constant Node := Get_Init_Expression (Expr);
            begin
               if Init /= Null_Node then
                  Put ('(');
                  Disp_Expression (Init);
                  Put (')');
               end if;
            end;

         when N_Conversion =>
            Disp_Implicit_Cast (Expr, "conv");

         when N_Post_Increment =>
            Disp_Expression (Get_Lvalue (Expr));
            Put ("++");
         when N_Pre_Increment =>
            Put ("++");
            Disp_Expression (Get_Lvalue (Expr));
         when N_Pre_Decrement =>
            Put ("--");
            Disp_Expression (Get_Lvalue (Expr));
         when N_Post_Decrement =>
            Disp_Expression (Get_Lvalue (Expr));
            Put ("--");

         when N_Assign_Operator =>
            Disp_Expression (Get_Lvalue (Expr));
            Put (' ');
            Disp_Binary_Operator (Get_Binary_Op (Expr));
            Put ('=');
            Put (' ');
            Disp_Expression (Get_Expression (Expr));

         when N_Membership =>
            Disp_Expression (Get_Expression (Expr));
            Put (' ');
            Put ("inside");
            Put (' ');
            Put ('{');
            Disp_Expressions (Get_Expressions (Expr));
            Put ('}');

         when N_Module
           | N_Program_Declaration
           | N_Var
           | N_Parameter
           | Nkinds_Nets
           | Nkinds_Net_Port
           | Nkinds_Tf_Port =>
            Disp_Identifier (Expr);

         when others =>
            Error_Kind ("disp_expression", Expr);
      end case;
   end Disp_Expression;

   procedure Disp_Range_Expr (Rng : Node)
   is
      Lsb : constant Node := Get_Lsb (Rng);
   begin
      Put ('[');
      Disp_Expression (Get_Msb (Rng));
      if Lsb /= Null_Node then
         Put (':');
         Disp_Expression (Lsb);
      end if;
      Put (']');
   end Disp_Range_Expr;

--     function Get_No_Array_Type (Atype : Node) return Node
--     is
--        Res : Node;
--     begin
--        Res := Atype;
--        while Get_Kind (Res) = N_Array loop
--           Res := Get_Element_Type (Res);
--        end loop;
--        return Res;
--     end Get_No_Array_Type;

   procedure Disp_Variable_Dimensions (Dt : Node)
   is
      El : Node;
   begin
      --  Array.
      El := Dt;
      loop
         case Get_Kind (El) is
            when N_Logic_Type
              | N_Bit_Type
              | N_Real_Type
              | N_Shortreal_Type
              | N_Void_Type
              | N_Typedef
              | N_Predefined_Typedef
              | N_Event_Type
              | N_String_Type
              | N_Chandle_Type
              | N_Enum_Type
              | N_Name
              | N_Scoped_Name
              | N_Class
              | N_Interface_Declaration
              | N_Modport
              | N_Class_Instance
              | N_Struct_Type
              | N_Packed_Struct_Type
              | N_Dotted_Name =>
               exit;
            when N_Log_Packed_Array_Cst
              | N_Bit_Packed_Array_Cst
              | N_Packed_Array =>
               exit;
            when N_Array =>
               Disp_Range_Expr (El);
               El := Get_Element_Data_Type (El);
            when N_Array_Cst =>
               Disp_Range_Cst (El);
               El := Get_Type_Element_Type (El);
            when N_Queue =>
               declare
                  Sz : constant Node := Get_Maximum_Size_Expr (El);
               begin
                  Put ("[$");
                  if Sz /= Null_Node then
                     Disp_Expression (Sz);
                  end if;
                  Put ("]");
               end;
               El := Get_Element_Data_Type (El);
            when N_Dynamic_Array =>
               Put ("[]");
               El := Get_Element_Data_Type (El);
            when N_Associative_Array =>
               declare
                  Index : constant Node := Get_Index_Data_Type (El);
               begin
                  Put ('[');
                  if Index = Null_Node then
                     Put ('*');
                  else
                     Disp_Data_Type (0, Index);
                  end if;
                  Put (']');
               end;
               El := Get_Element_Data_Type (El);
            when others =>
               Error_Kind ("disp_variable_dimensions", El);
         end case;
      end loop;
   end Disp_Variable_Dimensions;

   procedure Disp_Decl_Data_Type
     (Indent : Natural; Decl : Node; Is_First : Boolean := True)
   is
      --  Note: type can be null_node for parameters, new function...
      Dt : constant Node := Get_Data_Type (Decl);
   begin
      if Is_First
        and then Dt /= Null_Node
        and then (Dt /= Standard.Implicit_Typedef
                    or else Flag_Disp_Implicit_Type)
      then
         Disp_Data_Type (Indent, Dt);
         Put (' ');
      end if;

      Disp_Identifier (Decl);

      --  Array.
      if Dt /= Null_Node then
         Disp_Variable_Dimensions (Dt);
      end if;
   end Disp_Decl_Data_Type;

   procedure Disp_Default_Value (Expr : Node) is
   begin
      if Expr /= Null_Node then
         Put (" = ");
         Disp_Expression (Expr);
      end if;
   end Disp_Default_Value;

   procedure Disp_Enum_Type (Enum : Node)
   is
      Lit : Node;
      Base : Node;
   begin
      Put ("enum");
      Put (' ');
      Base := Get_Enum_Base_Data_Type (Enum);
      if Base /= Null_Node then
         Disp_Data_Type (0, Base);
         Put (' ');
      end if;
      Put ('{');
      Lit := Get_Enum_Names (Enum);
      loop
         Disp (Get_Identifier (Lit));
         Disp_Default_Value (Get_Expression (Lit));
         Lit := Get_Chain (Lit);
         exit when Lit = Null_Node;
         Put (", ");
      end loop;
      Put ('}');
   end Disp_Enum_Type;

   procedure Disp_Members (Indent : Natural; Head : Node)
   is
      Mem : Node;
   begin
      Mem := Head;
      Put_Line ("{");
      loop
         Put_Indent (Indent + 1);
         Disp_Decl_Data_Type (Indent, Mem);
         Disp_Default_Value (Get_Expression (Mem));
         Mem := Get_Chain (Mem);
         Put_Line (";");
         exit when Mem = Null_Node;
      end loop;
      Put_Indent (Indent);
      Put ('}');
   end Disp_Members;

   procedure Disp_Struct_Type (Indent : Natural; Stype : Node) is
   begin
      Put ("struct");
      Put (' ');
      Disp_Members (Indent, Get_Members (Stype));
   end Disp_Struct_Type;

   procedure Disp_Sign (N : Node) is
   begin
      if Get_Signed_Flag (N) then
         Put ("signed");
      else
         Put ("unsigned");
      end if;
   end Disp_Sign;

   procedure Disp_Packed_Struct_Type (Indent : Natural; Stype : Node) is
   begin
      Put ("struct");
      Put (' ');
      Put ("packed");
      Put (' ');
      if Get_Has_Sign (Stype) then
         Disp_Sign (Stype);
         Put (' ');
      end if;
      Disp_Members (Indent, Get_Members (Stype));
   end Disp_Packed_Struct_Type;

   procedure Disp_Parameter_Values (Chain : Node)
   is
      Param : Node;
      Id : Name_Id;
   begin
      if Chain = Null_Node then
         return;
      end if;
      Put (' ');
      Put ('#');
      Put ('(');
      Param := Chain;
      while Param /= Null_Node loop
         Id := Get_Identifier (Param);
         if Id /= Null_Identifier then
            Put ('.');
            Disp (Id);
            Put ('(');
         end if;

         case Get_Kind (Param) is
            when N_Parameter_Value_Expr =>
               Disp_Expression (Get_Expression (Param));
            when N_Parameter_Value_Type =>
               Disp_Data_Type (0, Get_Data_Type (Param));
            when others =>
               raise Internal_Error;
         end case;

         if Id /= Null_Identifier then
            Put (')');
         end if;

         Param := Get_Chain (Param);
         if Param /= Null_Node then
            Put (',');
            Put (' ');
         end if;
      end loop;
      Put (')');
   end Disp_Parameter_Values;

   procedure Disp_Class_Instance (N : Node) is
   begin
      Disp_Expression (Get_Class_Name (N));
      Disp_Parameter_Values (Get_Parameter_Values (N));
   end Disp_Class_Instance;

   procedure Disp_Data_Type (Indent : Natural; Dt : Node)
   is
      El : Node;
   begin
      if Dt = Null_Node then
         --  In case of error...
         return;
      end if;

      --  Element type.
      El := Dt;
      loop
         case Get_Kind (El) is
            when N_Logic_Type =>
               Put ("logic");
               if Get_Signed_Flag (El) then
                  Put (" signed");
               end if;
               exit;
            when N_Bit_Type =>
               Put ("bit");
               if Get_Signed_Flag (El) then
                  Put (" signed");
               end if;
               exit;
            when N_String_Type =>
               Put ("string");
               exit;
            when N_Event_Type =>
               Put ("event");
               exit;
            when N_Chandle_Type =>
               Put ("chandle");
               exit;
            when N_Real_Type =>
               Put ("real");
               exit;
            when N_Shortreal_Type =>
               Put ("shortreal");
               exit;
            when N_Void_Type =>
               Put ("void");
               exit;
            when N_Predefined_Typedef
              | N_Typedef =>
               if Flag_Disp_Implicit_Type then
                  if El = Standard.Implicit_Typedef then
                     Put ("logic");
                     exit;
                  elsif El = Standard.Implicit_Unsigned_Typedef then
                     Put ("logic unsigned");
                     exit;
                  elsif El = Standard.Implicit_Signed_Typedef then
                     Put ("logic signed");
                     exit;
                  end if;
               end if;
               declare
                  Id : constant Name_Id := Get_Identifier (El);
               begin
                  if Id /= No_Name_Id then
                     Disp (Id);
                  end if;
               end;
               exit;
            when N_Name =>
               Disp_Identifier (El);
               exit;
            when N_Class
              | N_Interface_Declaration =>
               Disp_Identifier (El);
               exit;
            when N_Modport =>
               Disp_Identifier (Get_Parent (El));
               Put ('.');
               Disp_Identifier (El);
               exit;
            when N_Class_Instance =>
               Disp_Class_Instance (El);
               exit;
            when N_Scoped_Name =>
               Disp_Expression (El);
               exit;
            when N_Dotted_Name =>
               Disp_Expression (El);
               exit;
            when N_Struct_Type =>
               Disp_Struct_Type (Indent, El);
               exit;
            when N_Packed_Struct_Type =>
               Disp_Packed_Struct_Type (Indent, El);
               exit;
            when N_Log_Packed_Array_Cst
              | N_Bit_Packed_Array_Cst
              | N_Array_Cst =>
               El := Get_Type_Element_Type (El);
            when N_Packed_Array
              | N_Array
              | N_Dynamic_Array
              | N_Queue
              | N_Associative_Array =>
               El := Get_Element_Data_Type (El);
            when N_Enum_Type =>
               Disp_Enum_Type (El);
               exit;
            when others =>
               Error_Kind ("disp_data_type", El);
         end case;
      end loop;

      --  Packed array and identifier
      El := Dt;
      loop
         case Get_Kind (El) is
            when N_Logic_Type
              | N_Bit_Type
              | N_Real_Type
              | N_Shortreal_Type
              | N_Void_Type
              | N_Typedef
              | N_Predefined_Typedef
              | N_String_Type
              | N_Chandle_Type
              | N_Event_Type
              | N_Enum_Type
              | N_Name
              | N_Scoped_Name
              | N_Class
              | N_Interface_Declaration
              | N_Modport
              | N_Class_Instance
              | N_Struct_Type
              | N_Packed_Struct_Type
              | N_Dotted_Name =>
               exit;
            when N_Log_Packed_Array_Cst
              | N_Bit_Packed_Array_Cst =>
               if Get_Signed_Flag (El) then
                  Put (" signed");
               end if;
               Disp_Range_Cst (El);
               El := Get_Type_Element_Type (El);
            when N_Packed_Array =>
               if Get_Has_Sign (El) then
                  if Get_Signed_Flag (El) then
                     Put (" signed");
                  else
                     Put (" unsigned");
                  end if;
               end if;
               Disp_Range_Expr (El);
               El := Get_Element_Data_Type (El);
            when N_Array
              | N_Dynamic_Array
              | N_Queue
              | N_Associative_Array =>
               El := Get_Element_Data_Type (El);
            when N_Array_Cst =>
               El := Get_Type_Element_Type (El);
            when others =>
               Error_Kind ("disp_data_type(2)", El);
         end case;
      end loop;
   end Disp_Data_Type;

   procedure Disp_Net_Type (Kind : Nkinds_Nets) is
   begin
      case Kind is
         when N_Wire
           | N_Wire_Direct =>
            Put ("wire");
         when N_Wand =>
            Put ("wand");
         when N_Wor =>
            Put ("wor");
         when N_Tri =>
            Put ("tri");
         when N_Tri0 =>
            Put ("tri0");
         when N_Tri1 =>
            Put ("tri1");
         when N_Triand =>
            Put ("triand");
         when N_Trior =>
            Put ("trior");
         when N_Trireg =>
            Put ("trireg");
         when N_Supply0 =>
            Put ("supply0");
         when N_Supply1 =>
            Put ("supply1");
         when N_Uwire =>
            Put ("uwire");
      end case;
   end Disp_Net_Type;

   procedure Disp_One_Net_Declaration (Indent : Natural; Net : Node) is
   begin
      Disp_Decl_Data_Type (Indent, Net, True);

      Disp_Default_Value (Get_Expression (Net));
   end Disp_One_Net_Declaration;

   procedure Disp_Net_Declaration (Indent : Natural; Net : in out Node)
   is
      Is_First : Boolean;
   begin
      Disp_Net_Type (Get_Kind (Net));
      Put (' ');

      Is_First := True;
      loop
         Disp_Decl_Data_Type (Indent, Net, Is_First);

         Disp_Default_Value (Get_Expression (Net));

         exit when not Get_Has_Identifier_List (Net);
         Net := Get_Chain (Net);
         Is_First := False;
         Put (',');
         Put (' ');
      end loop;
      Net := Get_Chain (Net);
   end Disp_Net_Declaration;

   procedure Disp_Port_Direction (Port : Node) is
   begin
      case Get_Kind (Port) is
         when N_Inout | N_Tf_Inout =>
            Put ("inout");
         when N_Input | N_Tf_Input =>
            Put ("input");
         when N_Output | N_Tf_Output =>
            Put ("output");
         when N_Tf_Ref =>
            Put ("ref");
         when N_Tf_Const_Ref =>
            Put ("const ref");
         when others =>
            Error_Kind ("disp_port_direction", Port);
      end case;
      Put (' ');
   end Disp_Port_Direction;

   procedure Disp_Lifetime (Decl : Node) is
   begin
      if Get_Has_Lifetime (Decl) then
         case Get_Lifetime (Decl) is
            when Life_Static =>
               Put ("static ");
            when Life_Automatic =>
               Put ("automatic ");
         end case;
      end if;
   end Disp_Lifetime;

   procedure Disp_Visibility (Decl : Node) is
   begin
      if Get_Has_Visibility (Decl) then
         case Get_Visibility (Decl) is
            when Visibility_None =>
               null;
            when Visibility_Public =>
               null;
            when Visibility_Protected =>
               Put ("protected ");
            when Visibility_Local =>
               Put ("local ");
         end case;
      end if;
   end Disp_Visibility;

   procedure Disp_Static_Flag (Decl : Node) is
   begin
      if Get_Static_Flag (Decl) then
         Put ("static ");
      end if;
   end Disp_Static_Flag;

   procedure Disp_Random_Flags (Decl : Node) is
   begin
      if Get_Random_Flag (Decl) then
         if Get_Randc_Flag (Decl) then
            Put ("randc ");
         else
            Put ("rand ");
         end if;
      end if;
   end Disp_Random_Flags;

   procedure Disp_Variable_Declarations
     (Indent : Natural; Decl : in out Node) is
   begin
      Disp_Lifetime (Decl);
      Disp_Visibility (Decl);
      Disp_Static_Flag (Decl);
      Disp_Random_Flags (Decl);

      if Get_Is_Const (Decl) then
         Put ("const ");
      end if;

      if Get_Has_Var (Decl) then
         Put ("var ");
      end if;

      Disp_Decl_Data_Type (Indent, Decl);
      Disp_Default_Value (Get_Expression (Decl));

      while Get_Has_Identifier_List (Decl) loop
         Put (',');
         Put (' ');
         Decl := Get_Chain (Decl);
         Disp_Decl_Data_Type (Indent, Decl, False);
         Disp_Default_Value (Get_Expression (Decl));
      end loop;
      Decl := Get_Chain (Decl);
   end Disp_Variable_Declarations;

   procedure Disp_Port_Declaration (Indent : Natural; Port : in out Node)
   is
      Decl : Node;
   begin
      case Get_Kind (Port) is
         when Nkinds_Net_Port =>
            if Get_Has_Direction (Port) or else Flag_Disp_Port_Omitted then
               Disp_Port_Direction (Port);
            end if;

            if Get_Complete_Flag (Port) or else Flag_Disp_Port_Omitted then
               Decl := Get_Redeclaration (Port);

               case Get_Kind (Decl) is
                  when N_Var =>
                     if Get_Has_Var (Decl) then
                        Put ("var");
                        Put (' ');
                     end if;
                  when Nkinds_Nets =>
                     Disp_Net_Type (Get_Kind (Decl));
                     Put (' ');
                  when others =>
                     Error_Kind ("disp_port_declaration", Decl);
               end case;
            end if;

         when N_Interface_Port
           | N_Modport_Port =>
            null;

         when others =>
            Error_Kind ("disp_port_declaration", Port);
      end case;

      Disp_Decl_Data_Type (Indent, Port);
      while Get_Has_Identifier_List (Port) loop
         Put (',');
         Put (' ');
         Port := Get_Chain (Port);
         Disp_Decl_Data_Type (Indent, Port, False);
      end loop;

      Port := Get_Chain (Port);
   end Disp_Port_Declaration;

   procedure Disp_Port_Declaration_Semicolon
     (Indent : Natural; Port : in out Node) is
   begin
      Disp_Port_Declaration (Indent, Port);
      Put_Line (";");
   end Disp_Port_Declaration_Semicolon;

   procedure Disp_Parameter_Port_List (Indent : Natural; Params : Node)
   is
      Param : Node;
      Def : Node;
   begin
      if Params = Null_Node then
         return;
      end if;
      Put (' ');
      Put_Line ("#(");
      Param := Params;
      loop
         Put_Indent (Indent + 1);
         case Get_Kind (Param) is
            when N_Type_Parameter =>
               if Get_Has_Type (Param) then
                  Put ("type ");
               end if;
               Disp_Identifier (Param);
               Def := Get_Default_Type (Param);
               if Def /= Null_Node then
                  Put (" = ");
                  Disp_Data_Type (Indent, Def);
               end if;
            when N_Parameter =>
               Disp_Decl_Data_Type (Indent, Param);
               Disp_Default_Value (Get_Expression (Param));
            when others =>
               Error_Kind ("disp_parameter_port_list", Param);
         end case;
         Param := Get_Chain (Param);
         exit when Param = Null_Node;
         Put_Line (",");
      end loop;
      Put (')');
   end Disp_Parameter_Port_List;

   procedure Disp_Parameter_Declaration (Indent : Natural; Param : Node)
   is
      Expr : Node;
   begin
      case Get_Kind (Param) is
         when N_Parameter =>
            Put ("parameter");
            Expr := Get_Parameter_Expression (Param);
         when N_Localparam =>
            Put ("localparam");
            Expr := Null_Node;
         when others =>
            raise Internal_Error;
      end case;
      Put (" ");
      Disp_Decl_Data_Type (Indent, Param);
      Put (" = ");
      if Expr = Null_Node then
         Expr := Get_Expression (Param);
      end if;
      if Expr /= Null_Node then
         --  For inserted localparam in generic.
         Disp_Expression (Expr);
      end if;
      Put (';');
      New_Line;
   end Disp_Parameter_Declaration;

   procedure Disp_Discipline_Declaration (Indent : Natural; N : Node)
   is
      Item : Node;
   begin
      Put ("discipline");
      Put (' ');
      Disp_Identifier (N);
      Put (';');
      New_Line;
      Item := Get_Discipline_Items (N);
      while Item /= Null_Node loop
         Put_Indent (Indent + 1);
         case Get_Kind (Item) is
            when N_Discipline_Domain =>
               Put ("domain");
               Put (' ');
               if Get_Continuous_Flag (Item) then
                  Put ("continuous");
               else
                  Put ("discrete");
               end if;
            when N_Discipline_Potential =>
               Put ("potential");
               Put (' ');
               Disp_Identifier (Get_Nature (Item));
            when N_Discipline_Flow =>
               Put ("flow");
               Put (' ');
               Disp_Identifier (Get_Nature (Item));
            when others =>
               Error_Kind ("disp_discipline_declaration", Item);
         end case;
         Put (';');
         New_Line;
         Item := Get_Chain (Item);
      end loop;
      Put_Indent (Indent);
      Put ("enddiscipline");
      New_Line;
   end Disp_Discipline_Declaration;

   procedure Disp_Nature_Declaration (Indent : Natural; N : Node)
   is
      Item : Node;
   begin
      Put ("nature");
      Put (' ');
      Disp_Identifier (N);
      Put (';');
      New_Line;
      Item := Get_Nature_Items (N);
      while Item /= Null_Node loop
         Put_Indent (Indent + 1);
         case Get_Kind (Item) is
            when N_Nature_Attribute =>
               Disp_Identifier (Item);
               Put (' ');
               Put ('=');
               Put (' ');
               Disp_Expression (Get_Expression (Item));
            when N_Nature_Access =>
               Put ("access");
               Put (' ');
               Put ('=');
               Put (' ');
               Disp_Identifier (Item);
            when others =>
               Error_Kind ("disp_nature_declaration", Item);
         end case;
         Put (';');
         New_Line;
         Item := Get_Chain (Item);
      end loop;
      Put_Indent (Indent);
      Put ("endnature");
      New_Line;
   end Disp_Nature_Declaration;

   procedure Disp_Contribution (Stmt : Node)  is
   begin
      Disp_Expression (Get_Lvalue (Stmt));
      Put (" <+ ");
      Disp_Expression (Get_Expression (Stmt));
      Put (';');
      New_Line;
   end Disp_Contribution;

   procedure Disp_Analog (Indent : Natural; Stmt : Node)
   is
      Item : Node;
   begin
      Put ("analog");
      New_Line;
      Item := Get_Statement (Stmt);
      Disp_Item (Indent + 1, Item);
   end Disp_Analog;

   procedure Disp_Statement_Chain (Indent : Natural; Stmt : Node) is
   begin
      Disp_Item_Chain (Indent + 1, Stmt);
   end Disp_Statement_Chain;

   procedure Disp_Seq_Block (Indent : Natural; Blk : Node)
   is
      Id : Name_Id;
   begin
      Put ("begin");
      Id := Get_Identifier (Blk);
      if Id /= Null_Identifier then
         Put (": ");
         Disp (Id);
      end if;
      New_Line;
      Disp_Item_Chain (Indent + 1,
                       Get_Block_Item_Declaration_Chain (Blk));
      Disp_Item_Chain (Indent + 1, Get_Statements_Chain (Blk));
      Put_Indent (Indent);
      Put_Line ("end");
   end Disp_Seq_Block;

   procedure Disp_Par_Block (Indent : Natural; Blk : Node)
   is
      Id : Name_Id;
   begin
      Put ("fork");
      Id := Get_Identifier (Blk);
      if Id /= Null_Identifier then
         Put (": ");
         Disp (Id);
      end if;
      New_Line;
      Disp_Item_Chain (Indent + 1,
                       Get_Block_Item_Declaration_Chain (Blk));
      Disp_Item_Chain (Indent + 1, Get_Statements_Chain (Blk));
      Put_Indent (Indent);

      case Get_Join_Option (Blk) is
         when Join_All =>
            Put ("join");
         when Join_Any =>
            Put ("join_any");
         when Join_None =>
            Put ("join_none");
      end case;
      Disp_End_Name (Blk);
      New_Line;
   end Disp_Par_Block;

   procedure Disp_If_Header (Stmt : Node) is
   begin
      Put ("if (");
      Disp_Expression (Get_Condition (Stmt));
      Put (")");
   end Disp_If_Header;

   procedure Disp_If (Indent : Natural; Stmt : Node)
   is
      E : Node;
   begin
      Disp_If_Header (Stmt);
      New_Line;

      Disp_Statement_Chain (Indent, Get_True_Stmt (Stmt));
      E := Get_False_Stmt (Stmt);
      if E /= Null_Node then
         Put_Indent (Indent);
         Put ("else");
         if Get_Kind (E) = N_If then
            --  Do not indent for 'else if'.
            Put (' ');
            Disp_Statement_Chain (Indent, E);
         else
            New_Line;
            Disp_Statement_Chain (Indent, E);
         end if;
      end if;
   end Disp_If;

   procedure Disp_Case_Header (Stmt : Node) is
   begin
      case Get_Kind (Stmt) is
         when N_Case =>
            Put ("case");
         when N_Casex =>
            Put ("casex");
         when N_Casez =>
            Put ("casez");
         when others =>
            raise Internal_Error;
      end case;
      Put (" (");
      Disp_Expression (Get_Expression (Stmt));
      Put (")");
   end Disp_Case_Header;

   procedure Disp_Case (Indent : Natural; Stmt : Node)
   is
      Item : Node;
      S : Node;
      E : Node;
   begin
      Disp_Case_Header (Stmt);
      New_Line;

      Item := Get_Case_Items (Stmt);
      while Item /= Null_Node loop
         Put_Indent (Indent);
         case Nkinds_Case_Item (Get_Kind (Item)) is
            when N_Default_Case_Item =>
               Put ("default");
            when N_Case_Item =>
               E := Get_Expression (Item);
               Disp_Expression (E);
         end case;
         S := Get_Statement (Item);
         if S = Null_Node then
            Put_Line (",");
         else
            Put_Line (":");
            Put_Indent (Indent + 1);
            Disp_Statement (Indent + 1, S);
         end if;
         Item := Get_Chain (Item);
         exit when Item = Null_Node;
      end loop;
      Put_Indent (Indent);
      Put_Line ("endcase");
   end Disp_Case;

   procedure Disp_Control (Ctrl : Node)
   is
      Expr : Node;
   begin
      if Ctrl = Null_Node then
         return;
      end if;
      Expr := Get_Expression (Ctrl);
      case Get_Kind (Ctrl) is
         when N_Event_Control =>
            Put ("@");
            if Get_Kind (Expr) = N_Implicit_Event then
               Put ("*");
            else
               Disp_Expression (Expr);
            end if;
            Put (' ');
         when N_Repeat_Control =>
            Put ("repeat (");
            Disp_Expression (Expr);
            Put (") ");
            Disp_Control (Get_Control (Ctrl));
         when N_Delay_Control =>
            Put ("#");
            if not Flag_Keep_Parentheses then
               Put ('(');
            end if;
            Disp_Expression (Expr);
            if not Flag_Keep_Parentheses then
               Put (')');
            end if;
            Put (" ");
         when others =>
            Error_Kind ("disp_control", Ctrl);
      end case;
   end Disp_Control;

   procedure Disp_Blocking_Assignment (Stmt : Node) is
   begin
      Disp_Expression (Get_Lvalue (Stmt));
      Put (" = ");
      Disp_Control (Get_Control (Stmt));
      Disp_Expression (Get_Expression (Stmt));
   end Disp_Blocking_Assignment;

   procedure Disp_Step_Assignment (Stmt : Node) is
   begin
      case Get_Kind (Stmt) is
         when Nkinds_Inc_Dec =>
            Disp_Expression (Stmt);
         when N_Blocking_Assign =>
            Disp_Blocking_Assignment (Stmt);
         when others =>
            Error_Kind ("disp_step_assignment", Stmt);
      end case;
   end Disp_Step_Assignment;

   procedure Disp_Step_Assignment_Chain (Head : Node)
   is
      Ass : Node;
   begin
      Ass := Head;
      loop
         Disp_Step_Assignment (Ass);
         Ass := Get_Chain (Ass);
         exit when Ass = Null_Node;
         Put (", ");
      end loop;
   end Disp_Step_Assignment_Chain;

   procedure Disp_For_Header (Stmt : Node)
   is
      Decl : Node;
   begin
      Put ("for (");
      Decl := Get_For_Initialization (Stmt);
      if Decl /= Null_Node then
         loop
            case Get_Kind (Decl) is
               when N_Var =>
                  Disp_Variable_Declarations (0, Decl);
               when N_Blocking_Assign =>
                  Disp_Blocking_Assignment (Decl);
                  Decl := Get_Chain (Decl);
               when others =>
                  Error_Kind ("disp_for(init)", Decl);
            end case;
            exit when Decl = Null_Node;
            Put (", ");
         end loop;
      end if;
      Put ("; ");
      Disp_Expression (Get_Condition (Stmt));
      Put ("; ");
      Disp_Step_Assignment_Chain (Get_Step_Assign (Stmt));
      Put (")");
   end Disp_For_Header;

   procedure Disp_For (Indent : Natural; Stmt : Node) is
   begin
      Disp_For_Header (Stmt);
      New_Line;
      Put_Indent (Indent + 1);
      Disp_Statement (Indent + 1, Get_Statement (Stmt));
   end Disp_For;

   procedure Disp_Foreach_Header (Hdr : Node)
   is
      Var : Node;
   begin
      Put ("foreach (");
      Disp_Expression (Get_Foreach_Array (Hdr));
      Put ('[');
      Var := Get_Foreach_Variables (Hdr);
      if Var /= Null_Node then
         loop
            Disp (Get_Identifier (Var));
            Var := Get_Chain (Var);
            exit when Var = Null_Node;
            Put (", ");
         end loop;
      end if;
      Put ("]");
      Put (")");
   end Disp_Foreach_Header;

   procedure Disp_Foreach (Indent : Natural; Stmt : Node) is
   begin
      Disp_Foreach_Header (Stmt);
      New_Line;
      Put_Indent (Indent + 1);
      Disp_Statement (Indent + 1, Get_Statement (Stmt));
   end Disp_Foreach;

   procedure Disp_Arguments (Args : Node; Has_Parenthesis : Boolean)
   is
      Arg : Node;
      Expr : Node;
      Port : Node;
   begin
      Arg := Args;
      if not Has_Parenthesis and then Args = Null_Node then
         return;
      end if;
      Put ('(');
      while Arg /= Null_Node loop
         Port := Get_Port (Arg);
         if Port /= Null_Node then
            Put ('.');
            Disp_Expression (Port);
            Put ('(');
         end if;
         Expr := Get_Expression (Arg);
         if Expr /= Null_Node then
            Disp_Expression (Expr);
         end if;
         if Port /= Null_Node then
            Put (')');
         end if;
         Arg := Get_Chain (Arg);
         exit when Arg = Null_Node;
         Put (", ");
      end loop;
      Put (')');
   end Disp_Arguments;

   procedure Disp_Label (Label : Node) is
   begin
      Put ("label");
      Put (Int32'Image (Get_Label_Number (Label)));
   end Disp_Label;

   procedure Disp_Non_Blocking_Assignment (Stmt : Node) is
   begin
      Disp_Expression (Get_Lvalue (Stmt));
      Put (" <= ");
      Disp_Control (Get_Control (Stmt));
      Disp_Expression (Get_Expression (Stmt));
   end Disp_Non_Blocking_Assignment;

   procedure Disp_While_Header (Stmt : Node) is
   begin
      Put ("while (");
      Disp_Expression (Get_Condition (Stmt));
      Put (") ");
   end Disp_While_Header;

   procedure Disp_Statement (Indent : Natural; Stmt : Node) is
   begin
      if Stmt = Null_Node then
         Put (";");
         return;
      end if;
      case Get_Kind (Stmt) is
         when N_Event_Control
           | N_Delay_Control
           | N_Repeat_Control =>
            Disp_Control (Stmt);
            if Get_Statement (Stmt) /= Null_Node then
               New_Line;
               Put_Indent (Indent);
               Disp_Statement (Indent, Get_Statement (Stmt));
            else
               Put (';');
               New_Line;
            end if;

         when N_Label =>
            Disp_Label (Stmt);
            Put (";");
            declare
               S : Node;
            begin
               S := Get_Chain (Stmt);
               while S /= Null_Node loop
                  Put_Indent (Indent);
                  Disp_Statement (Indent, S);
                  S := Get_Chain (S);
               end loop;
               S := Get_Label_Chain (Stmt);
               if S /= Null_Node then
                  Put_Indent (Indent);
                  Disp_Statement (Indent, S);
               end if;
            end;
         when N_Goto =>
            Put ("goto ");
            Disp_Label (Get_Label (Stmt));
            Put (";");

         when N_Seq_Block =>
            Disp_Seq_Block (Indent, Stmt);
         when N_Par_Block =>
            Disp_Par_Block (Indent, Stmt);
         when N_If =>
            Disp_If (Indent, Stmt);
         when N_For =>
            Disp_For (Indent, Stmt);
         when N_Foreach =>
            Disp_Foreach (Indent, Stmt);
         when N_Noblk_Assign =>
            Disp_Non_Blocking_Assignment (Stmt);
            Put_Line (";");
         when N_Blocking_Assign =>
            Disp_Blocking_Assignment (Stmt);
            Put_Line (";");
         when N_Assign_Operator =>
            Disp_Expression (Stmt);
            Put_Line (";");
         when N_Proc_Assign =>
            Put ("assign ");
            Disp_Expression (Get_Lvalue (Stmt));
            Put (" = ");
            Disp_Expression (Get_Expression (Stmt));
            Put_Line (";");
         when N_Proc_Deassign =>
            Put ("deassign ");
            Disp_Expression (Get_Lvalue (Stmt));
            Put_Line (";");
         when N_Pack_Assign
           | N_Unpack_Assign
           | N_Pack_Unpack_Assign =>
            Disp_Expression (Get_Lvalue (Stmt));
            Put (" = ");
            Disp_Expression (Get_Expression (Stmt));
         when N_Case
           | N_Casex
           | N_Casez =>
            Disp_Case (Indent, Stmt);
         when N_Subroutine_Call_Stmt =>
            if Get_Has_Void_Cast (Stmt) then
               Put ("void'(");
               Disp_Expression (Get_Call (Stmt));
               Put (")");
            else
               Disp_Expression (Get_Call (Stmt));
            end if;
            Put_Line (";");
         when N_Wait =>
            Put ("wait (");
            Disp_Expression (Get_Condition (Stmt));
            Put (") ");
            Disp_Statement (Indent, Get_Statement (Stmt));
            New_Line;
         when N_Repeat =>
            Put ("repeat (");
            Disp_Expression (Get_Expression (Stmt));
            Put (") ");
            Disp_Statement (Indent, Get_Statement (Stmt));
            New_Line;
         when N_While =>
            Disp_While_Header (Stmt);
            Disp_Statement_Chain (Indent, Get_Statement (Stmt));
            New_Line;
         when N_Do_While =>
            Put ("do");
            New_Line;
            Disp_Statement_Chain (Indent, Get_Statement (Stmt));
            Put_Indent (Indent);
            Put ("while (");
            Disp_Expression (Get_Condition (Stmt));
            Put (");");
            New_Line;
         when N_Forever =>
            Put ("forever ");
            Disp_Statement (Indent, Get_Statement (Stmt));
            New_Line;
         when N_Break_Stmt =>
            Put ("break");
            Put_Line (";");
         when N_Trigger =>
            Put ("-> ");
            Disp_Expression (Get_Event (Stmt));
            Put_Line (";");
         when N_Disable =>
            Put ("disable ");
            Disp_Identifier (Get_Statement (Stmt));
            Put_Line (";");
         when N_Continue_Stmt =>
            Put ("continue");
            Put (";");
         when N_Return_Stmt =>
            declare
               Expr : constant Node := Get_Expression (Stmt);
            begin
               Put ("return");
               if Expr /= Null_Node then
                  Put (' ');
                  Disp_Expression (Expr);
               end if;
               Put (";");
            end;
         when Nkinds_Inc_Dec =>
            Disp_Expression (Stmt);
            Put (";");
         when N_Disable_Fork =>
            Put ("disable ");
            Put ("fork");
            Put (';');
         when N_Wait_Fork =>
            Put ("wait ");
            Put ("fork");
            Put (';');
         when others =>
            Error_Kind ("disp_statement", Stmt);
      end case;
   end Disp_Statement;

   procedure Disp_Instance (Inst : Node; Name : Node; Indent : Natural)
   is
      Conn : Node;
      Port : Name_Id;
      Expr : Node;
   begin
      Disp_Expression (Name);
      Put (' ');
      Disp_Identifier (Inst);
      Disp_Parameter_Values (Get_Parameter_Values (Inst));
      Conn := Get_Connections (Inst);
      Put (" (");
      if Conn /= Null_Node then
         New_Line;
         loop
            Put_Indent (Indent + 1);
            Port := Get_Identifier (Conn);
            Expr := Get_Expression (Conn);
            if Port /= Null_Identifier then
               Put ('.');
               Disp (Port);
               Put (" (");
               if Expr /= Null_Node then
                  Disp_Expression (Expr);
               end if;
               Put (')');
            else
               if Expr /= Null_Node then
                  Disp_Expression (Expr);
               end if;
            end if;
            Conn := Get_Chain (Conn);
            exit when Conn = Null_Node;
            Put_Line (",");
         end loop;
      end if;
      Put (')');
      Put_Line (";");
   end Disp_Instance;

   procedure Disp_Tf_Port_List (Indent : Natural; Header : Node)
   is
      Port : Node;
   begin
      Port := Get_Tf_Ports_Chain (Header);
      Put (" (");
      if Port /= Null_Node then
         New_Line;
         loop
            Put_Indent (Indent + 1);
            if Get_Has_Direction (Port) then
               Disp_Port_Direction (Port);
            end if;
            Disp_Decl_Data_Type (Indent, Port);
            Disp_Default_Value (Get_Default_Value (Port));
            Port := Get_Chain (Port);
            exit when Port = Null_Node;
            Put_Line (",");
         end loop;
      end if;
      Put (')');
   end Disp_Tf_Port_List;

   procedure Disp_OOB_Prefix (Decl : Node)
   is
      Pfx : constant Node := Get_OOB_Prefix (Decl);
   begin
      Disp_Identifier (Pfx);
      Put ("::");
   end Disp_OOB_Prefix;

   procedure Disp_Pure_Flag (Decl : Node) is
   begin
      if Get_Pure_Flag (Decl) then
         Put ("pure ");
      end if;
   end Disp_Pure_Flag;

   procedure Disp_Virtual_Flag (Decl : Node) is
   begin
      if Get_Virtual_Flag (Decl) then
         Put ("virtual ");
      end if;
   end Disp_Virtual_Flag;

   procedure Disp_Pure_Virtual (Decl : Node) is
   begin
      Disp_Pure_Flag (Decl);
      Disp_Virtual_Flag (Decl);
   end Disp_Pure_Virtual;

   procedure Disp_Task (Indent : Natural; Decl : Node) is
   begin
      Disp_Visibility (Decl);
      Disp_Pure_Virtual (Decl);
      Disp_Static_Flag (Decl);
      Put ("task ");
      Disp_Lifetime (Decl);
      if Get_Kind (Decl) = N_OOB_Task then
         Disp_OOB_Prefix (Decl);
      end if;
      Disp_Identifier (Decl);
      if Get_Ansi_Port_Flag (Decl) then
         Disp_Tf_Port_List (Indent, Decl);
      end if;
      Put_Line (";");
      Disp_Item_Chain (Indent + 1,
                       Get_Tf_Item_Declaration_Chain (Decl));
      Disp_Statement_Chain (Indent, Get_Statements_Chain (Decl));
      Put_Indent (Indent);
      Put ("endtask");
      Disp_End_Name (Decl);
      New_Line;
   end Disp_Task;

   procedure Disp_Function (Indent : Natural; Decl : Node) is
   begin
      Disp_Visibility (Decl);
      Disp_Pure_Virtual (Decl);
      Disp_Static_Flag (Decl);
      Put ("function ");
      Disp_Lifetime (Decl);
      Disp_Data_Type (Indent, Get_Data_Type (Decl));
      Put (' ');
      if Get_Kind (Decl) = N_OOB_Function then
         Disp_OOB_Prefix (Decl);
      end if;
      Disp_Identifier (Decl);
      if Get_Ansi_Port_Flag (Decl) then
         Disp_Tf_Port_List (Indent, Decl);
      end if;
      Put_Line (";");
      if not Get_Pure_Flag (Decl) then
         Disp_Item_Chain (Indent + 1,
                          Get_Tf_Item_Declaration_Chain (Decl));
         Disp_Statement_Chain (Indent, Get_Statements_Chain (Decl));
         Put_Indent (Indent);
         Put ("endfunction");
         Disp_End_Name (Decl);
         New_Line;
      end if;
   end Disp_Function;

   procedure Disp_Extern_Routine (Indent : Natural; Decl : Node) is
   begin
      Put ("extern ");
      Disp_Static_Flag (Decl);
      Disp_Visibility (Decl);
      Disp_Virtual_Flag (Decl);
      case Get_Kind (Decl) is
         when N_Extern_Task =>
            Put ("task ");
            Disp_Identifier (Decl);
         when N_Extern_Function =>
            Put ("function ");
            Disp_Decl_Data_Type (Indent, Decl);
         when others =>
            Error_Kind ("disp_extern_routine", Decl);
      end case;
      if Get_Ansi_Port_Flag (Decl) then
         Disp_Tf_Port_List (Indent, Decl);
      end if;
      Put_Line (";");
   end Disp_Extern_Routine;

   procedure Disp_Extern_Function (Indent : Natural; Decl : Node)
     renames Disp_Extern_Routine;

   procedure Disp_Extern_Task (Indent : Natural; Decl : Node)
     renames Disp_Extern_Routine;

   procedure Put_DPI_Spec (Spec : DPI_Spec_Type) is
   begin
      case Spec is
         when DPI_Unknown =>
            null;
         when DPI_DPI_C =>
            Put ("""DPI-C""");
         when DPI_DPI =>
            Put ("""DPI""");
      end case;
   end Put_DPI_Spec;

   procedure Disp_Import_Function (Indent : Natural; Func : Node)
   is
      Id : Name_Id;
   begin
      Put ("import ");
      Put_DPI_Spec (Get_DPI_Spec (Func));
      Put (' ');
      if Get_Pure_Property (Func) then
         Put ("pure ");
      end if;
      if Get_Context_Property (Func) then
         Put ("context ");
      end if;
      Id := Get_C_Identifier (Func);
      if Id /= Null_Identifier then
         Disp (Id);
         Put (" = ");
      end if;
      Put ("function ");
      Disp_Decl_Data_Type (Indent, Func);
      Disp_Tf_Port_List (Indent, Func);
      Put_Line (";");
   end Disp_Import_Function;

   procedure Disp_Export_Function (Func : Node)
   is
      Id : Name_Id;
   begin
      Put ("export ");
      Put_DPI_Spec (Get_DPI_Spec (Func));
      Put (' ');
      Id := Get_C_Identifier (Func);
      if Id /= Null_Identifier then
         Disp (Id);
         Put (" = ");
      end if;
      Put ("function ");
      Disp_Identifier (Func);
      Put_Line (";");
   end Disp_Export_Function;

   procedure Disp_Opt_Delay (Dly : Node)
   is
      Expr : Node;
   begin
      if Dly = Null_Node then
         return;
      end if;
      Put ('#');
      if Get_Kind (Dly) = N_Delay then
         if not Flag_Keep_Parentheses then
            Put ('(');
         end if;
         Disp_Expression (Get_Rising_Delay (Dly));
         Expr := Get_Falling_Delay (Dly);
         if Expr /= Null_Node then
            Put (", ");
            Disp_Expression (Expr);
            Expr := Get_Highz_Delay (Dly);
            if Expr /= Null_Node then
               Put (", ");
               Disp_Expression (Expr);
            end if;
         end if;
         if not Flag_Keep_Parentheses then
            Put (')');
         end if;
      else
         Disp_Expression (Dly);
      end if;
      Put (' ');
   end Disp_Opt_Delay;

   procedure Disp_Gate_Kind (Gate : Node) is
   begin
      case Nkinds_Gate (Get_Kind (Gate)) is
         when N_Gate_And =>
            Put ("and");
         when N_Gate_Nand =>
            Put ("nand");
         when N_Gate_Or =>
            Put ("or");
         when N_Gate_Nor =>
            Put ("nor");
         when N_Gate_Xor =>
            Put ("xor");
         when N_Gate_Xnor =>
            Put ("xnor");
         when N_Gate_Not =>
            Put ("not");
         when N_Gate_Buf =>
            Put ("buf");
         when N_Gate_Bufif0 =>
            Put ("bufif0");
         when N_Gate_Bufif1 =>
            Put ("bufif1");
         when N_Gate_Notif0 =>
            Put ("notif0");
         when N_Gate_Notif1 =>
            Put ("notif1");
         when N_Gate_Pullup =>
            Put ("pullup");
         when N_Gate_Pulldown =>
            Put ("pulldown");
         when N_Gate_Nmos =>
            Put ("nmos");
         when N_Gate_Pmos =>
            Put ("pmos");
         when N_Gate_Cmos =>
            Put ("cmos");
         when N_Gate_Tran =>
            Put ("tran");
         when N_Gate_Tranif0 =>
            Put ("tranif0");
         when N_Gate_Tranif1 =>
            Put ("tranif1");
         when others =>
            Error_Kind ("disp_gate", Gate);
      end case;
   end Disp_Gate_Kind;

   procedure Disp_Gate (Indent : Natural; Gate : Node)
   is
      pragma Unreferenced (Indent);
      Id : Name_Id;
      Conn : Node;
   begin
      Disp_Gate_Kind (Gate);
      Put (' ');

      Disp_Opt_Delay (Get_Gate_Delay (Gate));

      Id := Get_Identifier (Gate);
      if Id /= Null_Identifier then
         Disp (Id);
      end if;

      Put ('(');
      Conn := Get_Gate_Terminals (Gate);
      loop
         Disp_Expression (Get_Expression (Conn));
         Conn := Get_Chain (Conn);
         exit when Conn = Null_Node;
         Put (", ");
      end loop;
      Put_Line (");");
   end Disp_Gate;

   procedure Disp_Loop_Generate (Indent : Natural; Stmt : Node)
   is
      Ass : Node;
      Item : Node;
   begin
      Put ("for (");
      Ass := Get_For_Initialization (Stmt);
      case Get_Kind (Ass) is
         when N_Genvar =>
            Put ("genvar ");
            Disp_Identifier (Ass);
            Put (" = ");
            Disp_Expression (Get_Expression (Ass));
         when N_Blocking_Assign =>
            Disp_Expression (Get_Lvalue (Ass));
            Put (" = ");
            Disp_Expression (Get_Expression (Ass));
         when others =>
            Error_Kind ("disp_loop_generate(1)", Ass);
      end case;
      Put ("; ");
      Disp_Expression (Get_Condition (Stmt));
      Put ("; ");
      Disp_Step_Assignment_Chain (Get_Step_Assign (Stmt));
      Put_Line (")");
      Item := Get_Generate_Block (Stmt);
      Disp_Item (Indent + 1, Item);
   end Disp_Loop_Generate;

   procedure Disp_Case_Generate (Indent : Natural; Stmt : Node)
   is
      Item : Node;
      E, S : Node;
   begin
      Put ("case");
      Put (" (");
      Disp_Expression (Get_Expression (Stmt));
      Put_Line (")");
      Item := Get_Case_Items (Stmt);
      while Item /= Null_Node loop
         Put_Indent (Indent);
         case Nkinds_Case_Item (Get_Kind (Item)) is
            when N_Default_Case_Item =>
               Put ("default");
            when N_Case_Item =>
               E := Get_Expression (Item);
               Disp_Expression (E);
         end case;
         S := Get_Statement (Item);
         if S = Null_Node then
            Put_Line (",");
         else
            Put_Line (":");
            Put_Indent (Indent + 1);
            Disp_Item (Indent + 1, S);
         end if;
         Item := Get_Chain (Item);
         exit when Item = Null_Node;
      end loop;
      Put_Indent (Indent);
      Put_Line ("endcase");
   end Disp_Case_Generate;

   procedure Disp_If_Generate (Indent : Natural; Stmt : Node)
   is
      Item : Node;
   begin
      Put ("if (");
      Disp_Expression (Get_Condition (Stmt));
      Put_Line (")");
      Item := Get_True_Block (Stmt);
      if Item /= Null_Node then
         Disp_Item (Indent + 1, Item);
      end if;
      Item := Get_False_Block (Stmt);
      if Item /= Null_Node then
         Put_Indent (Indent);
         Put_Line ("else");
         Disp_Item (Indent + 1, Item);
      end if;
   end Disp_If_Generate;

   procedure Disp_Class (Indent : Natural; N : Node)
   is
      Base : constant Node := Get_Base_Class_Type (N);
   begin
      Put_Indent (Indent);
      Disp_Virtual_Flag (N);
      Put ("class");
      Put (' ');
      Disp_Identifier (N);
      Disp_Parameter_Port_List (Indent, Get_Parameter_Port_Chain (N));
      if Base /= Null_Node then
         Put (' ');
         Put ("extends");
         Put (' ');
         Disp_Data_Type (0, Base);
      end if;
      Put (';');
      New_Line;

      Disp_Item_Chain (Indent + 1, Get_Class_Item_Chain (N));

      Put_Indent (Indent);
      Put ("endclass");
      Disp_End_Name (N);
      New_Line;
   end Disp_Class;

   procedure Disp_Constraint_Set (Indent : Natural; Set : Node)
   is
      Expr : Node;
   begin
      Put_Indent (Indent);
      Put ("{");
      New_Line;
      Expr := Set;
      while Expr /= Null_Node loop
         Put_Indent (Indent + 1);
         Disp_Constraint_Expression (Indent + 1, Expr);
         New_Line;
         Expr := Get_Chain (Expr);
      end loop;
      Put_Indent (Indent);
      Put_Line ("}");
   end Disp_Constraint_Set;

   procedure Disp_Constraint_Expression (Indent : Natural; Item : Node)
   is
      Els : Node;
   begin
      case Get_Kind (Item) is
         when N_Constraint_Expression =>
            Disp_Expression (Get_Expression (Item));
            Put (";");
         when N_Constraint_If =>
            Put ("if (");
            Disp_Expression (Get_Condition (Item));
            Put (")");
            New_Line;
            Disp_Constraint_Set (Indent, Get_Cond_True (Item));
            Els := Get_Cond_False (Item);
            if Els /= Null_Node then
               Put_Indent (Indent);
               Put_Line ("else");
               Disp_Constraint_Set (Indent, Els);
            end if;
         when N_Constraint_Foreach =>
            Disp_Foreach_Header (Item);
            Disp_Constraint_Set (Indent, Get_Constraint_Set (Item));
         when others =>
            Error_Kind ("disp_constraint_block_item", Item);
      end case;
   end Disp_Constraint_Expression;

   procedure Disp_Constraint_Block (Indent : Natural; Blk : Node)
   is
      El : Node;
   begin
      Put ("{");
      New_Line;
      if Blk /= Null_Node then
         El := Blk;
         loop
            Put_Indent (Indent + 1);
            Disp_Constraint_Expression (Indent + 1, El);
            El := Get_Chain (El);
            exit when El = Null_Node;
         end loop;
      end if;
      Put_Indent (Indent);
      Put_Line ("}");
   end Disp_Constraint_Block;

   procedure Disp_Continuous_Assignment (Stmt : Node) is
   begin
      Put ("assign ");
      Disp_Expression (Get_Lvalue (Stmt));
      Put (" = ");
      Disp_Expression (Get_Expression (Stmt));
   end Disp_Continuous_Assignment;

   procedure Disp_Package_Import (Item : Node) is
   begin
      Put ("import ");
      Disp_Expression (Get_Item_Name (Item));
      Put_Line (";");
   end Disp_Package_Import;

   procedure Disp_Modport_Declaration (Indent : Natural; Modport : Node)
   is
      pragma Unreferenced (Indent);
      Port : Node;
   begin
      Put ("modport ");
      Disp_Identifier (Modport);
      Put (' ');
      Put ('(');
      Port := Get_Modport_Ports_Chain (Modport);
      while Port /= Null_Node loop
         case Nkinds_Modport_Port (Get_Kind (Port)) is
            when N_Modport_Input =>
               Put ("input ");
            when N_Modport_Output =>
               Put ("output ");
            when N_Modport_Inout =>
               Put ("inout ");
            when others =>
               raise Internal_Error;
         end case;
         Disp_Identifier (Port);
         Port := Get_Chain (Port);
         exit when Port = Null_Node;
         Put (", ");
      end loop;
      Put_Line (");");
   end Disp_Modport_Declaration;

   procedure Disp_Port (Port : Node)
   is
      Id : constant Name_Id := Get_Identifier (Port);
   begin
      if Id /= Null_Identifier then
         Put ('.');
         Disp (Id);
         Put ('(');
         Disp_Expression (Get_Expression (Port));
         Put (')');
      else
         Disp_Expression (Get_Expression (Port));
      end if;
   end Disp_Port;

   procedure Disp_List_Of_Ports (Indent : Natural; Header : Node)
   is
      Port : Node;
      Has_Ports : Boolean;
   begin
      Port := Get_Ports_Chain (Header);
      Has_Ports := Get_Ansi_Port_Flag (Header) or else Port /= Null_Node;
      if Has_Ports then
         Put (" (");
      end if;
      if Port /= Null_Node then
         New_Line;
         loop
            Put_Indent (Indent + 1);
            if Get_Kind (Port) = N_Port then
               Disp_Port (Port);
               Port := Get_Chain (Port);
            else
               Disp_Port_Declaration (Indent, Port);
            end if;
            exit when Port = Null_Node;
            Put_Line (",");
         end loop;
      end if;
      if Has_Ports then
         Put (')');
      end if;
   end Disp_List_Of_Ports;

   procedure Disp_Module (M : Node; Indent : Natural := 0) is
   begin
      Put_Indent (Indent);
      Put ("module ");
      Disp_Identifier (M);

      Disp_Parameter_Port_List (Indent, Get_Parameter_Port_Chain (M));
      Disp_List_Of_Ports (Indent, M);
      Put (';');
      New_Line;

      Disp_Item_Chain (Indent + 1, Get_Items_Chain (M));
      Put_Indent (Indent);
      Put_Line ("endmodule");
   end Disp_Module;

   procedure Put_Udp_Symbol (Sym : Udp_Symbol)
   is
      C : Character;
   begin
      case Sym is
         when Udp_0    => C := '0';
         when Udp_1    => C := '0';
         when Udp_X    => C := 'x';
         when Udp_Qm   => C := '?';
         when Udp_B    => C := 'b';
         when Udp_R    => C := 'r';
         when Udp_F    => C := 'f';
         when Udp_P    => C := 'p';
         when Udp_N    => C := 'n';
         when Udp_Any  => C := '*';
         when Udp_No   => C := '-';
      end case;
      Put (C);
   end Put_Udp_Symbol;

   procedure Disp_Primitive (Indent : Natural; Udp : Node)
   is
      Port : Node;
      Ent : Node;
      Sym : Node;
   begin
      Put ("primitive ");
      Disp_Identifier (Udp);

      Port := Get_Ports_Chain (Udp);
      Put_Line (" (");
      loop
         Put_Indent (Indent + 1);
         Disp (Get_Identifier (Port));
         Port := Get_Chain (Port);
         exit when Port = Null_Node;
         Put_Line (",");
      end loop;
      Put_Line (");");

      Port := Get_Udp_Port_Declaration_Chain (Udp);
      while Port /= Null_Node loop
         Put_Indent (1);
         if Get_Kind (Port) = N_Var then
            Put ("reg ");
            Disp_Identifier (Port);
         else
            Disp_Port_Direction (Port);
            Disp_Identifier (Port);
         end if;
         Put_Line (";");
         Port := Get_Chain (Port);
      end loop;

      Put_Indent (1);
      Put ("table");
      New_Line;

      Ent := Get_Udp_Entries_Chain (Udp);
      while Ent /= Null_Node loop
         Put_Indent (1 * 2);
         Sym := Get_Input_Chain (Ent);

         while Sym /= Null_Node loop
            case Get_Kind (Sym) is
               when N_Udp_Level_Symbol =>
                  Put_Udp_Symbol (Get_Symbol (Sym));
                  if Get_Kind (Ent) = N_Udp_Sequential_Entry then
                     Put ("   ");
                  end if;
               when N_Udp_Change_Symbol =>
                  Put ('(');
                  Put_Udp_Symbol (Get_From_Symbol (Sym));
                  Put_Udp_Symbol (Get_To_Symbol (Sym));
                  Put (')');
               when others =>
                  raise Internal_Error;
            end case;
            Put (' ');
            Sym := Get_Chain (Sym);
         end loop;

         case Get_Kind (Ent) is
            when N_Udp_Combinational_Entry =>
               Put (':');
               Put (' ');
               Put_Udp_Symbol (Get_Output_Symbol (Ent));
            when N_Udp_Sequential_Entry =>
               Put (':');
               Put (' ');
               Put_Udp_Symbol (Get_Current_State (Ent));
               Put (' ');
               Put (':');
               Put (' ');
               Put_Udp_Symbol (Get_Next_State (Ent));
            when others =>
               raise Internal_Error;
         end case;
         Put_Line (";");
         Ent := Get_Chain (Ent);
      end loop;

      Put_Indent (1);
      Put ("endtable");
      New_Line;

      Put_Line ("endprimitive");
   end Disp_Primitive;

   procedure Disp_Interface_Declaration (Indent : Natural; N : Node) is
   begin
      Put_Indent (Indent);
      Put ("interface ");
      Disp_Identifier (N);
      Disp_Parameter_Port_List (Indent, Get_Parameter_Port_Chain (N));
      Disp_List_Of_Ports (Indent, N);
      Put (';');
      New_Line;

      Disp_Item_Chain (Indent + 1, Get_Items_Chain (N));
      Put_Indent (Indent);
      Put_Line ("endinterface");
   end Disp_Interface_Declaration;

   procedure Disp_Program_Declaration (Indent : Natural; M : Node) is
   begin
      Put_Indent (Indent);
      Put ("program ");
      Disp_Identifier (M);

      Disp_List_Of_Ports (Indent, M);
      Put (';');
      New_Line;

      Disp_Item_Chain (Indent + 1, Get_Items_Chain (M));
      Put_Indent (Indent);
      Put_Line ("endprogram");
   end Disp_Program_Declaration;

   procedure Disp_Package_Declaration (M : Node; Indent : Natural := 0) is
   begin
      Put_Indent (Indent);
      Put ("package ");
      Disp_Identifier (M);
      Put (';');
      New_Line;

      Disp_Item_Chain (Indent + 1, Get_Package_Item_Chain (M));
      Put_Indent (Indent);
      Put_Line ("endpackage");
   end Disp_Package_Declaration;

   procedure Disp_Source (Source : Node) is
   begin
      Disp_Item_Chain (0, Get_Descriptions (Source));
   end Disp_Source;

   procedure Disp_Item (Indent : Natural; Item : in out Node)
   is
      Res : Node;
   begin
      Res := Get_Chain (Item);
      Put_Indent (Indent);
      case Get_Kind (Item) is
         when N_Package_Import =>
            Disp_Package_Import (Item);
         when Nkinds_Net_Port
           | Nkinds_Tf_Port =>
            Disp_Port_Declaration_Semicolon (Indent, Item);
            Res := Item;
         when Nkinds_Nets =>
            Disp_Net_Declaration (Indent, Item);
            Res := Item;
            Put (';');
            New_Line;
         when N_Var =>
            Disp_Variable_Declarations (Indent, Item);
            Put_Line (";");
            Res := Item;
         when N_Genvar =>
            Put ("genvar ");
            Disp_Identifier (Item);
            Put_Line (";");
         when N_Parameter
           | N_Localparam =>
            Disp_Parameter_Declaration (Indent, Item);
         when N_Defparam =>
            Put ("defparam ");
            Disp_Expression (Get_Lvalue (Item));
            Put (" = ");
            Disp_Expression (Get_Expression (Item));
            Put_Line (";");
         when N_Modport =>
            Disp_Modport_Declaration (Indent, Item);
         when N_Class
           | N_Generic_Class =>
            Disp_Class (Indent, Item);
         when N_Typedef =>
            Put ("typedef ");
            Disp_Decl_Data_Type (Indent, Item);
            Put_Line (";");
         when N_Typedef_Class =>
            Put ("typedef ");
            Put ("class ");
            Disp (Get_Identifier (Item));
            Put_Line (";");
         when N_Typedef_Struct =>
            Put ("typedef ");
            Put ("struct ");
            Disp (Get_Identifier (Item));
            Put_Line (";");

         when N_Always =>
            Put ("always ");
            Disp_Statement (Indent + 1, Get_Statement (Item));
         when N_Always_Comb =>
            Put ("always_comb ");
            Disp_Statement (Indent + 1, Get_Statement (Item));
         when N_Always_Ff =>
            Put ("always_ff ");
            Disp_Statement (Indent + 1, Get_Statement (Item));
         when N_Initial =>
            Put ("initial ");
            Disp_Statement (Indent + 1, Get_Statement (Item));
         when N_Assign =>
            Put ("assign ");
            Disp_Expression (Get_Lvalue (Item));
            Put (" = ");
            Disp_Expression (Get_Expression (Item));
            Put_Line (";");
         when N_Module_Instance =>
            declare
               Module : constant Node := Get_Instance (Item);
            begin
               if Module = Null_Node then
                  Disp_Instance (Item, Get_Module (Item), Indent);
               else
                  Disp_Instance (Item, Module, Indent);
                  if Get_Kind (Module) = N_Module then
                     Disp_Module (Module, Indent + 1);
                  end if;
               end if;
            end;
         when N_Program_Instance =>
            declare
               Prog : constant Node := Get_Instance (Item);
            begin
               if Prog = Null_Node then
                  Disp_Instance (Item, Get_Module (Item), Indent);
               else
                  Disp_Instance (Item, Prog, Indent);
                  if Get_Kind (Prog) = N_Program_Declaration then
                     Disp_Program_Declaration (Indent + 1, Prog);
                  end if;
               end if;
            end;
         when N_Interface_Instance =>
            Disp_Instance (Item, Get_Interface_Name (Item), Indent);

         when N_Noblk_Assign
           | N_Blocking_Assign
           | N_Assign_Operator
           | N_If
           | N_Case
           | N_Casex
           | N_Casez
           | N_For
           | N_Foreach
           | N_Delay_Control
           | N_Event_Control
           | N_Repeat_Control
           | N_Wait
           | N_Repeat
           | N_While
           | N_Do_While
           | N_Forever
           | N_Trigger
           | N_Disable
           | N_Par_Block
           | N_Goto
           | N_Subroutine_Call_Stmt
           | N_Continue_Stmt
           | N_Break_Stmt
           | N_Return_Stmt
           | N_Disable_Fork
           | N_Wait_Fork
           | N_Pack_Assign
           | N_Unpack_Assign
           | N_Pack_Unpack_Assign =>
            Disp_Statement (Indent, Item);

         when Nkinds_Inc_Dec =>
            Disp_Expression (Item);
            Put_Line (";");

         when N_Task
           | N_OOB_Task =>
            Disp_Task (Indent, Item);
         when N_Function
           | N_OOB_Function =>
            Disp_Function (Indent, Item);
         when N_Import_DPI_Function =>
            Disp_Import_Function (Indent, Item);
         when N_Export_DPI_Function =>
            Disp_Export_Function (Item);
         when N_Extern_Function =>
            Disp_Extern_Function (Indent, Item);
         when N_Extern_Task =>
            Disp_Extern_Task (Indent, Item);
         when N_Seq_Block =>
            Disp_Seq_Block (Indent, Item);
         when N_Constraint =>
            Put ("constraint ");
            Disp_Identifier (Item);
            Put (' ');
            Disp_Constraint_Block (Indent, Get_Constraint_Block_Chain (Item));

         when Nkinds_Gate =>
            Disp_Gate (Indent, Item);

         when N_Specify =>
            --  FIXME: todo
            New_Line;

         when N_Generate_Region =>
            Put ("generate");
            New_Line;
            Disp_Item_Chain (Indent + 1,
                             Get_Generate_Item_Chain (Item));
            Put_Indent (Indent);
            Put_Line ("endgenerate");
         when N_Generate_Block =>
            Put ("begin");
            declare
               Id : constant Name_Id := Get_Identifier (Item);
            begin
               if Id /= Null_Identifier then
                  Put (": ");
                  Disp (Id);
               end if;
            end;
            New_Line;
            Disp_Item_Chain (Indent + 1,
                             Get_Generate_Item_Chain (Item));
            Put_Indent (Indent);
            Put_Line ("end");
         when N_Array_Generate_Block =>
            Disp_Item_Chain (Indent, Get_Generate_Item_Chain (Item));
         when N_Indexed_Generate_Block =>
            Put ("begin");
            Put (": ");
            Disp (Get_Identifier (Item));
            Put ('[');
            Disp_Int32 (Get_Generate_Index (Item));
            Put (']');
            New_Line;
            Disp_Item_Chain (Indent + 1,
                             Get_Generate_Item_Chain (Item));
            Put_Indent (Indent);
            Put_Line ("end");
         when N_Loop_Generate =>
            Disp_Loop_Generate (Indent, Item);
         when N_Case_Generate =>
            Disp_Case_Generate (Indent, Item);
         when N_If_Generate =>
            Disp_If_Generate (Indent, Item);

         when N_Module =>
            Disp_Module (Item, Indent);
         when N_Primitive =>
            Disp_Primitive (Indent, Item);
         when N_Interface_Declaration =>
            Disp_Interface_Declaration (Indent, Item);
         when N_Program_Declaration =>
            Disp_Program_Declaration (Indent, Item);
         when N_Package =>
            Disp_Package_Declaration (Item);

         when N_Discipline =>
            Disp_Discipline_Declaration (Indent, Item);
         when N_Nature =>
            Disp_Nature_Declaration (Indent, Item);

         when N_Analog =>
            Disp_Analog (Indent, Item);

         when N_Contribution =>
            Disp_Contribution (Item);
         when others =>
            Error_Kind ("disp_item", Item);
      end case;
      Item := Res;
   end Disp_Item;

   procedure Disp_Item_Chain (Indent : Natural; Head : Node)
   is
      Item : Node;
   begin
      Item := Head;
      while Item /= Null_Node loop
         Disp_Item (Indent, Item);
      end loop;
   end Disp_Item_Chain;

   procedure Disp_Item (Item : Node)
   is
      Item2 : Node;
   begin
      Item2 := Item;
      Disp_Item (1, Item2);
      New_Line;
   end Disp_Item;
end Verilog.Disp_Verilog;
