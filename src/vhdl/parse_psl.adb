--  VHDL PSL parser.
--  Copyright (C) 2009 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with PSL.Nodes; use PSL.Nodes;
with Iirs;
with Scanner; use Scanner;
with PSL.Errors; use PSL.Errors;
with PSL.Priorities; use PSL.Priorities;
with Parse;

package body Parse_Psl is
   function Create_Node_Loc (K : Nkind) return Node is
      Res : Node;
   begin
      Res := PSL.Nodes.Create_Node (K);
      Set_Location (Res, Get_Token_Location);
      return Res;
   end Create_Node_Loc;

   function Parse_Number return Node is
      Res : Node;
   begin
      if Current_Token = Tok_Integer then
         Res := Create_Node_Loc (N_Number);
         --  FIXME: handle overflow.
         Set_Value (Res, Uns32 (Current_Iir_Int64));
         Scan;
         return Res;
      elsif Current_Token = Tok_Inf then
         --  FIXME: create node
         Scan;
         return Null_Node;
      else
         Error_Msg_Parse ("number expected");
         return Null_Node;
      end if;
   end Parse_Number;

   procedure Parse_Count (N : Node) is
   begin
      Set_Low_Bound (N, Parse_Number);
      if Current_Token = Tok_To then
         Scan;
         Set_High_Bound (N, Parse_Number);
      end if;
   end Parse_Count;

   function Psl_To_Vhdl (N : Node) return Iirs.Iir
   is
      use Iirs;
      Res : Iir;
   begin
      case Get_Kind (N) is
         when N_HDL_Expr =>
            Res := Iirs.Iir (Get_HDL_Node (N));
            Free_Node (N);
            return Res;
         when others =>
            Error_Kind ("psl_to_vhdl", N);
      end case;
   end Psl_To_Vhdl;

   function Vhdl_To_Psl (N : Iirs.Iir) return Node
   is
      use Iirs;
      Res : Node;
   begin
      Res := Create_Node_Loc (N_HDL_Expr);
      if N /= Null_Iir then
         Set_Location (Res, Get_Location (N));
         Set_HDL_Node (Res, Int32 (N));
      end if;
      return Res;
   end Vhdl_To_Psl;

   function Parse_FL_Property (Prio : Priority) return Node;
   function Parse_Parenthesis_Boolean return Node;
   function Parse_Boolean (Parent_Prio : Priority) return Node;

   function Parse_Unary_Boolean (Full_Hdl_Expr : Boolean) return Node
   is
      use Parse;
      use Iirs;
      Left, Expr : Iir;
      Op : Iir_Kind;
   begin
      if Full_Hdl_Expr then
         Expr := Parse_Expression;
      else
         --  Boolean operators must be parse, *except* and/or that could be at
         --  upper layers (FL).
         Expr := Parse_Expression (Prio_Relation);
         loop
            case Current_Token is
               when Tok_Xor =>
                  Op := Iir_Kind_Xor_Operator;
               when Tok_Nand =>
                  Op := Iir_Kind_Nand_Operator;
               when Tok_Nor =>
                  Op := Iir_Kind_Nor_Operator;
               when Tok_Xnor =>
                  Op := Iir_Kind_Xnor_Operator;
               when others =>
                  exit;
            end case;

            Left := Expr;
            Expr := Create_Iir (Op);
            Set_Location (Expr, Get_Token_Location);
            Set_Left (Expr, Left);

            --  Skip operator.
            Scan;

            Set_Right (Expr, Parse_Expression (Prio_Relation));
         end loop;
      end if;

      return Vhdl_To_Psl (Expr);
   end Parse_Unary_Boolean;

   function Parse_Boolean_Rhs (Parent_Prio : Priority; Left : Node) return Node
   is
      Kind : Nkind;
      Prio : Priority;
      Res : Node;
      Tmp : Node;
   begin
      Res := Left;
      loop
         case Current_Token is
            when Tok_And =>
               Kind := N_And_Bool;
               Prio := Prio_Seq_And;
            when Tok_Or =>
               Kind := N_Or_Bool;
               Prio := Prio_Seq_Or;
            when others =>
               return Res;
         end case;
         if Parent_Prio >= Prio then
            return Res;
         end if;
         Tmp := Create_Node_Loc (Kind);
         Scan;
         Set_Left (Tmp, Res);
         Res := Tmp;
         Tmp := Parse_Boolean (Prio);
         Set_Right (Res, Tmp);
      end loop;
   end Parse_Boolean_Rhs;

   function Parse_Boolean (Parent_Prio : Priority) return Node
   is
   begin
      return Parse_Boolean_Rhs (Parent_Prio, Parse_Unary_Boolean (False));
   end Parse_Boolean;

   function Parse_Psl_Boolean return PSL_Node is
   begin
      return Parse_Boolean (Prio_Lowest);
   end Parse_Psl_Boolean;

   function Parse_Parenthesis_Boolean return Node is
      Res : Node;
   begin
      if Current_Token /= Tok_Left_Paren then
         Error_Msg_Parse ("'(' expected before boolean expression");
         return Null_Node;
      else
         Scan;
         Res := Parse_Psl_Boolean;
         if Current_Token = Tok_Right_Paren then
            Scan;
         else
            Error_Msg_Parse ("missing matching ')' for boolean expression");
         end if;
         return Res;
      end if;
   end Parse_Parenthesis_Boolean;

   function Parse_SERE (Prio : Priority) return Node is
      Left, Res : Node;
      Kind : Nkind;
      Op_Prio : Priority;
   begin
      Left := Parse_Psl_Sequence (True);
      loop
         case Current_Token is
            when Tok_Semi_Colon =>
               Kind := N_Concat_SERE;
               Op_Prio := Prio_Seq_Concat;
            when Tok_Colon =>
               Kind := N_Fusion_SERE;
               Op_Prio := Prio_Seq_Fusion;
            when Tok_Within =>
               Kind := N_Within_SERE;
               Op_Prio := Prio_Seq_Within;
            when Tok_Ampersand =>
               -- For non-length matching and, the operator is '&'.
               Kind := N_And_Seq;
               Op_Prio := Prio_Seq_And;
            when Tok_And_And =>
               Kind := N_Match_And_Seq;
               Op_Prio := Prio_Seq_And;
            when Tok_Bar =>
               Kind := N_Or_Seq;
               Op_Prio := Prio_Seq_Or;
--              when Tok_Bar_Bar =>
--                 Res := Create_Node_Loc (N_Or_Bool);
--                 Scan;
--                 Set_Left (Res, Left);
--                 Set_Right (Res, Parse_Boolean (Prio_Seq_Or));
--                 return Res;
            when others =>
               return Left;
         end case;
         if Prio >= Op_Prio then
            return Left;
         end if;
         Res := Create_Node_Loc (Kind);
         Scan;
         Set_Left (Res, Left);
         Set_Right (Res, Parse_SERE (Op_Prio));
         Left := Res;
      end loop;
   end Parse_SERE;

   --  precond : '{'
   --  postcond: next token after '}'
   function Parse_Braced_SERE return Node is
      Res : Node;
   begin
      pragma Assert (Current_Token = Tok_Left_Curly);
      Res := Create_Node_Loc (N_Braced_SERE);

      --  Skip '{'
      Scan;

      Set_SERE (Res, Parse_SERE (Prio_Lowest));

      if Current_Token /= Tok_Right_Curly then
         Error_Msg_Parse ("missing '}' after braced SERE");
      else
         --  Skip '}'
         Scan;
      end if;
      return Res;
   end Parse_Braced_SERE;

   --  Parse [ Count ] ']'
   function Parse_Maybe_Count (Kind : Nkind; Seq : Node) return Node is
      N : Node;
   begin
      N := Create_Node_Loc (Kind);
      Set_Sequence (N, Seq);
      Scan;
      if Current_Token /= Tok_Right_Bracket then
         Parse_Count (N);
      end if;
      if Current_Token /= Tok_Right_Bracket then
         Error_Msg_Parse ("missing ']'");
      else
         Scan;
      end if;
      return N;
   end Parse_Maybe_Count;

   procedure Parse_Bracket_Range (N : Node) is
   begin
      if Current_Token /= Tok_Left_Bracket then
         Error_Msg_Parse ("'[' expected");
      else
         Scan;
         Set_Low_Bound (N, Parse_Number);
         if Current_Token /= Tok_To then
            Error_Msg_Parse ("'to' expected in range after left bound");
         else
            Scan;
            Set_High_Bound (N, Parse_Number);
         end if;
         if Current_Token /= Tok_Right_Bracket then
            Error_Msg_Parse ("']' expected after range");
         else
            Scan;
         end if;
      end if;
   end Parse_Bracket_Range;

   function Parse_Bracket_Number return Node is
      Res : Node;
   begin
      if Current_Token /= Tok_Left_Bracket then
         Error_Msg_Parse ("'[' expected");
         return Null_Node;
      else
         Scan;
         Res := Parse_Number;
         if Current_Token /= Tok_Right_Bracket then
            Error_Msg_Parse ("']' expected after range");
         else
            Scan;
         end if;
         return Res;
      end if;
   end Parse_Bracket_Number;

   function Parse_Psl_Sequence (Full_Hdl_Expr : Boolean) return Node is
      Res, N : Node;
   begin
      case Current_Token is
         when Tok_Left_Curly =>
            Res := Parse_Braced_SERE;
            if Current_Token = Tok_Arobase then
               N := Create_Node_Loc (N_Clocked_SERE);
               Set_SERE (N, Res);

               --  Skip '@'
               Scan;

               Set_Boolean (N, Parse_Psl_Boolean);
               Res := N;
            end if;
         when Tok_Brack_Star =>
            return Parse_Maybe_Count (N_Star_Repeat_Seq, Null_Node);
         when Tok_Left_Paren =>
            Res := Parse_Parenthesis_Boolean;
            if Current_Token = Tok_Or
              or else Current_Token = Tok_And
            then
               Res := Parse_Boolean_Rhs (Prio_Lowest, Res);
            end if;
         when Tok_Brack_Plus_Brack =>
            Res := Create_Node_Loc (N_Plus_Repeat_Seq);
            Scan;
            return Res;
         when others =>
            --  Repeated_SERE
            Res := Parse_Unary_Boolean (Full_Hdl_Expr);
      end case;
      loop
         case Current_Token is
            when Tok_Brack_Star =>
               Res := Parse_Maybe_Count (N_Star_Repeat_Seq, Res);
            when Tok_Brack_Plus_Brack =>
               N := Create_Node_Loc (N_Plus_Repeat_Seq);
               Set_Sequence (N, Res);

               --  Skip '[+]'
               Scan;
               Res := N;
            when Tok_Brack_Arrow =>
               Res := Parse_Maybe_Count (N_Goto_Repeat_Seq, Res);
            when Tok_Brack_Equal =>
               N := Create_Node_Loc (N_Equal_Repeat_Seq);
               Set_Sequence (N, Res);

               --  Skip '[='
               Scan;
               Parse_Count (N);
               if Current_Token /= Tok_Right_Bracket then
                  Error_Msg_Parse ("missing ']'");
               else
                  Scan;
               end if;
               Res := N;
            when others =>
               return Res;
         end case;
      end loop;
   end Parse_Psl_Sequence;

   --  precond:  '('
   --  postcond: next token
   function Parse_Parenthesis_FL_Property return Node is
      Res : Node;
      Loc : Location_Type;
   begin
      Loc := Get_Token_Location;
      if Current_Token /= Tok_Left_Paren then
         Error_Msg_Parse ("'(' expected around property");
         return Parse_FL_Property (Prio_Lowest);
      else
         --  Skip '('.
         Scan;

         Res := Parse_FL_Property (Prio_Lowest);
         if Current_Token = Tok_Right_Paren then
            --  Skip ')'.
            Scan;
         else
            Error_Msg_Parse ("missing matching ')' for '(' at line "
                               & Image (Loc, False));
         end if;

         if Get_Kind (Res) = N_HDL_Expr then
            declare
               N : Iirs.Iir;
            begin
               N := Psl_To_Vhdl (Res);
               N := Parse.Parse_Binary_Expression (N, Parse.Prio_Expression);
               Res := Vhdl_To_Psl (N);
            end;
         end if;
         return Res;
      end if;
   end Parse_Parenthesis_FL_Property;

   --  Parse [ '!' ] '[' finite_Range ']' '(' FL_Property ')'
   function Parse_Range_Property (K : Nkind) return Node is
      Res : Node;
   begin
      Res := Create_Node_Loc (K);
      Set_Strong_Flag (Res, Scan_Exclam_Mark);
      Scan;
      Parse_Bracket_Range (Res);
      Set_Property (Res, Parse_Parenthesis_FL_Property);
      return Res;
   end Parse_Range_Property;

   --  Parse [ '!' ] '(' Boolean ')' '[' Range ']' '(' FL_Property ')'
   function Parse_Boolean_Range_Property (K : Nkind) return Node is
      Res : Node;
   begin
      Res := Create_Node_Loc (K);
      Set_Strong_Flag (Res, Scan_Exclam_Mark);
      Scan;
      Set_Boolean (Res, Parse_Parenthesis_Boolean);
      Parse_Bracket_Range (Res);
      Set_Property (Res, Parse_Parenthesis_FL_Property);
      return Res;
   end Parse_Boolean_Range_Property;

   function Parse_FL_Property_1 return Node
   is
      Res : Node;
      Tmp : Node;
   begin
      case Current_Token is
         when Tok_Always =>
            Res := Create_Node_Loc (N_Always);
            Scan;
            Set_Property (Res, Parse_FL_Property (Prio_FL_Invariance));
         when Tok_Never =>
            Res := Create_Node_Loc (N_Never);
            Scan;
            Set_Property (Res, Parse_FL_Property (Prio_FL_Invariance));
         when Tok_Eventually =>
            Res := Create_Node_Loc (N_Eventually);
            if not Scan_Exclam_Mark then
               Error_Msg_Parse ("'eventually' must be followed by '!'");
            end if;
            Scan;
            Set_Property (Res, Parse_FL_Property (Prio_FL_Occurence));
         when Tok_Next =>
            Res := Create_Node_Loc (N_Next);
            Scan;
            if Current_Token = Tok_Left_Bracket then
               Set_Number (Res, Parse_Bracket_Number);
               Set_Property (Res, Parse_Parenthesis_FL_Property);
            else
               Set_Property (Res, Parse_FL_Property (Prio_FL_Occurence));
            end if;
         when Tok_Next_A =>
            Res := Parse_Range_Property (N_Next_A);
         when Tok_Next_E =>
            Res := Parse_Range_Property (N_Next_E);
         when Tok_Next_Event =>
            Res := Create_Node_Loc (N_Next_Event);
            Scan;
            Set_Boolean (Res, Parse_Parenthesis_Boolean);
            if Current_Token = Tok_Left_Bracket then
               Set_Number (Res, Parse_Bracket_Number);
            end if;
            Set_Property (Res, Parse_Parenthesis_FL_Property);
         when Tok_Next_Event_A =>
            Res := Parse_Boolean_Range_Property (N_Next_Event_A);
         when Tok_Next_Event_E =>
            Res := Parse_Boolean_Range_Property (N_Next_Event_E);
         when Tok_Left_Paren =>
            return Parse_Parenthesis_FL_Property;
         when Tok_Left_Curly =>
            Res := Parse_Psl_Sequence (True);
            if Get_Kind (Res) = N_Braced_SERE
              and then Current_Token = Tok_Left_Paren
            then
               --  FIXME: must check that RES is really a sequence
               --  (and not a SERE).
               Tmp := Create_Node_Loc (N_Overlap_Imp_Seq);
               Set_Sequence (Tmp, Res);
               Set_Property (Tmp, Parse_Parenthesis_FL_Property);
               Res := Tmp;
            end if;
         when others =>
            Res := Parse_Psl_Sequence (False);
      end case;
      return Res;
   end Parse_FL_Property_1;

   function Parse_St_Binary_FL_Property (K : Nkind; Left : Node) return Node is
      Res : Node;
   begin
      Res := Create_Node_Loc (K);
      Set_Strong_Flag (Res, Scan_Exclam_Mark);
      Set_Inclusive_Flag (Res, Scan_Underscore);
      Scan;
      Set_Left (Res, Left);
      Set_Right (Res, Parse_FL_Property (Prio_FL_Bounding));
      return Res;
   end Parse_St_Binary_FL_Property;

   function Parse_Binary_FL_Property (K : Nkind; Left : Node; Prio : Priority)
                                     return Node
   is
      Res : Node;
   begin
      Res := Create_Node_Loc (K);
      Scan;
      Set_Left (Res, Left);
      Set_Right (Res, Parse_FL_Property (Prio));
      return Res;
   end Parse_Binary_FL_Property;

   function Parse_FL_Property (Prio : Priority) return Node
   is
      Res : Node;
      N : Node;
   begin
      Res := Parse_FL_Property_1;
      loop
         case Current_Token is
            when Tok_Minus_Greater =>
               if Prio > Prio_Bool_Imp then
                  return Res;
               end if;
               N := Create_Node_Loc (N_Log_Imp_Prop);
               Set_Left (N, Res);
               Scan;
               Set_Right (N, Parse_FL_Property (Prio_Bool_Imp));
               Res := N;
            when Tok_Bar_Arrow =>
               if Prio > Prio_Seq_Imp then
                  return Res;
               end if;
               N := Create_Node_Loc (N_Overlap_Imp_Seq);
               Set_Sequence (N, Res);
               Scan;
               Set_Property (N, Parse_FL_Property (Prio_Seq_Imp));
               Res := N;
            when Tok_Bar_Double_Arrow =>
               if Prio > Prio_Seq_Imp then
                  return Res;
               end if;
               N := Create_Node_Loc (N_Imp_Seq);
               Set_Sequence (N, Res);
               Scan;
               Set_Property (N, Parse_FL_Property (Prio_Seq_Imp));
               Res := N;
            when Tok_Abort =>
               if Prio > Prio_FL_Abort then
                  return Res;
               end if;
               N := Create_Node_Loc (N_Abort);
               Set_Property (N, Res);
               Scan;
               Set_Boolean (N, Parse_Boolean (Prio_Lowest));
               --  Left associative.
               return N;
            when Tok_Exclam_Mark =>
               N := Create_Node_Loc (N_Strong);
               Set_Property (N, Res);
               Scan;
               Res := N;
            when Tok_Until =>
               if Prio > Prio_FL_Bounding then
                  return Res;
               end if;
               Res := Parse_St_Binary_FL_Property (N_Until, Res);
            when Tok_Before =>
               if Prio > Prio_FL_Bounding then
                  return Res;
               end if;
               Res := Parse_St_Binary_FL_Property (N_Before, Res);
            when Tok_Or =>
               if Prio > Prio_Seq_Or then
                  return Res;
               end if;
               Res := Parse_Binary_FL_Property (N_Or_Prop, Res, Prio_Seq_Or);
            when Tok_And =>
               if Prio > Prio_Seq_And then
                  return Res;
               end if;
               Res := Parse_Binary_FL_Property (N_And_Prop, Res, Prio_Seq_And);
            when Token_Relational_Operator_Type =>
               return Vhdl_To_Psl
                 (Parse.Parse_Binary_Expression
                    (Psl_To_Vhdl (Res), Parse.Prio_Relation));
            when Tok_Colon
              | Tok_Bar
              | Tok_Ampersand
              | Tok_And_And =>
               Error_Msg_Parse ("SERE operator '" & Image (Current_Token)
                                  & "' is not allowed in property");
               Scan;
               N := Parse_FL_Property (Prio_Lowest);
               return Res;
            when Tok_Arobase =>
               if Prio > Prio_Clock_Event then
                  return Res;
               end if;
               N := Create_Node_Loc (N_Clock_Event);
               Set_Property (N, Res);
               Scan;
               Set_Boolean (N, Parse_Boolean (Prio_Clock_Event));
               Res := N;
            when others =>
               return Res;
         end case;
      end loop;
   end Parse_FL_Property;

   function Parse_Psl_Property return PSL_Node is
   begin
      return Parse_FL_Property (Prio_Lowest);
   end Parse_Psl_Property;

   --  precond:  identifier
   --  postcond: ';'
   --
   --  6.2.4.1  Property declaration
   --
   --  Property_Declaration ::=
   --     PROPERTY psl_identifier [ ( Formal_Parameter_List ) ] DEF_SYM
   --        property ;
   function Parse_Psl_Declaration (Tok : Token_Type) return PSL_Node
   is
      Res : Node;
      Param : Node;
      Last_Param : Node;
      Pkind : Nkind;
      Kind : Nkind;
   begin
      case Tok is
         when Tok_Psl_Property =>
            Kind := N_Property_Declaration;
         when Tok_Psl_Sequence =>
            Kind := N_Sequence_Declaration;
         when Tok_Psl_Endpoint =>
            Kind := N_Endpoint_Declaration;
         when others =>
            raise Internal_Error;
      end case;
      Res := Create_Node_Loc (Kind);
      if Current_Token = Tok_Identifier then
         Set_Identifier (Res, Current_Identifier);
         Scan;
      end if;

      --  Formal parameter list.
      if Current_Token = Tok_Left_Paren then
         Last_Param := Null_Node;
         loop
            --  precond: '(' or ';'.
            Scan;
            case Current_Token is
               when Tok_Psl_Const =>
                  Pkind := N_Const_Parameter;
               when Tok_Psl_Boolean =>
                  Pkind := N_Boolean_Parameter;
               when Tok_Psl_Property =>
                  Pkind := N_Property_Parameter;
               when Tok_Psl_Sequence =>
                  Pkind := N_Sequence_Parameter;
               when others =>
                  Error_Msg_Parse ("parameter type expected");
            end case;

            --  Formal parameters.
            loop
               --  precond: parameter_type or ','
               Scan;
               Param := Create_Node_Loc (Pkind);
               if Current_Token /= Tok_Identifier then
                  Error_Msg_Parse ("identifier for parameter expected");
               else
                  Set_Identifier (Param, Current_Identifier);
               end if;
               if Last_Param = Null_Node then
                  Set_Parameter_List (Res, Param);
               else
                  Set_Chain (Last_Param, Param);
               end if;
               Last_Param := Param;
               Scan;
               exit when Current_Token /= Tok_Comma;
            end loop;
            exit when Current_Token = Tok_Right_Paren;
            if Current_Token /= Tok_Semi_Colon then
               Error_Msg_Parse ("';' expected between formal parameter");
            end if;

         end loop;
         Scan;
      end if;

      if Current_Token /= Tok_Is then
         Error_Msg_Parse ("'is' expected after identifier");
      else
         --  Skip 'is'.
         Scan;
      end if;
      case Kind is
         when N_Property_Declaration =>
            Set_Property (Res, Parse_Psl_Property);
         when N_Sequence_Declaration
           | N_Endpoint_Declaration =>
            Set_Sequence (Res, Parse_Psl_Sequence (True));
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Parse_Psl_Declaration;

   function Is_Instantiated_Declaration (N : PSL_Node) return Boolean is
   begin
      return Get_Parameter_List (N) = Null_Node;
   end Is_Instantiated_Declaration;
end Parse_Psl;
