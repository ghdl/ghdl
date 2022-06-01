--  VHDL PSL parser.
--  Copyright (C) 2009 Tristan Gingold
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
with Errorout; use Errorout;
with PSL.Nodes; use PSL.Nodes;
with Vhdl.Nodes;
with Vhdl.Scanner; use Vhdl.Scanner;
with PSL.Errors; use PSL.Errors;
with PSL.Priorities; use PSL.Priorities;
with Vhdl.Parse;

package body Vhdl.Parse_Psl is
   subtype Vhdl_Node is Vhdl.Nodes.Iir;

   procedure Error_Msg_Parse (Msg: String) is
   begin
      Report_Msg (Msgid_Error, Errorout.Parse, Get_Token_Coord, Msg);
   end Error_Msg_Parse;

   procedure Error_Msg_Parse
     (Loc : Location_Type; Msg: String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Errorout.Parse, +Loc, Msg, Args);
   end Error_Msg_Parse;

   function Create_Node_Loc (K : Nkind) return Node
   is
      Res : Node;
   begin
      Res := PSL.Nodes.Create_Node (K);
      Set_Location (Res, Get_Token_Location);
      return Res;
   end Create_Node_Loc;

   function Parse_Number return Node
   is
      V : Int64;
      Res : Node;
   begin
      if Current_Token = Tok_Integer then
         Res := Create_Node_Loc (N_Number);
         --  FIXME: handle overflow.
         V := Current_Iir_Int64;
         if V > Int64 (Uns32'Last) then
            Error_Msg_Parse ("number if too large");
            V := Int64 (Uns32'Last);
         end if;
         Set_Value (Res, Uns32 (V));
         Scan;
         return Res;
      elsif Current_Token = Tok_Inf then
         Res := Create_Node_Loc (N_Inf);
         Scan;
         return Res;
      else
         Error_Msg_Parse ("number expected");
         return Null_Node;
      end if;
   end Parse_Number;

   procedure Check_Positive_Count (N : Node)
   is
      Low_B : constant Node := Get_Low_Bound (N);
      High_B : constant Node := Get_High_Bound (N);
      Low  : constant Uns32 := Get_Value (Low_B);
      High : Uns32;
   begin
      if Get_Kind (High_B) = N_Inf then
         return;
      end if;

      High := Get_Value (High_B);
      if Low > High then
         Error_Msg_Parse
           ("Low bound of range must be lower than High bound," &
              " actual range is:" &
              Uns32'Image (Low) & " to" & Uns32'Image (High));
      end if;
   end Check_Positive_Count;

   procedure Parse_Count (N : Node)
   is
      Hi : Node;
   begin
      Set_Low_Bound (N, Parse_Number);
      if Current_Token = Tok_To then
         Scan;
         Hi := Parse_Number;
         Set_High_Bound (N, Hi);
         if Hi /= Null_Node then
            Check_Positive_Count (N);
         end if;
      end if;
   end Parse_Count;

   function Psl_To_Vhdl (N : Node) return Vhdl_Node;

   function Binary_Psl_Operator_To_Vhdl (N : Node; Kind : Vhdl.Nodes.Iir_Kind)
                                        return Vhdl_Node
   is
      use Vhdl.Nodes;
      Res : Iir;
   begin
      Res := Create_Iir (Kind);
      Set_Location (Res, Get_Location (N));
      Set_Left (Res, Psl_To_Vhdl (Get_Left (N)));
      Set_Right (Res, Psl_To_Vhdl (Get_Right (N)));
      return Res;
   end Binary_Psl_Operator_To_Vhdl;

   function Psl_To_Vhdl (N : Node) return Vhdl_Node
   is
      use Vhdl.Nodes;
      Res : Iir;
   begin
      case Get_Kind (N) is
         when N_HDL_Expr =>
            Res := Vhdl_Node (Get_HDL_Node (N));
         when N_And_Prop =>
            Res := Binary_Psl_Operator_To_Vhdl (N, Iir_Kind_And_Operator);
         when N_Or_Prop =>
            Res := Binary_Psl_Operator_To_Vhdl (N, Iir_Kind_Or_Operator);
         when N_Paren_Prop =>
            Res := Create_Iir (Iir_Kind_Parenthesis_Expression);
            Set_Location (Res, Get_Location (N));
            Set_Expression (Res, Psl_To_Vhdl (Get_Property (N)));
         when others =>
            Error_Msg_Parse
              (+N, "PSL construct not allowed as VHDL expression");
            Res := Create_Iir (Iir_Kind_Error);
            Set_Location (Res, Get_Location (N));
      end case;
      Free_Node (N);
      return Res;
   end Psl_To_Vhdl;

   function Vhdl_To_Psl (N : Vhdl_Node) return Node
   is
      use Vhdl.Nodes;
      Res : PSL_Node;
   begin
      Res := Create_Node_Loc (N_HDL_Expr);
      if N /= Null_Iir then
         Set_Location (Res, Get_Location (N));
         Set_HDL_Node (Res, Int32 (N));
      end if;
      return Res;
   end Vhdl_To_Psl;

   function Parse_Psl_Sequence_Or_SERE (Full_Hdl_Expr : Boolean) return Node;
   function Parse_FL_Property (Prio : Priority) return Node;
   function Parse_Parenthesis_Boolean return Node;
   function Parse_Boolean (Parent_Prio : Priority) return Node;

   function Parse_Unary_Boolean (Full_Hdl_Expr : Boolean) return Node
   is
      use Parse;
      use Vhdl.Nodes;
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

   --  A.4.5 Sequential Extended Regular Expressions (SEREs)
   --  SERE ::=
   --      Boolean
   --    | Sequence
   --    | SERE ; SERE
   --    | SERE : SERE
   --    | Compound_SERE
   function Parse_SERE (Prio : Priority) return Node
   is
      Left, Res : Node;
      Kind : Nkind;
      Op_Prio : Priority;
   begin
      Left := Parse_Psl_Sequence_Or_SERE (True);
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

   --  A.4.7 Sequences
   --  Braced_SERE ::=
   --    { SERE }
   --
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
   function Parse_Brack_Star (Seq : Node) return Node
   is
      Res : Node;
   begin
      Res := Create_Node_Loc (N_Star_Repeat_Seq);
      Set_Sequence (Res, Seq);

      --  Skip '[->'
      Scan;

      if Current_Token /= Tok_Right_Bracket then
         Parse_Count (Res);
      end if;
      if Current_Token /= Tok_Right_Bracket then
         Error_Msg_Parse ("missing ']'");
      else
         Scan;
      end if;
      return Res;
   end Parse_Brack_Star;

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
         Check_Positive_Count(N);
      end if;
   end Parse_Bracket_Range;

   function Parse_Bracket_Number return Node
   is
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

   function Parse_Brack_Equal (Left : Node) return Node
   is
      Res : Node;
   begin
      Res := Create_Node_Loc (N_Equal_Repeat_Seq);
      Set_Boolean (Res, Left);

      --  Skip '[='
      Scan;
      Parse_Count (Res);
      if Current_Token /= Tok_Right_Bracket then
         Error_Msg_Parse ("missing ']'");
      else
         Scan;
      end if;
      return Res;
   end Parse_Brack_Equal;

   function Parse_Brack_Arrow (Left : Node) return Node
   is
      Res : Node;
   begin
      Res := Create_Node_Loc (N_Goto_Repeat_Seq);
      Set_Boolean (Res, Left);

      --  Skip '[->'
      Scan;
      if Current_Token /= Tok_Right_Bracket then
         Parse_Count (Res);
      end if;
      if Current_Token /= Tok_Right_Bracket then
         Error_Msg_Parse ("missing ']'");
      else
         Scan;
      end if;
      return Res;
   end Parse_Brack_Arrow;

   --  Parse:
   --     Boolean [= Count ]
   --   | Boolean [-> [ positive_Count ] ]
   --   | Boolean
   --  Where LEFT is the boolean expression
   function Parse_Boolean_Repeated_Sequence (Left : Node) return Node is
   begin
      case Current_Token is
         when Tok_Brack_Equal =>
            return Parse_Brack_Equal (Left);
         when Tok_Brack_Arrow =>
            return Parse_Brack_Arrow (Left);
         when others =>
            return Left;
      end case;
   end Parse_Boolean_Repeated_Sequence;

   --  Parse:
   --     Boolean [* [ Count ] ]
   --   | Sequence [* [ Count ] ]
   --   | Boolean [+]
   --   | Sequence [+]
   --  Where LEFT is a boolean expression or a sequence
   function Parse_Sequence_Repeated_Sequence (Left : Node) return Node
   is
      Res : Node;
      N : Node;
   begin
      Res := Left;
      loop
         case Current_Token is
            when Tok_Brack_Star =>
               Res := Parse_Brack_Star (Res);
            when Tok_Brack_Plus_Brack =>
               N := Create_Node_Loc (N_Plus_Repeat_Seq);
               Set_Sequence (N, Res);

               --  Skip '[+]'
               Scan;
               Res := N;
            when Tok_Brack_Arrow =>
               Error_Msg_Parse ("'[->' not allowed on a SERE");
               Res := Parse_Brack_Arrow (Res);
            when Tok_Brack_Equal =>
               Error_Msg_Parse ("'[=' not allowed on a SERE");
               Res := Parse_Brack_Equal (Res);
            when others =>
               exit;
         end case;
      end loop;
      return Res;
   end Parse_Sequence_Repeated_Sequence;

   function Parse_Psl_Sequence_Or_SERE (Full_Hdl_Expr : Boolean) return Node
   is
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
            return Parse_Brack_Star (Null_Node);
         when Tok_Left_Paren =>
            if Parse.Flag_Parse_Parenthesis then
               Res := Create_Node_Loc (N_Paren_Bool);
               --  Skip '('.
               Scan;
               Set_Boolean (Res, Parse_Psl_Boolean);
               if Current_Token = Tok_Right_Paren then
                  Scan;
               else
                  Error_Msg_Parse ("missing matching ')'");
               end if;
            else
               Res := Parse_Parenthesis_Boolean;
            end if;
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

            Res := Parse_Boolean_Repeated_Sequence (Res);
      end case;

      Res := Parse_Sequence_Repeated_Sequence (Res);

      return Res;
   end Parse_Psl_Sequence_Or_SERE;

   --  IEEE1850 A.4.7 Sequences
   --  Sequence ::=
   --      Sequence_Instance
   --    | Repeated_SERE
   --    | Braced_SERE
   --    | Clocked_SERE
   function Parse_Psl_Sequence return Node
   is
      Res : Node;
   begin
      Res := Parse_Psl_Sequence_Or_SERE (True);

      --  May not be a sequence!
      --  This test is also performed in sem_psl in order to fully handle
      --  sequence_instance.
      case Get_Kind (Res) is
         when N_Star_Repeat_Seq
           |  N_Goto_Repeat_Seq
           |  N_Plus_Repeat_Seq
           |  N_Equal_Repeat_Seq
           |  N_Braced_SERE
           |  N_Clocked_SERE =>
            null;
         when N_HDL_Expr =>
            --  Need to be checked later: can be a sequence instance or a
            --  boolean.
            null;
         when others =>
            Error_Msg_Parse ("sequence expected here");
      end case;

      return Res;
   end Parse_Psl_Sequence;

   --  precond:  '('
   --  postcond: next token
   function Parse_Parenthesis_FL_Property return Node
   is
      Prop : Node;
      Res : Node;
      Loc : Location_Type;
   begin
      Loc := Get_Token_Location;
      if Current_Token /= Tok_Left_Paren then
         Error_Msg_Parse ("'(' expected around property");
         return Parse_FL_Property (Prio_Lowest);
      else
         if Parse.Flag_Parse_Parenthesis then
            Res := Create_Node_Loc (N_Paren_Prop);
         end if;

         --  Skip '('.
         Scan;

         Prop := Parse_FL_Property (Prio_Lowest);

         if Current_Token = Tok_Right_Paren then
            --  Skip ')'.
            Scan;
         else
            Error_Msg_Parse ("missing matching ')' for '(' at line "
                               & Image (Loc, False));
         end if;

         if Get_Kind (Prop) = N_HDL_Expr then
            declare
               N : Vhdl_Node;
            begin
               N := Psl_To_Vhdl (Prop);
               N := Parse.Parse_Binary_Expression (N, Parse.Prio_Expression);
               Prop := Vhdl_To_Psl (N);
            end;
         end if;

         if Parse.Flag_Parse_Parenthesis then
            Set_Property (Res, Prop);
            return Res;
         else
            return Prop;
         end if;
      end if;
   end Parse_Parenthesis_FL_Property;

   --  Parse '[' finite_Range ']' '(' FL_Property ')'
   function Parse_Range_Property (K : Nkind; Strong : Boolean) return Node
   is
      Res : Node;
   begin
      Res := Create_Node_Loc (K);
      Set_Strong_Flag (Res, Strong);
      Scan;
      Parse_Bracket_Range (Res);
      Set_Property (Res, Parse_Parenthesis_FL_Property);
      return Res;
   end Parse_Range_Property;

   --  Parse '(' Boolean ')' '[' Range ']' '(' FL_Property ')'
   function Parse_Boolean_Range_Property (K : Nkind; Strong : Boolean)
                                         return Node
   is
      Res : Node;
   begin
      Res := Create_Node_Loc (K);
      Set_Strong_Flag (Res, Strong);
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
         when Tok_Eventually_Em =>
            Res := Create_Node_Loc (N_Eventually);
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
            Res := Parse_Range_Property (N_Next_A, False);
         when Tok_Next_A_Em =>
            Res := Parse_Range_Property (N_Next_A, True);
         when Tok_Next_E =>
            Res := Parse_Range_Property (N_Next_E, False);
         when Tok_Next_E_Em =>
            Res := Parse_Range_Property (N_Next_E, True);
         when Tok_Next_Event =>
            Res := Create_Node_Loc (N_Next_Event);
            Scan;
            Set_Boolean (Res, Parse_Parenthesis_Boolean);
            if Current_Token = Tok_Left_Bracket then
               Set_Number (Res, Parse_Bracket_Number);
            end if;
            Set_Property (Res, Parse_Parenthesis_FL_Property);
         when Tok_Next_Event_A =>
            Res := Parse_Boolean_Range_Property (N_Next_Event_A, False);
         when Tok_Next_Event_A_Em =>
            Res := Parse_Boolean_Range_Property (N_Next_Event_A, True);
         when Tok_Next_Event_E =>
            Res := Parse_Boolean_Range_Property (N_Next_Event_E, False);
         when Tok_Next_Event_E_Em =>
            Res := Parse_Boolean_Range_Property (N_Next_Event_E, True);
         when Tok_Left_Paren =>
            Res := Parse_Parenthesis_FL_Property;
            if Get_Kind (Res) = N_HDL_Expr then
               --  Might be a boolean expression followed by a SERE repeatition
               Res := Parse_Boolean_Repeated_Sequence (Res);
               Res := Parse_Sequence_Repeated_Sequence (Res);
               --  TODO: can be then a SERE (: ; | & && within)
            end if;
         when Tok_Left_Curly =>
            Res := Parse_Psl_Sequence_Or_SERE (True);
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
            Res := Parse_Psl_Sequence_Or_SERE (False);
      end case;
      return Res;
   end Parse_FL_Property_1;

   function Parse_St_Binary_FL_Property
     (K : Nkind; Left : Node; Strong : Boolean; Inclusive : Boolean)
     return Node
   is
      Res : Node;
   begin
      Res := Create_Node_Loc (K);
      Set_Strong_Flag (Res, Strong);
      Set_Inclusive_Flag (Res, Inclusive);
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

   --  During LR parsing, phrases before |-> and |=> are parsed as properties,
   --  but they are in fact sequences.  Convert them (in particular the
   --  boolean operators need to be rewritten).
   function Property_To_Sequence (N : Node) return Node
   is
      procedure Rewrite_Binary (Res : Node; N : Node) is
      begin
         Set_Location (Res, Get_Location (N));
         Set_Left (Res, Property_To_Sequence (Get_Left (N)));
         Set_Right (Res, Property_To_Sequence (Get_Right (N)));
         Free_Node (N);
      end Rewrite_Binary;
      Res : Node;
   begin
      case Get_Kind (N) is
         when N_Sequence_Instance
            | N_Star_Repeat_Seq
            | N_Plus_Repeat_Seq
            | N_Equal_Repeat_Seq
            | N_Goto_Repeat_Seq
            | N_Braced_SERE
            | N_Clocked_SERE =>
            return N;
         when N_And_Prop =>
            Res := Create_Node (N_And_Seq);
            Rewrite_Binary (Res, N);
            return Res;
         when N_Or_Prop =>
            Res := Create_Node (N_Or_Seq);
            Rewrite_Binary (Res, N);
            return Res;
         when N_Before =>
            Set_Left (N, Property_To_Sequence (Get_Left (N)));
            Set_Right (N, Property_To_Sequence (Get_Right (N)));
            return N;
         when N_Clock_Event
            | N_Always
            | N_Never
            | N_Eventually
            | N_Until
            | N_Property_Parameter
            | N_Property_Instance
            | N_Endpoint_Instance
            | N_Strong
            | N_Abort
            | N_Async_Abort
            | N_Sync_Abort
            | N_Next_Event_E
            | N_Next_Event_A
            | N_Next_Event
            | N_Next_E
            | N_Next_A
            | N_Next
            | N_Log_Imp_Prop
            | N_Log_Equiv_Prop
            | N_Paren_Prop =>
            Error_Msg_Parse (+N, "construct not allowed in sequences");
            return N;
         when N_Const_Parameter
            | N_Boolean_Parameter
            | N_Sequence_Parameter
            | N_Actual
            | N_And_Seq
            | N_Or_Seq
            | N_Imp_Seq
            | N_Equiv_Bool
            | N_Overlap_Imp_Seq
            | N_Match_And_Seq
            | N_Imp_Bool
            | N_Or_Bool
            | N_And_Bool
            | N_Not_Bool
            | N_Paren_Bool
            | N_Fusion_SERE
            | N_HDL_Expr
            | N_HDL_Bool
            | N_Hdl_Mod_Name
            | N_Concat_SERE
            | N_Within_SERE
            | N_False
            | N_True
            | N_Number
            | N_Inf
            | N_Name_Decl
            | N_Name
            | N_EOS
            | N_Error =>
            return N;
         when N_Vmode
           | N_Vunit
           | N_Vprop
           | N_Assert_Directive
           | N_Property_Declaration
           | N_Sequence_Declaration
           | N_Endpoint_Declaration =>
            raise Internal_Error;
      end case;
   end Property_To_Sequence;

   function Parse_Abort (Kind : Nkind; Left : Node) return Node
   is
      N : Node;
   begin
      N := Create_Node_Loc (Kind);
      Set_Property (N, Left);
      Scan;
      Set_Boolean (N, Parse_Boolean (Prio_Lowest));
      --  Left associative.
      return N;
   end Parse_Abort;

   --  A.4.4 PSL properties
   --  FL_Property::=
   --      Boolean
   --    | ( FL_Property )
   --    | Sequence [ ! ]
   --    | FL_property_name [ ( Actual_Parameter_List ) ]
   --    | FL_Property @ Clock_Expression
   --    | FL_Property abort Boolean
   --    | FL_Property async_abort Boolean
   --    | FL_Property sync_abort Boolean
   --    | Parameterized_Property
   --    | NOT_OP FL_Property
   --    | FL_Property AND_OP FL_Property
   --    | FL_Property OR_OP FL_Property
   --    | FL_Property -> FL_Property
   --    | FL_Property <-> FL_Property
   --    | always FL_Property
   --    | never FL_Property
   --    | next FL_Property
   --    | next! FL_Property
   --    | eventually! FL_Property
   --    | FL_Property until! FL_Property
   --    | FL_Property until FL_Property
   --    | FL_Property until!_ FL_Property
   --    | FL_Property until_ FL_Property
   --    | FL_Property before! FL_Property
   --    | FL_Property before FL_Property
   --    | FL_Property before!_ FL_Property
   --    | FL_Property before_ FL_Property
   --    | next [ Number ] ( FL_Property )
   --    | next! [ Number ] ( FL_Property )
   --    | next_a [ finite_Range ] ( FL_Property )
   --    | next_a! [ finite_Range ] ( FL_Property )
   --    | next_e [ finite_Range ] ( FL_Property )
   --    | next_e! [ finite_Range ] ( FL_Property )
   --    | next_event! ( Boolean ) ( FL_Property )
   --    | next_event ( Boolean ) ( FL_Property )
   --    | next_event! ( Boolean ) [ positive_Number ] ( FL_Property )
   --    | next_event ( Boolean ) [ positive_Number ] ( FL_Property )
   --    | next_event_a! ( Boolean ) [ finite_positive_Range ] ( FL_Property )
   --    | next_event_a ( Boolean ) [ finite_positive_Range ] ( FL_Property )
   --    | next_event_e! ( Boolean ) [ finite_positive_Range ] ( FL_Property )
   --    | next_event_e ( Boolean ) [ finite_positive_Range ] ( FL_Property )
   --    | { SERE } ( FL_Property )
   --    | Sequence |-> FL_Property
   --    | Sequence |=> FL_Property
   function Parse_FL_Property (Prio : Priority) return Node
   is
      Res : Node;
      N : Node;
   begin
      Res := Parse_FL_Property_1;
      loop
         case Current_Token is
            when Tok_Minus_Greater =>
               --   ->
               if Prio > Prio_Bool_Imp then
                  return Res;
               end if;
               N := Create_Node_Loc (N_Log_Imp_Prop);
               Set_Left (N, Res);
               Scan;
               Set_Right (N, Parse_FL_Property (Prio_Bool_Imp));
               Res := N;
            when Tok_Equiv_Arrow =>
               if Prio > Prio_Bool_Imp then
                  return Res;
               end if;
               N := Create_Node_Loc (N_Log_Equiv_Prop);
               Set_Left (N, Res);
               Scan;
               Set_Right (N, Parse_FL_Property (Prio_Bool_Imp));
               Res := N;
            when Tok_Bar_Arrow =>
               if Prio > Prio_Seq_Imp then
                  return Res;
               end if;
               N := Create_Node_Loc (N_Overlap_Imp_Seq);
               Set_Sequence (N, Property_To_Sequence (Res));
               Scan;
               Set_Property (N, Parse_FL_Property (Prio_Seq_Imp));
               Res := N;
            when Tok_Bar_Double_Arrow =>
               if Prio > Prio_Seq_Imp then
                  return Res;
               end if;
               N := Create_Node_Loc (N_Imp_Seq);
               Set_Sequence (N, Property_To_Sequence (Res));
               Scan;
               Set_Property (N, Parse_FL_Property (Prio_Seq_Imp));
               Res := N;
            when Tok_Abort =>
               if Prio > Prio_FL_Abort then
                  return Res;
               end if;
               return Parse_Abort (N_Abort, Res);
            when Tok_Sync_Abort =>
               if Prio > Prio_FL_Abort then
                  return Res;
               end if;
               return Parse_Abort (N_Sync_Abort, Res);
            when Tok_Async_Abort =>
               if Prio > Prio_FL_Abort then
                  return Res;
               end if;
               return Parse_Abort (N_Async_Abort, Res);
            when Tok_Exclam_Mark =>
               N := Create_Node_Loc (N_Strong);
               Set_Property (N, Res);
               Scan;
               Res := N;
            when Tok_Until =>
               if Prio > Prio_FL_Bounding then
                  return Res;
               end if;
               Res := Parse_St_Binary_FL_Property (N_Until, Res, False, False);
            when Tok_Until_Em =>
               if Prio > Prio_FL_Bounding then
                  return Res;
               end if;
               Res := Parse_St_Binary_FL_Property (N_Until, Res, True, False);
            when Tok_Until_Un =>
               if Prio > Prio_FL_Bounding then
                  return Res;
               end if;
               Res := Parse_St_Binary_FL_Property (N_Until, Res, False, True);
            when Tok_Until_Em_Un =>
               if Prio > Prio_FL_Bounding then
                  return Res;
               end if;
               Res := Parse_St_Binary_FL_Property (N_Until, Res, True, True);
            when Tok_Before =>
               if Prio > Prio_FL_Bounding then
                  return Res;
               end if;
               Res := Parse_St_Binary_FL_Property
                 (N_Before, Res, False, False);
            when Tok_Before_Em =>
               if Prio > Prio_FL_Bounding then
                  return Res;
               end if;
               Res := Parse_St_Binary_FL_Property (N_Before, Res, True, False);
            when Tok_Before_Un =>
               if Prio > Prio_FL_Bounding then
                  return Res;
               end if;
               Res := Parse_St_Binary_FL_Property (N_Before, Res, False, True);
            when Tok_Before_Em_Un =>
               if Prio > Prio_FL_Bounding then
                  return Res;
               end if;
               Res := Parse_St_Binary_FL_Property (N_Before, Res, True, True);
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

   --  A.4.4 PSL properties
   --  Property ::=
   --      FL_Property
   --    | ...
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
         when Tok_Property =>
            Kind := N_Property_Declaration;
         when Tok_Sequence =>
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
               when Tok_Property =>
                  Pkind := N_Property_Parameter;
               when Tok_Sequence =>
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
            Set_Sequence (Res, Parse_Psl_Sequence);
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Parse_Psl_Declaration;

   function Is_Instantiated_Declaration (N : PSL_Node) return Boolean is
   begin
      return Get_Parameter_List (N) = Null_Node;
   end Is_Instantiated_Declaration;
end Vhdl.Parse_Psl;
