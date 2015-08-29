--  VHDL parser.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Iir_Chains; use Iir_Chains;
with Ada.Text_IO; use Ada.Text_IO;
with Types; use Types;
with Tokens; use Tokens;
with Scanner; use Scanner;
with Iirs_Utils; use Iirs_Utils;
with Errorout; use Errorout;
with Std_Names; use Std_Names;
with Flags; use Flags;
with Parse_Psl;
with Name_Table;
with Str_Table;
with Files_Map; use Files_Map;
with Xrefs;

--  Recursive descendant parser.
--  Each subprogram (should) parse one production rules.
--  Rules are written in a comment just before the subprogram.
--  terminals are written in upper case.
--  non-terminal are written in lower case.
--  syntaxic category of a non-terminal are written in upper case.
--  eg: next_statement ::= [ label : ] NEXT [ LOOP_label ] [ WHEN condition ] ;
--  Or (|) must be aligned by the previous or, or with the '=' character.
--  Indentation is 4.
--
--  To document what is expected for input and what is left as an output
--  concerning token stream, a precond and a postcond comment shoud be
--  added before the above rules.
--    a token (such as IF or ';') means the current token is this token.
--    'a token' means the current token was analysed.
--    'next token' means the current token is to be analysed.


package body Parse is

   -- current_token must be valid.
   -- Leaves a token.
   function Parse_Simple_Expression (Primary : Iir := Null_Iir)
                                    return Iir_Expression;
   function Parse_Primary return Iir_Expression;
   function Parse_Use_Clause return Iir_Use_Clause;

   function Parse_Association_List return Iir;
   function Parse_Association_List_In_Parenthesis return Iir;

   function Parse_Sequential_Statements (Parent : Iir) return Iir;
   function Parse_Configuration_Item return Iir;
   function Parse_Block_Configuration return Iir_Block_Configuration;
   procedure Parse_Concurrent_Statements (Parent : Iir);
   function Parse_Subprogram_Declaration (Parent : Iir) return Iir;
   function Parse_Subtype_Indication (Name : Iir := Null_Iir) return Iir;
   procedure Parse_Component_Specification (Res : Iir);
   function Parse_Binding_Indication return Iir_Binding_Indication;
   function Parse_Aggregate return Iir;
   function Parse_Signature return Iir_Signature;
   procedure Parse_Declarative_Part (Parent : Iir);
   function Parse_Tolerance_Aspect_Opt return Iir;

   Expect_Error: exception;

   -- Copy the current location into an iir.
   procedure Set_Location (Node : Iir) is
   begin
      Set_Location (Node, Get_Token_Location);
   end Set_Location;

   procedure Set_End_Location (Node : Iir) is
   begin
      Set_End_Location (Node, Get_Token_Location);
   end Set_End_Location;

   procedure Unexpected (Where: String) is
   begin
      Error_Msg_Parse
        ("unexpected token '" & Image (Current_Token) & "' in a " & Where);
   end Unexpected;

--   procedure Unexpected_Eof is
--   begin
--      Error_Msg_Parse ("unexpected end of file");
--   end Unexpected_Eof;

   --  Emit an error if the current_token if different from TOKEN.
   --  Otherwise, accept the current_token (ie set it to tok_invalid, unless
   --  TOKEN is Tok_Identifier).
   procedure Expect (Token: Token_Type; Msg: String := "") is
   begin
      if Current_Token /= Token then
         if Msg'Length > 0 then
            Error_Msg_Parse (Msg);
            Error_Msg_Parse ("(found: " & Image (Current_Token) & ")");
         else
            Error_Msg_Parse
              (''' & Image(Token) & "' is expected instead of '"
               & Image (Current_Token) & ''');
         end if;
         raise Expect_Error;
      end if;

      -- Accept the current_token.
      if Current_Token /= Tok_Identifier then
         Invalidate_Current_Token;
      end if;
   exception
      when Parse_Error =>
         Put_Line ("found " & Token_Type'Image (Current_Token));
         if Current_Token = Tok_Identifier then
            Put_Line ("identifier: " & Name_Table.Image (Current_Identifier));
         end if;
         raise;
   end Expect;

   --  Scan a token and expect it.
   procedure Scan_Expect (Token: Token_Type; Msg: String := "") is
   begin
      Scan;
      Expect (Token, Msg);
   end Scan_Expect;

   --  If the current_token is an identifier, it must be equal to name.
   --  In this case, a token is eaten.
   --  If the current_token is not an identifier, this is a noop.
   procedure Check_End_Name (Name : Name_Id; Decl : Iir) is
   begin
      if Current_Token /= Tok_Identifier then
         return;
      end if;
      if Name = Null_Identifier then
         Error_Msg_Parse
           ("end label for an unlabeled declaration or statement");
      else
         if Current_Identifier /= Name then
            Error_Msg_Parse
              ("mispelling, """ & Name_Table.Image (Name) & """ expected");
         else
            Set_End_Has_Identifier (Decl, True);
            Xrefs.Xref_End (Get_Token_Location, Decl);
         end if;
      end if;

      --  Skip identifier (the label).
      Scan;
   end Check_End_Name;

   procedure Check_End_Name (Decl : Iir) is
   begin
      Check_End_Name (Get_Identifier (Decl), Decl);
   end Check_End_Name;


   --  Expect ' END tok [ name ] ; '
   procedure Check_End_Name (Tok : Token_Type; Decl : Iir) is
   begin
      if Current_Token /= Tok_End then
         Error_Msg_Parse ("""end " & Image (Tok) & ";"" expected");
      else
         Scan;
         if Current_Token /= Tok then
            Error_Msg_Parse
              ("""end"" must be followed by """ & Image (Tok) & """");
         else
            Set_End_Has_Reserved_Id (Decl, True);
            Scan;
         end if;
         Check_End_Name (Decl);
         Expect (Tok_Semi_Colon);
      end if;
   end Check_End_Name;

   procedure Eat_Tokens_Until_Semi_Colon is
   begin
      loop
         case Current_Token is
            when Tok_Semi_Colon
              | Tok_Eof =>
               exit;
            when others =>
               Scan;
         end case;
      end loop;
   end Eat_Tokens_Until_Semi_Colon;

   --  Expect and scan ';' emit an error message using MSG if not present.
   procedure Scan_Semi_Colon (Msg : String) is
   begin
      if Current_Token /= Tok_Semi_Colon then
         Error_Msg_Parse ("missing "";"" at end of " & Msg);
      else
         Scan;
      end if;
   end Scan_Semi_Colon;

   --  precond : next token
   --  postcond: next token.
   --
   --  [§ 4.3.2 ]
   --  mode ::= IN | OUT | INOUT | BUFFER | LINKAGE
   --
   --  If there is no mode, DEFAULT is returned.
   function Parse_Mode return Iir_Mode is
   begin
      case Current_Token is
         when Tok_In =>
            Scan;
            if Current_Token = Tok_Out then
               --  Nice message for Ada users...
               Error_Msg_Parse ("typo error, in out must be 'inout' in vhdl");
               Scan;
               return Iir_Inout_Mode;
            end if;
            return Iir_In_Mode;
         when Tok_Out =>
            Scan;
            return Iir_Out_Mode;
         when Tok_Inout =>
            Scan;
            return Iir_Inout_Mode;
         when Tok_Linkage =>
            Scan;
            return Iir_Linkage_Mode;
         when Tok_Buffer =>
            Scan;
            return Iir_Buffer_Mode;
         when others =>
            Error_Msg_Parse
              ("mode is 'in', 'out', 'inout', 'buffer' or 'linkage'");
            return Iir_In_Mode;
      end case;
   end Parse_Mode;

   --  precond : next token
   --  postcond: next token
   --
   --  [ LRM 4.3.1.2 ]
   --  signal_kind ::= REGISTER | BUS
   --
   --  If there is no signal_kind, then no_signal_kind is returned.
   procedure Parse_Signal_Kind
     (Is_Guarded : out Boolean; Signal_Kind : out Iir_Signal_Kind) is
   begin
      if Current_Token = Tok_Bus then
         --  Eat 'bus'
         Scan;

         Is_Guarded := True;
         Signal_Kind := Iir_Bus_Kind;
      elsif Current_Token = Tok_Register then
         --  Eat 'register'
         Scan;

         Is_Guarded := True;
         Signal_Kind := Iir_Register_Kind;
      else
         Is_Guarded := False;
         --  Avoid uninitialized variable.
         Signal_Kind := Iir_Bus_Kind;
      end if;
   end Parse_Signal_Kind;

   --  precond : next token
   --  postcond: next token
   --
   -- Parse a range.
   -- If LEFT is not null_iir, then it must be an expression corresponding to
   -- the left limit of the range, and the current_token must be either
   -- tok_to or tok_downto.
   -- If left is null_iir, the current token is used to create the left limit
   -- expression.
   --
   -- [3.1]
   -- range ::= RANGE_attribute_name
   --         | simple_expression direction simple_expression
   function Parse_Range_Expression (Left: Iir; Discrete: Boolean := False)
                                   return Iir
   is
      Res : Iir;
      Left1: Iir;
   begin
      if Left /= Null_Iir then
         Left1 := Left;
      else
         Left1 := Parse_Simple_Expression;
      end if;

      case Current_Token is
         when Tok_To =>
            Res := Create_Iir (Iir_Kind_Range_Expression);
            Set_Direction (Res, Iir_To);
         when Tok_Downto =>
            Res := Create_Iir (Iir_Kind_Range_Expression);
            Set_Direction (Res, Iir_Downto);
         when Tok_Range =>
            if not Discrete then
               Unexpected ("range definition");
            end if;
            Scan;
            if Current_Token = Tok_Box then
               Unexpected ("range expression expected");
               Scan;
               return Null_Iir;
            end if;
            Res := Parse_Range_Expression (Null_Iir, False);
            if Res /= Null_Iir then
               Set_Type (Res, Left1);
            end if;
            return Res;
         when others =>
            if Left1 = Null_Iir then
               return Null_Iir;
            end if;
            if Is_Range_Attribute_Name (Left1) then
               return Left1;
            end if;
            if Discrete
              and then Get_Kind (Left1) in Iir_Kinds_Denoting_Name
            then
               return Left1;
            end if;
            Error_Msg_Parse ("'to' or 'downto' expected");
            return Null_Iir;
      end case;
      Set_Left_Limit (Res, Left1);
      Location_Copy (Res, Left1);

      Scan;
      Set_Right_Limit (Res, Parse_Simple_Expression);
      return Res;
   end Parse_Range_Expression;

   --  [ 3.1 ]
   --  range_constraint ::= RANGE range
   --
   --  [ 3.1 ]
   --  range ::= range_attribute_name
   --          | simple_expression direction simple_expression
   --
   --  [ 3.1 ]
   --  direction ::= TO | DOWNTO

   --  precond:  TO or DOWNTO
   --  postcond: next token
   function Parse_Range_Right (Left : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Range_Expression);
      Set_Location (Res);
      Set_Left_Limit (Res, Left);

      case Current_Token is
         when Tok_To =>
            Set_Direction (Res, Iir_To);
         when Tok_Downto =>
            Set_Direction (Res, Iir_Downto);
         when others =>
            raise Internal_Error;
      end case;

      Scan;
      Set_Right_Limit (Res, Parse_Simple_Expression);
      return Res;
   end Parse_Range_Right;

   --  precond:  next token
   --  postcond: next token
   function Parse_Range return Iir
   is
      Left: Iir;
   begin
      Left := Parse_Simple_Expression;

      case Current_Token is
         when Tok_To
           | Tok_Downto =>
            return Parse_Range_Right (Left);
         when others =>
            if Left /= Null_Iir then
               if Is_Range_Attribute_Name (Left) then
                  return Left;
               end if;
               Error_Msg_Parse ("'to' or 'downto' expected");
            end if;
            return Null_Iir;
      end case;
   end Parse_Range;

   --  precond:  next token (after RANGE)
   --  postcond: next token
   function Parse_Range_Constraint return Iir is
   begin
      if Current_Token = Tok_Box then
         Error_Msg_Parse ("range constraint required");
         Scan;
         return Null_Iir;
      end if;

      return Parse_Range;
   end Parse_Range_Constraint;

   --  precond:  next token (after RANGE)
   --  postcond: next token
   function Parse_Range_Constraint_Of_Subtype_Indication
     (Type_Mark : Iir;
      Resolution_Indication : Iir := Null_Iir)
     return Iir
   is
      Def : Iir;
   begin
      Def := Create_Iir (Iir_Kind_Subtype_Definition);
      Location_Copy (Def, Type_Mark);
      Set_Subtype_Type_Mark (Def, Type_Mark);
      Set_Range_Constraint (Def, Parse_Range_Constraint);
      Set_Resolution_Indication (Def, Resolution_Indication);
      Set_Tolerance (Def, Parse_Tolerance_Aspect_Opt);

      return Def;
   end Parse_Range_Constraint_Of_Subtype_Indication;

   --  precond:  next token
   --  postcond: next token
   --
   --  [ 3.2.1 ]
   --  discrete_range ::= discrete_subtype_indication | range
   function Parse_Discrete_Range return Iir
   is
      Left: Iir;
   begin
      Left := Parse_Simple_Expression;

      case Current_Token is
         when Tok_To
           | Tok_Downto =>
            return Parse_Range_Right (Left);
         when Tok_Range =>
            return Parse_Subtype_Indication (Left);
         when others =>
            --  Either a /range/_attribute_name or a type_mark.
            return Left;
      end case;
   end Parse_Discrete_Range;

   --  Convert the STR (0 .. LEN - 1) into a operator symbol identifier.
   --  Emit an error message if the name is not an operator name.
   function Str_To_Operator_Name (Str_Id : String8_Id;
                                  Len : Nat32;
                                  Loc : Location_Type) return Name_Id
   is
      --  LRM93 2.1
      --  Extra spaces are not allowed in an operator symbol, and the
      --  case of letters is not signifiant.

      --  LRM93 2.1
      --  The sequence of characters represented by an operator symbol
      --  must be an operator belonging to one of classes of operators
      --  defined in section 7.2.

      procedure Bad_Operator_Symbol is
      begin
         Error_Msg_Parse ("""" & Str_Table.String_String8 (Str_Id, Len)
                          & """ is not an operator symbol", Loc);
      end Bad_Operator_Symbol;

      procedure Check_Vhdl93 is
      begin
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse ("""" & Str_Table.String_String8 (Str_Id, Len)
                             & """ is not a vhdl87 operator symbol", Loc);
         end if;
      end Check_Vhdl93;

      Id : Name_Id;
      C1, C2, C3, C4 : Character;
   begin
      C1 := Str_Table.Char_String8 (Str_Id, 1);
      case Len is
         when 1 =>
            --  =, <, >, +, -, *, /, &
            case C1 is
               when '=' =>
                  Id := Name_Op_Equality;
               when '>' =>
                  Id := Name_Op_Greater;
               when '<' =>
                  Id := Name_Op_Less;
               when '+' =>
                  Id := Name_Op_Plus;
               when '-' =>
                  Id := Name_Op_Minus;
               when '*' =>
                  Id := Name_Op_Mul;
               when '/' =>
                  Id := Name_Op_Div;
               when '&' =>
                  Id := Name_Op_Concatenation;
               when others =>
                  Bad_Operator_Symbol;
                  Id := Name_Op_Plus;
            end case;
         when 2 =>
            --  or, /=, <=, >=, **
            C2 := Str_Table.Char_String8 (Str_Id, 2);
            case C1 is
               when 'o' | 'O' =>
                  Id := Name_Or;
                  if C2 /= 'r' and C2 /= 'R' then
                     Bad_Operator_Symbol;
                  end if;
               when '/' =>
                  Id := Name_Op_Inequality;
                  if C2 /= '=' then
                     Bad_Operator_Symbol;
                  end if;
               when '<' =>
                  Id := Name_Op_Less_Equal;
                  if C2 /= '=' then
                     Bad_Operator_Symbol;
                  end if;
               when '>' =>
                  Id := Name_Op_Greater_Equal;
                  if C2 /= '=' then
                     Bad_Operator_Symbol;
                  end if;
               when '*' =>
                  Id := Name_Op_Exp;
                  if C2 /= '*' then
                     Bad_Operator_Symbol;
                  end if;
               when '?' =>
                  if Vhdl_Std < Vhdl_08 then
                     Bad_Operator_Symbol;
                     Id := Name_Op_Condition;
                  elsif C2 = '?' then
                     Id := Name_Op_Condition;
                  elsif C2 = '=' then
                     Id := Name_Op_Match_Equality;
                  elsif C2 = '<' then
                     Id := Name_Op_Match_Less;
                  elsif C2 = '>' then
                     Id := Name_Op_Match_Greater;
                  else
                     Bad_Operator_Symbol;
                     Id := Name_Op_Condition;
                  end if;
               when others =>
                  Bad_Operator_Symbol;
                  Id := Name_Op_Equality;
            end case;
         when 3 =>
            --  mod, rem, and, xor, nor, abs, not, sll, sla, sra, srl, rol
            --  ror
            C2 := Str_Table.Char_String8 (Str_Id, 2);
            C3 := Str_Table.Char_String8 (Str_Id, 3);
            case C1 is
               when 'm' | 'M' =>
                  Id := Name_Mod;
                  if (C2 /= 'o' and C2 /= 'O') or (C3 /= 'd' and C3 /= 'D')
                  then
                     Bad_Operator_Symbol;
                  end if;
               when 'a' | 'A' =>
                  if (C2 = 'n' or C2 = 'N') and (C3 = 'd' or C3 = 'D') then
                     Id := Name_And;
                  elsif (C2 = 'b' or C2 = 'B') and (C3 = 's' or C3 = 'S') then
                     Id := Name_Abs;
                  else
                     Id := Name_And;
                     Bad_Operator_Symbol;
                  end if;
               when 'x' | 'X' =>
                  Id := Name_Xor;
                  if (C2 /= 'o' and C2 /= 'O') or (C3 /= 'r' and C3 /= 'R')
                  then
                     Bad_Operator_Symbol;
                  end if;
               when 'n' | 'N' =>
                  if C2 = 'o' or C2 = 'O' then
                     if C3 = 'r' or C3 = 'R' then
                        Id := Name_Nor;
                     elsif C3 = 't' or C3 = 'T' then
                        Id := Name_Not;
                     else
                        Id := Name_Not;
                        Bad_Operator_Symbol;
                     end if;
                  else
                     Id := Name_Not;
                     Bad_Operator_Symbol;
                  end if;
               when 's' | 'S' =>
                  if C2 = 'l' or C2 = 'L' then
                     if C3 = 'l' or C3 = 'L' then
                        Check_Vhdl93;
                        Id := Name_Sll;
                     elsif C3 = 'a' or C3 = 'A' then
                        Check_Vhdl93;
                        Id := Name_Sla;
                     else
                        Id := Name_Sll;
                        Bad_Operator_Symbol;
                     end if;
                  elsif C2 = 'r' or C2 = 'R' then
                     if C3 = 'l' or C3 = 'L' then
                        Check_Vhdl93;
                        Id := Name_Srl;
                     elsif C3 = 'a' or C3 = 'A' then
                        Check_Vhdl93;
                        Id := Name_Sra;
                     else
                        Id := Name_Srl;
                        Bad_Operator_Symbol;
                     end if;
                  else
                     Id := Name_Sll;
                     Bad_Operator_Symbol;
                  end if;
               when 'r' | 'R' =>
                  if C2 = 'e' or C2 = 'E' then
                     if C3 = 'm' or C3 = 'M' then
                        Id := Name_Rem;
                     else
                        Id := Name_Rem;
                        Bad_Operator_Symbol;
                     end if;
                  elsif C2 = 'o' or C2 = 'O' then
                     if C3 = 'l' or C3 = 'L' then
                        Check_Vhdl93;
                        Id := Name_Rol;
                     elsif C3 = 'r' or C3 = 'R' then
                        Check_Vhdl93;
                        Id := Name_Ror;
                     else
                        Id := Name_Rol;
                        Bad_Operator_Symbol;
                     end if;
                  else
                     Id := Name_Rem;
                     Bad_Operator_Symbol;
                  end if;
               when '?' =>
                  if Vhdl_Std < Vhdl_08 then
                     Bad_Operator_Symbol;
                     Id := Name_Op_Match_Less_Equal;
                  else
                     if C2 = '<' and C3 = '=' then
                        Id := Name_Op_Match_Less_Equal;
                     elsif C2 = '>' and C3 = '=' then
                        Id := Name_Op_Match_Greater_Equal;
                     elsif C2 = '/' and C3 = '=' then
                        Id := Name_Op_Match_Inequality;
                     else
                        Bad_Operator_Symbol;
                        Id := Name_Op_Match_Less_Equal;
                     end if;
                  end if;
               when others =>
                  Id := Name_And;
                  Bad_Operator_Symbol;
            end case;
         when 4 =>
            --  nand, xnor
            C2 := Str_Table.Char_String8 (Str_Id, 2);
            C3 := Str_Table.Char_String8 (Str_Id, 3);
            C4 := Str_Table.Char_String8 (Str_Id, 4);
            if (C1 = 'n' or C1 = 'N')
              and (C2 = 'a' or C2 = 'A')
              and (C3 = 'n' or C3 = 'N')
              and (C4 = 'd' or C4 = 'D')
            then
               Id := Name_Nand;
            elsif  (C1 = 'x' or C1 = 'X')
              and (C2 = 'n' or C2 = 'N')
              and (C3 = 'o' or C3 = 'O')
              and (C4 = 'r' or C4 = 'R')
            then
               Check_Vhdl93;
               Id := Name_Xnor;
            else
               Id := Name_Nand;
               Bad_Operator_Symbol;
            end if;
         when others =>
            Id := Name_Op_Plus;
            Bad_Operator_Symbol;
      end case;
      return Id;
   end Str_To_Operator_Name;

   function Scan_To_Operator_Name (Loc : Location_Type) return Name_Id is
   begin
      return Str_To_Operator_Name
        (Current_String_Id, Current_String_Length, Loc);
   end Scan_To_Operator_Name;
   pragma Inline (Scan_To_Operator_Name);

   --  Convert string literal STR to an operator symbol.
   --  Emit an error message if the string is not an operator name.
   function String_To_Operator_Symbol (Str : Iir) return Iir
   is
      Id : Name_Id;
      Res : Iir;
   begin
      Id := Str_To_Operator_Name
        (Get_String8_Id (Str), Get_String_Length (Str), Get_Location (Str));
      Res := Create_Iir (Iir_Kind_Operator_Symbol);
      Location_Copy (Res, Str);
      Set_Identifier (Res, Id);
      Free_Iir (Str);
      return Res;
   end String_To_Operator_Symbol;

   --  precond : next token
   --  postcond: next token
   --
   --  [ §6.1 ]
   --  name ::= simple_name
   --         | operator_symbol
   --         | selected_name
   --         | indexed_name
   --         | slice_name
   --         | attribute_name
   --
   --  [ §6.2 ]
   --  simple_name ::= identifier
   --
   --  [ §6.5 ]
   --  slice_name ::= prefix ( discrete_range )
   --
   --  [ §6.3 ]
   --  selected_name ::= prefix . suffix
   --
   --  [ §6.1 ]
   --  prefix ::= name
   --           | function_call
   --
   --  [ §6.3 ]
   --  suffix ::= simple_name
   --           | character_literal
   --           | operator_symbol
   --           | ALL
   --
   --  [ §3.2.1 ]
   --  discrete_range ::= DISCRETE_subtype_indication | range
   --
   --  [ §3.1 ]
   --  range ::= RANGE_attribute_name
   --          | simple_expression direction simple_expression
   --
   --  [ §3.1 ]
   --  direction ::= TO | DOWNTO
   --
   --  [ §6.6 ]
   --  attribute_name ::=
   --      prefix [ signature ] ' attribute_designator [ ( expression ) ]
   --
   --  [ §6.6 ]
   --  attribute_designator ::= ATTRIBUTE_simple_name
   --
   --  Note: in order to simplify the parsing, this function may return a
   --  signature without attribute designator. Signatures may appear at 3
   --  places:
   --  - in attribute name
   --  - in alias declaration
   --  - in entity designator
   function Parse_Name_Suffix (Pfx : Iir; Allow_Indexes: Boolean := True)
     return Iir
   is
      Res: Iir;
      Prefix: Iir;
   begin
      Res := Pfx;
      loop
         Prefix := Res;

         case Current_Token is
            when Tok_Left_Bracket =>
               if Get_Kind (Prefix) = Iir_Kind_String_Literal8 then
                  Prefix := String_To_Operator_Symbol (Prefix);
               end if;

               --  There is a signature. They are normally followed by an
               --  attribute.
               Res := Parse_Signature;
               Set_Signature_Prefix (Res, Prefix);

            when Tok_Tick =>
               -- There is an attribute.
               if Get_Kind (Prefix) = Iir_Kind_String_Literal8 then
                  Prefix := String_To_Operator_Symbol (Prefix);
               end if;

               Scan;
               if Current_Token = Tok_Left_Paren then
                  -- A qualified expression.
                  Res := Create_Iir (Iir_Kind_Qualified_Expression);
                  Set_Type_Mark (Res, Prefix);
                  Location_Copy (Res, Prefix);
                  Set_Expression (Res, Parse_Aggregate);
                  return Res;
               elsif Current_Token /= Tok_Range
                 and then Current_Token /= Tok_Identifier
               then
                  Expect (Tok_Identifier, "required for an attribute name");
                  return Null_Iir;
               end if;
               Res := Create_Iir (Iir_Kind_Attribute_Name);
               Set_Identifier (Res, Current_Identifier);
               Set_Location (Res);
               if Get_Kind (Prefix) = Iir_Kind_Signature then
                  Set_Attribute_Signature (Res, Prefix);
                  Set_Prefix (Res, Get_Signature_Prefix (Prefix));
               else
                  Set_Prefix (Res, Prefix);
               end if;

               -- accept the identifier.
               Scan;

            when Tok_Left_Paren =>
               if not Allow_Indexes then
                  return Res;
               end if;

               if Get_Kind (Prefix) = Iir_Kind_String_Literal8 then
                  Prefix := String_To_Operator_Symbol (Prefix);
               end if;

               Res := Create_Iir (Iir_Kind_Parenthesis_Name);
               Set_Location (Res);
               Set_Prefix (Res, Prefix);
               Set_Association_Chain
                 (Res, Parse_Association_List_In_Parenthesis);

            when Tok_Dot =>
               if Get_Kind (Prefix) = Iir_Kind_String_Literal8 then
                  Prefix := String_To_Operator_Symbol (Prefix);
               end if;

               Scan;
               case Current_Token is
                  when Tok_All =>
                     Res := Create_Iir (Iir_Kind_Selected_By_All_Name);
                     Set_Location (Res);
                     Set_Prefix (Res, Prefix);
                  when Tok_Identifier
                    | Tok_Character =>
                     Res := Create_Iir (Iir_Kind_Selected_Name);
                     Set_Location (Res);
                     Set_Prefix (Res, Prefix);
                     Set_Identifier (Res, Current_Identifier);
                  when Tok_String =>
                     Res := Create_Iir (Iir_Kind_Selected_Name);
                     Set_Location (Res);
                     Set_Prefix (Res, Prefix);
                     Set_Identifier
                       (Res, Scan_To_Operator_Name (Get_Token_Location));
                  when others =>
                     Error_Msg_Parse ("an identifier or all is expected");
               end case;
               Scan;
            when others =>
               return Res;
         end case;
      end loop;
   end Parse_Name_Suffix;

   --  Precond:  next token
   --  Postcond: next token
   --
   --  LRM08 8.7 External names
   --
   --  external_pathname ::=
   --      package_pathname
   --    | absolute_pathname
   --    | relative_pathname
   --
   --  package_pathname ::=
   --    @ library_logical_name . package_simple_name .
   --      { package_simple_name . } object_simple_name
   --
   --  absolute_pathname ::=
   --    . partial_pathname
   --
   --  relative_pathname ::=
   --    { ^ . } partial_pathname
   --
   --  partial_pathname ::= { pathname_element . } object_simple_name
   --
   --  pathname_element ::=
   --      entity_simple_name
   --    | component_instantiation_label
   --    | block_label
   --    | generate_statement_label [ ( static_expression ) ]
   --    | package_simple_name
   function Parse_External_Pathname return Iir
   is
      Res : Iir;
      Last : Iir;
      El : Iir;
   begin
      case Current_Token is
         when Tok_Arobase =>
            Res := Create_Iir (Iir_Kind_Package_Pathname);
            Set_Location (Res);
            Last := Res;

            --  Skip '@'
            Scan;

            if Current_Token /= Tok_Identifier then
               Error_Msg_Parse ("library name expected after '@'");
            else
               Set_Identifier (Res, Current_Identifier);

               --  Skip ident
               Scan;
            end if;

            if Current_Token /= Tok_Dot then
               Error_Msg_Parse ("'.' expected after library name");
            else
               --  Skip '.'
               Scan;
            end if;

         when Tok_Dot =>
            Res := Create_Iir (Iir_Kind_Absolute_Pathname);
            Set_Location (Res);
            Last := Res;

            --  Skip '.'
            Scan;

         when Tok_Caret =>
            Last := Null_Iir;
            loop
               El := Create_Iir (Iir_Kind_Relative_Pathname);
               Set_Location (El);

               --  Skip '^'
               Scan;

               if Current_Token /= Tok_Dot then
                  Error_Msg_Parse ("'.' expected after '^'");
               else
                  --  Skip '.'
                  Scan;
               end if;

               if Last = Null_Iir then
                  Res := El;
               else
                  Set_Pathname_Suffix (Last, El);
               end if;
               Last := El;

               exit when Current_Token /= Tok_Caret;
            end loop;

         when Tok_Identifier =>
            Last := Null_Iir;

         when others =>
            Last := Null_Iir;
            --  Error is handled just below.
      end case;

      --  Parse pathname elements.
      loop
         if Current_Token /= Tok_Identifier then
            Error_Msg_Parse ("pathname element expected");
            --  FIXME: resync.
            return Res;
         end if;

         El := Create_Iir (Iir_Kind_Pathname_Element);
         Set_Location (El);
         Set_Identifier (El, Current_Identifier);
         if Last = Null_Iir then
            Res := El;
         else
            Set_Pathname_Suffix (Last, El);
         end if;
         Last := El;

         --  Skip identifier
         Scan;

         exit when Current_Token /= Tok_Dot;

         --  Skip '.'
         Scan;
      end loop;

      return Res;
   end Parse_External_Pathname;

   --  Precond:  '<<'
   --  Postcond: next token
   --
   --  LRM08 8.7 External names
   --  external_name ::=
   --      external_constant_name
   --    | external_signal_name
   --    | external_variable_name
   --
   --  external_constant_name ::=
   --    << CONSTANT external_pathname : subtype_indication >>
   --
   --  external_signal_name ::=
   --   << SIGNAL external_pathname : subtype_indication >>
   --
   --  external_variable_name ::=
   --   << VARIABLE external_pathname : subtype_indication >>
   function Parse_External_Name return Iir
   is
      Loc : Location_Type;
      Res : Iir;
      Kind : Iir_Kind;
   begin
      Loc := Get_Token_Location;

      --  Skip '<<'
      Scan;

      case Current_Token is
         when Tok_Constant =>
            Kind := Iir_Kind_External_Constant_Name;
            --  Skip 'constant'
            Scan;
         when Tok_Signal =>
            Kind := Iir_Kind_External_Signal_Name;
            --  Skip 'signal'
            Scan;
         when Tok_Variable =>
            Kind := Iir_Kind_External_Variable_Name;
            --  Skip 'variable'
            Scan;
         when others =>
            Error_Msg_Parse
              ("constant, signal or variable expected after <<");
            Kind := Iir_Kind_External_Signal_Name;
      end case;

      Res := Create_Iir (Kind);
      Set_Location (Res, Loc);

      Set_External_Pathname (Res, Parse_External_Pathname);

      if Current_Token /= Tok_Colon then
         Error_Msg_Parse ("':' expected after external pathname");
      else
         --  Skip ':'
         Scan;
      end if;

      Set_Subtype_Indication (Res, Parse_Subtype_Indication);

      if Current_Token /= Tok_Double_Greater then
         Error_Msg_Parse ("'>>' expected at end of external name");
      else
         --  Skip '>>'
         Scan;
      end if;

      return Res;
   end Parse_External_Name;

   --  Precond: next token (identifier, string or '<<')
   --  Postcond: next token
   --
   --  LRM08 8. Names
   --  name ::=
   --     simple_name
   --   | operator_symbol
   --   | character_literal    --  FIXME: not handled.
   --   | selected_name
   --   | indexed_name
   --   | slice_name
   --   | attribute_name
   --   | external_name
   function Parse_Name (Allow_Indexes: Boolean := True) return Iir
   is
      Res: Iir;
   begin
      case Current_Token is
         when Tok_Identifier =>
            Res := Create_Iir (Iir_Kind_Simple_Name);
            Set_Identifier (Res, Current_Identifier);
            Set_Location (Res);

            --  Skip identifier
            Scan;

         when Tok_String =>
            --  For operator symbol, such as: "+" (A, B).
            Res := Create_Iir (Iir_Kind_String_Literal8);
            Set_String8_Id (Res, Current_String_Id);
            Set_String_Length (Res, Current_String_Length);
            Set_Location (Res);

            --  Skip string
            Scan;
         when Tok_Double_Less =>
            if Vhdl_Std < Vhdl_08 then
               Error_Msg_Parse ("external name not allowed before vhdl 08");
            end if;
            Res := Parse_External_Name;
         when others =>
            Error_Msg_Parse ("identifier expected here");
            raise Parse_Error;
      end case;

      return Parse_Name_Suffix (Res, Allow_Indexes);
   end Parse_Name;

   --  Emit an error message if MARK doesn't have the form of a type mark.
   procedure Check_Type_Mark (Mark : Iir) is
   begin
      case Get_Kind (Mark) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            null;
         when others =>
            Error_Msg_Parse ("type mark must be a name of a type", Mark);
      end case;
   end Check_Type_Mark;

   --  precond : next token
   --  postcond: next token
   --
   --  [ 4.2 ]
   --  type_mark ::= type_name
   --              | subtype_name
   function Parse_Type_Mark (Check_Paren : Boolean := False) return Iir
   is
      Res : Iir;
      Old : Iir;
      pragma Unreferenced (Old);
   begin
      Res := Parse_Name (Allow_Indexes => False);
      Check_Type_Mark (Res);
      if Check_Paren and then Current_Token = Tok_Left_Paren then
         Error_Msg_Parse ("index constraint not allowed here");
         Old := Parse_Name_Suffix (Res, True);
      end if;
      return Res;
   end Parse_Type_Mark;

   --  precond : CONSTANT, SIGNAL, VARIABLE. FILE or identifier
   --  postcond: next token (';' or ')')
   --
   --  [ LRM93 4.3.2 ] [ LRM08 6.5.2 ]
   --  interface_declaration ::= interface_constant_declaration
   --                          | interface_signal_declaration
   --                          | interface_variable_declaration
   --                          | interface_file_declaration
   --
   --
   --  [ LRM93 3.2.2 ]
   --  identifier_list ::= identifier { , identifier }
   --
   --  [ LRM93 4.3.2 ]
   --  interface_constant_declaration ::=
   --      [ CONSTANT ] identifier_list : [ IN ] subtype_indication
   --          [ := STATIC_expression ]
   --
   --  [ LRM93 4.3.2 ]
   --  interface_file_declaration ::= FILE identifier_list : subtype_indication
   --
   --  [ LRM93 4.3.2 ]
   --  interface_signal_declaration ::=
   --      [ SIGNAL ] identifier_list : [ mode ] subtype_indication [ BUS ]
   --          [ := STATIC_expression ]
   --
   --  [ LRM93 4.3.2 ]
   --  interface_variable_declaration ::=
   --      [ VARIABLE ] identifier_list : [ mode ] subtype_indication
   --          [ := STATIC_expression ]
   --
   --  The default kind of interface declaration is DEFAULT.
   function Parse_Interface_Object_Declaration (Ctxt : Interface_Kind_Type)
                                               return Iir
   is
      Kind : Iir_Kind;
      Res, Last : Iir;
      First, Prev_First : Iir;
      Inter: Iir;
      Is_Default : Boolean;
      Interface_Mode: Iir_Mode;
      Interface_Type: Iir;
      Is_Guarded : Boolean;
      Signal_Kind: Iir_Signal_Kind;
      Default_Value: Iir;
      Has_Mode : Boolean;
      Has_Class : Boolean;
   begin
      Res := Null_Iir;
      Last := Null_Iir;

      --  LRM08 6.5.2 Interface object declarations
      --  Interface obejcts include interface constants that appear as
      --  generics of a design entity, a component, a block, a package or
      --  a subprogram, or as constant parameter of subprograms; interface
      --  signals that appear as ports of a design entity, component or
      --  block, or as signal parameters of subprograms; interface variables
      --  that appear as variable parameter subprograms; interface files
      --  that appear as file parameters of subrograms.
      case Current_Token is
         when Tok_Identifier =>
            --  The class of the object is unknown.  Select default
            --  according to the above rule, assuming the mode is IN.  If
            --  the mode is not IN, Parse_Interface_Object_Declaration will
            --  change the class.
            case Ctxt is
               when Generic_Interface_List
                 | Parameter_Interface_List =>
                  Kind := Iir_Kind_Interface_Constant_Declaration;
               when Port_Interface_List =>
                  Kind := Iir_Kind_Interface_Signal_Declaration;
            end case;
         when Tok_Constant =>
            Kind := Iir_Kind_Interface_Constant_Declaration;
         when Tok_Signal =>
            if Ctxt = Generic_Interface_List then
               Error_Msg_Parse
                 ("signal interface not allowed in generic clause");
            end if;
            Kind := Iir_Kind_Interface_Signal_Declaration;
         when Tok_Variable =>
            if Ctxt not in Parameter_Interface_List then
               Error_Msg_Parse
                 ("variable interface not allowed in generic or port clause");
            end if;
            Kind := Iir_Kind_Interface_Variable_Declaration;
         when Tok_File =>
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse ("file interface not allowed in vhdl 87");
            end if;
            if Ctxt not in Parameter_Interface_List then
               Error_Msg_Parse
                 ("variable interface not allowed in generic or port clause");
            end if;
            Kind := Iir_Kind_Interface_File_Declaration;
         when others =>
            --  Fall back in case of parse error.
            Kind := Iir_Kind_Interface_Variable_Declaration;
      end case;

      Inter := Create_Iir (Kind);

      if Current_Token = Tok_Identifier then
         Is_Default := True;
         Has_Class := False;
      else
         Is_Default := False;
         Has_Class := True;

         --  Skip 'signal', 'variable', 'constant' or 'file'.
         Scan;
      end if;

      Prev_First := Last;
      First := Inter;
      loop
         if Current_Token /= Tok_Identifier then
            Expect (Tok_Identifier);
         end if;
         Set_Identifier (Inter, Current_Identifier);
         Set_Location (Inter);

         if Res = Null_Iir then
            Res := Inter;
         else
            Set_Chain (Last, Inter);
         end if;
         Last := Inter;

         --  Skip identifier
         Scan;

         exit when Current_Token = Tok_Colon;
         Expect (Tok_Comma, "',' or ':' expected after identifier");

         --  Skip ','
         Scan;

         Inter := Create_Iir (Kind);
      end loop;

      Expect (Tok_Colon, "':' must follow the interface element identifier");

      --  Skip ':'
      Scan;

      --  Parse mode.
      case Current_Token is
         when Tok_In
           | Tok_Out
           | Tok_Inout
           | Tok_Linkage
           | Tok_Buffer =>
            Interface_Mode := Parse_Mode;
            Has_Mode := True;
         when others =>
            Interface_Mode := Iir_Unknown_Mode;
            Has_Mode := False;
      end case;

      --  LRM93 2.1.1  LRM08 4.2.2.1
      --  If the mode is INOUT or OUT, and no object class is explicitly
      --  specified, variable is assumed.
      if Is_Default
        and then Ctxt in Parameter_Interface_List
        and then Interface_Mode in Iir_Out_Modes
      then
         --  Convert into variable.
         declare
            O_Interface : Iir_Interface_Constant_Declaration;
            N_Interface : Iir_Interface_Variable_Declaration;
         begin
            O_Interface := First;
            while O_Interface /= Null_Iir loop
               N_Interface :=
                 Create_Iir (Iir_Kind_Interface_Variable_Declaration);
               Location_Copy (N_Interface, O_Interface);
               Set_Identifier (N_Interface,
                               Get_Identifier (O_Interface));
               if Prev_First = Null_Iir then
                  Res := N_Interface;
               else
                  Set_Chain (Prev_First, N_Interface);
               end if;
               Prev_First := N_Interface;
               if O_Interface = First then
                  First := N_Interface;
               end if;
               Last := N_Interface;
               Inter := Get_Chain (O_Interface);
               Free_Iir (O_Interface);
               O_Interface := Inter;
            end loop;
            Inter := First;
         end;
      end if;

      --  Parse mode (and handle default mode).
      case Iir_Kinds_Interface_Object_Declaration (Get_Kind (Inter)) is
         when Iir_Kind_Interface_File_Declaration =>
            if Interface_Mode /= Iir_Unknown_Mode then
               Error_Msg_Parse
                 ("mode can't be specified for a file interface");
            end if;
            Interface_Mode := Iir_Inout_Mode;
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_Variable_Declaration =>
            --  LRM93 4.3.2
            --  If no mode is explicitly given in an interface declaration
            --  other than an interface file declaration, mode IN is
            --  assumed.
            if Interface_Mode = Iir_Unknown_Mode then
               Interface_Mode := Iir_In_Mode;
            end if;
         when Iir_Kind_Interface_Constant_Declaration =>
            if Interface_Mode = Iir_Unknown_Mode then
               Interface_Mode := Iir_In_Mode;
            elsif Interface_Mode /= Iir_In_Mode then
               Error_Msg_Parse ("mode must be 'in' for a constant");
               Interface_Mode := Iir_In_Mode;
            end if;
      end case;

      Interface_Type := Parse_Subtype_Indication;

      --  Signal kind (but only for signal).
      if Get_Kind (Inter) = Iir_Kind_Interface_Signal_Declaration then
         Parse_Signal_Kind (Is_Guarded, Signal_Kind);
      else
         Is_Guarded := False;
         Signal_Kind := Iir_Register_Kind;
      end if;

      if Current_Token = Tok_Assign then
         if Get_Kind (Inter) = Iir_Kind_Interface_File_Declaration then
            Error_Msg_Parse
              ("default expression not allowed for an interface file");
         end if;

         --  Skip ':='
         Scan;

         Default_Value := Parse_Expression;
      else
         Default_Value := Null_Iir;
      end if;

      --  Subtype_Indication and Default_Value are set only on the first
      --  interface.
      Set_Subtype_Indication (First, Interface_Type);
      if Get_Kind (First) /= Iir_Kind_Interface_File_Declaration then
         Set_Default_Value (First, Default_Value);
      end if;

      Inter := First;
      while Inter /= Null_Iir loop
         Set_Mode (Inter, Interface_Mode);
         Set_Is_Ref (Inter, Inter /= First);
         Set_Has_Mode (Inter, Has_Mode);
         Set_Has_Class (Inter, Has_Class);
         Set_Has_Identifier_List (Inter, Inter /= Last);
         if Get_Kind (Inter) = Iir_Kind_Interface_Signal_Declaration then
            Set_Guarded_Signal_Flag (Inter, Is_Guarded);
            Set_Signal_Kind (Inter, Signal_Kind);
         end if;
         Inter := Get_Chain (Inter);
      end loop;

      return Res;
   end Parse_Interface_Object_Declaration;

   --  Precond : 'package'
   --  Postcond: next token
   --
   --  LRM08 6.5.5 Interface package declarations
   --  interface_package_declaration ::=
   --    PACKAGE identifier IS NEW uninstantiated_package name
   --      interface_package_generic_map_aspect
   --
   --  interface_package_generic_map_aspect ::=
   --       generic_map_aspect
   --     | GENERIC MAP ( <> )
   --     | GENERIC MAP ( DEFAULT )
   function Parse_Interface_Package_Declaration return Iir
   is
      Inter : Iir;
      Map : Iir;
   begin
      Inter := Create_Iir (Iir_Kind_Interface_Package_Declaration);

      --  Skip 'package'
      Scan_Expect (Tok_Identifier,
                   "an identifier is expected after ""package""");
      Set_Identifier (Inter, Current_Identifier);
      Set_Location (Inter);

      --  Skip identifier
      Scan_Expect (Tok_Is);

      --  Skip 'is'
      Scan_Expect (Tok_New);

      --  Skip 'new'
      Scan;

      Set_Uninstantiated_Package_Name (Inter, Parse_Name (False));

      Expect (Tok_Generic);

      --  Skip 'generic'
      Scan_Expect (Tok_Map);

      --  Skip 'map'
      Scan_Expect (Tok_Left_Paren);

      --  Skip '('
      Scan;

      case Current_Token is
         when Tok_Box =>
            Map := Null_Iir;
            --  Skip '<>'
            Scan;
         when others =>
            Map := Parse_Association_List;
      end case;
      Set_Generic_Map_Aspect_Chain (Inter, Map);

      Expect (Tok_Right_Paren);

      --  Skip ')'
      Scan;

      return Inter;
   end Parse_Interface_Package_Declaration;

   --  Precond : '('
   --  Postcond: next token
   --
   --  LRM08 6.5.6 Interface lists
   --  interface_list ::= interface_element { ';' interface_element }
   --
   --  interface_element ::= interface_declaration
   function Parse_Interface_List (Ctxt : Interface_Kind_Type; Parent : Iir)
                                 return Iir
   is
      Res, Last : Iir;
      Inters : Iir;
      Next : Iir;
      Prev_Loc : Location_Type;
   begin
      Expect (Tok_Left_Paren);

      Res := Null_Iir;
      Last := Null_Iir;
      loop
         Prev_Loc := Get_Token_Location;

         --  Skip '(' or ';'
         Scan;

         case Current_Token is
            when Tok_Identifier
              | Tok_Signal
              | Tok_Variable
              | Tok_Constant
              | Tok_File =>
               --  An inteface object.
               Inters := Parse_Interface_Object_Declaration (Ctxt);
            when Tok_Package =>
               if Ctxt /= Generic_Interface_List then
                  Error_Msg_Parse
                    ("package interface only allowed in generic interface");
               elsif Flags.Vhdl_Std < Vhdl_08 then
                  Error_Msg_Parse
                    ("package interface not allowed before vhdl 08");
               end if;
               Inters := Parse_Interface_Package_Declaration;
            when Tok_Right_Paren =>
               if Res = Null_Iir then
                  Error_Msg_Parse
                    ("empty interface list not allowed", Prev_Loc);
               else
                  Error_Msg_Parse
                    ("extra ';' at end of interface list", Prev_Loc);
               end if;
               exit;
            when others =>
               Error_Msg_Parse
                 ("'signal', 'constant', 'variable', 'file' "
                  & "or identifier expected");
               --  Use a variable interface as a fall-back.
               Inters := Parse_Interface_Object_Declaration (Ctxt);
         end case;

         --  Chain
         if Last = Null_Iir then
            Res := Inters;
         else
            Set_Chain (Last, Inters);
         end if;

         --  Set parent and set Last to the last interface.
         Last := Inters;
         loop
            Set_Parent (Last, Parent);
            Next := Get_Chain (Last);
            exit when Next = Null_Iir;
            Last := Next;
         end loop;

         exit when Current_Token /= Tok_Semi_Colon;
      end loop;

      if Current_Token /= Tok_Right_Paren then
         Error_Msg_Parse ("')' expected at end of interface list");
      end if;

      --  Skip ')'
      Scan;

      return Res;
   end Parse_Interface_List;

   --  precond : PORT
   --  postcond: next token
   --
   --  [ §1.1.1 ]
   --  port_clause ::= PORT ( port_list ) ;
   --
   --  [ §1.1.1.2 ]
   --  port_list ::= PORT_interface_list
   procedure Parse_Port_Clause (Parent : Iir)
   is
      Res: Iir;
      El : Iir;
   begin
      --  Skip 'port'
      pragma Assert (Current_Token = Tok_Port);
      Scan;

      Res := Parse_Interface_List (Port_Interface_List, Parent);

      --  Check the interface are signal interfaces.
      El := Res;
      while El /= Null_Iir loop
         if Get_Kind (El) /= Iir_Kind_Interface_Signal_Declaration then
            Error_Msg_Parse ("port must be a signal", El);
         end if;
         El := Get_Chain (El);
      end loop;

      Scan_Semi_Colon ("port clause");
      Set_Port_Chain (Parent, Res);
   end Parse_Port_Clause;

   --  precond : GENERIC
   --  postcond: next token
   --
   --  [ LRM93 1.1.1, LRM08 6.5.6.2 ]
   --  generic_clause ::= GENERIC ( generic_list ) ;
   --
   --  [ LRM93 1.1.1.1, LRM08 6.5.6.2]
   --  generic_list ::= GENERIC_interface_list
   procedure Parse_Generic_Clause (Parent : Iir)
   is
      Res: Iir;
   begin
      --  Skip 'generic'
      pragma Assert (Current_Token = Tok_Generic);
      Scan;

      Res := Parse_Interface_List (Generic_Interface_List, Parent);
      Set_Generic_Chain (Parent, Res);

      Scan_Semi_Colon ("generic clause");
   end Parse_Generic_Clause;

   --  precond : a token.
   --  postcond: next token
   --
   --  [ §1.1.1 ]
   --  entity_header ::=
   --      [ FORMAL_generic_clause ]
   --      [ FORMAL_port_clause ]
   --
   --  [ §4.5 ]
   --          [ LOCAL_generic_clause ]
   --          [ LOCAL_port_clause ]
   procedure Parse_Generic_Port_Clauses (Parent : Iir)
   is
      Has_Port, Has_Generic : Boolean;
   begin
      Has_Port := False;
      Has_Generic := False;
      loop
         if Current_Token = Tok_Generic then
            if Has_Generic then
               Error_Msg_Parse ("at most one generic clause is allowed");
            end if;
            if Has_Port then
               Error_Msg_Parse ("generic clause must precede port clause");
            end if;
            Has_Generic := True;
            Parse_Generic_Clause (Parent);
         elsif Current_Token = Tok_Port then
            if Has_Port then
               Error_Msg_Parse ("at most one port clause is allowed");
            end if;
            Has_Port := True;
            Parse_Port_Clause (Parent);
         else
            exit;
         end if;
      end loop;
   end Parse_Generic_Port_Clauses;

   --  precond : a token
   --  postcond: next token
   --
   --  [ LRM93 3.1.1 ]
   --  enumeration_type_definition ::=
   --      ( enumeration_literal { , enumeration_literal } )
   --
   --  [ LRM93 3.1.1 ]
   --  enumeration_literal ::= identifier | character_literal
   function Parse_Enumeration_Type_Definition (Parent : Iir)
      return Iir_Enumeration_Type_Definition
   is
      Pos: Iir_Int32;
      Enum_Lit: Iir_Enumeration_Literal;
      Enum_Type: Iir_Enumeration_Type_Definition;
      Enum_List : Iir_List;
   begin
      --  This is an enumeration.
      Enum_Type := Create_Iir (Iir_Kind_Enumeration_Type_Definition);
      Set_Location (Enum_Type);
      Enum_List := Create_Iir_List;
      Set_Enumeration_Literal_List (Enum_Type, Enum_List);

      --  LRM93 3.1.1
      --  The position number of the first listed enumeration literal is zero.
      Pos := 0;

      --  Eat '('.
      Scan;

      if Current_Token = Tok_Right_Paren then
         Error_Msg_Parse ("at least one literal must be declared");
         Scan;
         return Enum_Type;
      end if;
      loop
         if Current_Token /= Tok_Identifier
           and then Current_Token /= Tok_Character
         then
            if Current_Token = Tok_Eof then
               Error_Msg_Parse ("unexpected end of file");
               return Enum_Type;
            end if;
            Error_Msg_Parse ("identifier or character expected");
         end if;

         Enum_Lit := Create_Iir (Iir_Kind_Enumeration_Literal);
         Set_Identifier (Enum_Lit, Current_Identifier);
         Set_Parent (Enum_Lit, Parent);
         Set_Location (Enum_Lit);
         Set_Enum_Pos (Enum_Lit, Pos);

         -- LRM93 3.1.1
         -- the position number for each additional enumeration literal is
         -- one more than that if its predecessor in the list.
         Pos := Pos + 1;

         Append_Element (Enum_List, Enum_Lit);

         --  Skip identifier or character.
         Scan;

         exit when Current_Token = Tok_Right_Paren;
         if Current_Token /= Tok_Comma then
            Error_Msg_Parse ("')' or ',' is expected after an enum literal");
         end if;

         --  Skip ','.
         Scan;

         if Current_Token = Tok_Right_Paren then
            Error_Msg_Parse ("extra ',' ignored");
            exit;
         end if;
      end loop;

      --  Skip ')'.
      Scan;

      return Enum_Type;
   end Parse_Enumeration_Type_Definition;

   --  precond : ARRAY
   --  postcond: ??
   --
   --  [ LRM93 3.2.1 ]
   --  array_type_definition ::= unconstrained_array_definition
   --                          | constrained_array_definition
   --
   --   unconstrained_array_definition ::=
   --      ARRAY ( index_subtype_definition { , index_subtype_definition } )
   --      OF element_subtype_indication
   --
   --   constrained_array_definition ::=
   --      ARRAY index_constraint OF element_subtype_indication
   --
   --   index_subtype_definition ::= type_mark RANGE <>
   --
   --   index_constraint ::= ( discrete_range { , discrete_range } )
   --
   --   discrete_range ::= discrete_subtype_indication | range
   --
   --  [ LRM08 5.3.2.1 ]
   --  array_type_definition ::= unbounded_array_definition
   --                          | constrained_array_definition
   --
   --   unbounded_array_definition ::=
   --      ARRAY ( index_subtype_definition { , index_subtype_definition } )
   --      OF element_subtype_indication
   function Parse_Array_Definition return Iir
   is
      Index_Constrained : Boolean;
      Array_Constrained : Boolean;
      First : Boolean;
      Res_Type: Iir;
      Index_List : Iir_List;

      Loc : Location_Type;
      Def : Iir;
      Type_Mark : Iir;
      Element_Subtype : Iir;
   begin
      Loc := Get_Token_Location;

      --  Skip 'array', scan '('
      Scan_Expect (Tok_Left_Paren);
      Scan;

      First := True;
      Index_List := Create_Iir_List;

      loop
         --  The accepted syntax can be one of:
         --  * index_subtype_definition, which is:
         --    * type_mark RANGE <>
         --  * discrete_range, which is either:
         --    * /discrete/_subtype_indication
         --      * [ resolution_indication ] type_mark [ range_constraint ]
         --        * range_constraint ::= RANGE range
         --    * range
         --      * /range/_attribute_name
         --      * simple_expression direction simple_expression

         --  Parse a simple expression (for the range), which can also parse a
         --  name.
         Type_Mark := Parse_Simple_Expression;

         case Current_Token is
            when Tok_Range =>
               --  Skip 'range'
               Scan;

               if Current_Token = Tok_Box then
                  --  Parsed 'RANGE <>': this is an index_subtype_definition.
                  Index_Constrained := False;
                  Scan;
                  Def := Type_Mark;
               else
                  --  This is a /discrete/_subtype_indication
                  Index_Constrained := True;
                  Def :=
                    Parse_Range_Constraint_Of_Subtype_Indication (Type_Mark);
               end if;
            when Tok_To
              | Tok_Downto =>
               --  A range
               Index_Constrained := True;
               Def := Parse_Range_Right (Type_Mark);
            when others =>
               --  For a /range/_attribute_name
               Index_Constrained := True;
               Def := Type_Mark;
         end case;

         Append_Element (Index_List, Def);

         if First then
            Array_Constrained := Index_Constrained;
            First := False;
         else
            if Array_Constrained /= Index_Constrained then
               Error_Msg_Parse
                 ("cannot mix constrained and unconstrained index");
            end if;
         end if;
         exit when Current_Token /= Tok_Comma;
         Scan;
      end loop;

      --  Skip ')' and 'of'
      Expect (Tok_Right_Paren);
      Scan_Expect (Tok_Of);
      Scan;

      Element_Subtype := Parse_Subtype_Indication;

      if Array_Constrained then
         --  Sem_Type will create the array type.
         Res_Type := Create_Iir (Iir_Kind_Array_Subtype_Definition);
         Set_Element_Subtype (Res_Type, Element_Subtype);
         Set_Index_Constraint_List (Res_Type, Index_List);
      else
         Res_Type := Create_Iir (Iir_Kind_Array_Type_Definition);
         Set_Element_Subtype_Indication (Res_Type, Element_Subtype);
         Set_Index_Subtype_Definition_List (Res_Type, Index_List);
      end if;
      Set_Location (Res_Type, Loc);

      return Res_Type;
   end Parse_Array_Definition;

   --  precond : UNITS
   --  postcond: next token
   --
   --  [ LRM93 3.1.3 ]
   --  physical_type_definition ::=
   --      range_constraint
   --          UNITS
   --              base_unit_declaration
   --              { secondary_unit_declaration }
   --          END UNITS [ PHYSICAL_TYPE_simple_name ]
   --
   --  [ LRM93 3.1.3 ]
   --  base_unit_declaration ::= identifier ;
   --
   --  [ LRM93 3.1.3 ]
   --  secondary_unit_declaration ::= identifier = physical_literal ;
   function Parse_Physical_Type_Definition (Parent : Iir)
     return Iir_Physical_Type_Definition
   is
      use Iir_Chains.Unit_Chain_Handling;
      Res: Iir_Physical_Type_Definition;
      Unit: Iir_Unit_Declaration;
      Last : Iir_Unit_Declaration;
      Multiplier : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Physical_Type_Definition);
      Set_Location (Res);

      --  Skip 'units'
      Expect (Tok_Units);
      Scan;

      --  Parse primary unit.
      Expect (Tok_Identifier);
      Unit := Create_Iir (Iir_Kind_Unit_Declaration);
      Set_Location (Unit);
      Set_Parent (Unit, Parent);
      Set_Identifier (Unit, Current_Identifier);

      --  Skip identifier
      Scan;

      Scan_Semi_Colon ("primary unit");

      Build_Init (Last);
      Append (Last, Res, Unit);

      --  Parse secondary units.
      while Current_Token /= Tok_End loop
         Unit := Create_Iir (Iir_Kind_Unit_Declaration);
         Set_Location (Unit);
         Set_Parent (Unit, Parent);
         Set_Identifier (Unit, Current_Identifier);

         --  Skip identifier.
         Scan_Expect (Tok_Equal);

         --  Skip '='.
         Scan;

         Multiplier := Parse_Primary;
         Set_Physical_Literal (Unit, Multiplier);
         case Get_Kind (Multiplier) is
            when Iir_Kind_Simple_Name
              | Iir_Kind_Selected_Name
              | Iir_Kind_Physical_Int_Literal =>
               null;
            when Iir_Kind_Physical_Fp_Literal =>
               Error_Msg_Parse
                 ("secondary units may only be defined with integer literals");
            when others =>
               Error_Msg_Parse ("a physical literal is expected here");
         end case;
         Append (Last, Res, Unit);
         Scan_Semi_Colon ("secondary unit");
      end loop;

      --  Skip 'end'.
      Scan;

      Expect (Tok_Units);
      Set_End_Has_Reserved_Id (Res, True);

      --  Skip 'units'.
      Scan;
      return Res;
   end Parse_Physical_Type_Definition;

   --  precond : RECORD
   --  postcond: next token
   --
   --  [ LRM93 3.2.2 ]
   --  record_type_definition ::=
   --      RECORD
   --          element_declaration
   --          { element_declaration }
   --      END RECORD [ RECORD_TYPE_simple_name ]
   --
   --  element_declaration ::=
   --      identifier_list : element_subtype_definition
   --
   --  element_subtype_definition ::= subtype_indication
   function Parse_Record_Type_Definition return Iir_Record_Type_Definition
   is
      Res: Iir_Record_Type_Definition;
      El_List : Iir_List;
      El: Iir_Element_Declaration;
      First : Iir;
      Pos: Iir_Index32;
      Subtype_Indication: Iir;
   begin
      Res := Create_Iir (Iir_Kind_Record_Type_Definition);
      Set_Location (Res);
      El_List := Create_Iir_List;
      Set_Elements_Declaration_List (Res, El_List);

      --  Skip 'record'
      Scan;

      Pos := 0;
      First := Null_Iir;
      loop
         pragma Assert (First = Null_Iir);
         --  Parse identifier_list
         loop
            El := Create_Iir (Iir_Kind_Element_Declaration);
            Set_Location (El);
            if First = Null_Iir then
               First := El;
            end if;
            Expect (Tok_Identifier);
            Set_Identifier (El, Current_Identifier);
            Append_Element (El_List, El);
            Set_Element_Position (El, Pos);
            Pos := Pos + 1;
            if First = Null_Iir then
               First := El;
            end if;

            --  Skip identifier
            Scan;

            exit when Current_Token /= Tok_Comma;

            Set_Has_Identifier_List (El, True);

            --  Skip ','
            Scan;
         end loop;

         --  Scan ':'.
         Expect (Tok_Colon);
         Scan;

         --  Parse element subtype indication.
         Subtype_Indication := Parse_Subtype_Indication;
         Set_Subtype_Indication (First, Subtype_Indication);

         First := Null_Iir;
         Scan_Semi_Colon ("element declaration");
         exit when Current_Token = Tok_End;
      end loop;

      --  Skip 'end'
      Scan_Expect (Tok_Record);
      Set_End_Has_Reserved_Id (Res, True);

      --  Skip 'record'
      Scan;

      return Res;
   end Parse_Record_Type_Definition;

   --  precond : ACCESS
   --  postcond: ?
   --
   --  [ LRM93 3.3]
   --  access_type_definition ::= ACCESS subtype_indication.
   function Parse_Access_Type_Definition return Iir_Access_Type_Definition
   is
      Res : Iir_Access_Type_Definition;
   begin
      Res := Create_Iir (Iir_Kind_Access_Type_Definition);
      Set_Location (Res);

      --  Skip 'access'
      Expect (Tok_Access);
      Scan;

      Set_Designated_Subtype_Indication (Res, Parse_Subtype_Indication);

      return Res;
   end Parse_Access_Type_Definition;

   --  precond : FILE
   --  postcond: next token
   --
   --  [ LRM93 3.4 ]
   --  file_type_definition ::= FILE OF type_mark
   function Parse_File_Type_Definition return Iir_File_Type_Definition
   is
      Res : Iir_File_Type_Definition;
      Type_Mark: Iir;
   begin
      Res := Create_Iir (Iir_Kind_File_Type_Definition);
      Set_Location (Res);
      -- Accept token 'file'.
      Scan_Expect (Tok_Of);
      Scan;
      Type_Mark := Parse_Type_Mark (Check_Paren => True);
      if Get_Kind (Type_Mark) not in Iir_Kinds_Denoting_Name then
         Error_Msg_Parse ("type mark expected");
      else
         Set_File_Type_Mark (Res, Type_Mark);
      end if;
      return Res;
   end Parse_File_Type_Definition;

   --  precond : PROTECTED
   --  postcond: ';'
   --
   --  [ 3.5 ]
   --  protected_type_definition ::= protected_type_declaration
   --                              | protected_type_body
   --
   --  [ 3.5.1 ]
   --  protected_type_declaration ::= PROTECTED
   --                                     protected_type_declarative_part
   --                                 END PROTECTED [ simple_name ]
   --
   --  protected_type_declarative_part ::=
   --     { protected_type_declarative_item }
   --
   --  protected_type_declarative_item ::=
   --       subprogram_declaration
   --     | attribute_specification
   --     | use_clause
   --
   --  [ 3.5.2 ]
   --  protected_type_body ::= PROTECTED BODY
   --                              protected_type_body_declarative_part
   --                          END PROTECTED BODY [ simple_name ]
   --
   --  protected_type_body_declarative_part ::=
   --      { protected_type_body_declarative_item }
   --
   --  protected_type_body_declarative_item ::=
   --        subprogram_declaration
   --      | subprogram_body
   --      | type_declaration
   --      | subtype_declaration
   --      | constant_declaration
   --      | variable_declaration
   --      | file_declaration
   --      | alias_declaration
   --      | attribute_declaration
   --      | attribute_specification
   --      | use_clause
   --      | group_template_declaration
   --      | group_declaration
   function Parse_Protected_Type_Definition
     (Ident : Name_Id; Loc : Location_Type) return Iir
   is
      Res : Iir;
      Decl : Iir;
   begin
      Scan;
      if Current_Token = Tok_Body then
         Res := Create_Iir (Iir_Kind_Protected_Type_Body);
         Scan;
         Decl := Res;
      else
         Decl := Create_Iir (Iir_Kind_Type_Declaration);
         Res := Create_Iir (Iir_Kind_Protected_Type_Declaration);
         Set_Location (Res, Loc);
         Set_Type_Definition (Decl, Res);
      end if;
      Set_Identifier (Decl, Ident);
      Set_Location (Decl, Loc);

      Parse_Declarative_Part (Res);

      Expect (Tok_End);
      Scan_Expect (Tok_Protected);
      Set_End_Has_Reserved_Id (Res, True);
      if Get_Kind (Res) = Iir_Kind_Protected_Type_Body then
         Scan_Expect (Tok_Body);
      end if;
      Scan;
      Check_End_Name (Ident, Res);
      return Decl;
   end Parse_Protected_Type_Definition;

   --  precond : TYPE
   --  postcond: a token
   --
   --  [ LRM93 4.1 ]
   --  type_definition ::= scalar_type_definition
   --                    | composite_type_definition
   --                    | access_type_definition
   --                    | file_type_definition
   --                    | protected_type_definition
   --
   --  [ LRM93 3.1 ]
   --  scalar_type_definition ::= enumeration_type_definition
   --                           | integer_type_definition
   --                           | floating_type_definition
   --                           | physical_type_definition
   --
   --  [ LRM93 3.2 ]
   --  composite_type_definition ::= array_type_definition
   --                              | record_type_definition
   --
   --  [ LRM93 3.1.2 ]
   --  integer_type_definition ::= range_constraint
   --
   --  [ LRM93 3.1.4 ]
   --  floating_type_definition ::= range_constraint
   function Parse_Type_Declaration (Parent : Iir) return Iir
   is
      Def : Iir;
      Loc : Location_Type;
      Ident : Name_Id;
      Decl : Iir;
   begin
      -- The current token must be type.
      pragma Assert (Current_Token = Tok_Type);

      -- Get the identifier
      Scan_Expect (Tok_Identifier,
                   "an identifier is expected after 'type' keyword");
      Loc := Get_Token_Location;
      Ident := Current_Identifier;

      --  Skip identifier
      Scan;

      if Current_Token = Tok_Semi_Colon then
         --  If there is a ';', this is an imcomplete type declaration.
         Invalidate_Current_Token;
         Decl := Create_Iir (Iir_Kind_Type_Declaration);
         Set_Identifier (Decl, Ident);
         Set_Location (Decl, Loc);
         return Decl;
      end if;

      if Current_Token /= Tok_Is then
         Error_Msg_Parse ("'is' expected here");
         --  Act as if IS token was forgotten.
      else
         --  Eat IS token.
         Scan;
      end if;

      case Current_Token is
         when Tok_Left_Paren =>
            --  This is an enumeration.
            Def := Parse_Enumeration_Type_Definition (Parent);
            Decl := Null_Iir;

         when Tok_Range =>
            --  This is a range definition.
            Decl := Create_Iir (Iir_Kind_Anonymous_Type_Declaration);
            Set_Identifier (Decl, Ident);
            Set_Location (Decl, Loc);

            --  Skip 'range'
            Scan;

            Def := Parse_Range_Constraint;
            Set_Type_Definition (Decl, Def);

            if Current_Token = Tok_Units then
               --  A physical type definition.
               declare
                  Unit_Def : Iir;
               begin
                  Unit_Def := Parse_Physical_Type_Definition (Parent);
                  if Current_Token = Tok_Identifier then
                     if Flags.Vhdl_Std = Vhdl_87 then
                        Error_Msg_Parse
                          ("simple_name not allowed here in vhdl87");
                     end if;
                     Check_End_Name (Get_Identifier (Decl), Unit_Def);
                  end if;
                  if Def /= Null_Iir then
                     Set_Type (Def, Unit_Def);
                  end if;
               end;
            end if;

         when Tok_Array =>
            Def := Parse_Array_Definition;
            Decl := Null_Iir;

         when Tok_Record =>
            Decl := Create_Iir (Iir_Kind_Type_Declaration);
            Set_Identifier (Decl, Ident);
            Set_Location (Decl, Loc);
            Def := Parse_Record_Type_Definition;
            Set_Type_Definition (Decl, Def);
            if Current_Token = Tok_Identifier then
               if Flags.Vhdl_Std = Vhdl_87 then
                  Error_Msg_Parse ("simple_name not allowed here in vhdl87");
               end if;
               Check_End_Name (Get_Identifier (Decl), Def);
            end if;

         when Tok_Access =>
            Def := Parse_Access_Type_Definition;
            Decl := Null_Iir;

         when Tok_File =>
            Def := Parse_File_Type_Definition;
            Decl := Null_Iir;

         when Tok_Identifier =>
            if Current_Identifier = Name_Protected then
               Error_Msg_Parse ("protected type not allowed in vhdl87/93");
               Decl := Parse_Protected_Type_Definition (Ident, Loc);
            else
               Error_Msg_Parse ("type '" & Name_Table.Image (Ident) &
                                "' cannot be defined from another type");
               Error_Msg_Parse ("(you should declare a subtype)");
               Decl := Create_Iir (Iir_Kind_Type_Declaration);
               Eat_Tokens_Until_Semi_Colon;
            end if;

         when Tok_Protected =>
            if Flags.Vhdl_Std < Vhdl_00 then
               Error_Msg_Parse ("protected type not allowed in vhdl87/93");
            end if;
            Decl := Parse_Protected_Type_Definition (Ident, Loc);

         when others =>
            Error_Msg_Parse
              ("type definition starting with a keyword such as RANGE, ARRAY");
            Error_Msg_Parse
              (" FILE, RECORD or '(' is expected here");
            Eat_Tokens_Until_Semi_Colon;
            Decl := Create_Iir (Iir_Kind_Type_Declaration);
      end case;

      if Decl = Null_Iir then
         case Get_Kind (Def) is
            when Iir_Kind_Enumeration_Type_Definition
              | Iir_Kind_Access_Type_Definition
              | Iir_Kind_Array_Type_Definition
              | Iir_Kind_File_Type_Definition =>
               Decl := Create_Iir (Iir_Kind_Type_Declaration);
            when Iir_Kind_Array_Subtype_Definition =>
               Decl := Create_Iir (Iir_Kind_Anonymous_Type_Declaration);
            when others =>
               Error_Kind ("parse_type_declaration", Def);
         end case;
         Set_Type_Definition (Decl, Def);
      end if;
      Set_Identifier (Decl, Ident);
      Set_Location (Decl, Loc);

      -- ';' is expected after end of type declaration
      Expect (Tok_Semi_Colon);
      Invalidate_Current_Token;
      return Decl;
   end Parse_Type_Declaration;

   --  precond: '(' or identifier
   --  postcond: next token
   --
   --  [ LRM08 6.3 ]
   --
   --  resolution_indication ::=
   --      resolution_function_name | ( element_resolution )
   --
   --  element_resolution ::=
   --      array_element_resolution | record_resolution
   --
   --  array_element_resolution ::= resolution_indication
   --
   --  record_resolution ::=
   --      record_element_resolution { , record_element_resolution }
   --
   --  record_element_resolution ::=
   --      record_element_simple_name resolution_indication
   function Parse_Resolution_Indication return Iir
   is
      Ind : Iir;
      Def : Iir;
      Loc : Location_Type;
   begin
      if Current_Token = Tok_Identifier then
         --  Resolution function name.
         return Parse_Name (Allow_Indexes => False);
      elsif Current_Token = Tok_Left_Paren then
         --  Element resolution.
         Loc := Get_Token_Location;

         --  Eat '('
         Scan;

         Ind := Parse_Resolution_Indication;
         if Current_Token = Tok_Identifier
           or else Current_Token = Tok_Left_Paren
         then
            declare
               Id : Name_Id;
               El : Iir;
               First, Last : Iir;
            begin
               --  This was in fact a record_resolution.
               if Get_Kind (Ind) = Iir_Kind_Simple_Name then
                  Id := Get_Identifier (Ind);
               else
                  Error_Msg_Parse ("element name expected", Ind);
                  Id := Null_Identifier;
               end if;
               Free_Iir (Ind);

               Def := Create_Iir (Iir_Kind_Record_Resolution);
               Set_Location (Def, Loc);
               Sub_Chain_Init (First, Last);
               loop
                  El := Create_Iir (Iir_Kind_Record_Element_Resolution);
                  Set_Location (El, Loc);
                  Set_Identifier (El, Id);
                  Set_Resolution_Indication (El, Parse_Resolution_Indication);
                  Sub_Chain_Append (First, Last, El);
                  exit when Current_Token = Tok_Right_Paren;

                  --  Eat ','
                  Expect (Tok_Comma);
                  Scan;

                  if Current_Token /= Tok_Identifier then
                     Error_Msg_Parse ("record element identifier expected");
                     exit;
                  end if;
                  Id := Current_Identifier;
                  Loc := Get_Token_Location;

                  --  Eat identifier
                  Scan;
               end loop;
               Set_Record_Element_Resolution_Chain (Def, First);
            end;
         else
            Def := Create_Iir (Iir_Kind_Array_Element_Resolution);
            Set_Location (Def, Loc);
            Set_Resolution_Indication (Def, Ind);
         end if;

         --  Eat ')'
         Expect (Tok_Right_Paren);
         Scan;

         return Def;
      else
         Error_Msg_Parse ("resolution indication expected");
         raise Parse_Error;
      end if;
   end Parse_Resolution_Indication;

   --  precond : '('
   --  postcond: next token
   --
   --  [ LRM08 6.3 Subtype declarations ]
   --  element_constraint ::=
   --      array_constraint | record_constraint
   --
   --  [ LRM08 5.3.2.1 Array types ]
   --  array_constraint ::=
   --      index_constraint [ array_element_constraint ]
   --      | ( open ) [ array_element_constraint ]
   --
   --  array_element_constraint ::= element_constraint
   --
   --  RES is the resolution_indication of the subtype indication.
   function Parse_Element_Constraint return Iir
   is
      Def : Iir;
      El : Iir;
      Index_List : Iir_List;
   begin
      --  Index_constraint.
      Def := Create_Iir (Iir_Kind_Array_Subtype_Definition);
      Set_Location (Def);

      --  Eat '('.
      Scan;

      if Current_Token = Tok_Open then
         --  Eat 'open'.
         Scan;
      else
         Index_List := Create_Iir_List;
         Set_Index_Constraint_List (Def, Index_List);
         --  index_constraint ::= (discrete_range {, discrete_range} )
         loop
            El := Parse_Discrete_Range;
            Append_Element (Index_List, El);

            exit when Current_Token = Tok_Right_Paren;

            --  Eat ','
            Expect (Tok_Comma);
            Scan;
         end loop;
      end if;

      --  Eat ')'
      Expect (Tok_Right_Paren);
      Scan;

      if Current_Token = Tok_Left_Paren then
         Set_Element_Subtype (Def, Parse_Element_Constraint);
      end if;
      return Def;
   end Parse_Element_Constraint;

   --  precond : tolerance
   --  postcond: next token
   --
   --  [ LRM93 4.2 ]
   --  tolerance_aspect ::= TOLERANCE string_expression
   function Parse_Tolerance_Aspect_Opt return Iir is
   begin
      if AMS_Vhdl
        and then Current_Token = Tok_Tolerance
      then
         Scan;
         return Parse_Expression;
      else
         return Null_Iir;
      end if;
   end Parse_Tolerance_Aspect_Opt;

   --  precond : identifier or '('
   --  postcond: next token
   --
   --  [ LRM93 4.2 ]
   --  subtype_indication ::=
   --      [ RESOLUTION_FUNCTION_name ] type_mark [ constraint ]
   --
   --  constraint ::= range_constraint | index_constraint
   --
   --  [ LRM08 6.3 ]
   --  subtype_indication ::=
   --      [ resolution_indication ] type_mark [ constraint ]
   --
   --  constraint ::=
   --      range_constraint | array_constraint | record_constraint
   --
   --  NAME is the type_mark when already parsed (in range expression or
   --   allocator by type).
   function Parse_Subtype_Indication (Name : Iir := Null_Iir)
     return Iir
   is
      Type_Mark : Iir;
      Def: Iir;
      Resolution_Indication: Iir;
      Tolerance : Iir;
   begin
      -- FIXME: location.
      Resolution_Indication := Null_Iir;
      Def := Null_Iir;

      if Name /= Null_Iir then
         --  The type_mark was already parsed.
         Type_Mark := Name;
         Check_Type_Mark (Name);
      else
         if Current_Token = Tok_Left_Paren then
            if Vhdl_Std < Vhdl_08 then
               Error_Msg_Parse
                 ("resolution_indication not allowed before vhdl08");
            end if;
            Resolution_Indication := Parse_Resolution_Indication;
         end if;
         if Current_Token /= Tok_Identifier then
            Error_Msg_Parse ("type mark expected in a subtype indication");
            raise Parse_Error;
         end if;
         Type_Mark := Parse_Type_Mark (Check_Paren => False);
      end if;

      if Current_Token = Tok_Identifier then
         if Resolution_Indication /= Null_Iir then
            Error_Msg_Parse ("resolution function already indicated");
         end if;
         Resolution_Indication := Type_Mark;
         Type_Mark := Parse_Type_Mark (Check_Paren => False);
      end if;

      case Current_Token is
         when Tok_Left_Paren =>
            --  element_constraint.
            Def := Parse_Element_Constraint;
            Set_Subtype_Type_Mark (Def, Type_Mark);
            Set_Resolution_Indication (Def, Resolution_Indication);
            Set_Tolerance (Def, Parse_Tolerance_Aspect_Opt);

         when Tok_Range =>
            --  range_constraint.
            --  Skip 'range'
            Scan;

            Def := Parse_Range_Constraint_Of_Subtype_Indication
              (Type_Mark, Resolution_Indication);

         when others =>
            Tolerance := Parse_Tolerance_Aspect_Opt;
            if Resolution_Indication /= Null_Iir
              or else Tolerance /= Null_Iir
            then
               --  A subtype needs to be created.
               Def := Create_Iir (Iir_Kind_Subtype_Definition);
               Location_Copy (Def, Type_Mark);
               Set_Subtype_Type_Mark (Def, Type_Mark);
               Set_Resolution_Indication (Def, Resolution_Indication);
               Set_Tolerance (Def, Tolerance);
            else
               --  This is just an alias.
               Def := Type_Mark;
            end if;
      end case;
      return Def;
   end Parse_Subtype_Indication;

   --  precond : SUBTYPE
   --  postcond: ';'
   --
   --  [ §4.2 ]
   --  subtype_declaration ::= SUBTYPE identifier IS subtype_indication ;
   function Parse_Subtype_Declaration (Parent : Iir)
                                      return Iir_Subtype_Declaration
   is
      Decl: Iir_Subtype_Declaration;
      Def: Iir;
   begin
      Decl := Create_Iir (Iir_Kind_Subtype_Declaration);

      Scan_Expect (Tok_Identifier);
      Set_Identifier (Decl, Current_Identifier);
      Set_Parent (Decl, Parent);
      Set_Location (Decl);

      --  Skip identifier.
      Scan_Expect (Tok_Is);

      --  Skip 'is'.
      Scan;

      Def := Parse_Subtype_Indication;
      Set_Subtype_Indication (Decl, Def);

      Expect (Tok_Semi_Colon);
      return Decl;
   end Parse_Subtype_Declaration;

   --  precond : NATURE
   --  postcond: a token
   --
   --  [ §4.8 ]
   --  nature_definition ::= scalar_nature_definition
   --                    | composite_nature_definition
   --
   --  [ §3.5.1 ]
   --  scalar_nature_definition ::= type_mark ACROSS
   --                               type_mark THROUGH
   --                               identifier REFERENCE
   --
   --  [ §3.5.2 ]
   --  composite_nature_definition ::= array_nature_definition
   --                              | record_nature_definition
   function Parse_Nature_Declaration return Iir
   is
      Def : Iir;
      Ref : Iir;
      Loc : Location_Type;
      Ident : Name_Id;
      Decl : Iir;
   begin
      -- The current token must be type.
      if Current_Token /= Tok_Nature then
         raise Program_Error;
      end if;

      -- Get the identifier
      Scan_Expect (Tok_Identifier,
                   "an identifier is expected after 'nature'");
      Loc := Get_Token_Location;
      Ident := Current_Identifier;

      Scan;

      if Current_Token /= Tok_Is then
         Error_Msg_Parse ("'is' expected here");
         --  Act as if IS token was forgotten.
      else
         --  Eat IS token.
         Scan;
      end if;

      case Current_Token is
         when Tok_Array =>
            --  TODO
            Error_Msg_Parse ("array nature definition not supported");
            Def := Null_Iir;
            Eat_Tokens_Until_Semi_Colon;
         when Tok_Record =>
            --  TODO
            Error_Msg_Parse ("record nature definition not supported");
            Def := Null_Iir;
            Eat_Tokens_Until_Semi_Colon;
         when Tok_Identifier =>
            Def := Create_Iir (Iir_Kind_Scalar_Nature_Definition);
            Set_Location (Def, Loc);
            Set_Across_Type (Def, Parse_Type_Mark);
            if Current_Token = Tok_Across then
               Scan;
            else
               Expect (Tok_Across, "'across' expected after type mark");
            end if;
            Set_Through_Type (Def, Parse_Type_Mark);
            if Current_Token = Tok_Through then
               Scan;
            else
               Expect (Tok_Across, "'through' expected after type mark");
            end if;
            if Current_Token = Tok_Identifier then
               Ref := Create_Iir (Iir_Kind_Terminal_Declaration);
               Set_Identifier (Ref, Current_Identifier);
               Set_Location (Ref);
               Set_Reference (Def, Ref);
               Scan;
               if Current_Token = Tok_Reference then
                  Scan;
               else
                  Expect (Tok_Reference, "'reference' expected");
                  Eat_Tokens_Until_Semi_Colon;
               end if;
            else
               Error_Msg_Parse ("reference identifier expected");
               Eat_Tokens_Until_Semi_Colon;
            end if;
         when others =>
            Error_Msg_Parse ("nature definition expected here");
            Eat_Tokens_Until_Semi_Colon;
      end case;

      Decl := Create_Iir (Iir_Kind_Nature_Declaration);
      Set_Nature (Decl, Def);
      Set_Identifier (Decl, Ident);
      Set_Location (Decl, Loc);

      -- ';' is expected after end of type declaration
      Expect (Tok_Semi_Colon);
      Invalidate_Current_Token;
      return Decl;
   end Parse_Nature_Declaration;

   --  precond : identifier
   --  postcond: next token
   --
   --  LRM 4.8 Nature declaration
   --
   --  subnature_indication ::=
   --      nature_mark [ index_constraint ]
   --      [ TOLERANCE string_expression ACROSS string_expression THROUGH ]
   --
   --  nature_mark ::=
   --      nature_name | subnature_name
   function Parse_Subnature_Indication return Iir is
      Nature_Mark : Iir;
   begin
      if Current_Token /= Tok_Identifier then
         Error_Msg_Parse ("nature mark expected in a subnature indication");
         raise Parse_Error;
      end if;
      Nature_Mark := Parse_Name (Allow_Indexes => False);

      if Current_Token = Tok_Left_Paren then
         --  TODO
         Error_Msg_Parse
           ("index constraint not supported for subnature indication");
         raise Parse_Error;
      end if;

      if Current_Token = Tok_Tolerance then
         Error_Msg_Parse
           ("tolerance not supported for subnature indication");
         raise Parse_Error;
      end if;
      return Nature_Mark;
   end Parse_Subnature_Indication;

   --  precond : TERMINAL
   --  postcond: ;
   --
   --  [ 4.3.1.5 Terminal declarations ]
   --  terminal_declaration ::=
   --      TERMINAL identifier_list : subnature_indication
   function Parse_Terminal_Declaration (Parent : Iir) return Iir
   is
      --  First and last element of the chain to be returned.
      First, Last : Iir;
      Terminal : Iir;
      Subnature : Iir;
   begin
      Sub_Chain_Init (First, Last);

      loop
         -- 'terminal' or "," was just scanned.
         Terminal := Create_Iir (Iir_Kind_Terminal_Declaration);
         Scan_Expect (Tok_Identifier);
         Set_Identifier (Terminal, Current_Identifier);
         Set_Location (Terminal);
         Set_Parent (Terminal, Parent);

         Sub_Chain_Append (First, Last, Terminal);

         Scan;
         exit when Current_Token = Tok_Colon;
         if Current_Token /= Tok_Comma then
            Error_Msg_Parse
              ("',' or ':' is expected after "
                 & "identifier in terminal declaration");
            raise Expect_Error;
         end if;
      end loop;

      -- The colon was parsed.
      Scan;
      Subnature := Parse_Subnature_Indication;

      Terminal := First;
      while Terminal /= Null_Iir loop
         -- Type definitions are factorized.  This is OK, but not done by
         -- sem.
         if Terminal = First then
            Set_Nature (Terminal, Subnature);
         else
            Set_Nature (Terminal, Null_Iir);
         end if;
         Terminal := Get_Chain (Terminal);
      end loop;
      Expect (Tok_Semi_Colon);
      return First;
   end Parse_Terminal_Declaration;

   --  precond : QUANTITY
   --  postcond: ;
   --
   --  [ 4.3.1.6 Quantity declarations ]
   --  quantity_declaration ::=
   --      free_quantity_declaration
   --      | branch_quantity_declaration
   --      | source_quantity_declaration
   --
   --  free_quantity_declaration ::=
   --      QUANTITY identifier_list : subtype_indication [ := expression ] ;
   --
   --  branch_quantity_declaration ::=
   --      QUANTITY [ across_aspect ] [ through_aspect ] terminal_aspect ;
   --
   --  source_quantity_declaration ::=
   --      QUANTITY identifier_list : subtype_indication source_aspect ;
   --
   --  across_aspect ::=
   --      identifier_list [ tolerance_aspect ] [ := expression ] ACROSS
   --
   --  through_aspect ::=
   --      identifier_list [ tolerance_aspect ] [ := expression ] THROUGH
   --
   --  terminal_aspect ::=
   --      plus_terminal_name [ TO minus_terminal_name ]
   function Parse_Quantity_Declaration (Parent : Iir) return Iir
   is
      --  First and last element of the chain to be returned.
      First, Last : Iir;
      Object : Iir;
      New_Object : Iir;
      Tolerance : Iir;
      Default_Value : Iir;
      Kind : Iir_Kind;
      Plus_Terminal : Iir;
   begin
      Sub_Chain_Init (First, Last);

      --  Eat 'quantity'
      Scan;

      loop
         --  Quantity or "," was just scanned.  We assume a free quantity
         --  declaration and will change to branch or source quantity if
         --  necessary.
         Object := Create_Iir (Iir_Kind_Free_Quantity_Declaration);
         Expect (Tok_Identifier);
         Set_Identifier (Object, Current_Identifier);
         Set_Location (Object);
         Set_Parent (Object, Parent);

         Sub_Chain_Append (First, Last, Object);

         --  Eat identifier
         Scan;
         exit when Current_Token /= Tok_Comma;

         --  Eat ','
         Scan;
      end loop;

      case Current_Token is
         when Tok_Colon =>
            --  Either a free quantity (or a source quantity)
            --  TODO
            raise Program_Error;
         when Tok_Tolerance
           | Tok_Assign
           | Tok_Across
           | Tok_Through =>
            --  A branch quantity

            --  Parse tolerance aspect
            Tolerance := Parse_Tolerance_Aspect_Opt;

            --  Parse default value
            if Current_Token = Tok_Assign then
               Scan;
               Default_Value := Parse_Expression;
            else
               Default_Value := Null_Iir;
            end if;

            case Current_Token is
               when Tok_Across =>
                  Kind := Iir_Kind_Across_Quantity_Declaration;
               when Tok_Through =>
                  Kind := Iir_Kind_Through_Quantity_Declaration;
               when others =>
                  Error_Msg_Parse ("'across' or 'through' expected here");
                  Eat_Tokens_Until_Semi_Colon;
                  raise Expect_Error;
            end case;

            --  Eat across/through
            Scan;

            --  Change declarations
            Object := First;
            Sub_Chain_Init (First, Last);
            while Object /= Null_Iir loop
               New_Object := Create_Iir (Kind);
               Location_Copy (New_Object, Object);
               Set_Identifier (New_Object, Get_Identifier (Object));
               Set_Parent (New_Object, Parent);
               Set_Tolerance (New_Object, Tolerance);
               Set_Default_Value (New_Object, Default_Value);

               Sub_Chain_Append (First, Last, New_Object);

               if Object /= First then
                  Set_Plus_Terminal (New_Object, Null_Iir);
               end if;
               New_Object := Get_Chain (Object);
               Free_Iir (Object);
               Object := New_Object;
            end loop;

            --  Parse terminal (or first identifier of through declarations)
            Plus_Terminal := Parse_Name;

            case Current_Token is
               when Tok_Comma
                 | Tok_Tolerance
                 | Tok_Assign
                 | Tok_Through
                 | Tok_Across =>
                  --  Through quantity declaration.  Convert the Plus_Terminal
                  --  to a declaration.
                  Object := Create_Iir (Iir_Kind_Through_Quantity_Declaration);
                  New_Object := Object;
                  Location_Copy (Object, Plus_Terminal);
                  if Get_Kind (Plus_Terminal) /= Iir_Kind_Simple_Name then
                     Error_Msg_Parse
                       ("identifier for quantity declaration expected");
                  else
                     Set_Identifier (Object, Get_Identifier (Plus_Terminal));
                  end if;
                  Set_Plus_Terminal (Object, Null_Iir);
                  Free_Iir (Plus_Terminal);

                  loop
                     Set_Parent (Object, Parent);
                     Sub_Chain_Append (First, Last, Object);
                     exit when Current_Token /= Tok_Comma;
                     Scan;

                     Object := Create_Iir
                       (Iir_Kind_Through_Quantity_Declaration);
                     Set_Location (Object);
                     if Current_Token /= Tok_Identifier then
                        Error_Msg_Parse
                          ("identifier for quantity declaration expected");
                     else
                        Set_Identifier (Object, Current_Identifier);
                        Scan;
                     end if;
                     Set_Plus_Terminal (Object, Null_Iir);

                  end loop;

                  --  Parse tolerance aspect
                  Set_Tolerance (Object, Parse_Tolerance_Aspect_Opt);

                  --  Parse default value
                  if Current_Token = Tok_Assign then
                     Scan;
                     Set_Default_Value (Object, Parse_Expression);
                  end if;

                  --  Scan 'through'
                  if Current_Token = Tok_Through then
                     Scan;
                  elsif Current_Token = Tok_Across then
                     Error_Msg_Parse ("across quantity declaration must appear"
                                        & " before though declaration");
                     Scan;
                  else
                     Error_Msg_Parse ("'through' expected");
                  end if;

                  --  Parse plus terminal
                  Plus_Terminal := Parse_Name;
               when others =>
                  null;
            end case;

            Set_Plus_Terminal (First, Plus_Terminal);

            --  Parse minus terminal (if present)
            if Current_Token = Tok_To then
               Scan;
               Set_Minus_Terminal (First, Parse_Name);
            end if;
         when others =>
            Error_Msg_Parse ("missign type or across/throught aspect "
                               & "in quantity declaration");
            Eat_Tokens_Until_Semi_Colon;
            raise Expect_Error;
      end case;
      Expect (Tok_Semi_Colon);
      return First;
   end Parse_Quantity_Declaration;

   --  precond : token (CONSTANT, SIGNAL, VARIABLE, FILE)
   --  postcond: ;
   --
   --  KIND can be iir_kind_constant_declaration, iir_kind_file_declaration
   --   or iir_kind_variable_declaration
   --
   --  [ LRM93 4.3.1 ]
   --  object_declaration ::= constant_declaration
   --                       | signal_declaration
   --                       | variable_declaration
   --                       | file_declaration
   --
   --  [ LRM93 4.3.1.1 ]
   --  constant_declaration ::=
   --      CONSTANT identifier_list : subtype_indication [ := expression ]
   --
   --  [ LRM87 4.3.2 ]
   --  file_declaration ::=
   --      FILE identifier : subtype_indication IS [ mode ] file_logical_name
   --
   --  [ LRM93 4.3.1.4 ]
   --  file_declaration ::=
   --      FILE identifier_list : subtype_indication [ file_open_information ]
   --
   --  [ LRM93 4.3.1.4 ]
   --  file_open_information ::=
   --      [ OPEN FILE_OPEN_KIND_expression ] IS file_logical_name
   --
   --  [ LRM93 4.3.1.4 ]
   --  file_logical_name ::= STRING_expression
   --
   --  [ LRM93 4.3.1.3 ]
   --  variable_declaration ::=
   --      [ SHARED ] VARIABLE identifier_list : subtype_indication
   --          [ := expression ]
   --
   --  [ LRM93 4.3.1.2 ]
   --  signal_declaration ::=
   --      SIGNAL identifier_list : subtype_information [ signal_kind ]
   --          [ := expression ]
   --
   --  [ LRM93 4.3.1.2 ]
   --  signal_kind ::= REGISTER | BUS
   --
   --  FIXME: file_open_information.
   function Parse_Object_Declaration (Parent : Iir) return Iir
   is
      --  First and last element of the chain to be returned.
      First, Last : Iir;
      Object: Iir;
      Object_Type: Iir;
      Default_Value : Iir;
      Mode: Iir_Mode;
      Signal_Kind : Iir_Signal_Kind;
      Is_Guarded : Boolean;
      Open_Kind : Iir;
      Logical_Name : Iir;
      Kind: Iir_Kind;
      Shared : Boolean;
      Has_Mode : Boolean;
   begin
      Sub_Chain_Init (First, Last);

      -- object keyword was just scanned.
      case Current_Token is
         when Tok_Signal =>
            Kind := Iir_Kind_Signal_Declaration;
         when Tok_Constant =>
            Kind := Iir_Kind_Constant_Declaration;
         when Tok_File =>
            Kind := Iir_Kind_File_Declaration;
         when Tok_Variable =>
            Kind := Iir_Kind_Variable_Declaration;
            Shared := False;
         when Tok_Shared =>
            Kind := Iir_Kind_Variable_Declaration;
            Shared := True;
            Scan_Expect (Tok_Variable);
         when others =>
            raise Internal_Error;
      end case;

      loop
         -- object or "," was just scanned.
         Object := Create_Iir (Kind);
         if Kind = Iir_Kind_Variable_Declaration then
            Set_Shared_Flag (Object, Shared);
         end if;
         Scan_Expect (Tok_Identifier);
         Set_Identifier (Object, Current_Identifier);
         Set_Location (Object);
         Set_Parent (Object, Parent);

         Sub_Chain_Append (First, Last, Object);

         Scan;
         exit when Current_Token = Tok_Colon;
         if Current_Token /= Tok_Comma then
            case Current_Token is
               when Tok_Assign =>
                  Error_Msg_Parse ("missign type in " & Disp_Name (Kind));
                  exit;
               when others =>
                  Error_Msg_Parse
                    ("',' or ':' is expected after identifier in "
                     & Disp_Name (Kind));
                  raise Expect_Error;
            end case;
         end if;
         Set_Has_Identifier_List (Object, True);
      end loop;

      -- Eat ':'
      Scan;

      Object_Type := Parse_Subtype_Indication;

      if Kind = Iir_Kind_Signal_Declaration then
         Parse_Signal_Kind (Is_Guarded, Signal_Kind);
      end if;

      if Current_Token = Tok_Assign then
         if Kind = Iir_Kind_File_Declaration then
            Error_Msg_Parse
              ("default expression not allowed for a file declaration");
         end if;

         --  Skip ':='.
         Scan;

         Default_Value := Parse_Expression;
      elsif Current_Token = Tok_Equal then
         Error_Msg_Parse ("= should be := for initial value");

         --  Skip '='
         Scan;

         Default_Value := Parse_Expression;
      else
         Default_Value := Null_Iir;
      end if;

      if Kind = Iir_Kind_File_Declaration then
         if Current_Token = Tok_Open then
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse
                 ("'open' and open kind expression not allowed in vhdl 87");
            end if;
            Scan;
            Open_Kind := Parse_Expression;
         else
            Open_Kind := Null_Iir;
         end if;

         --  LRM 4.3.1.4
         --  The default mode is IN, if no mode is specified.
         Mode := Iir_In_Mode;

         Logical_Name := Null_Iir;
         Has_Mode := False;
         if Current_Token = Tok_Is then
            --  Skip 'is'.
            Scan;

            case Current_Token is
               when Tok_In | Tok_Out | Tok_Inout =>
                  if Flags.Vhdl_Std >= Vhdl_93 then
                     Error_Msg_Parse ("mode allowed only in vhdl 87");
                  end if;
                  Mode := Parse_Mode;
                  if Mode = Iir_Inout_Mode then
                     Error_Msg_Parse ("inout mode not allowed for file");
                  end if;
                  Has_Mode := True;
               when others =>
                  null;
            end case;
            Logical_Name := Parse_Expression;
         elsif Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse ("file name expected (vhdl 87)");
         end if;
      end if;

      Set_Subtype_Indication (First, Object_Type);
      if Kind /= Iir_Kind_File_Declaration then
         Set_Default_Value (First, Default_Value);
      end if;

      Object := First;
      while Object /= Null_Iir loop
         case Kind is
            when Iir_Kind_File_Declaration =>
               Set_Mode (Object, Mode);
               Set_File_Open_Kind (Object, Open_Kind);
               Set_File_Logical_Name (Object, Logical_Name);
               Set_Has_Mode (Object, Has_Mode);
            when Iir_Kind_Signal_Declaration =>
               Set_Guarded_Signal_Flag (Object, Is_Guarded);
               Set_Signal_Kind (Object, Signal_Kind);
            when others =>
               null;
         end case;
         Set_Is_Ref (Object, Object /= First);
         Object := Get_Chain (Object);
      end loop;

      --  ';' is not eaten.
      Expect (Tok_Semi_Colon);

      return First;
   end Parse_Object_Declaration;

   --  precond : COMPONENT
   --  postcond: ';'
   --
   --  [ §4.5 ]
   --  component_declaration ::=
   --      COMPONENT identifier [ IS ]
   --          [ LOCAL_generic_clause ]
   --          [ LOCAL_port_clause ]
   --      END COMPONENT [ COMPONENT_simple_name ] ;
   function Parse_Component_Declaration
     return Iir_Component_Declaration
   is
      Component: Iir_Component_Declaration;
   begin
      Component := Create_Iir (Iir_Kind_Component_Declaration);
      Scan_Expect (Tok_Identifier,
                   "an identifier is expected after 'component'");
      Set_Identifier (Component, Current_Identifier);
      Set_Location (Component);
      Scan;
      if Current_Token = Tok_Is then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse ("""is"" keyword is not allowed here by vhdl 87");
         end if;
         Set_Has_Is (Component, True);
         Scan;
      end if;
      Parse_Generic_Port_Clauses (Component);
      Check_End_Name (Tok_Component, Component);
      return Component;
   end Parse_Component_Declaration;

   --  precond : '['
   --  postcond: next token after ']'
   --
   --  [ 2.3.2 ]
   --  signature ::= [ [ type_mark { , type_mark } ] [ RETURN type_mark ] ]
   function Parse_Signature return Iir_Signature
   is
      Res : Iir_Signature;
      List : Iir_List;
   begin
      Expect (Tok_Left_Bracket);
      Res := Create_Iir (Iir_Kind_Signature);
      Set_Location (Res);

      --  Skip '['
      Scan;

      --  List of type_marks.
      if Current_Token = Tok_Identifier then
         List := Create_Iir_List;
         Set_Type_Marks_List (Res, List);
         loop
            Append_Element (List, Parse_Type_Mark (Check_Paren => True));
            exit when Current_Token /= Tok_Comma;
            Scan;
         end loop;
      end if;

      if Current_Token = Tok_Return then
         --  Skip 'return'
         Scan;

         Set_Return_Type_Mark (Res, Parse_Name);
      end if;

      --  Skip ']'
      Expect (Tok_Right_Bracket);
      Scan;

      return Res;
   end Parse_Signature;

   --  precond : ALIAS
   --  postcond: a token
   --
   --  [ LRM93 4.3.3 ]
   --  alias_declaration ::=
   --      ALIAS alias_designator [ : subtype_indication ]
   --          IS name [ signature ] ;
   --
   --  [ LRM93 4.3.3 ]
   --  alias_designator ::= identifier | character_literal | operator_symbol
   --
   --  FIXME: signature is not part of the node.
   function Parse_Alias_Declaration return Iir
   is
      Res: Iir;
      Ident : Name_Id;
   begin
      --  Eat 'alias'.
      Scan;

      Res := Create_Iir (Iir_Kind_Object_Alias_Declaration);
      Set_Location (Res);

      case Current_Token is
         when Tok_Identifier =>
            Ident := Current_Identifier;
         when Tok_Character =>
            Ident := Current_Identifier;
         when Tok_String =>
            Ident := Scan_To_Operator_Name (Get_Token_Location);
            --  FIXME: vhdl87
            --  FIXME: operator symbol.
         when others =>
            Error_Msg_Parse ("alias designator expected");
      end case;

      --  Eat identifier.
      Set_Identifier (Res, Ident);
      Scan;

      if Current_Token = Tok_Colon then
         Scan;
         Set_Subtype_Indication (Res, Parse_Subtype_Indication);
      end if;

      --  FIXME: nice message if token is ':=' ?
      Expect (Tok_Is);
      Scan;
      Set_Name (Res, Parse_Name);

      return Res;
   end Parse_Alias_Declaration;

   --  precond : FOR
   --  postcond: ';'
   --
   --  [ §5.2 ]
   --  configuration_specification ::=
   --      FOR component_specification binding_indication ;
   function Parse_Configuration_Specification
     return Iir_Configuration_Specification
   is
      Res : Iir_Configuration_Specification;
   begin
      Res := Create_Iir (Iir_Kind_Configuration_Specification);
      Set_Location (Res);
      Expect (Tok_For);
      Scan;
      Parse_Component_Specification (Res);
      Set_Binding_Indication (Res, Parse_Binding_Indication);
      Expect (Tok_Semi_Colon);
      return Res;
   end Parse_Configuration_Specification;

   --  precond : next token
   --  postcond: next token
   --
   --  [ § 5.2 ]
   --  entity_class := ENTITY | ARCHITECTURE | CONFIGURATION | PROCEDURE
   --                | FUNCTION | PACKAGE | TYPE | SUBTYPE | CONSTANT
   --                | SIGNAL | VARIABLE | COMPONENT | LABEL | LITERAL
   --                | UNITS | GROUP | FILE
   function Parse_Entity_Class return Token_Type
   is
      Res : Token_Type;
   begin
      case Current_Token is
         when Tok_Entity
           | Tok_Architecture
           | Tok_Configuration
           | Tok_Procedure
           | Tok_Function
           | Tok_Package
           | Tok_Type
           | Tok_Subtype
           | Tok_Constant
           | Tok_Signal
           | Tok_Variable
           | Tok_Component
           | Tok_Label =>
            null;
         when Tok_Literal
           | Tok_Units
           | Tok_Group
           | Tok_File =>
            null;
         when others =>
            Error_Msg_Parse
              (''' & Tokens.Image (Current_Token) & "' is not a entity class");
      end case;
      Res := Current_Token;
      Scan;
      return Res;
   end Parse_Entity_Class;

   function Parse_Entity_Class_Entry return Iir_Entity_Class
   is
      Res : Iir_Entity_Class;
   begin
      Res := Create_Iir (Iir_Kind_Entity_Class);
      Set_Location (Res);
      Set_Entity_Class (Res, Parse_Entity_Class);
      return Res;
   end Parse_Entity_Class_Entry;

   --  precond : next token
   --  postcond: next token
   --
   --  [ §5.1 ]
   --  entity_designator ::= entity_tag [ signature ]
   --
   --  entity_tag ::= simple_name | character_literal | operator_symbol
   function Parse_Entity_Designator return Iir
   is
      Res : Iir;
      Name : Iir;
   begin
      case Current_Token is
         when Tok_Identifier =>
            Res := Create_Iir (Iir_Kind_Simple_Name);
            Set_Location (Res);
            Set_Identifier (Res, Current_Identifier);
         when Tok_Character =>
            Res := Create_Iir (Iir_Kind_Character_Literal);
            Set_Location (Res);
            Set_Identifier (Res, Current_Identifier);
         when Tok_String =>
            Res := Create_Iir (Iir_Kind_Operator_Symbol);
            Set_Location (Res);
            Set_Identifier (Res, Scan_To_Operator_Name (Get_Token_Location));
         when others =>
            Error_Msg_Parse ("identifier, character or string expected");
            raise Expect_Error;
      end case;
      Scan;
      if Current_Token = Tok_Left_Bracket then
         Name := Res;
         Res := Parse_Signature;
         Set_Signature_Prefix (Res, Name);
      end if;
      return Res;
   end Parse_Entity_Designator;

   --  precond : next token
   --  postcond: IS
   --
   --  [ §5.1 ]
   --  entity_name_list ::= entity_designator { , entity_designator }
   --                     | OTHERS
   --                     | ALL
   procedure Parse_Entity_Name_List
     (Attribute : Iir_Attribute_Specification)
   is
      List : Iir_List;
      El : Iir;
   begin
      case Current_Token is
         when Tok_All =>
            List := Iir_List_All;
            Scan;
         when Tok_Others =>
            List := Iir_List_Others;
            Scan;
         when others =>
            List := Create_Iir_List;
            loop
               El := Parse_Entity_Designator;
               Append_Element (List, El);
               exit when Current_Token /= Tok_Comma;
               Scan;
            end loop;
      end case;
      Set_Entity_Name_List (Attribute, List);
      if Current_Token = Tok_Colon then
         Scan;
         Set_Entity_Class (Attribute, Parse_Entity_Class);
      else
         Error_Msg_Parse
           ("missing ':' and entity kind in attribute specification");
      end if;
   end Parse_Entity_Name_List;

   --  precond : ATTRIBUTE
   --  postcond: ';'
   --
   --  [ 4.4 ]
   --  attribute_declaration ::= ATTRIBUTE identifier : type_mark ;
   --
   --  [ 5.1 ]
   --  attribute_specification ::=
   --     ATTRIBUTE attribute_designator OF entity_specification
   --       IS expression ;
   function Parse_Attribute return Iir
   is
      Loc : Location_Type;
      Ident : Name_Id;
   begin
      Expect (Tok_Attribute);
      Scan_Expect (Tok_Identifier);
      Loc := Get_Token_Location;
      Ident := Current_Identifier;
      Scan;
      case Current_Token is
         when Tok_Colon =>
            declare
               Res : Iir_Attribute_Declaration;
            begin
               Res := Create_Iir (Iir_Kind_Attribute_Declaration);
               Set_Location (Res, Loc);
               Set_Identifier (Res, Ident);
               Scan;
               Set_Type_Mark (Res, Parse_Type_Mark (Check_Paren => True));
               Expect (Tok_Semi_Colon);
               return Res;
            end;
         when Tok_Of =>
            declare
               Res : Iir_Attribute_Specification;
               Designator : Iir_Simple_Name;
            begin
               Res := Create_Iir (Iir_Kind_Attribute_Specification);
               Set_Location (Res, Loc);
               Designator := Create_Iir (Iir_Kind_Simple_Name);
               Set_Location (Designator, Loc);
               Set_Identifier (Designator, Ident);
               Set_Attribute_Designator (Res, Designator);
               Scan;
               Parse_Entity_Name_List (Res);
               Expect (Tok_Is);
               Scan;
               Set_Expression (Res, Parse_Expression);
               Expect (Tok_Semi_Colon);
               return Res;
            end;
         when others =>
            Error_Msg_Parse ("':' or 'of' expected after identifier");
            return Null_Iir;
      end case;
   end Parse_Attribute;

   --  precond : GROUP
   --  postcond: ';'
   --
   --  [ §4.6 ]
   --  group_template_declaration ::=
   --     GROUP identifier IS (entity_class_entry_list) ;
   --
   --  entity_class_entry_list ::= entity_class_entry { , entity_class_entry }
   --
   --  entity_class_entry ::= entity_class [ <> ]
   function Parse_Group return Iir is
      Loc : Location_Type;
      Ident : Name_Id;
   begin
      Expect (Tok_Group);
      Scan_Expect (Tok_Identifier);
      Loc := Get_Token_Location;
      Ident := Current_Identifier;
      Scan;
      case Current_Token is
         when Tok_Is =>
            declare
               use Iir_Chains.Entity_Class_Entry_Chain_Handling;
               Res : Iir_Group_Template_Declaration;
               El : Iir_Entity_Class;
               Last : Iir_Entity_Class;
            begin
               Res := Create_Iir (Iir_Kind_Group_Template_Declaration);
               Set_Location (Res, Loc);
               Set_Identifier (Res, Ident);
               Scan_Expect (Tok_Left_Paren);
               Scan;
               Build_Init (Last);
               loop
                  Append (Last, Res, Parse_Entity_Class_Entry);
                  if Current_Token = Tok_Box then
                     El := Create_Iir (Iir_Kind_Entity_Class);
                     Set_Location (El);
                     Set_Entity_Class (El, Tok_Box);
                     Append (Last, Res, El);
                     Scan;
                     if Current_Token = Tok_Comma then
                        Error_Msg_Parse
                          ("'<>' is allowed only for the last "
                            & "entity class entry");
                     end if;
                  end if;
                  exit when Current_Token = Tok_Right_Paren;
                  Expect (Tok_Comma);
                  Scan;
               end loop;
               Scan_Expect (Tok_Semi_Colon);
               return Res;
            end;
         when Tok_Colon =>
            declare
               Res : Iir_Group_Declaration;
               List : Iir_Group_Constituent_List;
            begin
               Res := Create_Iir (Iir_Kind_Group_Declaration);
               Set_Location (Res, Loc);
               Set_Identifier (Res, Ident);
               Scan;
               Set_Group_Template_Name
                 (Res, Parse_Name (Allow_Indexes => False));
               Expect (Tok_Left_Paren);
               Scan;
               List := Create_Iir_List;
               Set_Group_Constituent_List (Res, List);
               loop
                  Append_Element (List, Parse_Name (Allow_Indexes => False));
                  exit when Current_Token = Tok_Right_Paren;
                  Expect (Tok_Comma);
                  Scan;
               end loop;
               Scan_Expect (Tok_Semi_Colon);
               return Res;
            end;
         when others =>
            Error_Msg_Parse ("':' or 'is' expected here");
            return Null_Iir;
      end case;
   end Parse_Group;

   --  precond : next token
   --  postcond: ':'
   --
   --  [ §5.4 ]
   --  signal_list ::= signal_name { , signal_name }
   --                | OTHERS
   --                | ALL
   function Parse_Signal_List return Iir_List
   is
      Res : Iir_List;
   begin
      case Current_Token is
         when Tok_Others =>
            Scan;
            return Iir_List_Others;
         when Tok_All =>
            Scan;
            return Iir_List_All;
         when others =>
            Res := Create_Iir_List;
            loop
               Append_Element (Res, Parse_Name);
               exit when Current_Token = Tok_Colon;
               Expect (Tok_Comma);
               Scan;
            end loop;
            return Res;
      end case;
   end Parse_Signal_List;

   --  precond : DISCONNECT
   --  postcond: ';'
   --
   --  [ §5.4 ]
   --  disconnection_specification ::=
   --      DISCONNECT guarded_signal_specification AFTER time_expression ;
   function Parse_Disconnection_Specification
     return Iir_Disconnection_Specification
   is
      Res : Iir_Disconnection_Specification;
   begin
      Res := Create_Iir (Iir_Kind_Disconnection_Specification);
      Set_Location (Res);

      --  Skip 'disconnect'
      Expect (Tok_Disconnect);
      Scan;

      Set_Signal_List (Res, Parse_Signal_List);

      --  Skip ':'
      Expect (Tok_Colon);
      Scan;

      Set_Type_Mark (Res, Parse_Type_Mark (Check_Paren => True));

      --  Skip 'after'
      Expect (Tok_After);
      Scan;

      Set_Expression (Res, Parse_Expression);
      return Res;
   end Parse_Disconnection_Specification;

   --  precond : next token
   --  postcond: next token
   --
   --  [ LRM93 4 ]
   --  declaration ::= type_declaration
   --                | subtype_declaration
   --                | object_declaration
   --                | interface_declaration
   --                | alias_declaration
   --                | attribute_declaration
   --                | component_declaration
   --                | group_template_declaration
   --                | group_declaration
   --                | entity_declaration
   --                | configuration_declaration
   --                | subprogram_declaration
   --                | package_declaration
   procedure Parse_Declarative_Part (Parent : Iir)
   is
      use Declaration_Chain_Handling;
      Last_Decl : Iir;
      Decl : Iir;
   begin
      Build_Init (Last_Decl);
      loop
         Decl := Null_Iir;
         case Current_Token is
            when Tok_Invalid =>
               raise Internal_Error;
            when Tok_Type =>
               Decl := Parse_Type_Declaration (Parent);

               --  LRM 2.5  Package declarations
               --  If a package declarative item is a type declaration that is
               --  a full type declaration whose type definition is a
               --  protected_type definition, then that protected type
               --  definition must not be a protected type body.
               if Decl /= Null_Iir
                 and then Get_Kind (Decl) = Iir_Kind_Protected_Type_Body
               then
                  case Get_Kind (Parent) is
                     when Iir_Kind_Package_Declaration =>
                        Error_Msg_Parse ("protected type body not allowed "
                                         & "in package declaration", Decl);
                     when others =>
                        null;
                  end case;
               end if;
            when Tok_Subtype =>
               Decl := Parse_Subtype_Declaration (Parent);
            when Tok_Nature =>
               Decl := Parse_Nature_Declaration;
            when Tok_Terminal =>
               Decl := Parse_Terminal_Declaration (Parent);
            when Tok_Quantity =>
               Decl := Parse_Quantity_Declaration (Parent);
            when Tok_Signal =>
               case Get_Kind (Parent) is
                  when Iir_Kind_Function_Body
                    | Iir_Kind_Procedure_Body =>
                     Error_Msg_Parse
                       ("signal declaration not allowed in subprogram body");
                  when Iir_Kinds_Process_Statement =>
                     Error_Msg_Parse
                       ("signal declaration not allowed in process");
                  when others =>
                     null;
               end case;
               Decl := Parse_Object_Declaration (Parent);
            when Tok_Constant =>
               Decl := Parse_Object_Declaration (Parent);
            when Tok_Variable =>
               --  FIXME: remove this message (already checked during sem).
               case Get_Kind (Parent) is
                  when Iir_Kind_Entity_Declaration
                    | Iir_Kind_Architecture_Body
                    | Iir_Kind_Block_Statement
                    | Iir_Kind_Package_Declaration
                    | Iir_Kind_Package_Body =>
                     --  FIXME: replace HERE with the kind of declaration
                     --  ie: "not allowed in a package" rather than "here".
                     Error_Msg_Parse ("variable declaration not allowed here");
                  when others =>
                     null;
               end case;
               Decl := Parse_Object_Declaration (Parent);
            when Tok_Shared =>
               if Flags.Vhdl_Std <= Vhdl_87 then
                  Error_Msg_Parse ("shared variable not allowed in vhdl 87");
               end if;
               Decl := Parse_Object_Declaration (Parent);
            when Tok_File =>
               Decl := Parse_Object_Declaration (Parent);
            when Tok_Function
              | Tok_Procedure
              | Tok_Pure
              | Tok_Impure =>
               Decl := Parse_Subprogram_Declaration (Parent);
            when Tok_Alias =>
               Decl := Parse_Alias_Declaration;
            when Tok_Component =>
               case Get_Kind (Parent) is
                  when Iir_Kind_Entity_Declaration
                    | Iir_Kind_Procedure_Body
                    | Iir_Kind_Function_Body
                    | Iir_Kinds_Process_Statement =>
                     Error_Msg_Parse
                       ("component declaration are not allowed here");
                  when others =>
                     null;
               end case;
               Decl := Parse_Component_Declaration;
            when Tok_For =>
               case Get_Kind (Parent) is
                  when Iir_Kind_Entity_Declaration
                    | Iir_Kind_Function_Body
                    | Iir_Kind_Procedure_Body
                    | Iir_Kinds_Process_Statement =>
                     Error_Msg_Parse
                       ("configuration specification not allowed here");
                  when others =>
                     null;
               end case;
               Decl := Parse_Configuration_Specification;
            when Tok_Attribute =>
               Decl := Parse_Attribute;
            when Tok_Disconnect =>
               case Get_Kind (Parent) is
                  when Iir_Kind_Function_Body
                    | Iir_Kind_Procedure_Body
                    | Iir_Kinds_Process_Statement =>
                     Error_Msg_Parse
                       ("disconnect specification not allowed here");
                  when others =>
                     null;
               end case;
               Decl := Parse_Disconnection_Specification;
            when Tok_Use =>
               Decl := Parse_Use_Clause;
            when Tok_Group =>
               Decl := Parse_Group;

            when Tok_Identifier =>
               Error_Msg_Parse
                 ("object class keyword such as 'variable' is expected");
               Eat_Tokens_Until_Semi_Colon;
            when Tok_Semi_Colon =>
               Error_Msg_Parse ("';' (semi colon) not allowed alone");
               Scan;
            when others =>
               exit;
         end case;
         if Decl /= Null_Iir then
            Append_Subchain (Last_Decl, Parent, Decl);
         end if;

         if Current_Token = Tok_Semi_Colon or Current_Token = Tok_Invalid then
            Scan;
         end if;
      end loop;
   end Parse_Declarative_Part;

   --  precond : ENTITY
   --  postcond: ';'
   --
   --  [ §1.1 ]
   --  entity_declaration ::=
   --      ENTITY identifier IS
   --          entiy_header
   --          entity_declarative_part
   --      [ BEGIN
   --          entity_statement_part ]
   --      END [ ENTITY ] [ ENTITY_simple_name ]
   --
   --  [ §1.1.1 ]
   --  entity_header ::=
   --      [ FORMAL_generic_clause ]
   --      [ FORMAL_port_clause ]
   procedure Parse_Entity_Declaration (Unit : Iir_Design_Unit)
   is
      Res: Iir_Entity_Declaration;
   begin
      Expect (Tok_Entity);
      Res := Create_Iir (Iir_Kind_Entity_Declaration);

      --  Get identifier.
      Scan_Expect (Tok_Identifier,
                   "an identifier is expected after ""entity""");
      Set_Identifier (Res, Current_Identifier);
      Set_Location (Res);

      Scan_Expect (Tok_Is, "missing ""is"" after identifier");
      Scan;

      Parse_Generic_Port_Clauses (Res);

      Parse_Declarative_Part (Res);

      if Current_Token = Tok_Begin then
         Set_Has_Begin (Res, True);
         Scan;
         Parse_Concurrent_Statements (Res);
      end if;

      --   end keyword is expected to finish an entity declaration
      Expect (Tok_End);
      Set_End_Location (Unit);

      Scan;
      if Current_Token = Tok_Entity then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse ("""entity"" keyword not allowed here by vhdl 87");
         end if;
         Set_End_Has_Reserved_Id (Res, True);
         Scan;
      end if;
      Check_End_Name (Res);
      Expect (Tok_Semi_Colon);
      Invalidate_Current_Token;
      Set_Library_Unit (Unit, Res);
   end Parse_Entity_Declaration;

   --  [ LRM93 7.3.2 ]
   --  choice ::= simple_expression
   --           | discrete_range
   --           | ELEMENT_simple_name
   --           | OTHERS
   function Parse_A_Choice (Expr: Iir) return Iir
   is
      A_Choice: Iir;
      Expr1: Iir;
   begin
      if Expr = Null_Iir then
         if Current_Token = Tok_Others then
            A_Choice := Create_Iir (Iir_Kind_Choice_By_Others);
            Set_Location (A_Choice);

            --  Skip 'others'
            Scan;

            return A_Choice;
         else
            Expr1 := Parse_Expression;

            if Expr1 = Null_Iir then
               --  Handle parse error now.
               --  FIXME: skip until '=>'.
               A_Choice := Create_Iir (Iir_Kind_Choice_By_Expression);
               Set_Location (A_Choice);
               return A_Choice;
            end if;
         end if;
      else
         Expr1 := Expr;
      end if;
      if Is_Range_Attribute_Name (Expr1) then
         A_Choice := Create_Iir (Iir_Kind_Choice_By_Range);
         Location_Copy (A_Choice, Expr1);
         Set_Choice_Range (A_Choice, Expr1);
         return A_Choice;
      elsif Current_Token = Tok_To or else Current_Token = Tok_Downto then
         A_Choice := Create_Iir (Iir_Kind_Choice_By_Range);
         Location_Copy (A_Choice, Expr1);
         Set_Choice_Range (A_Choice, Parse_Range_Right (Expr1));
         return A_Choice;
      else
         A_Choice := Create_Iir (Iir_Kind_Choice_By_Expression);
         Location_Copy (A_Choice, Expr1);
         Set_Choice_Expression (A_Choice, Expr1);
         return A_Choice;
      end if;
   end Parse_A_Choice;

   --  [ LRM93 7.3.2 ]
   --  choices ::= choice { | choice }
   --
   -- Leave tok_double_arrow as current token.
   function Parse_Choices (Expr: Iir) return Iir
   is
      First, Last : Iir;
      A_Choice: Iir;
      Expr1 : Iir;
   begin
      Sub_Chain_Init (First, Last);
      Expr1 := Expr;
      loop
         A_Choice := Parse_A_Choice (Expr1);
         if First /= Null_Iir then
            Set_Same_Alternative_Flag (A_Choice, True);
            if Get_Kind (A_Choice) = Iir_Kind_Choice_By_Others then
               Error_Msg_Parse ("'others' choice must be alone");
            end if;
         end if;
         Sub_Chain_Append (First, Last, A_Choice);
         if Current_Token /= Tok_Bar then
            return First;
         end if;
         Scan;
         Expr1 := Null_Iir;
      end loop;
   end Parse_Choices;

   --  precond : '('
   --  postcond: next token
   --
   --  This can be an expression or an aggregate.
   --
   --  [ LRM93 7.3.2 ]
   --  aggregate ::= ( element_association { , element_association } )
   --
   --  [ LRM93 7.3.2 ]
   --  element_association ::= [ choices => ] expression
   function Parse_Aggregate return Iir
   is
      use Iir_Chains.Association_Choices_Chain_Handling;
      Expr: Iir;
      Res: Iir;
      Last : Iir;
      Assoc: Iir;
      Loc : Location_Type;
   begin
      Loc := Get_Token_Location;

      --  Skip '('
      Scan;

      if Current_Token /= Tok_Others then
         Expr := Parse_Expression;
         case Current_Token is
            when Tok_Comma
              | Tok_Double_Arrow
              | Tok_Bar =>
               --  This is really an aggregate
               null;
            when Tok_Right_Paren =>
               -- This was just a braced expression.

               -- Eat ')'.
               Scan;

               if Get_Kind (Expr) = Iir_Kind_Aggregate then
                  --  Parenthesis around aggregate is useless and change the
                  --  context for array aggregate.
                  Warning_Msg_Sem
                    ("suspicious parenthesis around aggregate", Expr);
               elsif not Flag_Parse_Parenthesis then
                  return Expr;
               end if;

               --  Create a node for the parenthesis.
               Res := Create_Iir (Iir_Kind_Parenthesis_Expression);
               Set_Location (Res, Loc);
               Set_Expression (Res, Expr);
               return Res;

            when Tok_Semi_Colon =>
               --  Surely a missing parenthesis.
               --  FIXME: in case of multiple missing parenthesises, several
               --    messages will be displayed
               Error_Msg_Parse ("missing ')' for opening parenthesis at "
                                & Image (Loc, Filename => False));
               return Expr;
            when others =>
               --  Surely a parse error...
               null;
         end case;
      else
         Expr := Null_Iir;
      end if;
      Res := Create_Iir (Iir_Kind_Aggregate);
      Set_Location (Res, Loc);
      Build_Init (Last);
      loop
         if Current_Token = Tok_Others then
            Assoc := Parse_A_Choice (Null_Iir);
            Expect (Tok_Double_Arrow);
            Scan;
            Expr := Parse_Expression;
         else
            if Expr = Null_Iir then
               Expr := Parse_Expression;
            end if;
            if Expr = Null_Iir then
               return Null_Iir;
            end if;
            case Current_Token is
               when Tok_Comma
                 | Tok_Right_Paren =>
                  Assoc := Create_Iir (Iir_Kind_Choice_By_None);
                  Location_Copy (Assoc, Expr);
               when others =>
                  Assoc := Parse_Choices (Expr);
                  Expect (Tok_Double_Arrow);
                  Scan;
                  Expr := Parse_Expression;
            end case;
         end if;
         Set_Associated_Expr (Assoc, Expr);
         Append_Subchain (Last, Res, Assoc);
         exit when Current_Token = Tok_Right_Paren;
         Expect (Tok_Comma);
         Scan;
         Expr := Null_Iir;
      end loop;
      Scan;
      return Res;
   end Parse_Aggregate;

   --  precond : NEW
   --  postcond: next token
   --
   --  [LRM93 7.3.6]
   --  allocator ::= NEW subtype_indication
   --              | NEW qualified_expression
   function Parse_Allocator return Iir
   is
      Loc: Location_Type;
      Res : Iir;
      Expr: Iir;
   begin
      Loc := Get_Token_Location;

      -- Accept 'new'.
      Scan;
      Expr := Parse_Name (Allow_Indexes => False);
      if Get_Kind (Expr) /= Iir_Kind_Qualified_Expression then
         -- This is a subtype_indication.
         Res := Create_Iir (Iir_Kind_Allocator_By_Subtype);
         Expr := Parse_Subtype_Indication (Expr);
         Set_Subtype_Indication (Res, Expr);
      else
         Res := Create_Iir (Iir_Kind_Allocator_By_Expression);
         Set_Expression (Res, Expr);
      end if;

      Set_Location (Res, Loc);
      return Res;
   end Parse_Allocator;

   --  precond : tok_bit_string
   --  postcond: tok_bit_string
   --
   --  Simply create the node for a bit string.
   function Parse_Bit_String return Iir
   is
      use Name_Table;
      Res : Iir;
      C : Character;
      B : Base_Type;
   begin
      Res := Create_Iir (Iir_Kind_String_Literal8);
      Set_Location (Res);
      Set_String8_Id (Res, Current_String_Id);
      Set_String_Length (Res, Current_String_Length);
      if Nam_Buffer (1) = 's' then
         Set_Has_Sign (Res, True);
         Set_Has_Signed (Res, True);
         pragma Assert (Nam_Length = 2);
         C := Name_Table.Nam_Buffer (2);
      elsif Nam_Buffer (1) = 'u' then
         Set_Has_Sign (Res, True);
         Set_Has_Signed (Res, False);
         pragma Assert (Nam_Length = 2);
         C := Nam_Buffer (2);
      else
         Set_Has_Sign (Res, False);
         Set_Has_Signed (Res, False);
         pragma Assert (Nam_Length = 1);
         C := Nam_Buffer (1);
      end if;

      case C is
         when 'b' =>
            B := Base_2;
         when 'o' =>
            B := Base_8;
         when 'd' =>
            B := Base_10;
         when 'x' =>
            B := Base_16;
         when others =>
            raise Internal_Error;
      end case;
      Set_Bit_String_Base (Res, B);

      return Res;
   end Parse_Bit_String;

   --  Scan returns an expanded bit value.  Adjust the expanded bit value as
   --  required by the length.
   procedure Resize_Bit_String (Lit : Iir; Nlen : Nat32)
   is
      use Str_Table;

      Old_Len : constant Nat32 := Get_String_Length (Lit);
      Is_Signed : constant Boolean := Get_Has_Signed (Lit);
      Id : constant String8_Id := Get_String8_Id (Lit);
      C : Nat8;
   begin
      if Nlen > Old_Len then
         --  Extend.

         --  LRM08 15.8
         --  -- If the length is greater than the number of characters in the
         --     expanded bit value and the base specifier is B, UB, O, UO, X,
         --     UX or D, the bit string value is obtained by concatenating a
         --     string of 0 digits to the left of the expanded bit value.  The
         --     number of 0 digits in the string is such that the number of
         --     characters in the result of the concatenation is the length of
         --     the bit string literal.
         --
         --  -- If the length is greater than the number of characters in the
         --     expanded bit value and the base specifier is SB, SO or SX, the
         --     bit string value is obtained by concatenating the the left of
         --     the expanded bit value a string, each of whose characters is
         --     the leftmost character of the expanded bit value.  The number
         --     of characters in the string is such that the number of
         --     characters in the result of the concatenation is the length of
         --     the bit string literal.
         if Is_Signed then
            if Old_Len = 0 then
               Error_Msg_Parse
                 ("cannot expand an empty signed bit string", Lit);
               C := Character'Pos ('0');
            else
               C := Element_String8 (Id, 1);
            end if;
         else
            C := Character'Pos ('0');
         end if;
         Resize_String8 (Nlen);
         --  Shift (position 1 is the MSB).
         for I in reverse 1 .. Old_Len loop
            Set_Element_String8 (Id, I + Nlen - Old_Len,
                                 Element_String8 (Id, I));
         end loop;
         for I in 1 .. Nlen - Old_Len loop
            Set_Element_String8 (Id, I, C);
         end loop;
         Set_String_Length (Lit, Nlen);

      elsif Nlen < Old_Len then
         --  Reduce.

         --  LRM08 15.8
         --  -- If the length is less than the number of characters in the
         --     expanded bit value and the base specifier is B, UB, O, UO, X,
         --     UX or D, the bit string value is obtained by deleting
         --     sufficient characters from the left of the expanded bit value
         --     to yield a string whose length is the length of the bit string
         --     literal.  It is an error if any of the character so deleted is
         --     other than the digit 0.
         --
         --  -- If the length is less than the number of characters in the
         --     expanded bit value and the base specifier is SB, SO or SX, the
         --     bit string value is obtained by deleting sufficient characters
         --     from the left of the expanded bit value to yield a string whose
         --     length is the length of the bit string literal.  It is an error
         --     if any of the characters so deleted differs from the leftmost
         --     remaining character.
         if Is_Signed then
            C := Element_String8 (Id, 1 + Old_Len - Nlen);
         else
            C := Character'Pos ('0');
         end if;
         for I in 1 .. Old_Len - Nlen loop
            if Element_String8 (Id, I) /= C then
               Error_Msg_Parse
                 ("truncation of bit string changes the value", Lit);
               --  Avoid error storm.
               exit;
            end if;
         end loop;
         --  Shift (position 1 is the MSB).
         for I in 1 .. Nlen loop
            Set_Element_String8 (Id, I,
                                 Element_String8 (Id, I + Old_Len - Nlen));
         end loop;
         Resize_String8 (Nlen);
         Set_String_Length (Lit, Nlen);

      else
         --  LRM08 15.8
         --  -- If the length is equal to the number of characters in the
         --     expanded bit value, the string literal value is the expanded
         --     bit value itself.
         null;
      end if;
   end Resize_Bit_String;

   --  Precond : next token after tok_integer
   --  postcond: likewise
   --
   --  Return an integer_literal or a physical_literal.
   function Parse_Integer_Literal (Val : Iir_Int64) return Iir
   is
      Res : Iir;
   begin
      if Current_Token = Tok_Identifier then
         -- physical literal
         Res := Create_Iir (Iir_Kind_Physical_Int_Literal);
         Set_Unit_Name (Res, Parse_Name (Allow_Indexes => False));
      else
         -- integer literal
         Res := Create_Iir (Iir_Kind_Integer_Literal);
      end if;
      Set_Value (Res, Val);
      return Res;
   end Parse_Integer_Literal;

   --  precond : next token
   --  postcond: next token
   --
   --  [ LRM93 7.1 ]
   --  primary ::= name
   --            | literal
   --            | aggregate
   --            | function_call
   --            | qualified_expression
   --            | type_conversion
   --            | allocator
   --            | ( expression )
   --
   --  [ LRM93 7.3.1 ]
   --  literal ::= numeric_literal
   --            | enumeration_literal
   --            | string_literal
   --            | bit_string_literal
   --            | NULL
   --
   --  [ LRM93 7.3.1 ]
   --  numeric_literal ::= abstract_literal
   --                    | physical_literal
   --
   --  [ LRM93 13.4 ]
   --  abstract_literal ::= decimal_literal | based_literal
   --
   --  [ LRM93 3.1.3 ]
   --  physical_literal ::= [ abstract_literal ] UNIT_name
   function Parse_Primary return Iir_Expression
   is
      Res: Iir_Expression;
      Int: Iir_Int64;
      Fp: Iir_Fp64;
      Loc: Location_Type;
   begin
      case Current_Token is
         when Tok_Integer =>
            Int := Current_Iir_Int64;
            Loc := Get_Token_Location;

            --  Skip integer
            Scan;

            Res := Parse_Integer_Literal (Int);
            Set_Location (Res, Loc);
            return Res;

         when Tok_Real =>
            Fp := Current_Iir_Fp64;
            Loc := Get_Token_Location;

            --  Skip real
            Scan;

            if Current_Token = Tok_Identifier then
               -- physical literal
               Res := Create_Iir (Iir_Kind_Physical_Fp_Literal);
               Set_Unit_Name (Res, Parse_Name (Allow_Indexes => False));
            else
               -- real literal
               Res := Create_Iir (Iir_Kind_Floating_Point_Literal);
            end if;
            Set_Location (Res, Loc);
            Set_Fp_Value (Res, Fp);
            return Res;

         when Tok_Identifier
           | Tok_Double_Less =>
            return Parse_Name (Allow_Indexes => True);
         when Tok_Character =>
            Res := Current_Text;
            Scan;
            if Current_Token = Tok_Tick then
               Error_Msg_Parse
                 ("prefix of an attribute can't be a character literal");
               --  skip tick.
               Scan;
               --  skip attribute designator
               Scan;
            end if;
            return Res;
         when Tok_Left_Paren =>
            return Parse_Aggregate;
         when Tok_String =>
            return Parse_Name;
         when Tok_Null =>
            Res := Create_Iir (Iir_Kind_Null_Literal);
            Set_Location (Res);
            Scan;
            return Res;
         when Tok_New =>
            return Parse_Allocator;

         when Tok_Integer_Letter =>
            Int := Current_Iir_Int64;
            Loc := Get_Token_Location;

            --  Skip integer
            Scan;

            if Current_Token = Tok_Bit_String then
               Res := Parse_Bit_String;
               Set_Has_Length (Res, True);

               --  Skip bit string
               Scan;

               --  Resize.
               Resize_Bit_String (Res, Nat32 (Int));
            else
               Error_Msg_Parse
                 ("space is required between number and unit name",
                  Get_Token_Location);
               Res := Parse_Integer_Literal (Int);
            end if;
            Set_Location (Res, Loc);
            return Res;

         when Tok_Bit_String =>
            Res := Parse_Bit_String;

            --  Skip bit string
            Scan;

            return Res;

         when Tok_Minus
           | Tok_Plus =>
            Error_Msg_Parse
              ("'-' and '+' are not allowed in primary, use parenthesis");
            return Parse_Simple_Expression;
         when Tok_Comma
           | Tok_Semi_Colon
           | Tok_Eof
           | Tok_End =>
            --  Token not to be skipped
            Unexpected ("primary");
            return Null_Iir;
         when others =>
            Unexpected ("primary");
            Scan;
            return Null_Iir;
      end case;
   end Parse_Primary;

   --  precond : next token
   --  postcond: next token
   --
   --  [ §7.1 ]
   --  factor ::= primary [ ** primary ]
   --           | ABS primary
   --           | NOT primary
   --           | logical_operator primary  [ VHDL08 9.1 ]
   function Build_Unary_Factor (Primary : Iir; Op : Iir_Kind) return Iir is
      Res : Iir;
   begin
      if Primary /= Null_Iir then
         return Primary;
      end if;
      Res := Create_Iir (Op);
      Set_Location (Res);
      Scan;
      Set_Operand (Res, Parse_Primary);
      return Res;
   end Build_Unary_Factor;

   function Build_Unary_Factor_08 (Primary : Iir; Op : Iir_Kind) return Iir is
   begin
      if Primary /= Null_Iir then
         return Primary;
      end if;
      if Flags.Vhdl_Std < Vhdl_08 then
         Error_Msg_Parse ("missing left operand of logical expression");
         --  Skip operator
         Scan;
         return Parse_Primary;
      else
         return Build_Unary_Factor (Primary, Op);
      end if;
   end Build_Unary_Factor_08;

   function Parse_Factor (Primary : Iir := Null_Iir) return Iir_Expression is
      Res, Left: Iir_Expression;
   begin
      case Current_Token is
         when Tok_Abs =>
            return Build_Unary_Factor (Primary, Iir_Kind_Absolute_Operator);
         when Tok_Not =>
            return Build_Unary_Factor (Primary, Iir_Kind_Not_Operator);

         when Tok_And =>
            return Build_Unary_Factor_08
              (Primary, Iir_Kind_Reduction_And_Operator);
         when Tok_Or =>
            return Build_Unary_Factor_08
              (Primary, Iir_Kind_Reduction_Or_Operator);
         when Tok_Nand =>
            return Build_Unary_Factor_08
              (Primary, Iir_Kind_Reduction_Nand_Operator);
         when Tok_Nor =>
            return Build_Unary_Factor_08
              (Primary, Iir_Kind_Reduction_Nor_Operator);
         when Tok_Xor =>
            return Build_Unary_Factor_08
              (Primary, Iir_Kind_Reduction_Xor_Operator);
         when Tok_Xnor =>
            return Build_Unary_Factor_08
              (Primary, Iir_Kind_Reduction_Xnor_Operator);

         when others =>
            if Primary /= Null_Iir then
               Left := Primary;
            else
               Left := Parse_Primary;
            end if;
            if Current_Token = Tok_Double_Star then
               Res := Create_Iir (Iir_Kind_Exponentiation_Operator);
               Set_Location (Res);
               Scan;
               Set_Left (Res, Left);
               Set_Right (Res, Parse_Primary);
               return Res;
            else
               return Left;
            end if;
      end case;
   end Parse_Factor;

   --  precond : next token
   --  postcond: next token
   --
   --  [ §7.1 ]
   --  term ::= factor { multiplying_operator factor }
   --
   --  [ §7.2 ]
   --  multiplying_operator ::= * | / | MOD | REM
   function Parse_Term (Primary : Iir) return Iir_Expression is
      Res, Tmp: Iir_Expression;
   begin
      Res := Parse_Factor (Primary);
      while Current_Token in Token_Multiplying_Operator_Type loop
         case Current_Token is
            when Tok_Star =>
               Tmp := Create_Iir (Iir_Kind_Multiplication_Operator);
            when Tok_Slash =>
               Tmp := Create_Iir (Iir_Kind_Division_Operator);
            when Tok_Mod =>
               Tmp := Create_Iir (Iir_Kind_Modulus_Operator);
            when Tok_Rem =>
               Tmp := Create_Iir (Iir_Kind_Remainder_Operator);
            when others =>
               raise Program_Error;
         end case;
         Set_Location (Tmp);
         Set_Left (Tmp, Res);
         Scan;
         Set_Right (Tmp, Parse_Factor);
         Res := Tmp;
      end loop;
      return Res;
   end Parse_Term;

   --  precond : next token
   --  postcond: next token
   --
   --  [ §7.1 ]
   --  simple_expression ::= [ sign ] term { adding_operator term }
   --
   --  [ §7.2 ]
   --  sign ::= + | -
   --
   --  [ §7.2 ]
   --  adding_operator ::= + | - | &
   function Parse_Simple_Expression (Primary : Iir := Null_Iir)
                                    return Iir_Expression
   is
      Res, Tmp: Iir_Expression;
   begin
      if Current_Token in Token_Sign_Type
        and then Primary = Null_Iir
      then
         case Current_Token is
            when Tok_Plus =>
               Res := Create_Iir (Iir_Kind_Identity_Operator);
            when Tok_Minus =>
               Res := Create_Iir (Iir_Kind_Negation_Operator);
            when others =>
               raise Program_Error;
         end case;
         Set_Location (Res);
         Scan;
         Set_Operand (Res, Parse_Term (Null_Iir));
      else
         Res := Parse_Term (Primary);
      end if;
      while Current_Token in Token_Adding_Operator_Type loop
         case Current_Token is
            when Tok_Plus =>
               Tmp := Create_Iir (Iir_Kind_Addition_Operator);
            when Tok_Minus =>
               Tmp := Create_Iir (Iir_Kind_Substraction_Operator);
            when Tok_Ampersand =>
               Tmp := Create_Iir (Iir_Kind_Concatenation_Operator);
            when others =>
               raise Program_Error;
         end case;
         Set_Location (Tmp);
         Scan;
         Set_Left (Tmp, Res);
         Set_Right (Tmp, Parse_Term (Null_Iir));
         Res := Tmp;
      end loop;
      return Res;
   end Parse_Simple_Expression;

   --  precond : next token
   --  postcond: next token
   --
   --  [ §7.1 ]
   --  shift_expression ::=
   --      simple_expression [ shift_operator simple_expression ]
   --
   --  [ §7.2 ]
   --  shift_operator ::= SLL | SRL | SLA | SRA | ROL | ROR
   function Parse_Shift_Expression return Iir_Expression is
      Res, Tmp: Iir_Expression;
   begin
      Tmp := Parse_Simple_Expression;
      if Current_Token not in Token_Shift_Operator_Type then
         return Tmp;
      elsif Flags.Vhdl_Std = Vhdl_87 then
         Error_Msg_Parse ("shift operators not allowed in vhdl 87");
      end if;
      case Current_Token is
         when Tok_Sll =>
            Res := Create_Iir (Iir_Kind_Sll_Operator);
         when Tok_Sla =>
            Res := Create_Iir (Iir_Kind_Sla_Operator);
         when Tok_Srl =>
            Res := Create_Iir (Iir_Kind_Srl_Operator);
         when Tok_Sra =>
            Res := Create_Iir (Iir_Kind_Sra_Operator);
         when Tok_Rol =>
            Res := Create_Iir (Iir_Kind_Rol_Operator);
         when Tok_Ror =>
            Res := Create_Iir (Iir_Kind_Ror_Operator);
         when others =>
            raise Program_Error;
      end case;
      Set_Location (Res);
      Scan;
      Set_Left (Res, Tmp);
      Set_Right (Res, Parse_Simple_Expression);
      return Res;
   end Parse_Shift_Expression;

   --  precond : next token (relational_operator)
   --  postcond: next token
   --
   --  [ §7.1 ]
   --     relational_operator shift_expression
   function Parse_Relation_Rhs (Left : Iir) return Iir
   is
      Res, Tmp: Iir_Expression;
   begin
      Tmp := Left;

      --  This loop is just to handle errors such as a = b = c.
      loop
         case Current_Token is
            when Tok_Equal =>
               Res := Create_Iir (Iir_Kind_Equality_Operator);
            when Tok_Not_Equal =>
               Res := Create_Iir (Iir_Kind_Inequality_Operator);
            when Tok_Less =>
               Res := Create_Iir (Iir_Kind_Less_Than_Operator);
            when Tok_Less_Equal =>
               Res := Create_Iir (Iir_Kind_Less_Than_Or_Equal_Operator);
            when Tok_Greater =>
               Res := Create_Iir (Iir_Kind_Greater_Than_Operator);
            when Tok_Greater_Equal =>
               Res := Create_Iir (Iir_Kind_Greater_Than_Or_Equal_Operator);
            when Tok_Match_Equal =>
               Res := Create_Iir (Iir_Kind_Match_Equality_Operator);
            when Tok_Match_Not_Equal =>
               Res := Create_Iir (Iir_Kind_Match_Inequality_Operator);
            when Tok_Match_Less =>
               Res := Create_Iir (Iir_Kind_Match_Less_Than_Operator);
            when Tok_Match_Less_Equal =>
               Res := Create_Iir (Iir_Kind_Match_Less_Than_Or_Equal_Operator);
            when Tok_Match_Greater =>
               Res := Create_Iir (Iir_Kind_Match_Greater_Than_Operator);
            when Tok_Match_Greater_Equal =>
               Res := Create_Iir
                 (Iir_Kind_Match_Greater_Than_Or_Equal_Operator);
            when others =>
               raise Program_Error;
         end case;
         Set_Location (Res);
         Scan;
         Set_Left (Res, Tmp);
         Set_Right (Res, Parse_Shift_Expression);
         exit when Current_Token not in Token_Relational_Operator_Type;
         Error_Msg_Parse
           ("use parenthesis for consecutive relational expressions");
         Tmp := Res;
      end loop;
      return Res;
   end Parse_Relation_Rhs;

   --  precond : next token
   --  postcond: next token
   --
   --  [ §7.1 ]
   --  relation ::= shift_expression [ relational_operator shift_expression ]
   --
   --  [ §7.2 ]
   --  relational_operator ::= = | /= | < | <= | > | >=
   --                        | ?= | ?/= | ?< | ?<= | ?> | ?>=
   function Parse_Relation return Iir
   is
      Tmp: Iir;
   begin
      Tmp := Parse_Shift_Expression;
      if Current_Token not in Token_Relational_Operator_Type then
         return Tmp;
      end if;

      return Parse_Relation_Rhs (Tmp);
   end Parse_Relation;

   --  precond : next token
   --  postcond: next token
   --
   --  [ §7.1 ]
   --  expression ::= relation { AND relation }
   --               | relation { OR relation }
   --               | relation { XOR relation }
   --               | relation [ NAND relation }
   --               | relation [ NOR relation }
   --               | relation { XNOR relation }
   function Parse_Expression_Rhs (Left : Iir) return Iir
   is
      Res, Tmp: Iir;

      --  OP_TOKEN contains the operator combinaison.
      Op_Token: Token_Type;
   begin
      Tmp := Left;
      Op_Token := Tok_Invalid;
      loop
         case Current_Token is
            when Tok_And =>
               Res := Create_Iir (Iir_Kind_And_Operator);
            when Tok_Or =>
               Res := Create_Iir (Iir_Kind_Or_Operator);
            when Tok_Xor =>
               Res := Create_Iir (Iir_Kind_Xor_Operator);
            when Tok_Nand =>
               Res := Create_Iir (Iir_Kind_Nand_Operator);
            when Tok_Nor =>
               Res := Create_Iir (Iir_Kind_Nor_Operator);
            when Tok_Xnor =>
               if Flags.Vhdl_Std = Vhdl_87 then
                  Error_Msg_Parse ("'xnor' keyword not allowed in vhdl 87");
               end if;
               Res := Create_Iir (Iir_Kind_Xnor_Operator);
            when others =>
               return Tmp;
         end case;

         if Op_Token = Tok_Invalid then
            Op_Token := Current_Token;
         else
            --  Check after the case, since current_token may not be an
            --  operator...
            --  TODO: avoid repetition of this message ?
            if Op_Token = Tok_Nand or Op_Token = Tok_Nor then
               Error_Msg_Parse ("sequence of 'nor' or 'nand' not allowed");
               Error_Msg_Parse ("('nor' and 'nand' are not associative)");
            end if;
            if Op_Token /= Current_Token then
               --  Expression is a sequence of relations, with the same
               --  operator.
               Error_Msg_Parse ("only one type of logical operators may be "
                                & "used to combine relation");
            end if;
         end if;

         Set_Location (Res);
         Scan;

         --  Catch errors for Ada programmers.
         if Current_Token = Tok_Then or Current_Token = Tok_Else then
            Error_Msg_Parse ("""or else"" and ""and then"" sequences "
                             & "are not allowed in vhdl");
            Error_Msg_Parse ("""and"" and ""or"" are short-circuit "
                             & "operators for BIT and BOOLEAN types");
            Scan;
         end if;

         Set_Left (Res, Tmp);
         Set_Right (Res, Parse_Relation);
         Tmp := Res;
      end loop;
   end Parse_Expression_Rhs;

   --  precond : next token
   --  postcond: next token
   --
   --  LRM08 9.1 General
   --  expression ::= condition_operator primary
   --              |  logical_expression
   function Parse_Expression return Iir_Expression
   is
      Res : Iir;
   begin
      if Current_Token = Tok_Condition then
         Res := Create_Iir (Iir_Kind_Condition_Operator);
         Set_Location (Res);

         --  Skip '??'
         Scan;

         Set_Operand (Res, Parse_Primary);
      else
         Res := Parse_Expression_Rhs (Parse_Relation);
      end if;

      return Res;
   end Parse_Expression;

   --  precond : next token
   --  postcond: next token.
   --
   --  [ §8.4 ]
   --  waveform ::= waveform_element { , waveform_element }
   --             | UNAFFECTED
   --
   --  [ §8.4.1 ]
   --  waveform_element ::= VALUE_expression [ AFTER TIME_expression ]
   --                     | NULL [ AFTER TIME_expression ]
   function Parse_Waveform return Iir_Waveform_Element
   is
      Res: Iir_Waveform_Element;
      We, Last_We : Iir_Waveform_Element;
   begin
      if Current_Token = Tok_Unaffected then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse ("'unaffected' is not allowed in vhdl87");
         end if;
         Scan;
         return Null_Iir;
      else
         Sub_Chain_Init (Res, Last_We);
         loop
            We := Create_Iir (Iir_Kind_Waveform_Element);
            Sub_Chain_Append (Res, Last_We, We);
            Set_Location (We);
            --  Note: NULL is handled as a null_literal.
            Set_We_Value (We, Parse_Expression);
            if Current_Token = Tok_After then
               Scan;
               Set_Time (We, Parse_Expression);
            end if;
            exit when Current_Token /= Tok_Comma;
            Scan;
         end loop;
         return Res;
      end if;
   end Parse_Waveform;

   --  precond : next token
   --  postcond: next token
   --
   --  [ §8.4 ]
   --  delay_mechanism ::= TRANSPORT
   --                    | [ REJECT TIME_expression ] INERTIAL
   procedure Parse_Delay_Mechanism (Assign: Iir) is
   begin
      if Current_Token = Tok_Transport then
         Set_Delay_Mechanism (Assign, Iir_Transport_Delay);
         Scan;
      else
         Set_Delay_Mechanism (Assign, Iir_Inertial_Delay);
         if Current_Token = Tok_Reject then
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse
                 ("'reject' delay mechanism not allowed in vhdl 87");
            end if;
            Scan;
            Set_Reject_Time_Expression (Assign, Parse_Expression);
            Expect (Tok_Inertial);
            Scan;
         elsif Current_Token = Tok_Inertial then
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse
                 ("'inertial' keyword not allowed in vhdl 87");
            end if;
            Scan;
         end if;
      end if;
   end Parse_Delay_Mechanism;

   --  precond : next token
   --  postcond: next token
   --
   --  [ §9.5 ]
   --  options ::= [ GUARDED ] [ delay_mechanism ]
   procedure Parse_Options (Stmt : Iir) is
   begin
      if Current_Token = Tok_Guarded then
         Set_Guard (Stmt, Stmt);
         Scan;
      end if;
      Parse_Delay_Mechanism (Stmt);
   end Parse_Options;

   --  precond : next tkoen
   --  postcond: ';'
   --
   --  [ §9.5.1 ]
   --  conditional_signal_assignment ::=
   --      target <= options conditional_waveforms ;
   --
   --  [ §9.5.1 ]
   --  conditional_waveforms ::=
   --      { waveform WHEN condition ELSE }
   --      waveform [ WHEN condition ]
   function Parse_Conditional_Signal_Assignment (Target: Iir) return Iir
   is
      use Iir_Chains.Conditional_Waveform_Chain_Handling;
      Res: Iir;
      Cond_Wf, Last_Cond_Wf : Iir_Conditional_Waveform;
   begin
      Res := Create_Iir (Iir_Kind_Concurrent_Conditional_Signal_Assignment);
      Set_Target (Res, Target);
      Location_Copy (Res, Get_Target (Res));

      case Current_Token is
         when Tok_Less_Equal =>
            null;
         when Tok_Assign =>
            Error_Msg_Parse ("':=' not allowed in concurrent statement, "
                             & "replaced by '<='");
         when others =>
            Expect (Tok_Less_Equal);
      end case;
      Scan;

      Parse_Options (Res);

      Build_Init (Last_Cond_Wf);
      loop
         Cond_Wf := Create_Iir (Iir_Kind_Conditional_Waveform);
         Append (Last_Cond_Wf, Res, Cond_Wf);
         Set_Location (Cond_Wf);
         Set_Waveform_Chain (Cond_Wf, Parse_Waveform);
         exit when Current_Token /= Tok_When;
         Scan;
         Set_Condition (Cond_Wf, Parse_Expression);
         if Current_Token /= Tok_Else then
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse ("else missing in vhdl 87");
            end if;
            exit;
         end if;
         Scan;
      end loop;
      Expect (Tok_Semi_Colon);
      return Res;
   end Parse_Conditional_Signal_Assignment;

   --  precond : WITH
   --  postcond: ';'
   --
   --  [ §9.5.2 ]
   --  selected_signal_assignment ::=
   --      WITH expresion SELECT
   --          target <= options selected_waveforms ;
   --
   --  [ §9.5.2 ]
   --  selected_waveforms ::=
   --      { waveform WHEN choices , }
   --      waveform WHEN choices
   function Parse_Selected_Signal_Assignment return Iir
   is
      use Iir_Chains.Selected_Waveform_Chain_Handling;
      Res: Iir;
      Assoc: Iir;
      Wf_Chain : Iir_Waveform_Element;
      Target : Iir;
      Last : Iir;
   begin
      Scan;  -- accept 'with' token.
      Res := Create_Iir (Iir_Kind_Concurrent_Selected_Signal_Assignment);
      Set_Location (Res);
      Set_Expression (Res, Parse_Expression);

      Expect (Tok_Select, "'select' expected after expression");
      Scan;
      if Current_Token = Tok_Left_Paren then
         Target := Parse_Aggregate;
      else
         Target := Parse_Name (Allow_Indexes => True);
      end if;
      Set_Target (Res, Target);
      Expect (Tok_Less_Equal);
      Scan;

      Parse_Options (Res);

      Build_Init (Last);
      loop
         Wf_Chain := Parse_Waveform;
         Expect (Tok_When, "'when' expected after waveform");
         Scan;
         Assoc := Parse_Choices (Null_Iir);
         Set_Associated_Chain (Assoc, Wf_Chain);
         Append_Subchain (Last, Res, Assoc);
         exit when Current_Token = Tok_Semi_Colon;
         Expect (Tok_Comma, "',' (comma) expected after choice");
         Scan;
      end loop;
      return Res;
   end Parse_Selected_Signal_Assignment;

   --  precond : next token
   --  postcond: next token.
   --
   --  [ §8.1 ]
   --  sensitivity_list ::= SIGNAL_name { , SIGNAL_name }
   procedure Parse_Sensitivity_List (List: Iir_Designator_List)
   is
      El : Iir;
   begin
      loop
         El := Parse_Name (Allow_Indexes => True);
         case Get_Kind (El) is
            when Iir_Kind_Simple_Name
              | Iir_Kind_Parenthesis_Name
              | Iir_Kind_Selected_Name
              | Iir_Kind_Slice_Name
              | Iir_Kind_Attribute_Name
              | Iir_Kind_Selected_By_All_Name
              | Iir_Kind_Indexed_Name =>
               null;
            when others =>
               Error_Msg_Parse
                 ("only names are allowed in a sensitivity list");
         end case;
         Append_Element (List, El);
         exit when Current_Token /= Tok_Comma;
         Scan;
      end loop;
   end Parse_Sensitivity_List;

   --  precond : ASSERT
   --  postcond: next token
   --  Note: this fill an sequential or a concurrent statement.
   --
   --  [ §8.2 ]
   --  assertion ::= ASSERT condition
   --      [ REPORT expression ] [ SEVERITY expression ]
   procedure Parse_Assertion (Stmt: Iir) is
   begin
      Set_Location (Stmt);
      Scan;
      Set_Assertion_Condition (Stmt, Parse_Expression);
      if Current_Token = Tok_Report then
         Scan;
         Set_Report_Expression (Stmt, Parse_Expression);
      end if;
      if Current_Token = Tok_Severity then
         Scan;
         Set_Severity_Expression (Stmt, Parse_Expression);
         if Current_Token = Tok_Report then
            --  Nice message in case of inversion.
            Error_Msg_Parse
              ("report expression must precede severity expression");
            Scan;
            Set_Report_Expression (Stmt, Parse_Expression);
         end if;
      end if;
   end Parse_Assertion;

   --  precond : REPORT
   --  postcond: next token
   --
   --  [ 8.3 ]
   --  report_statement ::= REPORT expression [ SEVERITY expression ]
   function Parse_Report_Statement return Iir_Report_Statement
   is
      Res : Iir_Report_Statement;
   begin
      Res := Create_Iir (Iir_Kind_Report_Statement);
      Set_Location (Res);
      if Flags.Vhdl_Std = Vhdl_87 then
         Error_Msg_Parse ("report statement not allowed in vhdl87");
      end if;
      Scan;
      Set_Report_Expression (Res, Parse_Expression);
      if Current_Token = Tok_Severity then
         Scan;
         Set_Severity_Expression (Res, Parse_Expression);
      end if;
      return Res;
   end Parse_Report_Statement;

   -- precond : WAIT
   -- postcond: ';'
   --
   --  [ §8.1 ]
   --  wait_statement ::=
   --      [ label : ] WAIT [ sensitivity_clause ] [ condition_clause ]
   --          [ timeout_clause ] ;
   --
   --  [ §8.1 ]
   --  sensitivity_clause ::= ON sensitivity_list
   --
   --  [ §8.1 ]
   --  condition_clause ::= UNTIL conditiion
   --
   --  [ §8.1 ]
   --  timeout_clause ::= FOR TIME_expression
   function Parse_Wait_Statement return Iir_Wait_Statement
   is
      Res: Iir_Wait_Statement;
      List: Iir_List;
   begin
      Res := Create_Iir (Iir_Kind_Wait_Statement);
      Set_Location (Res);
      Scan;
      case Current_Token is
         when Tok_On =>
            List := Create_Iir_List;
            Set_Sensitivity_List (Res, List);
            Scan;
            Parse_Sensitivity_List (List);
         when Tok_Until =>
            null;
         when Tok_For =>
            null;
         when Tok_Semi_Colon =>
            return Res;
         when others =>
            Error_Msg_Parse ("'on', 'until', 'for' or ';' expected");
            Eat_Tokens_Until_Semi_Colon;
            return Res;
      end case;
      case Current_Token is
         when Tok_On =>
            Error_Msg_Parse ("only one sensitivity is allowed");
            -- FIXME: sync
            return Res;
         when Tok_Until =>
            Scan;
            Set_Condition_Clause (Res, Parse_Expression);
         when Tok_For =>
            null;
         when Tok_Semi_Colon =>
            return Res;
         when others =>
            Error_Msg_Parse ("'until', 'for' or ';' expected");
            Eat_Tokens_Until_Semi_Colon;
            return Res;
      end case;
      case Current_Token is
         when Tok_On =>
            Error_Msg_Parse ("only one sensitivity clause is allowed");
            -- FIXME: sync
            return Res;
         when Tok_Until =>
            Error_Msg_Parse ("only one condition clause is allowed");
            -- FIXME: sync
            return Res;
         when Tok_For =>
            Scan;
            Set_Timeout_Clause (Res, Parse_Expression);
            return Res;
         when Tok_Semi_Colon =>
            return Res;
         when others =>
            Error_Msg_Parse ("'for' or ';' expected");
            Eat_Tokens_Until_Semi_Colon;
            return Res;
      end case;
   end Parse_Wait_Statement;

   --  precond : IF
   --  postcond: next token.
   --
   --  [ §8.7 ]
   --  if_statement ::=
   --    [ IF_label : ]
   --        IF condition THEN
   --            sequence_of_statements
   --        { ELSIF condition THEN
   --            sequence_of_statements }
   --        [ ELSE
   --            sequence_of_statements ]
   --        END IF [ IF_label ] ;
   --
   -- FIXME: end label.
   function Parse_If_Statement (Parent : Iir) return Iir_If_Statement
   is
      Res: Iir_If_Statement;
      Clause: Iir;
      N_Clause: Iir;
   begin
      Res := Create_Iir (Iir_Kind_If_Statement);
      Set_Location (Res);
      Set_Parent (Res, Parent);
      Scan;
      Clause := Res;
      loop
         Set_Condition (Clause, Parse_Expression);
         Expect (Tok_Then, "'then' is expected here");
         Scan;
         Set_Sequential_Statement_Chain
           (Clause, Parse_Sequential_Statements (Res));
         exit when Current_Token = Tok_End;
         N_Clause := Create_Iir (Iir_Kind_Elsif);
         Set_Location (N_Clause);
         Set_Else_Clause (Clause, N_Clause);
         Clause := N_Clause;
         if Current_Token = Tok_Else then
            Scan;
            Set_Sequential_Statement_Chain
              (Clause, Parse_Sequential_Statements (Res));
            exit;
         elsif Current_Token = Tok_Elsif then
            Scan;
         else
            Error_Msg_Parse ("'else' or 'elsif' expected");
         end if;
      end loop;
      Expect (Tok_End);
      Scan_Expect (Tok_If);
      Scan;
      return Res;
   end Parse_If_Statement;

   function Parenthesis_Name_To_Procedure_Call (Name: Iir; Kind : Iir_Kind)
                                               return Iir
   is
      Res: Iir;
      Call : Iir_Procedure_Call;
   begin
      Res := Create_Iir (Kind);
      Location_Copy (Res, Name);
      Call := Create_Iir (Iir_Kind_Procedure_Call);
      Location_Copy (Call, Name);
      Set_Procedure_Call (Res, Call);
      case Get_Kind (Name) is
         when Iir_Kind_Parenthesis_Name =>
            Set_Prefix (Call, Get_Prefix (Name));
            Set_Parameter_Association_Chain
              (Call, Get_Association_Chain (Name));
            Free_Iir (Name);
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            Set_Prefix (Call, Name);
         when Iir_Kind_Attribute_Name =>
            Error_Msg_Parse ("attribute cannot be used as procedure call");
         when others =>
            Error_Kind ("parenthesis_name_to_procedure_call", Name);
      end case;
      return Res;
   end Parenthesis_Name_To_Procedure_Call;

   --  precond : identifier
   --  postcond: next token
   --
   --  [ LRM93 8.9 ]
   --  parameter_specification ::= identifier IN discrete_range
   function Parse_Parameter_Specification (Parent : Iir)
                                          return Iir_Iterator_Declaration
   is
      Decl : Iir_Iterator_Declaration;
   begin
      Decl := Create_Iir (Iir_Kind_Iterator_Declaration);
      Set_Location (Decl);
      Set_Parent (Decl, Parent);

      Expect (Tok_Identifier);
      Set_Identifier (Decl, Current_Identifier);

      --  Skip identifier
      Scan_Expect (Tok_In);

      --  Skip 'in'
      Scan;

      Set_Discrete_Range (Decl, Parse_Discrete_Range);
      return Decl;
   end Parse_Parameter_Specification;

   --  precond:  '<='
   --  postcond: next token
   --
   --  [ §8.4 ]
   --  signal_assignment_statement ::=
   --      [ label : ] target <= [ delay_mechanism ] waveform ;
   function Parse_Signal_Assignment_Statement (Target : Iir) return Iir
   is
      Stmt : Iir;
      Wave_Chain : Iir_Waveform_Element;
   begin
      Stmt := Create_Iir (Iir_Kind_Signal_Assignment_Statement);
      Location_Copy (Stmt, Target);
      Set_Target (Stmt, Target);
      Scan;
      Parse_Delay_Mechanism (Stmt);
      Wave_Chain := Parse_Waveform;
      --  LRM 8.4 Signal assignment statement
      --  It is an error is the reserved word UNAFFECTED appears as a
      --  waveform in a (sequential) signa assignment statement.
      if Wave_Chain = Null_Iir then
         Error_Msg_Parse
           ("'unaffected' is not allowed in a sequential statement");
      end if;
      Set_Waveform_Chain (Stmt, Wave_Chain);
      return Stmt;
   end Parse_Signal_Assignment_Statement;

   --  precond:  ':='
   --  postcond: next token
   --
   --  [ §8.5 ]
   --  variable_assignment_statement ::=
   --      [ label : ] target := expression ;
   function Parse_Variable_Assignment_Statement (Target : Iir) return Iir
   is
      Stmt : Iir;
   begin
      Stmt := Create_Iir (Iir_Kind_Variable_Assignment_Statement);
      Location_Copy (Stmt, Target);
      Set_Target (Stmt, Target);
      Scan;
      Set_Expression (Stmt, Parse_Expression);
      return Stmt;
   end Parse_Variable_Assignment_Statement;

   --  precond:  next token
   --  postcond: next token
   --
   --  [ 8 ]
   --  sequence_of_statement ::= { sequential_statement }
   --
   --  [ 8 ]
   --  sequential_statement ::= wait_statement
   --                         | assertion_statement
   --                         | report_statement
   --                         | signal_assignment_statement
   --                         | variable_assignment_statement
   --                         | procedure_call_statement
   --                         | if_statement
   --                         | case_statement
   --                         | loop_statement
   --                         | next_statement
   --                         | exit_statement
   --                         | return_statement
   --                         | null_statement
   --
   --  [ 8.13 ]
   --  null_statement ::= [ label : ] NULL ;
   --
   --  [ 8.12 ]
   --  return_statement ::= [ label : ] RETURN [ expression ]
   --
   --  [ 8.10 ]
   --  next_statement ::= [ label : ] NEXT [ LOOP_label ] [ WHEN condition ] ;
   --
   --  [ 8.11 ]
   --  exit_statement ::= [ label : ] EXIT [ LOOP_label ] [ WHEN condition ] ;
   --
   --  [ 8.9 ]
   --  loop_statement ::=
   --      [ LOOP_label : ]
   --          [ iteration_scheme ] LOOP
   --              sequence_of_statements
   --          END LOOP [ LOOP_label ] ;
   --
   --  [ 8.9 ]
   --  iteration_scheme ::= WHILE condition
   --                     | FOR LOOP_parameter_specification
   --
   --  [ 8.8 ]
   --  case_statement ::=
   --      [ CASE_label : ]
   --          CASE expression IS
   --              case_statement_alternative
   --              { case_statement_alternative }
   --          END CASE [ CASE_label ] ;
   --
   --  [ 8.8 ]
   --  case_statement_alternative ::= WHEN choices => sequence_of_statements
   --
   --  [ 8.2 ]
   --  assertion_statement ::= [ label : ] assertion ;
   --
   --  [ 8.3 ]
   --  report_statement ::= [ label : ] REPORT expression SEVERITY expression ;
   function Parse_Sequential_Assignment_Statement (Target : Iir) return Iir
   is
      Stmt : Iir;
      Call : Iir;
   begin
      if Current_Token = Tok_Less_Equal then
         return Parse_Signal_Assignment_Statement (Target);
      elsif Current_Token = Tok_Assign then
         return Parse_Variable_Assignment_Statement (Target);
      elsif Current_Token = Tok_Semi_Colon then
         return Parenthesis_Name_To_Procedure_Call
           (Target, Iir_Kind_Procedure_Call_Statement);
      else
         Error_Msg_Parse ("""<="" or "":="" expected instead of "
                          & Image (Current_Token));
         Stmt := Create_Iir (Iir_Kind_Procedure_Call_Statement);
         Call := Create_Iir (Iir_Kind_Procedure_Call);
         Set_Prefix (Call, Target);
         Set_Procedure_Call (Stmt, Call);
         Set_Location (Call);
         Eat_Tokens_Until_Semi_Colon;
         return Stmt;
      end if;
   end Parse_Sequential_Assignment_Statement;

   function Parse_Sequential_Statements (Parent : Iir)
     return Iir
   is
      First_Stmt : Iir;
      Last_Stmt : Iir;
      Stmt: Iir;
      Label: Name_Id;
      Loc : Location_Type;
      Target : Iir;
   begin
      First_Stmt := Null_Iir;
      Last_Stmt := Null_Iir;
      -- Expect a current_token.
      loop
         Loc := Get_Token_Location;
         if Current_Token = Tok_Identifier then
            Label := Current_Identifier;
            Scan;
            if Current_Token = Tok_Colon then
               Scan;
            else
               Target := Create_Iir (Iir_Kind_Simple_Name);
               Set_Identifier (Target, Label);
               Set_Location (Target, Loc);
               Label := Null_Identifier;
               Target := Parse_Name_Suffix (Target, True);
               Stmt := Parse_Sequential_Assignment_Statement (Target);
               goto Has_Stmt;
            end if;
         else
            Label := Null_Identifier;
         end if;

         case Current_Token is
            when Tok_Null =>
               Stmt := Create_Iir (Iir_Kind_Null_Statement);
               Scan;
            when Tok_Assert =>
               Stmt := Create_Iir (Iir_Kind_Assertion_Statement);
               Parse_Assertion (Stmt);
            when Tok_Report =>
               Stmt := Parse_Report_Statement;
            when Tok_If =>
               Stmt := Parse_If_Statement (Parent);
               Set_Label (Stmt, Label);
               Set_Location (Stmt, Loc);
               if Flags.Vhdl_Std >= Vhdl_93c then
                  Check_End_Name (Stmt);
               end if;
            when Tok_Identifier
              | Tok_String =>
               --  String for an expanded name with operator_symbol prefix.
               Stmt := Parse_Sequential_Assignment_Statement (Parse_Name);
            when Tok_Left_Paren =>
               declare
                  Target : Iir;
               begin
                  Target := Parse_Aggregate;
                  if Current_Token = Tok_Less_Equal then
                     Stmt := Parse_Signal_Assignment_Statement (Target);
                  elsif Current_Token = Tok_Assign then
                     Stmt := Parse_Variable_Assignment_Statement (Target);
                  else
                     Error_Msg_Parse ("'<=' or ':=' expected");
                     return First_Stmt;
                  end if;
               end;

            when Tok_Return =>
               Stmt := Create_Iir (Iir_Kind_Return_Statement);
               Scan;
               if Current_Token /= Tok_Semi_Colon then
                  Set_Expression (Stmt, Parse_Expression);
               end if;

            when Tok_For =>
               Stmt := Create_Iir (Iir_Kind_For_Loop_Statement);
               Set_Location (Stmt, Loc);
               Set_Label (Stmt, Label);

               --  Skip 'for'
               Scan;

               Set_Parameter_Specification
                 (Stmt, Parse_Parameter_Specification (Stmt));

               --  Skip 'loop'
               Expect (Tok_Loop);
               Scan;

               Set_Sequential_Statement_Chain
                 (Stmt, Parse_Sequential_Statements (Stmt));

               --  Skip 'end'
               Expect (Tok_End);
               Scan_Expect (Tok_Loop);

               --  Skip 'loop'
               Scan;

               Check_End_Name (Stmt);
               --  A loop statement can have a label, even in vhdl87.
               Label := Null_Identifier;

            when Tok_While
              | Tok_Loop =>
               Stmt := Create_Iir (Iir_Kind_While_Loop_Statement);
               Set_Location (Stmt);
               Set_Label (Stmt, Label);
               if Current_Token = Tok_While then
                  Scan;
                  Set_Condition (Stmt, Parse_Expression);
                  Expect (Tok_Loop);
               end if;
               Scan;
               Set_Sequential_Statement_Chain
                 (Stmt, Parse_Sequential_Statements (Stmt));
               Expect (Tok_End);
               Scan_Expect (Tok_Loop);
               Scan;
               Check_End_Name (Stmt);
               --  A loop statement can have a label, even in vhdl87.
               Label := Null_Identifier;

            when Tok_Next
              | Tok_Exit =>
               if Current_Token = Tok_Next then
                  Stmt := Create_Iir (Iir_Kind_Next_Statement);
               else
                  Stmt := Create_Iir (Iir_Kind_Exit_Statement);
               end if;

               --  Skip 'next' or 'exit'.
               Scan;

               if Current_Token = Tok_Identifier then
                  Set_Loop_Label (Stmt, Parse_Name (Allow_Indexes => False));
               end if;

               if Current_Token = Tok_When then
                  --  Skip 'when'.
                  Scan;

                  Set_Condition (Stmt, Parse_Expression);
               end if;

            when Tok_Case =>
               declare
                  use Iir_Chains.Case_Statement_Alternative_Chain_Handling;
                  Assoc: Iir;
                  Last_Assoc : Iir;
               begin
                  Stmt := Create_Iir (Iir_Kind_Case_Statement);
                  Set_Location (Stmt);
                  Set_Label (Stmt, Label);
                  Scan;
                  Set_Expression (Stmt, Parse_Expression);
                  Expect (Tok_Is);
                  Scan;
                  if Current_Token = Tok_End then
                     Error_Msg_Parse ("missing alternative in case statement");
                  end if;
                  Build_Init (Last_Assoc);
                  while Current_Token /= Tok_End loop
                     --  Eat 'when'
                     Expect (Tok_When);
                     Scan;

                     if Current_Token = Tok_Double_Arrow then
                        Error_Msg_Parse ("missing expression in alternative");
                        Assoc := Create_Iir (Iir_Kind_Choice_By_Expression);
                        Set_Location (Assoc);
                     else
                        Assoc := Parse_Choices (Null_Iir);
                     end if;

                     --  Eat '=>'
                     Expect (Tok_Double_Arrow);
                     Scan;

                     Set_Associated_Chain
                       (Assoc, Parse_Sequential_Statements (Stmt));
                     Append_Subchain (Last_Assoc, Stmt, Assoc);
                  end loop;

                  --  Eat 'end', 'case'
                  Scan_Expect (Tok_Case);
                  Scan;

                  if Flags.Vhdl_Std >= Vhdl_93c then
                     Check_End_Name (Stmt);
                  end if;
               end;
            when Tok_Wait =>
               Stmt := Parse_Wait_Statement;
            when others =>
               return First_Stmt;
         end case;
         << Has_Stmt >> null;
         Set_Parent (Stmt, Parent);
         Set_Location (Stmt, Loc);
         if Label /= Null_Identifier then
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Sem
                 ("this statement can't have a label in vhdl 87", Stmt);
            else
               Set_Label (Stmt, Label);
            end if;
         end if;
         Scan_Semi_Colon ("statement");

         --  Append it to the chain.
         if First_Stmt = Null_Iir then
            First_Stmt := Stmt;
         else
            Set_Chain (Last_Stmt, Stmt);
         end if;
         Last_Stmt := Stmt;
      end loop;
   end Parse_Sequential_Statements;

   --  precond : PROCEDURE, FUNCTION, PURE or IMPURE.
   --  postcond: ';'
   --
   --  [ §2.1 ]
   --  subprogram_declaration ::= subprogram_specification ;
   --
   --  [ §2.1 ]
   --  subprogram_specification ::=
   --      PROCEDURE designator [ ( formal_parameter_list ) ]
   --    | [ PURE | IMPURE ] FUNCTION designator [ ( formal_parameter_list ) ]
   --          RETURN type_mark
   --
   --  [ §2.2 ]
   --  subprogram_body ::=
   --      subprogram_specification IS
   --          subprogram_declarative_part
   --      BEGIN
   --          subprogram_statement_part
   --      END [ subprogram_kind ] [ designator ] ;
   --
   --  [ §2.1 ]
   --  designator ::= identifier | operator_symbol
   --
   --  [ §2.1 ]
   --  operator_symbol ::= string_literal
   function Parse_Subprogram_Declaration (Parent : Iir) return Iir
   is
      Kind : Iir_Kind;
      Inters : Iir;
      Subprg: Iir;
      Subprg_Body : Iir;
      Old : Iir;
      pragma Unreferenced (Old);
   begin
      --  Create the node.
      case Current_Token is
         when Tok_Procedure =>
            Kind := Iir_Kind_Procedure_Declaration;
         when Tok_Function
           | Tok_Pure
           | Tok_Impure =>
            Kind := Iir_Kind_Function_Declaration;
         when others =>
            raise Internal_Error;
      end case;
      Subprg := Create_Iir (Kind);
      Set_Location (Subprg);
      Set_Implicit_Definition (Subprg, Iir_Predefined_None);

      case Current_Token is
         when Tok_Procedure =>
            null;
         when Tok_Function =>
            --  LRM93 2.1
            --  A function is impure if its specification contains the
            --  reserved word IMPURE; otherwise it is said to be pure.
            Set_Pure_Flag (Subprg, True);
         when Tok_Pure
           | Tok_Impure =>
            Set_Pure_Flag (Subprg, Current_Token = Tok_Pure);
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse
                 ("'pure' and 'impure' are not allowed in vhdl 87");
            end if;
            Set_Has_Pure (Subprg, True);
            --  FIXME: what to do in case of error ??

            --  Eat 'pure' or 'impure'.
            Scan;

            Expect (Tok_Function, "'function' must follow 'pure' or 'impure'");
         when others =>
            raise Internal_Error;
      end case;

      --  Eat 'procedure' or 'function'.
      Scan;

      if Current_Token = Tok_Identifier then
         Set_Identifier (Subprg, Current_Identifier);
         Set_Location (Subprg);
      elsif Current_Token = Tok_String then
         if Kind = Iir_Kind_Procedure_Declaration then
            --  LRM93 2.1
            --  A procedure designator is always an identifier.
            Error_Msg_Parse ("a procedure name must be an identifier");
         end if;
         --  LRM93 2.1
         --  A function designator is either an identifier or an operator
         --  symbol.
         Set_Identifier (Subprg, Scan_To_Operator_Name (Get_Token_Location));
         Set_Location (Subprg);
      else
         --  Just to display a parse error.
         Expect (Tok_Identifier);
      end if;

      --  Eat designator (identifier or string).
      Scan;

      if Current_Token = Tok_Left_Paren then
         --  Parse the interface declaration.
         if Kind = Iir_Kind_Function_Declaration then
            Inters := Parse_Interface_List
              (Function_Parameter_Interface_List, Subprg);
         else
            Inters := Parse_Interface_List
              (Procedure_Parameter_Interface_List, Subprg);
         end if;
         Set_Interface_Declaration_Chain (Subprg, Inters);
      end if;

      if Current_Token = Tok_Return then
         if Kind = Iir_Kind_Procedure_Declaration then
            Error_Msg_Parse ("'return' not allowed for a procedure");
            Error_Msg_Parse ("(remove return part or define a function)");

            --  Skip 'return'
            Scan;

            Old := Parse_Type_Mark;
         else
            --  Skip 'return'
            Scan;

            Set_Return_Type_Mark
              (Subprg, Parse_Type_Mark (Check_Paren => True));
         end if;
      else
         if Kind = Iir_Kind_Function_Declaration then
            Error_Msg_Parse ("'return' expected");
         end if;
      end if;

      if Current_Token = Tok_Semi_Colon then
         return Subprg;
      end if;

      --  The body.
      Set_Has_Body (Subprg, True);
      if Kind = Iir_Kind_Function_Declaration then
         Subprg_Body := Create_Iir (Iir_Kind_Function_Body);
      else
         Subprg_Body := Create_Iir (Iir_Kind_Procedure_Body);
      end if;
      Location_Copy (Subprg_Body, Subprg);

      Set_Subprogram_Body (Subprg, Subprg_Body);
      Set_Subprogram_Specification (Subprg_Body, Subprg);
      Set_Chain (Subprg, Subprg_Body);

      if Get_Kind (Parent) = Iir_Kind_Package_Declaration then
         Error_Msg_Parse ("subprogram body not allowed in package spec");
      end if;
      Expect (Tok_Is);
      Scan;
      Parse_Declarative_Part (Subprg_Body);
      Expect (Tok_Begin);
      Scan;
      Set_Sequential_Statement_Chain
        (Subprg_Body, Parse_Sequential_Statements (Subprg_Body));
      Expect (Tok_End);
      Scan;

      case Current_Token is
         when Tok_Function =>
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse ("'function' not allowed here by vhdl 87");
            end if;
            if Kind = Iir_Kind_Procedure_Declaration then
               Error_Msg_Parse ("'procedure' expected instead of 'function'");
            end if;
            Set_End_Has_Reserved_Id (Subprg_Body, True);
            Scan;
         when Tok_Procedure =>
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse ("'procedure' not allowed here by vhdl 87");
            end if;
            if Kind = Iir_Kind_Function_Declaration then
               Error_Msg_Parse ("'function' expected instead of 'procedure'");
            end if;
            Set_End_Has_Reserved_Id (Subprg_Body, True);
            Scan;
         when others =>
            null;
      end case;
      case Current_Token is
         when Tok_Identifier =>
            Check_End_Name (Get_Identifier (Subprg), Subprg_Body);
         when Tok_String =>
            if Scan_To_Operator_Name (Get_Token_Location)
              /= Get_Identifier (Subprg)
            then
               Error_Msg_Parse
                 ("mispelling, 'end """ & Image_Identifier (Subprg)
                  & """;' expected");
            end if;
            Set_End_Has_Identifier (Subprg_Body, True);
            Scan;
         when others =>
            null;
      end case;
      Expect (Tok_Semi_Colon);
      return Subprg;
   end Parse_Subprogram_Declaration;

   --  precond:  PROCESS
   --  postcond: null
   --
   --  [ LRM87 9.2 / LRM08 11.3 ]
   --  process_statement ::=
   --    [ PROCESS_label : ]
   --       [ POSTPONED ] PROCESS [ ( process_sensitivity_list ) ] [ IS ]
   --           process_declarative_part
   --       BEGIN
   --           process_statement_part
   --       END [ POSTPONED ] PROCESS [ PROCESS_label ] ;
   --
   --  process_sensitivity_list ::= ALL | sensitivity_list
   function Parse_Process_Statement
     (Label: Name_Id; Loc : Location_Type; Is_Postponed : Boolean)
     return Iir
   is
      Res: Iir;
      Sensitivity_List : Iir_List;
   begin
      --  Skip 'process'
      Scan;

      if Current_Token = Tok_Left_Paren then
         Res := Create_Iir (Iir_Kind_Sensitized_Process_Statement);

         --  Skip '('
         Scan;

         if Current_Token = Tok_All then
            if Vhdl_Std < Vhdl_08 then
               Error_Msg_Parse
                 ("all sensitized process allowed only in vhdl 08");
            end if;
            Sensitivity_List := Iir_List_All;

            --  Skip 'all'
            Scan;
         else
            Sensitivity_List := Create_Iir_List;
            Parse_Sensitivity_List (Sensitivity_List);
         end if;
         Set_Sensitivity_List (Res, Sensitivity_List);

         --  Skip ')'
         Expect (Tok_Right_Paren);
         Scan;
      else
         Res := Create_Iir (Iir_Kind_Process_Statement);
      end if;

      Set_Location (Res, Loc);
      Set_Label (Res, Label);
      Set_Has_Label (Res, Label /= Null_Identifier);

      if Current_Token = Tok_Is then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse ("""is"" not allowed here by vhdl 87");
         end if;
         Set_Has_Is (Res, True);

         --  Skip 'is'
         Scan;
      end if;

      -- declarative part.
      Parse_Declarative_Part (Res);

      --  Skip 'begin'.
      Expect (Tok_Begin);
      Scan;

      Set_Sequential_Statement_Chain (Res, Parse_Sequential_Statements (Res));

      --  Skip 'end'.
      Expect (Tok_End);
      Scan;

      if Current_Token = Tok_Postponed then
         if not Is_Postponed then
            --  LRM93 9.2
            --  If the reserved word POSTPONED appears at the end of a process
            --  statement, the process must be a postponed process.
            Error_Msg_Parse ("process is not a postponed process");
         end if;

         Set_End_Has_Postponed (Res, True);

         --  Skip 'postponed',
         Scan;
      end if;

      if Current_Token = Tok_Semi_Colon then
         Error_Msg_Parse ("""end"" must be followed by ""process""");
      else
         Expect (Tok_Process);
         Scan;
         Set_End_Has_Reserved_Id (Res, True);
         Check_End_Name (Res);
         Expect (Tok_Semi_Colon);
      end if;
      return Res;
   end Parse_Process_Statement;

   -- precond : NEXT_TOKEN
   -- postcond: NEXT_TOKEN
   --
   --  [ LRM93 4.3.2.2 ]
   --  association_list ::= association_element { , association_element }
   --
   --  [ LRM93 4.3.2.2 ]
   --  association_element ::= [ formal_part => ] actual_part
   --
   --  [ LRM93 4.3.2.2 ]
   --  actual_part ::= actual_designator
   --                | FUNCTION_name ( actual_designator )
   --                | type_mark ( actual_designator )
   --
   --  [ LRM93 4.3.2.2 ]
   --  actual_designator ::= expression
   --                      | SIGNAL_name
   --                      | VARIABLE_name
   --                      | FILE_name
   --                      | OPEN
   --
   --  [ LRM93 4.3.2.2 ]
   --  formal_part ::= formal_designator
   --                | FUNCTION_name ( formal_designator )
   --                | type_mark ( formal_designator )
   --
   --  [ LRM93 4.3.2.2 ]
   --  formal_designator ::= GENERIC_name
   --                      | PORT_name
   --                      | PARAMETER_name
   --
   --  Note: an actual part is parsed as an expression.
   function Parse_Association_List return Iir
   is
      Res, Last: Iir;
      El: Iir;
      Formal: Iir;
      Actual: Iir;
      Nbr_Assocs : Natural;
      Loc : Location_Type;
   begin
      Sub_Chain_Init (Res, Last);

      if Current_Token = Tok_Right_Paren then
         Error_Msg_Parse ("empty association list is not allowed");
         return Res;
      end if;

      Nbr_Assocs := 1;
      loop
         --  Parse formal and actual.
         Loc := Get_Token_Location;
         Formal := Null_Iir;

         if Current_Token /= Tok_Open then
            Actual := Parse_Expression;
            case Current_Token is
               when Tok_To
                 | Tok_Downto =>
                  --  To/downto can appear in slice name (which are parsed as
                  --  function call).

                  if Actual = Null_Iir then
                     --  Left expression is missing ie: (downto x).
                     Scan;
                     Actual := Parse_Expression;
                  else
                     Actual := Parse_Range_Expression (Actual);
                  end if;
                  if Nbr_Assocs /= 1 then
                     Error_Msg_Parse ("multi-dimensional slice is forbidden");
                  end if;

               when Tok_Double_Arrow =>
                  Formal := Actual;

                  --  Skip '=>'
                  Scan;
                  Loc := Get_Token_Location;

                  if Current_Token /= Tok_Open then
                     Actual := Parse_Expression;
                  end if;

               when others =>
                  null;
            end case;
         end if;

         if Current_Token = Tok_Open then
            El := Create_Iir (Iir_Kind_Association_Element_Open);
            Set_Location (El);

            --  Skip 'open'
            Scan;
         else
            El := Create_Iir (Iir_Kind_Association_Element_By_Expression);
            Set_Location (El, Loc);
            Set_Actual (El, Actual);
         end if;
         Set_Formal (El, Formal);

         Sub_Chain_Append (Res, Last, El);
         exit when Current_Token = Tok_Right_Paren;
         Expect (Tok_Comma);
         Scan;
         Nbr_Assocs := Nbr_Assocs + 1;
      end loop;

      return Res;
   end Parse_Association_List;

   -- precond : NEXT_TOKEN
   -- postcond: NEXT_TOKEN
   --
   -- Parse: '(' association_list ')'
   function Parse_Association_List_In_Parenthesis return Iir
   is
      Res : Iir;
   begin
      --  Skip '('
      Expect (Tok_Left_Paren);
      Scan;

      Res := Parse_Association_List;

      --  Skip ')'
      Scan;

      return Res;
   end Parse_Association_List_In_Parenthesis;

   --  precond : GENERIC
   --  postcond: next token
   --
   --  [ LRM93 5.2.1.2, LRM08 6.5.7.2 ]
   --  generic_map_aspect ::= GENERIC MAP ( GENERIC_association_list )
   function Parse_Generic_Map_Aspect return Iir is
   begin
      Expect (Tok_Generic);
      Scan_Expect (Tok_Map);
      Scan;
      return Parse_Association_List_In_Parenthesis;
   end Parse_Generic_Map_Aspect;

   --  precond : PORT
   --  postcond: next token
   --
   --  [ §5.2.1.2 ]
   --  port_map_aspect ::= PORT MAP ( PORT_association_list )
   function Parse_Port_Map_Aspect return Iir is
   begin
      Expect (Tok_Port);
      Scan_Expect (Tok_Map);
      Scan;
      return Parse_Association_List_In_Parenthesis;
   end Parse_Port_Map_Aspect;

   --  precond : COMPONENT | ENTIY | CONFIGURATION
   --  postcond : next_token
   --
   --  instantiated_unit ::=
   --      [ COMPONENT ] component_name
   --      ENTITY entity_name [ ( architecture_identifier ) ]
   --      CONFIGURATION configuration_name
   function Parse_Instantiated_Unit return Iir
   is
      Res : Iir;
   begin
      if Flags.Vhdl_Std = Vhdl_87 then
         Error_Msg_Parse
           ("component instantiation using keyword 'component', 'entity',");
         Error_Msg_Parse (" or 'configuration' is not allowed in vhdl87");
      end if;

      case Current_Token is
         when Tok_Component =>
            Scan;
            return Parse_Name (False);
         when Tok_Entity =>
            Res := Create_Iir (Iir_Kind_Entity_Aspect_Entity);
            Set_Location (Res);
            Scan;
            Set_Entity_Name (Res, Parse_Name (False));
            if Current_Token = Tok_Left_Paren then
               Scan_Expect (Tok_Identifier);
               Set_Architecture (Res, Current_Text);
               Scan_Expect (Tok_Right_Paren);
               Scan;
            end if;
            return Res;
         when Tok_Configuration =>
            Res := Create_Iir (Iir_Kind_Entity_Aspect_Configuration);
            Set_Location (Res);
            Scan_Expect (Tok_Identifier);
            Set_Configuration_Name (Res, Parse_Name (False));
            return Res;
         when others =>
            raise Internal_Error;
      end case;
   end Parse_Instantiated_Unit;

   --  precond : next token
   --  postcond: ';'
   --
   --  component_instantiation_statement ::=
   --      INSTANTIATION_label :
   --          instantiated_unit [ generic_map_aspect ] [ port_map_aspect ] ;
   function Parse_Component_Instantiation (Name: Iir)
      return Iir_Component_Instantiation_Statement is
      Res: Iir_Component_Instantiation_Statement;
   begin
      Res := Create_Iir (Iir_Kind_Component_Instantiation_Statement);
      Set_Location (Res);

      Set_Instantiated_Unit (Res, Name);

      if Current_Token = Tok_Generic then
         Set_Generic_Map_Aspect_Chain (Res, Parse_Generic_Map_Aspect);
      end if;
      if Current_Token = Tok_Port then
         Set_Port_Map_Aspect_Chain (Res, Parse_Port_Map_Aspect);
      end if;
      Expect (Tok_Semi_Colon);
      return Res;
   end Parse_Component_Instantiation;

   --  precond : next token
   --  postcond: next token
   --
   --  [ §9.1 ]
   --  block_header ::= [ generic_clause [ generic_map_aspect ; ] ]
   --                   [ port_clause [ port_map_aspect ; ] ]
   function Parse_Block_Header return Iir_Block_Header is
      Res : Iir_Block_Header;
   begin
      Res := Create_Iir (Iir_Kind_Block_Header);
      Set_Location (Res);
      if Current_Token = Tok_Generic then
         Parse_Generic_Clause (Res);
         if Current_Token = Tok_Generic then
            Set_Generic_Map_Aspect_Chain (Res, Parse_Generic_Map_Aspect);
            Scan_Semi_Colon ("generic map aspect");
         end if;
      end if;
      if Current_Token = Tok_Port then
         Parse_Port_Clause (Res);
         if Current_Token = Tok_Port then
            Set_Port_Map_Aspect_Chain (Res, Parse_Port_Map_Aspect);
            Scan_Semi_Colon ("port map aspect");
         end if;
      end if;
      return Res;
   end Parse_Block_Header;

   --  precond : BLOCK
   --  postcond: ';'
   --
   --  [ §9.1 ]
   --  block_statement ::=
   --      BLOCK_label :
   --          BLOCK [ ( GUARD_expression ) ] [ IS ]
   --              block_header
   --              block_declarative_part
   --          BEGIN
   --              block_statement_part
   --          END BLOCK [ BLOCK_label ] ;
   --
   --  [ §9.1 ]
   --  block_declarative_part ::= { block_declarative_item }
   --
   --  [ §9.1 ]
   --  block_statement_part ::= { concurrent_statement }
   function Parse_Block_Statement (Label: Name_Id; Loc : Location_Type)
     return Iir_Block_Statement
   is
      Res : Iir_Block_Statement;
      Guard : Iir_Guard_Signal_Declaration;
   begin
      if Label = Null_Identifier then
         Error_Msg_Parse ("a block statement must have a label");
      end if;

      -- block was just parsed.
      Res := Create_Iir (Iir_Kind_Block_Statement);
      Set_Location (Res, Loc);
      Set_Label (Res, Label);
      Scan;
      if Current_Token = Tok_Left_Paren then
         Guard := Create_Iir (Iir_Kind_Guard_Signal_Declaration);
         Set_Location (Guard);
         Set_Guard_Decl (Res, Guard);
         Scan;
         Set_Guard_Expression (Guard, Parse_Expression);
         Expect (Tok_Right_Paren, "a ')' is expected after guard expression");
         Scan;
      end if;
      if Current_Token = Tok_Is then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse ("'is' not allowed here in vhdl87");
         end if;
         Scan;
      end if;
      if Current_Token = Tok_Generic or Current_Token = Tok_Port then
         Set_Block_Header (Res, Parse_Block_Header);
      end if;
      if Current_Token /= Tok_Begin then
         Parse_Declarative_Part (Res);
      end if;
      Expect (Tok_Begin);
      Scan;
      Parse_Concurrent_Statements (Res);
      Check_End_Name (Tok_Block, Res);
      return Res;
   end Parse_Block_Statement;

   --  Precond : next token
   --  Postcond: next token after 'end'
   --
   --  [ LRM08 11.8 ] Generate statements
   --  generate_statement_body ::=
   --        [ block_declarative_part
   --     BEGIN ]
   --        { concurrent_statement }
   --     [ END [ alternative_label ] ; ]
   --
   --  This corresponds to the following part of LRM93 9.7:
   --        [ { block_declarative_item }
   --     BEGIN ]
   --        { concurrent_statement }
   --  Note there is no END.  This part is followed by:
   --     END GENERATE [ /generate/_label ] ;
   function Parse_Generate_Statement_Body (Parent : Iir; Label : Name_Id)
                                          return Iir
   is
      Bod : Iir;
   begin
      Bod := Create_Iir (Iir_Kind_Generate_Statement_Body);
      Set_Location (Bod);
      Set_Parent (Bod, Parent);
      Set_Alternative_Label (Bod, Label);
      Set_Has_Label (Bod, Label /= Null_Identifier);

      --  Check for a block declarative item.
      case Current_Token is
         when
         --  subprogram_declaration
         --  subprogram_body
           Tok_Procedure
           | Tok_Function
           | Tok_Pure
           | Tok_Impure
         --  type_declaration
           | Tok_Type
         --  subtype_declaration
           | Tok_Subtype
         --  constant_declaration
           | Tok_Constant
         --  signal_declaration
           | Tok_Signal
         --  shared_variable_declaration
           | Tok_Shared
           | Tok_Variable
         --  file_declaration
           | Tok_File
         --  alias_declaration
           | Tok_Alias
         --  component_declaration
           | Tok_Component
         --  attribute_declaration
         --  attribute_specification
           | Tok_Attribute
         --  configuration_specification
           | Tok_For
         --  disconnection_specification
           | Tok_Disconnect
         --  use_clause
           | Tok_Use
         --  group_template_declaration
         --  group_declaration
           | Tok_Group
           | Tok_Begin =>
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse
                 ("declarations not allowed in a generate in vhdl87");
            end if;
            Parse_Declarative_Part (Bod);
            Expect (Tok_Begin);
            Set_Has_Begin (Bod, True);

            --  Skip 'begin'
            Scan;
         when others =>
            null;
      end case;

      Parse_Concurrent_Statements (Bod);

      case Current_Token is
         when Tok_Elsif
           | Tok_Else =>
            if Get_Kind (Parent) = Iir_Kind_If_Generate_Statement then
               return Bod;
            end if;
         when others =>
            null;
      end case;

      Expect (Tok_End);

      --  Skip 'end'
      Scan;

      if Vhdl_Std >= Vhdl_08 and then Current_Token /= Tok_Generate then
         --  This is the 'end' of the generate_statement_body.
         Set_Has_End (Bod, True);
         Check_End_Name (Label, Bod);
         Scan_Semi_Colon ("generate statement body");

         Expect (Tok_End);

         --  Skip 'end'
         Scan;
      end if;

      return Bod;
   end Parse_Generate_Statement_Body;

   --  precond : FOR
   --  postcond: ';'
   --
   --  [ LRM93 9.7 ]
   --  generate_statement ::=
   --      GENERATE_label : generation_scheme GENERATE
   --          [ { block_declarative_item }
   --      BEGIN ]
   --          { concurrent_statement }
   --      END GENERATE [ GENERATE_label ] ;
   --
   --  [ LRM93 9.7 ]
   --  generation_scheme ::=
   --      FOR GENERATE_parameter_specification
   --      | IF condition
   --
   --  [ LRM08 11.8 ]
   --  for_generate_statement ::=
   --     /generate/_label :
   --        FOR /generate/_parameter_specification GENERATE
   --           generate_statement_body
   --        END GENERATE [ /generate/_label ] ;
   --
   --  FIXME: block_declarative item.
   function Parse_For_Generate_Statement (Label : Name_Id; Loc : Location_Type)
                                         return Iir
   is
      Res : Iir;
   begin
      if Label = Null_Identifier then
         Error_Msg_Parse ("a generate statement must have a label");
      end if;
      Res := Create_Iir (Iir_Kind_For_Generate_Statement);
      Set_Location (Res, Loc);
      Set_Label (Res, Label);

      --  Skip 'for'
      Scan;

      Set_Parameter_Specification (Res, Parse_Parameter_Specification (Res));

      --  Skip 'generate'
      Expect (Tok_Generate);
      Scan;

      Set_Generate_Statement_Body
        (Res, Parse_Generate_Statement_Body (Res, Null_Identifier));

      Expect (Tok_Generate);
      Set_End_Has_Reserved_Id (Res, True);

      --  Skip 'generate'
      Scan;

      --  LRM93 9.7
      --  If a label appears at the end of a generate statement, it must repeat
      --  the generate label.
      Check_End_Name (Res);
      Expect (Tok_Semi_Colon);
      return Res;
   end Parse_For_Generate_Statement;

   --  precond : IF
   --  postcond: ';'
   --
   --  [ LRM93 9.7 ]
   --  generate_statement ::=
   --      /generate/_label : generation_scheme GENERATE
   --          [ { block_declarative_item }
   --      BEGIN ]
   --          { concurrent_statement }
   --      END GENERATE [ /generate/_label ] ;
   --
   --  [ LRM93 9.7 ]
   --  generation_scheme ::=
   --      FOR GENERATE_parameter_specification
   --      | IF condition
   --
   --  [ LRM08 11.8 ]
   --  if_generate_statement ::=
   --     /generate/_label :
   --     IF [ /alternative/_label : ] condition GENERATE
   --        generate_statement_body
   --     { ELSIF [ /alternative/_label : ] condition GENERATE
   --        generate_statement_body }
   --     [ ELSE [ /alternative/_label : ] GENERATE
   --        generate_statement_body ]
   --     END GENERATE [ /generate/_label ] ;
   function Parse_If_Generate_Statement (Label : Name_Id; Loc : Location_Type)
                                        return Iir_Generate_Statement
   is
      Res : Iir_Generate_Statement;
      Alt_Label : Name_Id;
      Alt_Loc : Location_Type;
      Cond : Iir;
      Clause : Iir;
      Bod : Iir;
      Last : Iir;
   begin
      if Label = Null_Identifier then
         Error_Msg_Parse ("a generate statement must have a label");
      end if;
      Res := Create_Iir (Iir_Kind_If_Generate_Statement);
      Set_Location (Res, Loc);
      Set_Label (Res, Label);

      --  Skip 'if'.
      Scan;

      Clause := Res;
      Last := Null_Iir;
      loop
         Cond := Parse_Expression;

         Alt_Label := Null_Identifier;
         if Current_Token = Tok_Colon then
            if Get_Kind (Cond) = Iir_Kind_Simple_Name then
               if Vhdl_Std < Vhdl_08 then
                  Error_Msg_Parse
                    ("alternative label not allowed before vhdl08");
               end if;

               --  In fact the parsed condition was an alternate label.
               Alt_Label := Get_Identifier (Cond);
               Alt_Loc := Get_Location (Cond);
               Free_Iir (Cond);
            else
               Error_Msg_Parse ("alternative label must be an identifier");
               Free_Iir (Cond);
            end if;

            --  Skip ':'
            Scan;

            Cond := Parse_Expression;
         end if;

         Set_Condition (Clause, Cond);

         --  Skip 'generate'
         Expect (Tok_Generate);
         Scan;

         Bod := Parse_Generate_Statement_Body (Res, Alt_Label);

         if Alt_Label /= Null_Identifier then
            --  Set location on the label, for xrefs.
            Set_Location (Bod, Alt_Loc);
         end if;

         Set_Generate_Statement_Body (Clause, Bod);

         if Last /= Null_Iir then
            Set_Generate_Else_Clause (Last, Clause);
         end if;
         Last := Clause;

         exit when Current_Token /= Tok_Elsif;
      end loop;

      if Current_Token = Tok_Else then
         if Vhdl_Std < Vhdl_08 then
            Error_Msg_Parse ("else generate not allowed before vhdl08");
         end if;

         Clause := Create_Iir (Iir_Kind_If_Generate_Else_Clause);
         Set_Location (Clause);

         --  Skip 'else'
         Scan;

         if Current_Token = Tok_Identifier then
            Alt_Label := Current_Identifier;
            Alt_Loc := Get_Token_Location;

            --  Skip identifier
            Scan;

            Expect (Tok_Colon);

            --  Skip ':'
            Scan;
         else
            Alt_Label := Null_Identifier;
         end if;

         --  Skip 'generate'
         Expect (Tok_Generate);
         Scan;

         Bod := Parse_Generate_Statement_Body (Res, Alt_Label);
         if Alt_Label /= Null_Identifier then
            --  Set location on the label, for xrefs.
            Set_Location (Bod, Alt_Loc);
         end if;

         Set_Generate_Statement_Body (Clause, Bod);

         Set_Generate_Else_Clause (Last, Clause);
      end if;

      Expect (Tok_Generate);
      Set_End_Has_Reserved_Id (Res, True);

      --  Skip 'generate'
      Scan;

      --  LRM93 9.7
      --  If a label appears at the end of a generate statement, it must repeat
      --  the generate label.
      Check_End_Name (Res);
      Expect (Tok_Semi_Colon);
      return Res;
   end Parse_If_Generate_Statement;

   --  precond : first token
   --  postcond: END
   --
   --  [ §9 ]
   --  concurrent_statement ::= block_statement
   --                         | process_statement
   --                         | concurrent_procedure_call_statement
   --                         | concurrent_assertion_statement
   --                         | concurrent_signal_assignment_statement
   --                         | component_instantiation_statement
   --                         | generate_statement
   --
   --  [ §9.4 ]
   --  concurrent_assertion_statement ::=
   --      [ label : ] [ POSTPONED ] assertion ;
   --
   --  [ §9.3 ]
   --  concurrent_procedure_call_statement ::=
   --      [ label : ] [ POSTPONED ] procedure_call ;
   --
   --  [ §9.5 ]
   --  concurrent_signal_assignment_statement ::=
   --      [ label : ] [ POSTPONED ] conditional_signal_assignment
   --    | [ label : ] [ POSTPONED ] selected_signal_assignment
   function Parse_Concurrent_Assignment (Target : Iir) return Iir
   is
      Res : Iir;
   begin
      case Current_Token is
         when Tok_Less_Equal
           | Tok_Assign =>
            -- This is a conditional signal assignment.
            -- Error for ':=' is handled by the subprogram.
            return Parse_Conditional_Signal_Assignment (Target);
         when Tok_Semi_Colon =>
            -- a procedure call or a component instantiation.
            -- Parse it as a procedure call, may be revert to a
            -- component instantiation during sem.
            Expect (Tok_Semi_Colon);
            return Parenthesis_Name_To_Procedure_Call
              (Target, Iir_Kind_Concurrent_Procedure_Call_Statement);
         when Tok_Generic | Tok_Port =>
            -- or a component instantiation.
            return Parse_Component_Instantiation (Target);
         when others =>
            -- or a simple simultaneous statement
            if AMS_Vhdl then
               Res := Create_Iir (Iir_Kind_Simple_Simultaneous_Statement);
               Set_Simultaneous_Left (Res, Parse_Simple_Expression (Target));
               if Current_Token /= Tok_Equal_Equal then
                  Error_Msg_Parse ("'==' expected after expression");
               else
                  Set_Location (Res);
                  Scan;
               end if;
               Set_Simultaneous_Right (Res, Parse_Simple_Expression);
               Set_Tolerance (Res, Parse_Tolerance_Aspect_Opt);
               Expect (Tok_Semi_Colon);
               return Res;
            else
               return Parse_Conditional_Signal_Assignment
                 (Parse_Simple_Expression (Target));
            end if;
      end case;
   end Parse_Concurrent_Assignment;

   function Parse_Psl_Default_Clock return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Psl_Default_Clock);
      Scanner.Flag_Psl := True;
      Scan_Expect (Tok_Psl_Clock);
      Scan_Expect (Tok_Is);
      Scan;
      Set_Psl_Boolean (Res, Parse_Psl.Parse_Psl_Boolean);
      Expect (Tok_Semi_Colon);
      Scanner.Flag_Scan_In_Comment := False;
      Scanner.Flag_Psl := False;
      return Res;
   end Parse_Psl_Default_Clock;

   function Parse_Psl_Declaration return Iir
   is
      Tok : constant Token_Type := Current_Token;
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Psl_Declaration);

      --  Skip 'property', 'sequence' or 'endpoint'.
      Scan;

      if Current_Token /= Tok_Identifier then
         Error_Msg_Parse ("property name expected here");
      else
         Set_Identifier (Res, Current_Identifier);
      end if;

      Scanner.Flag_Psl := True;
      Set_Psl_Declaration (Res, Parse_Psl.Parse_Psl_Declaration (Tok));
      Expect (Tok_Semi_Colon);
      Scanner.Flag_Scan_In_Comment := False;
      Scanner.Flag_Psl := False;

      return Res;
   end Parse_Psl_Declaration;

   function Parse_Psl_Assert_Statement return Iir
   is
      Res : Iir;
   begin
      case Current_Token is
         when Tok_Assert =>
            Res := Create_Iir (Iir_Kind_Psl_Assert_Statement);
         when Tok_Psl_Cover =>
            Res := Create_Iir (Iir_Kind_Psl_Cover_Statement);
         when others =>
            raise Internal_Error;
      end case;

      --  Skip 'assert'
      Scan;

      Set_Psl_Property (Res, Parse_Psl.Parse_Psl_Property);

      --  No more PSL tokens after the property.
      Scanner.Flag_Psl := False;

      if Current_Token = Tok_Report then
         --  Skip 'report'
         Scan;

         Set_Report_Expression (Res, Parse_Expression);
      end if;

      if Current_Token = Tok_Severity then
         --  Skip 'severity'
         Scan;

         Set_Severity_Expression (Res, Parse_Expression);
      end if;

      Expect (Tok_Semi_Colon);
      Scanner.Flag_Scan_In_Comment := False;
      return Res;
   end Parse_Psl_Assert_Statement;

   procedure Parse_Concurrent_Statements (Parent : Iir)
   is
      Last_Stmt : Iir;
      Stmt: Iir;
      Label: Name_Id;
      Id: Iir;
      Postponed : Boolean;
      Loc : Location_Type;
      Target : Iir;

      procedure Postponed_Not_Allowed is
      begin
         if Postponed then
            Error_Msg_Parse ("'postponed' not allowed here");
            Postponed := False;
         end if;
      end Postponed_Not_Allowed;

      procedure Label_Not_Allowed is
      begin
         if Label /= Null_Identifier then
            Error_Msg_Parse ("'postponed' not allowed here");
            Label := Null_Identifier;
         end if;
      end Label_Not_Allowed;
   begin
      -- begin was just parsed.
      Last_Stmt := Null_Iir;
      loop
         Stmt := Null_Iir;
         Label := Null_Identifier;
         Postponed := False;
         Loc := Get_Token_Location;

         -- Try to find a label.
         if Current_Token = Tok_Identifier then
            Label := Current_Identifier;

            --  Skip identifier
            Scan;

            if Current_Token = Tok_Colon then
               --  The identifier is really a label.

               --  Skip ':'
               Scan;
            else
               --  This is not a label.  Assume a concurrent assignment.
               Target := Create_Iir (Iir_Kind_Simple_Name);
               Set_Location (Target, Loc);
               Set_Identifier (Target, Label);
               Label := Null_Identifier;
               Target := Parse_Name_Suffix (Target);

               Stmt := Parse_Concurrent_Assignment (Target);
               goto Has_Stmt;
            end if;
         end if;

         if Current_Token = Tok_Postponed then
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse ("'postponed' is not allowed in vhdl 87");
            else
               Postponed := True;
            end if;

            --  Skip 'postponed'
            Scan;
         end if;

         case Current_Token is
            when Tok_End | Tok_Else | Tok_Elsif | Tok_When =>
               --  End of list.  'else', 'elseif' and 'when' can be used to
               --  separate statements in a generate statement.
               Postponed_Not_Allowed;
               if Label /= Null_Identifier then
                  Error_Msg_Parse ("label is not allowed here");
               end if;
               return;
            when Tok_Identifier =>
               Target := Parse_Name (Allow_Indexes => True);
               Stmt := Parse_Concurrent_Assignment (Target);
               if Get_Kind (Stmt) = Iir_Kind_Component_Instantiation_Statement
                 and then Postponed
               then
                  Error_Msg_Parse ("'postponed' not allowed for " &
                                   "an instantiation statement");
                  Postponed := False;
               end if;
            when Tok_Left_Paren =>
               Id := Parse_Aggregate;
               if Current_Token = Tok_Less_Equal then
                  -- This is a conditional signal assignment.
                  Stmt := Parse_Conditional_Signal_Assignment (Id);
               else
                  Error_Msg_Parse ("'<=' expected after aggregate");
                  Eat_Tokens_Until_Semi_Colon;
               end if;
            when Tok_Process =>
               Stmt := Parse_Process_Statement (Label, Loc, Postponed);
            when Tok_Assert =>
               if Vhdl_Std >= Vhdl_08
                 or else (Flag_Psl_Comment and then Flag_Scan_In_Comment)
               then
                  Stmt := Parse_Psl_Assert_Statement;
               else
                  Stmt := Create_Iir (Iir_Kind_Concurrent_Assertion_Statement);
                  Parse_Assertion (Stmt);
                  Expect (Tok_Semi_Colon);
               end if;
            when Tok_With =>
               Stmt := Parse_Selected_Signal_Assignment;
            when Tok_Block =>
               Postponed_Not_Allowed;
               Stmt := Parse_Block_Statement (Label, Loc);
            when Tok_If =>
               Postponed_Not_Allowed;
               Stmt := Parse_If_Generate_Statement (Label, Loc);
            when Tok_For =>
               Postponed_Not_Allowed;
               Stmt := Parse_For_Generate_Statement (Label, Loc);
            when Tok_Eof =>
               Error_Msg_Parse ("unexpected end of file, 'END;' expected");
               return;
            when Tok_Component
              | Tok_Entity
              | Tok_Configuration =>
               Postponed_Not_Allowed;
               declare
                  Unit : Iir;
               begin
                  Unit := Parse_Instantiated_Unit;
                  Stmt := Parse_Component_Instantiation (Unit);
               end;
            when Tok_Psl_Default =>
               Postponed_Not_Allowed;
               Label_Not_Allowed;
               Stmt := Parse_Psl_Default_Clock;
            when Tok_Psl_Property
              | Tok_Psl_Sequence
              | Tok_Psl_Endpoint =>
               Postponed_Not_Allowed;
               Label_Not_Allowed;
               Stmt := Parse_Psl_Declaration;
            when Tok_Psl_Cover =>
               Postponed_Not_Allowed;
               Stmt := Parse_Psl_Assert_Statement;
            when others =>
               --  FIXME: improve message:
               --  instead of 'unexpected token 'signal' in conc stmt list'
               --  report: 'signal declarations are not allowed in conc stmt'
               Unexpected ("concurrent statement list");
               Eat_Tokens_Until_Semi_Colon;
         end case;

         << Has_Stmt >> null;

         --  Stmt can be null in case of error.
         if Stmt /= Null_Iir then
            Set_Location (Stmt, Loc);
            if Label /= Null_Identifier then
               Set_Label (Stmt, Label);
            end if;
            Set_Parent (Stmt, Parent);
            if Postponed then
               Set_Postponed_Flag (Stmt, True);
            end if;
            --  Append it to the chain.
            if Last_Stmt = Null_Iir then
               Set_Concurrent_Statement_Chain (Parent, Stmt);
            else
               Set_Chain (Last_Stmt, Stmt);
            end if;
            Last_Stmt := Stmt;
         end if;

         Scan;
      end loop;
   end Parse_Concurrent_Statements;

   --  precond : LIBRARY
   --  postcond: ;
   --
   --  [ LRM93 11.2 ]
   --  library_clause ::= LIBRARY logical_name_list
   function Parse_Library_Clause return Iir
   is
      First, Last : Iir;
      Library: Iir_Library_Clause;
   begin
      Sub_Chain_Init (First, Last);
      Expect (Tok_Library);
      loop
         Library := Create_Iir (Iir_Kind_Library_Clause);

         --  Skip 'library' or ','.
         Scan_Expect (Tok_Identifier);

         Set_Identifier (Library, Current_Identifier);
         Set_Location (Library);
         Sub_Chain_Append (First, Last, Library);

         --  Skip identifier.
         Scan;

         exit when Current_Token = Tok_Semi_Colon;
         Expect (Tok_Comma);

         Set_Has_Identifier_List (Library, True);
      end loop;

      --  Skip ';'.
      Scan;
      return First;
   end Parse_Library_Clause;

   --  precond : USE
   --  postcond: ;
   --
   --  [ §10.4 ]
   --  use_clause ::= USE selected_name { , selected_name }
   --
   --  FIXME: should be a list.
   function Parse_Use_Clause return Iir_Use_Clause
   is
      Use_Clause: Iir_Use_Clause;
      First, Last : Iir;
   begin
      First := Null_Iir;
      Last := Null_Iir;
      Scan;
      loop
         Use_Clause := Create_Iir (Iir_Kind_Use_Clause);
         Set_Location (Use_Clause);
         Expect (Tok_Identifier);
         Set_Selected_Name (Use_Clause, Parse_Name);

         --  Chain use clauses.
         if First = Null_Iir then
            First := Use_Clause;
         else
            Set_Use_Clause_Chain (Last, Use_Clause);
         end if;
         Last := Use_Clause;

         exit when Current_Token = Tok_Semi_Colon;
         Expect (Tok_Comma);
         Scan;
      end loop;
      return First;
   end Parse_Use_Clause;

   --  precond : ARCHITECTURE
   --  postcond: ';'
   --
   --  [ §1.2 ]
   --  architecture_body ::=
   --      ARCHITECTURE identifier OF ENTITY_name IS
   --          architecture_declarative_part
   --      BEGIN
   --          architecture_statement_part
   --      END [ ARCHITECTURE ] [ ARCHITECTURE_simple_name ] ;
   procedure Parse_Architecture_Body (Unit : Iir_Design_Unit)
   is
      Res: Iir_Architecture_Body;
   begin
      Expect (Tok_Architecture);
      Res := Create_Iir (Iir_Kind_Architecture_Body);

      -- Get identifier.
      Scan_Expect (Tok_Identifier);
      Set_Identifier (Res, Current_Identifier);
      Set_Location (Res);
      Scan;
      if Current_Token = Tok_Is then
         Error_Msg_Parse ("architecture identifier is missing");
      else
         Expect (Tok_Of);
         Scan;
         Set_Entity_Name (Res, Parse_Name (False));
         Expect (Tok_Is);
      end if;

      Scan;
      Parse_Declarative_Part (Res);

      Expect (Tok_Begin);
      Scan;
      Parse_Concurrent_Statements (Res);
      -- end was scanned.
      Set_End_Location (Unit);
      Scan;
      if Current_Token = Tok_Architecture then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse
              ("'architecture' keyword not allowed here by vhdl 87");
         end if;
         Set_End_Has_Reserved_Id (Res, True);
         Scan;
      end if;
      Check_End_Name (Res);
      Expect (Tok_Semi_Colon);
      Set_Library_Unit (Unit, Res);
   end Parse_Architecture_Body;

   --  precond : next token
   --  postcond: a token
   --
   --  [ §5.2 ]
   --  instantiation_list ::= INSTANTIATION_label { , INSTANTIATION_label }
   --                       | OTHERS
   --                       | ALL
   function Parse_Instantiation_List return Iir_List
   is
      Res : Iir_List;
   begin
      case Current_Token is
         when Tok_All =>
            Scan;
            return Iir_List_All;
         when Tok_Others =>
            Scan;
            return Iir_List_Others;
         when Tok_Identifier =>
            Res := Create_Iir_List;
            loop
               Append_Element (Res, Current_Text);
               Scan;
               exit when Current_Token /= Tok_Comma;
               Expect (Tok_Comma);
               Scan;
            end loop;
            return Res;
         when others =>
            Error_Msg_Parse ("instantiation list expected");
            return Null_Iir_List;
      end case;
   end Parse_Instantiation_List;

   --  precond : next token
   --  postcond: next token
   --
   --  [ §5.2 ]
   --  component_specification ::= instantiation_list : COMPONENT_name
   procedure Parse_Component_Specification (Res : Iir)
   is
      List : Iir_List;
   begin
      List := Parse_Instantiation_List;
      Set_Instantiation_List (Res, List);
      Expect (Tok_Colon);
      Scan_Expect (Tok_Identifier);
      Set_Component_Name (Res, Parse_Name);
   end Parse_Component_Specification;

   --  precond : next token
   --  postcond: next token
   --
   --  [ §5.2.1.1 ]
   --  entity_aspect ::= ENTITY ENTITY_name [ ( ARCHITECTURE_identifier ) ]
   --                  | CONFIGURATION CONFIGURATION_name
   --                  | OPEN
   function Parse_Entity_Aspect return Iir
   is
      Res : Iir;
   begin
      case Current_Token is
         when Tok_Entity =>
            Res := Create_Iir (Iir_Kind_Entity_Aspect_Entity);
            Set_Location (Res);
            Scan_Expect (Tok_Identifier);
            Set_Entity_Name (Res, Parse_Name (False));
            if Current_Token = Tok_Left_Paren then
               Scan_Expect (Tok_Identifier);
               Set_Architecture (Res, Current_Text);
               Scan_Expect (Tok_Right_Paren);
               Scan;
            end if;
         when Tok_Configuration =>
            Res := Create_Iir (Iir_Kind_Entity_Aspect_Configuration);
            Set_Location (Res);
            Scan_Expect (Tok_Identifier);
            Set_Configuration_Name (Res, Parse_Name (False));
         when Tok_Open =>
            Res := Create_Iir (Iir_Kind_Entity_Aspect_Open);
            Set_Location (Res);
            Scan;
         when others =>
            --  FIXME: if the token is an identifier, try as if the 'entity'
            --  keyword is missing.
            Error_Msg_Parse
              ("'entity', 'configuration' or 'open' keyword expected");
      end case;
      return Res;
   end Parse_Entity_Aspect;

   --  precond : next token
   --  postcond: next token
   --
   --  [ §5.2.1 ]
   --  binding_indication ::=
   --      [ USE entity_aspect ]
   --      [ generic_map_aspect ]
   --      [ port_map_aspect ]
   function Parse_Binding_Indication return Iir_Binding_Indication
   is
      Res : Iir_Binding_Indication;
   begin
      case Current_Token is
         when Tok_Use
           | Tok_Generic
           | Tok_Port =>
            null;
         when others =>
            return Null_Iir;
      end case;
      Res := Create_Iir (Iir_Kind_Binding_Indication);
      Set_Location (Res);
      if Current_Token = Tok_Use then
         Scan;
         Set_Entity_Aspect (Res, Parse_Entity_Aspect);
      end if;
      if Current_Token = Tok_Generic then
         Set_Generic_Map_Aspect_Chain (Res, Parse_Generic_Map_Aspect);
      end if;
      if Current_Token = Tok_Port then
         Set_Port_Map_Aspect_Chain (Res, Parse_Port_Map_Aspect);
      end if;
      return Res;
   end Parse_Binding_Indication;

   --  precond : ':' after instantiation_list.
   --  postcond: ';'
   --
   --  [ §1.3.2 ]
   --  component_configuration ::=
   --      FOR component_specification
   --          [ binding_indication ; ]
   --          [ block_configuration ]
   --      END FOR ;
   function Parse_Component_Configuration (Loc : Location_Type;
                                           Inst_List : Iir_List)
     return Iir_Component_Configuration
   is
      Res : Iir_Component_Configuration;
   begin
      Res := Create_Iir (Iir_Kind_Component_Configuration);
      Set_Location (Res, Loc);

      --  Component specification.
      Set_Instantiation_List (Res, Inst_List);
      Expect (Tok_Colon);
      Scan_Expect (Tok_Identifier);
      Set_Component_Name (Res, Parse_Name);

      case Current_Token is
         when Tok_Use
           | Tok_Generic
           | Tok_Port =>
            Set_Binding_Indication (Res, Parse_Binding_Indication);
            Scan_Semi_Colon ("binding indication");
         when others =>
            null;
      end case;
      if Current_Token = Tok_For then
         Set_Block_Configuration (Res, Parse_Block_Configuration);
         --  Eat ';'.
         Scan;
      end if;
      Expect (Tok_End);
      Scan_Expect (Tok_For);
      Scan_Expect (Tok_Semi_Colon);
      return Res;
   end Parse_Component_Configuration;

   --  precond : FOR
   --  postcond: ';'
   --
   --  [ 1.3.1 ]
   --  block_configuration ::=
   --      FOR block_specification
   --          { use_clause }
   --          { configuration_item }
   --      END FOR ;
   --
   --  [ 1.3.1 ]
   --  block_specification ::=
   --      ARCHITECTURE_name
   --    | BLOCK_STATEMENT_label
   --    | GENERATE_STATEMENT_label [ ( index_specification ) ]
   function Parse_Block_Configuration_Suffix (Loc : Location_Type;
                                              Block_Spec : Iir)
     return Iir
   is
      Res : Iir_Block_Configuration;
   begin
      Res := Create_Iir (Iir_Kind_Block_Configuration);
      Set_Location (Res, Loc);

      Set_Block_Specification (Res, Block_Spec);

      --  Parse use clauses.
      if Current_Token = Tok_Use then
         declare
            Last : Iir;
            use Declaration_Chain_Handling;
         begin
            Build_Init (Last);

            while Current_Token = Tok_Use loop
               Append_Subchain (Last, Res, Parse_Use_Clause);
               --  Eat ';'.
               Scan;
            end loop;
         end;
      end if;

      --  Parse configuration item list
      declare
         use Iir_Chains.Configuration_Item_Chain_Handling;
         Last : Iir;
      begin
         Build_Init (Last);
         while Current_Token /= Tok_End loop
            Append (Last, Res, Parse_Configuration_Item);
            --  Eat ';'.
            Scan;
         end loop;
      end;
      Scan_Expect (Tok_For);
      Scan_Expect (Tok_Semi_Colon);
      return Res;
   end Parse_Block_Configuration_Suffix;

   function Parse_Block_Configuration return Iir_Block_Configuration
   is
      Loc : Location_Type;
   begin
      Loc := Get_Token_Location;
      Expect (Tok_For);

      --  Parse label.
      Scan;
      return Parse_Block_Configuration_Suffix (Loc, Parse_Name);
   end Parse_Block_Configuration;

   --  precond : FOR
   --  postcond: ';'
   --
   --  [ §1.3.1 ]
   --  configuration_item ::= block_configuration
   --                       | component_configuration
   function Parse_Configuration_Item return Iir
   is
      Loc : Location_Type;
      List : Iir_List;
      El : Iir;
   begin
      Loc := Get_Token_Location;
      Expect (Tok_For);
      Scan;

      --  ALL and OTHERS are tokens from an instantiation list.
      --  Thus, the rule is a component_configuration.
      case Current_Token is
         when Tok_All =>
            Scan;
            return Parse_Component_Configuration (Loc, Iir_List_All);
         when Tok_Others =>
            Scan;
            return Parse_Component_Configuration (Loc, Iir_List_Others);
         when Tok_Identifier =>
            El := Current_Text;
            Scan;
            case Current_Token is
               when Tok_Colon =>
                  --  The identifier was a label from an instantiation list.
                  List := Create_Iir_List;
                  Append_Element (List, El);
                  return Parse_Component_Configuration (Loc, List);
               when Tok_Comma =>
                  --  The identifier was a label from an instantiation list.
                  List := Create_Iir_List;
                  Append_Element (List, El);
                  loop
                     Scan_Expect (Tok_Identifier);
                     Append_Element (List, Current_Text);
                     Scan;
                     exit when Current_Token /= Tok_Comma;
                  end loop;
                  return Parse_Component_Configuration (Loc, List);
               when Tok_Left_Paren =>
                  El := Parse_Name_Suffix (El);
                  return Parse_Block_Configuration_Suffix (Loc, El);
               when Tok_Use | Tok_For | Tok_End =>
                  --  Possibilities for a block_configuration.
                  --  FIXME: should use 'when others' ?
                  return Parse_Block_Configuration_Suffix (Loc, El);
               when others =>
                  Error_Msg_Parse
                    ("block_configuration or component_configuration "
                     & "expected");
                  raise Parse_Error;
            end case;
         when others =>
            Error_Msg_Parse ("configuration item expected");
            raise Parse_Error;
      end case;
   end Parse_Configuration_Item;

   --  precond : next token
   --  postcond: next token
   --
   --  [§ 1.3]
   --  configuration_declarative_part ::= { configuration_declarative_item }
   --
   --  [§ 1.3]
   --  configuration_declarative_item ::= use_clause
   --                                   | attribute_specification
   --                                   | group_declaration
   --  FIXME: attribute_specification, group_declaration
   procedure Parse_Configuration_Declarative_Part (Parent : Iir)
   is
      use Declaration_Chain_Handling;
      Last : Iir;
      El : Iir;
   begin
      Build_Init (Last);
      loop
         case Current_Token is
            when Tok_Invalid =>
               raise Internal_Error;
            when Tok_Use =>
               Append_Subchain (Last, Parent, Parse_Use_Clause);
            when Tok_Attribute =>
               El := Parse_Attribute;
               if El /= Null_Iir then
                  if Get_Kind (El) /= Iir_Kind_Attribute_Specification then
                     Error_Msg_Parse
                       ("attribute declaration not allowed here");
                  end if;
                  Append (Last, Parent, El);
               end if;
            when Tok_Group =>
               El := Parse_Group;
               if El /= Null_Iir then
                  if Get_Kind (El) /= Iir_Kind_Group_Declaration then
                     Error_Msg_Parse
                       ("group template declaration not allowed here");
                  end if;
                  Append (Last, Parent, El);
               end if;
            when others =>
               exit;
         end case;
         Scan;
      end loop;
   end Parse_Configuration_Declarative_Part;

   --  precond : CONFIGURATION
   --  postcond: ';'
   --
   --  [ LRM93 1.3 ]
   --  configuration_declaration ::=
   --      CONFIGURATION identifier OF ENTITY_name IS
   --          configuration_declarative_part
   --          block_configuration
   --      END [ CONFIGURATION ] [ CONFIGURATION_simple_name ] ;
   --
   --  [ LRM93 1.3 ]
   --  configuration_declarative_part ::= { configuration_declarative_item }
   procedure Parse_Configuration_Declaration (Unit : Iir_Design_Unit)
   is
      Res : Iir_Configuration_Declaration;
   begin
      if Current_Token /= Tok_Configuration then
         raise Program_Error;
      end if;
      Res := Create_Iir (Iir_Kind_Configuration_Declaration);

      -- Get identifier.
      Scan_Expect (Tok_Identifier);
      Set_Identifier (Res, Current_Identifier);
      Set_Location (Res);

      --  Skip identifier.
      Scan_Expect (Tok_Of);

      --  Skip 'of'.
      Scan;

      Set_Entity_Name (Res, Parse_Name (False));

      --  Skip 'is'.
      Expect (Tok_Is);
      Scan;

      Parse_Configuration_Declarative_Part (Res);

      Set_Block_Configuration (Res, Parse_Block_Configuration);

      Scan_Expect (Tok_End);
      Set_End_Location (Unit);

      --  Skip 'end'.
      Scan;

      if Current_Token = Tok_Configuration then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse
              ("'configuration' keyword not allowed here by vhdl 87");
         end if;
         Set_End_Has_Reserved_Id (Res, True);

         --  Skip 'configuration'.
         Scan;
      end if;

      -- LRM93 1.3
      -- If a simple name appears at the end of a configuration declaration, it
      -- must repeat the identifier of the configuration declaration.
      Check_End_Name (Res);
      Expect (Tok_Semi_Colon);
      Set_Library_Unit (Unit, Res);
   end Parse_Configuration_Declaration;

   --  precond : generic
   --  postcond: next token
   --
   --  LRM08 4.7
   --  package_header ::=
   --      [ generic_clause               -- LRM08 6.5.6.2
   --      [ generic_map aspect ; ] ]
   function Parse_Package_Header return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Package_Header);
      Parse_Generic_Clause (Res);

      if Current_Token = Tok_Generic then
         Set_Generic_Map_Aspect_Chain (Res, Parse_Generic_Map_Aspect);
         Scan_Semi_Colon ("generic map aspect");
      end if;
      return Res;
   end Parse_Package_Header;

   --  precond : token (after 'IS')
   --  postcond: ';'
   --
   --  [ LRM93 2.5, LRM08 4.7 ]
   --  package_declaration ::=
   --      PACKAGE identifier IS
   --          package_header           -- LRM08
   --          package_declarative_part
   --      END [ PACKAGE ] [ PACKAGE_simple_name ] ;
   procedure Parse_Package_Declaration
     (Unit : Iir_Design_Unit; Id : Name_Id; Loc : Location_Type)
   is
      Res: Iir_Package_Declaration;
   begin
      Res := Create_Iir (Iir_Kind_Package_Declaration);
      Set_Location (Res, Loc);
      Set_Identifier (Res, Id);

      if Current_Token = Tok_Generic then
         if Vhdl_Std < Vhdl_08 then
            Error_Msg_Parse ("generic packages not allowed before vhdl 2008");
         end if;
         Set_Package_Header (Res, Parse_Package_Header);
      end if;

      Parse_Declarative_Part (Res);

      Expect (Tok_End);
      Set_End_Location (Unit);

      --  Skip 'end'
      Scan;

      if Current_Token = Tok_Package then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse ("'package' keyword not allowed here by vhdl 87");
         end if;
         Set_End_Has_Reserved_Id (Res, True);

         --  Skip 'package'.
         Scan;
      end if;

      Check_End_Name (Res);
      Expect (Tok_Semi_Colon);
      Set_Library_Unit (Unit, Res);
   end Parse_Package_Declaration;

   --  precond : BODY
   --  postcond: ';'
   --
   --  [ LRM93 2.6, LRM08 4.8 ]
   --  package_body ::=
   --      PACKAGE BODY PACKAGE_simple_name IS
   --          package_body_declarative_part
   --      END [ PACKAGE BODY ] [ PACKAGE_simple_name ] ;
   procedure Parse_Package_Body (Unit : Iir_Design_Unit)
   is
      Res: Iir;
   begin
      Res := Create_Iir (Iir_Kind_Package_Body);
      Set_Location (Res);

      -- Get identifier.
      Expect (Tok_Identifier);
      Set_Identifier (Res, Current_Identifier);
      Scan_Expect (Tok_Is);
      Scan;

      Parse_Declarative_Part (Res);

      Expect (Tok_End);
      Set_End_Location (Unit);

      --  Skip 'end'
      Scan;

      if Current_Token = Tok_Package then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse ("'package' keyword not allowed here by vhdl 87");
         end if;
         Set_End_Has_Reserved_Id (Res, True);

         --  Skip 'package'
         Scan;

         if Current_Token /= Tok_Body then
            Error_Msg_Parse ("missing 'body' after 'package'");
         else
            --  Skip 'body'
            Scan;
         end if;
      end if;

      Check_End_Name (Res);
      Expect (Tok_Semi_Colon);
      Set_Library_Unit (Unit, Res);
   end Parse_Package_Body;

   --  precond : NEW
   --  postcond: ';'
   --
   --  [ LRM08 4.9 ]
   --  package_instantiation_declaration ::=
   --      PACKAGE identifier IS NEW uninstantiated_package_name
   --         [ generic_map_aspect ] ;
   function Parse_Package_Instantiation_Declaration
     (Id : Name_Id; Loc : Location_Type)
     return Iir
   is
      Res: Iir;
   begin
      Res := Create_Iir (Iir_Kind_Package_Instantiation_Declaration);
      Set_Location (Res, Loc);
      Set_Identifier (Res, Id);

      --  Skip 'new'
      Scan;

      Set_Uninstantiated_Package_Name (Res, Parse_Name (False));

      if Current_Token = Tok_Generic then
         Set_Generic_Map_Aspect_Chain (Res, Parse_Generic_Map_Aspect);
      end if;

      Expect (Tok_Semi_Colon);

      return Res;
   end Parse_Package_Instantiation_Declaration;

   --  precond : PACKAGE
   --  postcond: ';'
   --
   --    package_declaration
   --  | package_body
   --  | package_instantiation_declaration
   procedure Parse_Package (Unit : Iir_Design_Unit)
   is
      Loc : Location_Type;
      Id : Name_Id;
   begin
      --  Skip 'package'
      Scan;

      if Current_Token = Tok_Body then
         --  Skip 'body'
         Scan;

         Parse_Package_Body (Unit);
      else
         Expect (Tok_Identifier);
         Id := Current_Identifier;
         Loc := Get_Token_Location;

         --  Skip identifier.
         Scan;

         --  Skip 'is'.
         Expect (Tok_Is);
         Scan;

         if Current_Token = Tok_New then
            Set_Library_Unit
              (Unit,
               Parse_Package_Instantiation_Declaration (Id, Loc));
            --  Note: there is no 'end' in instantiation.
            Set_End_Location (Unit, Get_Token_Location);
         else
            Parse_Package_Declaration (Unit, Id, Loc);
         end if;
      end if;
   end Parse_Package;

   procedure Parse_Context_Declaration_Or_Reference
     (Unit : Iir_Design_Unit; Clause : out Iir);

   --  Precond:  next token
   --  Postcond: next token
   --
   --  LRM93 11.3
   --  LRM08 13.4 Context clauses
   --  context_clause ::= { context_item }
   --
   --  context_item ::= library_clause | use_clause | context_reference
   procedure Parse_Context_Clause (Unit : Iir)
   is
      use Context_Items_Chain_Handling;
      Last : Iir;
      Els : Iir;
   begin
      Build_Init (Last);

      loop
         case Current_Token is
            when Tok_Library =>
               Els := Parse_Library_Clause;
            when Tok_Use =>
               Els := Parse_Use_Clause;
               Scan;
            when Tok_Context =>
               Parse_Context_Declaration_Or_Reference (Unit, Els);
               if Els = Null_Iir then
                  --  This was a context declaration.  No more clause.

                  --  LRM08 13.1 Design units
                  --  It is an error if the context clause preceding a library
                  --  unit that is a context declaration is not empty.
                  if Get_Context_Items (Unit) /= Null_Iir then
                     Error_Msg_Sem
                       ("context declaration does not allow context "
                          & "clauses before it", Get_Context_Items (Unit));
                  end if;

                  return;
               else
                  --  Skip ';'.
                  Scan;
               end if;
            when Tok_With =>
               --  Be Ada friendly.
               Error_Msg_Parse ("'with' not allowed in context clause "
                                  & "(try 'use' or 'library')");
               Els := Parse_Use_Clause;
               Scan;
            when others =>
               exit;
         end case;
         Append_Subchain (Last, Unit, Els);
      end loop;
   end Parse_Context_Clause;

   --  Precond:  IS
   --
   --  LRM08 13.13 Context declarations
   --  context_declaration ::=
   --    CONTEXT identifier IS
   --       context_clause
   --    END [ CONTEXT ] [ /context/_simple_name ] ;
   procedure Parse_Context_Declaration (Unit : Iir; Decl : Iir) is
   begin
      Set_Library_Unit (Unit, Decl);

      --  Skip 'is'
      Scan;

      Parse_Context_Clause (Decl);

      Expect (Tok_End);
      Set_End_Location (Unit);

      --  Skip 'end'
      Scan;

      if Current_Token = Tok_Context then
         Set_End_Has_Reserved_Id (Decl, True);

         --  Skip 'context'.
         Scan;
      end if;

      Check_End_Name (Decl);
      Expect (Tok_Semi_Colon);
   end Parse_Context_Declaration;

   --  Precond:  next token after selected_name.
   --  Postcond: ;
   --
   --  LRM08 13.4 Context clauses
   --
   --  context_reference ::=
   --     CONTEXT selected_name { , selected_name }
   function Parse_Context_Reference
     (Loc : Location_Type; Name : Iir) return Iir
   is
      Ref : Iir;
      First, Last : Iir;
   begin
      Ref := Create_Iir (Iir_Kind_Context_Reference);
      Set_Location (Ref, Loc);
      Set_Selected_Name (Ref, Name);
      First := Ref;
      Last := Ref;

      loop
         exit when Current_Token = Tok_Semi_Colon;
         Expect (Tok_Comma);

         --  Skip ','.
         Scan;

         Ref := Create_Iir (Iir_Kind_Context_Reference);
         Set_Location (Ref, Loc);
         Set_Selected_Name (Ref, Parse_Name);

         Set_Context_Reference_Chain (Last, Ref);
         Last := Ref;
      end loop;

      return First;
   end Parse_Context_Reference;

   --  Precond:  CONTEXT
   --
   procedure Parse_Context_Declaration_Or_Reference
     (Unit : Iir_Design_Unit; Clause : out Iir)
   is
      Loc : Location_Type;
      Name : Iir;
      Res : Iir;
   begin
      Loc := Get_Token_Location;

      --  Skip 'context'.
      Scan;

      Name := Parse_Name;

      if Current_Token = Tok_Is then
         Res := Create_Iir (Iir_Kind_Context_Declaration);
         Set_Location (Res, Loc);
         if Get_Kind (Name) = Iir_Kind_Simple_Name then
            Set_Identifier (Res, Get_Identifier (Name));
         else
            Error_Msg_Parse ("identifier for context expected", Name);
         end if;
         Free_Iir (Name);

         Parse_Context_Declaration (Unit, Res);
         Clause := Null_Iir;
      else
         Clause := Parse_Context_Reference (Loc, Name);
      end if;
   end Parse_Context_Declaration_Or_Reference;

   -- Parse a design_unit.
   -- The lexical scanner must have been initialized, but without a
   -- current_token.
   --
   --  [ §11.1 ]
   --  design_unit ::= context_clause library_unit
   function Parse_Design_Unit return Iir_Design_Unit
   is
      Res: Iir_Design_Unit;
      Unit: Iir;
   begin
      -- Internal check: there must be no current_token.
      if Current_Token /= Tok_Invalid then
         raise Internal_Error;
      end if;
      Scan;
      if Current_Token = Tok_Eof then
         return Null_Iir;
      end if;

      -- Create the design unit node.
      Res := Create_Iir (Iir_Kind_Design_Unit);
      Set_Location (Res);
      Set_Date_State (Res, Date_Extern);

      Parse_Context_Clause (Res);

      if Get_Library_Unit (Res) = Null_Iir then
         --  Parse library unit.  Context declaration are already parsed.
         case Current_Token is
            when Tok_Entity =>
               Parse_Entity_Declaration (Res);
            when Tok_Architecture =>
               Parse_Architecture_Body (Res);
            when Tok_Package =>
               Parse_Package (Res);
            when Tok_Configuration =>
               Parse_Configuration_Declaration (Res);
            when others =>
               Error_Msg_Parse
                 ("entity, architecture, package or configuration "
                    & "keyword expected");
               return Null_Iir;
         end case;
      end if;

      Unit := Get_Library_Unit (Res);
      Set_Design_Unit (Unit, Res);
      Set_Identifier (Res, Get_Identifier (Unit));
      Set_Date (Res, Date_Parsed);
      Invalidate_Current_Token;
      return Res;
   exception
      when Expect_Error =>
         raise Compilation_Error;
   end Parse_Design_Unit;

   --  [ §11.1 ]
   --  design_file ::= design_unit { design_unit }
   function Parse_Design_File return Iir_Design_File
   is
      Res : Iir_Design_File;
      Design, Last_Design : Iir_Design_Unit;
   begin
      Res := Create_Iir (Iir_Kind_Design_File);
      Set_Location (Res);

      Last_Design := Null_Iir;
      loop
         Design := Parse.Parse_Design_Unit;
         exit when Design = Null_Iir;
         Set_Design_File (Design, Res);

         --  Append unit to the design file.
         if Last_Design = Null_Iir then
            Set_First_Design_Unit (Res, Design);
         else
            Set_Chain (Last_Design, Design);
         end if;
         Last_Design := Design;
         Set_Last_Design_Unit (Res, Last_Design);
      end loop;

      if Last_Design = Null_Iir then
         Error_Msg_Parse ("design file is empty (no design unit found)");
      end if;

      return Res;
   exception
      when Parse_Error =>
         return Null_Iir;
   end Parse_Design_File;
end Parse;
