--  VHDL parser.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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

with Std_Names; use Std_Names;
with Flags; use Flags;
with Str_Table;
with Errorout; use Errorout;
with File_Comments; use File_Comments;

with Vhdl.Nodes_Utils; use Vhdl.Nodes_Utils;
with Vhdl.Tokens; use Vhdl.Tokens;
with Vhdl.Scanner; use Vhdl.Scanner;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Parse_Psl;
with Vhdl.Xrefs;
with Vhdl.Elocations; use Vhdl.Elocations;
with Vhdl.Comments; use Vhdl.Comments;

with PSL.Types; use PSL.Types;

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

package body Vhdl.Parse is

   -- current_token must be valid.
   -- Leaves a token.
   function Parse_Primary return Iir_Expression;
   function Parse_Use_Clause return Iir_Use_Clause;

   function Parse_Association_List return Iir;
   function Parse_Association_List_In_Parenthesis return Iir;

   function Parse_Sequential_Statements (Parent : Iir) return Iir;
   function Parse_Configuration_Item return Iir;
   function Parse_Block_Configuration return Iir_Block_Configuration;
   procedure Parse_Concurrent_Statements (Parent : Iir);
   function Parse_Subprogram_Declaration return Iir;
   function Parse_Subtype_Indication (Name : Iir := Null_Iir) return Iir;
   function Parse_Subnature_Indication return Iir;
   function Parse_Interface_List (Ctxt : Interface_Kind_Type; Parent : Iir)
                                 return Iir;
   procedure Parse_Component_Specification (Res : Iir);
   function Parse_Binding_Indication return Iir_Binding_Indication;
   function Parse_Aggregate return Iir;
   function Parse_Signature return Iir_Signature;
   procedure Parse_Declarative_Part (Parent : Iir; Package_Parent : Iir);
   function Parse_Tolerance_Aspect_Opt return Iir;
   function Parse_Package (Parent : Iir) return Iir;

   function Parse_Simultaneous_If_Statement (Label : Name_Id;
                                             Label_Loc : Location_Type;
                                             If_Loc : Location_Type;
                                             First_Cond : Iir) return Iir;
   function Parse_Simultaneous_Case_Statement
     (Label : Name_Id; Loc : Location_Type; Expr : Iir) return Iir;
   function Parse_Generic_Map_Aspect return Iir;

   --  Maximum number of nested parenthesis, before generating an error.
   Max_Parenthesis_Depth : constant Natural := 1000;

   --  Current number of open parenthesis (in expressions).
   Parenthesis_Depth : Natural := 0;

   -- Missing parenthesis has been reported already. Flag used to
   -- remember only first report
   Parenthesis_Reported : Boolean := False;

   -- Copy the current location into an iir.
   procedure Set_Location (Node : Iir) is
   begin
      Set_Location (Node, Get_Token_Location);
   end Set_Location;

   -- Disp a message during parse
   -- The location of the current token is automatically displayed before
   -- the message.
   procedure Error_Msg_Parse (Msg: String; Arg1 : Earg_Type) is
   begin
      Report_Msg (Msgid_Error, Errorout.Parse, Get_Token_Coord,
                  Msg, (1 => Arg1));
   end Error_Msg_Parse;

   procedure Error_Msg_Parse (Msg: String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Errorout.Parse, Get_Token_Coord, Msg, Args);
   end Error_Msg_Parse;

   procedure Check_Vhdl_At_Least_2008 (Msg: String) is
   begin
      if Vhdl_Std < Vhdl_08 then
         Report_Msg (Msgid_Error, Errorout.Parse, Get_Token_Coord, Msg &
                     " not allowed before VHDL 2008. Compile with --std=08");
      end if;
   end Check_Vhdl_At_Least_2008;

   procedure Error_Msg_Parse (Loc : Location_Type;
                              Msg: String;
                              Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Errorout.Parse, +Loc, Msg, Args);
   end Error_Msg_Parse;

   procedure Unexpected (Where: String) is
   begin
      Error_Msg_Parse ("unexpected token %t in a " & Where, +Current_Token);
   end Unexpected;

   procedure Expect_Error (Token: Token_Type; Msg: String := "")
   is
      Loc : Location_Type;
   begin
      case Token is
         when Tok_Semi_Colon
            | Tok_Right_Paren
            | Tok_Comma =>
            --  Improve the location of the error: point just after the last
            --  token so that new lines don't have a bad effect.
            Loc := Get_Prev_Location;
         when others =>
            Loc := Get_Token_Location;
      end case;

      if Msg'Length > 0 then
         Report_Start_Group;
         Error_Msg_Parse (Loc, Msg, Args => No_Eargs);
         Error_Msg_Parse (Loc, "(found: %t)", (1 => +Current_Token));
         Report_End_Group;
      elsif Current_Token = Tok_Identifier then
         Error_Msg_Parse (Loc, "%t is expected instead of %i",
                          (+Token, +Current_Identifier));
      else
         Error_Msg_Parse
           (Loc, "%t is expected instead of %t", (+Token, +Current_Token));
      end if;
   end Expect_Error;

   --  Emit an error if the current_token if different from TOKEN.
   --  Otherwise, accept the current_token (ie set it to tok_invalid, unless
   --  TOKEN is Tok_Identifier).
   procedure Expect (Token: Token_Type; Msg: String := "") is
   begin
      if Current_Token /= Token then
         Expect_Error (Token, Msg);
      end if;
   end Expect;

   procedure Expect_Scan (Token: Token_Type; Msg: String := "") is
   begin
      if Current_Token = Token then
         --  Skip token.
         Scan;
      else
         Expect_Error (Token, Msg);
      end if;
   end Expect_Scan;

   --  Expect the identifier for node RES.
   procedure Scan_Identifier (Res : Iir) is
   begin
      Set_Location (Res);
      if Current_Token = Tok_Identifier then
         Set_Identifier (Res, Current_Identifier);

         --  Skip identifier.
         Scan;
      else
         Expect (Tok_Identifier);
      end if;
   end Scan_Identifier;

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
            Error_Msg_Parse ("misspelling, %i expected", +Name);
         else
            Set_End_Has_Identifier (Decl, True);
            Xrefs.Xref_End (Get_Token_Location, Decl);
         end if;
      end if;

      --  Skip identifier.
      Scan;
   end Check_End_Name;

   procedure Check_End_Name (Decl : Iir) is
   begin
      Check_End_Name (Get_Identifier (Decl), Decl);
   end Check_End_Name;

   --  Skip the reserved identifier after 'end'.
   procedure Scan_End_Token (Tok : Token_Type; Decl : Iir) is
   begin
      if Current_Token /= Tok then
         Error_Msg_Parse ("""end"" must be followed by %t", +Tok);
         case Current_Token is
            when Tok_If
              | Tok_Loop
              | Tok_Case
              | Tok_Process =>
               --  Mismatching token.
               Scan;
            when others =>
               null;
         end case;
      else
         Set_End_Has_Reserved_Id (Decl, True);

         --  Skip tok.
         Scan;
      end if;
   end Scan_End_Token;

   --  Expect ' END tok [ name ] ; '
   procedure Check_End_Name (Tok : Token_Type; Decl : Iir) is
   begin
      if Current_Token /= Tok_End then
         Error_Msg_Parse ("""end " & Image (Tok) & ";"" expected");
      else
         --  Skip 'end'.
         Scan;

         Scan_End_Token (Tok, Decl);

         Check_End_Name (Decl);
      end if;
   end Check_End_Name;

   procedure Skip_Until_Semi_Colon is
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
   end Skip_Until_Semi_Colon;

   procedure Resync_To_End_Of_Statement is
   begin
      loop
         case Current_Token is
            when Tok_Eof
              | Tok_Semi_Colon
              | Tok_End =>
               exit;
            when Tok_If
              | Tok_Else
              | Tok_Case
              | Tok_For
              | Tok_While
              | Tok_Loop
              | Tok_Wait
              | Tok_Assert =>
               --  Sequential statement.
               exit;
            when Tok_Process
              | Tok_Block =>
               --  Concurrent statement.
               exit;
            when others =>
               Scan;
         end case;
      end loop;
   end Resync_To_End_Of_Statement;

   procedure Resync_To_End_Of_Declaration is
   begin
      loop
         case Current_Token is
            when Tok_Eof =>
               exit;
            when Tok_Semi_Colon =>
               Scan;
               exit;
            when Tok_End
              | Tok_Begin =>
               --  End of current block.
               exit;
            when Tok_Signal
              | Tok_Variable
              | Tok_Constant
              | Tok_File
              | Tok_Alias
              | Tok_Type
              | Tok_Subtype
              | Tok_Use
              | Tok_Component
              | Tok_Attribute
              | Tok_Group
              | Tok_For
              | Tok_Disconnect
              | Tok_Shared
              | Tok_Impure
              | Tok_Pure
              | Tok_Function
              | Tok_Procedure
              | Tok_Package =>
               --  Start of a new declaration
               exit;
            when others =>
               --  Eat.
               Scan;
         end case;
      end loop;
   end Resync_To_End_Of_Declaration;

   procedure Resync_To_Next_Unit is
   begin
      --  Resync.
      loop
         case Current_Token is
            when Tok_Eof =>
               exit;
            when Tok_Semi_Colon =>
               --  Skip ';'.
               Scan;
               exit;
            when Tok_Library
              | Tok_Use
              | Tok_Architecture
              | Tok_Entity
              | Tok_Package
              | Tok_Configuration
              | Tok_Context =>
               --  Possible start of a new unit.
               exit;
            when Tok_End =>
               Skip_Until_Semi_Colon;
            when others =>
               Scan;
         end case;
      end loop;
   end Resync_To_Next_Unit;

   procedure Skip_Until_Closing_Parenthesis
   is
      Level : Natural;
   begin
      Level := 0;

      --  Skip '('.
      Scan;

      loop
         case Current_Token is
            when Tok_Right_Paren =>
               if Level = 0 then
                  --  Skip ')'.
                  Scan;
                  exit;
               end if;
               Level := Level - 1;
            when Tok_Left_Paren =>
               Level := Level + 1;
            when Tok_Eof
              | Tok_Semi_Colon
              | Tok_End
              | Tok_Then
              | Tok_Else
              | Tok_Loop =>
               exit;
            when others =>
               null;
         end case;

         Scan;
      end loop;
   end Skip_Until_Closing_Parenthesis;

   --  Return True if at the end of the list, False if there is another
   --  interface.
   function Resync_To_End_Of_Interface return Boolean
   is
      Nested : Natural;
   begin
      Nested := 0;
      loop
         case Current_Token is
            when Tok_End
              | Tok_Port
              | Tok_Is
              | Tok_Begin
              | Tok_Eof =>
               --  Certainly comes after interface list.
               return True;
            when Tok_Left_Paren =>
               Nested := Nested + 1;
            when Tok_Right_Paren =>
               if Nested = 0 then
                  --  Skip ')'.
                  Scan;

                  return True;
               end if;
               Nested := Nested - 1;
            when Tok_Semi_Colon =>
               if Nested = 0 then
                  --  Skip ';'.
                  Scan;

                  return False;
               end if;
            when Tok_Signal
              | Tok_Variable
              | Tok_Constant
              | Tok_File
              | Tok_Function
              | Tok_Procedure
              | Tok_Type
              | Tok_Package =>
               --  Next interface ?
               return False;
            when Tok_Colon
              | Tok_Identifier
              | Tok_In
              | Tok_Out
              | Tok_Inout
              | Tok_Buffer
              | Tok_Linkage =>
               --  Certainly part of an interface.
               null;
            when others =>
               null;
         end case;

         --  Skip token.
         Scan;
      end loop;
   end Resync_To_End_Of_Interface;

   -- Search for next colon (likely before subtype_indication).
   -- Others detect when we are totally lost. Semi-colon likely
   -- at the end of line, Double_Greater at the end of external
   -- name.
   procedure Resync_To_End_Of_External_Name is
   begin
      loop
         case Current_Token is
            when Tok_Colon
               | Tok_Semi_Colon
               | Tok_Begin
               | Tok_Eof
               | Tok_Double_Greater =>
               exit;
            when others =>
               Scan;
         end case;
      end loop;
   end Resync_To_End_Of_External_Name;

   procedure Error_Missing_Semi_Colon (Msg : String) is
   begin
      Error_Msg_Parse (Get_Prev_Location, "missing "";"" at end of " & Msg);
   end Error_Missing_Semi_Colon;

   procedure Error_Variable_Location (Kind : Iir_Kind)
   is
      Prefix : constant String := "non-";
      Common : constant String := "shared variable declaration not allowed ";
   begin
      case Kind is

         -- Non-Shared variables
         when Iir_Kind_Entity_Declaration =>
            Error_Msg_Parse (Prefix & Common & "in entity declaration");
         when Iir_Kind_Architecture_Body =>
            Error_Msg_Parse (Prefix & Common & "in architecture body");
         when Iir_Kind_Block_Statement =>
            Error_Msg_Parse (Prefix & Common & "in block statement");
         when Iir_Kind_Generate_Statement_Body =>
            Error_Msg_Parse (Prefix & Common & "in generate statement body");
         when Iir_Kind_Package_Declaration =>
            Error_Msg_Parse (Prefix & Common & "in package declaration");
         when Iir_Kind_Package_Body =>
            Error_Msg_Parse (Prefix & Common & "in entity body");
         when Iir_Kind_Protected_Type_Declaration =>
            Error_Msg_Parse
              (Prefix & Common & "in protected type declaration");

         -- Shared variables
         when Iir_Kind_Function_Body =>
            Error_Msg_Parse (Common & "in function body");
         when Iir_Kinds_Process_Statement =>
            Error_Msg_Parse (Common & "in process statement");
         when Iir_Kind_Protected_Type_Body =>
            Error_Msg_Parse (Common & "in protected type body");
         when Iir_Kind_Simultaneous_Procedural_Statement =>
            Error_Msg_Parse (Common & "in procedural statement");

         when others =>
            Error_Msg_Parse (Prefix & Common & "here");
      end case;
   end Error_Variable_Location;

   procedure Error_Missing_Parenthesis(Loc : Location_Type) is
   begin
      if not Parenthesis_Reported then
         if Parenthesis_Depth > 1 then
            Error_Msg_Parse
            ("missing ')' for opening parenthesis at %l. " &
               "Total missing parenthesis: " &
               Integer'Image(Parenthesis_Depth), +Loc);
            Parenthesis_Reported := True;
         else
            Error_Msg_Parse
            ("missing ')' for opening parenthesis at %l. " , +Loc);
         end if;
      end if;
      if Parenthesis_Depth = 1 then
         Parenthesis_Reported := False;
      end if;
   end Error_Missing_Parenthesis;

   --  Expect and scan ';' emit an error message using MSG if not present.
   procedure Scan_Semi_Colon (Msg : String) is
   begin
      if Current_Token /= Tok_Semi_Colon then
         Error_Missing_Semi_Colon (Msg);
      else
         Scan;
      end if;
   end Scan_Semi_Colon;

   procedure Scan_Semi_Colon_Declaration (Msg : String) is
   begin
      if Current_Token = Tok_Semi_Colon then
         --  Skip ';'.
         Scan;
      else
         Error_Missing_Semi_Colon (Msg);

         Resync_To_End_Of_Declaration;
      end if;
   end Scan_Semi_Colon_Declaration;

   procedure Scan_Semi_Colon_Unit (Msg : String) is
   begin
      if Current_Token = Tok_Semi_Colon then
         --  Skip ';'.
         Scan;
      else
         Error_Missing_Semi_Colon (Msg);
         Resync_To_Next_Unit;
      end if;
   end Scan_Semi_Colon_Unit;

   function Create_Error_Node (Orig : Iir := Null_Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Error (Orig);
      if Orig = Null_Iir then
         Set_Location (Res);
      end if;
      return Res;
   end Create_Error_Node;

   --  precond : next token
   --  postcond: next token.
   --
   --  [ LRM93 4.3.2 ]
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
               Error_Msg_Parse
                 ("typo error, 'in out' must be 'inout' in vhdl");
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
            --  Cannot happen.
            raise Internal_Error;
      end case;
   end Parse_Mode;

   --  precond : next token
   --  postcond: next token
   --
   --  [ LRM93 4.3.1.2 ]
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

   --  precond : TO, DOWNTO
   --  postcond: next token
   --
   -- Parse a range.
   -- If LEFT is not null_iir, then it must be an expression corresponding to
   -- the left limit of the range, and the current_token must be either
   -- tok_to or tok_downto.
   -- If left is null_iir, the current token is used to create the left limit
   -- expression.
   --
   --  [ LRM93 3.1 ]
   --  range_constraint ::= RANGE range
   --
   --  [ LRM93 3.1 ]
   --  range ::= RANGE_attribute_name
   --         | simple_expression direction simple_expression
   --
   --  direction ::= TO | DOWNTO
   function Parse_Range_Expression (Left : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Range_Expression);

      if Left /= Null_Iir then
         Set_Left_Limit_Expr (Res, Left);
         Location_Copy (Res, Left);
      end if;

      case Current_Token is
         when Tok_To =>
            Set_Direction (Res, Dir_To);
         when Tok_Downto =>
            Set_Direction (Res, Dir_Downto);
         when others =>
            raise Internal_Error;
      end case;

      --  Skip 'to' or 'downto'.
      Scan;

      Set_Right_Limit_Expr (Res, Parse_Expression (Prio_Simple));
      return Res;
   end Parse_Range_Expression;

   --  precond:  next token
   --  postcond: next token
   function Parse_Range return Iir
   is
      Left: Iir;
   begin
      Left := Parse_Expression (Prio_Simple);

      case Current_Token is
         when Tok_To
           | Tok_Downto =>
            return Parse_Range_Expression (Left);
         when others =>
            if Left /= Null_Iir then
               if Is_Range_Attribute_Name (Left) then
                  return Left;
               end if;
               Error_Msg_Parse ("'to' or 'downto' expected");
            end if;
            return Create_Error_Node (Left);
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
      if Type_Mark /= Null_Iir then
         Location_Copy (Def, Type_Mark);
         Set_Subtype_Type_Mark (Def, Type_Mark);
      else
         Set_Location (Def);
      end if;
      Set_Range_Constraint (Def, Parse_Range_Constraint);
      Set_Resolution_Indication (Def, Resolution_Indication);
      Set_Tolerance (Def, Parse_Tolerance_Aspect_Opt);

      return Def;
   end Parse_Range_Constraint_Of_Subtype_Indication;

   --  precond:  next token
   --  postcond: next token
   --
   --  [ LRM93 3.2.1 ]
   --  discrete_range ::= discrete_subtype_indication | range
   function Parse_Discrete_Range return Iir
   is
      Left: Iir;
   begin
      Left := Parse_Expression (Prio_Simple);

      case Current_Token is
         when Tok_To
           | Tok_Downto =>
            return Parse_Range_Expression (Left);
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
         Error_Msg_Parse
           (+Loc, "%s is not an operator symbol", (1 => +((Str_Id, Len))));
      end Bad_Operator_Symbol;

      procedure Check_Vhdl93 is
      begin
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse
              (+Loc, "%s is not a vhdl87 operator symbol",
               (1 => +((Str_Id, Len))));
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

   --  [ LRM93 6.6 ]
   --  attribute_name ::=
   --      prefix [ signature ] ' attribute_designator [ ( expression ) ]
   --
   function Parse_Attribute_Name (Prefix : Iir) return Iir
   is
      Res : Iir;
   begin
      case Current_Token is
         when Tok_Range
            | Tok_Identifier
            | Tok_Stable =>
            --  Tok_Stable is possible within PSL expressions.
            null;
         when Tok_Across
            | Tok_Through
            | Tok_Reference
            | Tok_Tolerance =>
            --  AMS reserved words.
            null;
         when Tok_Subtype =>
            Check_Vhdl_At_Least_2008 ("'subtype attribute");
         when others =>
            return Null_Iir;
      end case;

      Res := Create_Iir (Iir_Kind_Attribute_Name);
      Set_Identifier (Res, Current_Identifier);
      Set_Location (Res);
      if Get_Kind (Prefix) = Iir_Kind_Signature then
         Set_Attribute_Signature (Res, Prefix);

         --  Transfer the prefix from the signature to the attribute.
         Set_Prefix (Res, Get_Signature_Prefix (Prefix));
         Set_Signature_Prefix (Prefix, Null_Iir);
      else
         Set_Prefix (Res, Prefix);
      end if;

      return Res;
   end Parse_Attribute_Name;

   --  precond : next token
   --  postcond: next token
   --
   --  [ LRM93 6.1 ]
   --  name ::= simple_name
   --         | operator_symbol
   --         | selected_name
   --         | indexed_name
   --         | slice_name
   --         | attribute_name
   --
   --  [ LRM93 6.2 ]
   --  simple_name ::= identifier
   --
   --  [ LRM93 6.5 ]
   --  slice_name ::= prefix ( discrete_range )
   --
   --  [ LRM93 6.3 ]
   --  selected_name ::= prefix . suffix
   --
   --  [ LRM93 6.1 ]
   --  prefix ::= name
   --           | function_call
   --
   --  [ LRM93 6.3 ]
   --  suffix ::= simple_name
   --           | character_literal
   --           | operator_symbol
   --           | ALL
   --
   --  [ LRM93 3.2.1 ]
   --  discrete_range ::= DISCRETE_subtype_indication | range
   --
   --  [ LRM93 3.1 ]
   --  range ::= RANGE_attribute_name
   --          | simple_expression direction simple_expression
   --
   --  [ LRM93 3.1 ]
   --  direction ::= TO | DOWNTO
   --
   --  [ LRM93 6.6 ]
   --  attribute_designator ::= ATTRIBUTE_simple_name
   --
   --  Note: in order to simplify the parsing, this function may return a
   --  signature without attribute designator. Signatures may appear at 3
   --  places:
   --  - in attribute name
   --  - in alias declaration
   --  - in entity designator
   function Parse_Name_Suffix (Pfx : Iir;
                               Allow_Indexes: Boolean := True;
                               Allow_Signature : Boolean := False)
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

               --  Skip '''.
               Scan;

               if Current_Token = Tok_Left_Paren then
                  -- A qualified expression.
                  Res := Create_Iir (Iir_Kind_Qualified_Expression);
                  Set_Type_Mark (Res, Prefix);
                  Location_Copy (Res, Prefix);
                  Set_Expression (Res, Parse_Aggregate);
                  return Res;
               else
                  Res := Parse_Attribute_Name (Prefix);
                  if Res = Null_Iir then
                     Error_Msg_Parse ("attribute identifier expected after '");
                     return Create_Error_Node (Prefix);
                  end if;

                  -- accept the identifier.
                  Scan;
               end if;

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

               --  Skip '.'.
               Scan;

               case Current_Token is
                  when Tok_All =>
                     Res := Create_Iir (Iir_Kind_Selected_By_All_Name);
                     Set_Location (Res);
                     Set_Prefix (Res, Prefix);

                     --  Skip 'all'.
                     Scan;

                  when Tok_Identifier
                    | Tok_Character =>
                     Res := Create_Iir (Iir_Kind_Selected_Name);
                     Set_Location (Res);
                     Set_Prefix (Res, Prefix);
                     Set_Identifier (Res, Current_Identifier);

                     --  Skip identifier/character.
                     Scan;

                  when Tok_String =>
                     Res := Create_Iir (Iir_Kind_Selected_Name);
                     Set_Location (Res);
                     Set_Prefix (Res, Prefix);
                     Set_Identifier
                       (Res, Scan_To_Operator_Name (Get_Token_Location));

                     --  Skip string.
                     Scan;

                  when others =>
                     Error_Msg_Parse
                       ("identifier or ""all"" is expected after '.'");
                     Res := Prefix;
               end case;

            when others =>
               if not Allow_Signature
                 and then Get_Kind (Res) = Iir_Kind_Signature
               then
                  --  Not as a name.
                  Error_Msg_Parse ("signature name not expected here");
                  Prefix := Get_Signature_Prefix (Res);
                  Set_Signature_Prefix (Res, Null_Iir);
                  Free_Iir (Res);
                  Res := Prefix;
               end if;
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

            --  Skip '@'.
            Scan;

            if Current_Token /= Tok_Identifier then
               Error_Msg_Parse ("library name expected after '@'");
            else
               Set_Identifier (Res, Current_Identifier);

               --  Skip identifier.
               Scan;
            end if;

            if Current_Token /= Tok_Dot then
               Error_Msg_Parse ("'.' expected after library name");
            else
               --  Skip '.'.
               Scan;
            end if;

         when Tok_Dot =>
            Res := Create_Iir (Iir_Kind_Absolute_Pathname);
            Set_Location (Res);
            Last := Res;

            --  Skip '.'.
            Scan;

         when Tok_Caret =>
            Last := Null_Iir;
            loop
               El := Create_Iir (Iir_Kind_Relative_Pathname);
               Set_Location (El);

               --  Skip '^'.
               Scan;

               if Current_Token /= Tok_Dot then
                  Error_Msg_Parse ("'.' expected after '^'");
               else
                  --  Skip '.'.
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
            Resync_To_End_Of_External_Name;
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

         --  Skip identifier.
         Scan;

         if Current_Token = Tok_Left_Paren then
            --  Skip '('.
            Scan;

            Set_Pathname_Expression (El, Parse_Expression);

            --  Skip ')'.
            Expect_Scan (Tok_Right_Paren);
         end if;

         exit when Current_Token /= Tok_Dot;

         --  Skip '.'.
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

      --  Skip '<<'.
      Scan;

      case Current_Token is
         when Tok_Constant =>
            Kind := Iir_Kind_External_Constant_Name;
            --  Skip 'constant'.
            Scan;
         when Tok_Signal =>
            Kind := Iir_Kind_External_Signal_Name;
            --  Skip 'signal'.
            Scan;
         when Tok_Variable =>
            Kind := Iir_Kind_External_Variable_Name;
            --  Skip 'variable'.
            Scan;
         when others =>
            Error_Msg_Parse
              ("constant, signal or variable expected after '<<'");
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

   --  LRM09 8.2 Simple names
   --  simple_name ::= identifier
   function Parse_Simple_Name return Iir
   is
      Res : Iir;
   begin
      Expect (Tok_Identifier);

      Res := Create_Iir (Iir_Kind_Simple_Name);
      Set_Identifier (Res, Current_Identifier);
      Set_Location (Res);

      --  Skip identifier
      Scan;

      return Res;
   end Parse_Simple_Name;

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
   function Parse_Any_Name
     (Allow_Indexes: Boolean; Allow_Signature : Boolean) return Iir
   is
      Res: Iir;
   begin
      case Current_Token is
         when Tok_Identifier =>
            Res := Parse_Simple_Name;

         when Tok_String =>
            --  For operator symbol, such as: "+" (A, B).
            Res := Create_Iir (Iir_Kind_String_Literal8);
            Set_String8_Id (Res, Current_String_Id);
            Set_String_Length (Res, Current_String_Length);
            Set_Literal_Length (Res, Get_Token_Length);
            Set_Location (Res);

            --  Skip string
            Scan;
         when Tok_Double_Less =>
            Check_Vhdl_At_Least_2008 ("external name");
            Res := Parse_External_Name;
         when others =>
            if Current_Token = Tok_Invalid then
               Error_Msg_Parse ("name expected here");
            else
               Error_Msg_Parse
                 ("name expected here, found %t", +Current_Token);
            end if;
            return Create_Error_Node;
      end case;

      return Parse_Name_Suffix (Res, Allow_Indexes, Allow_Signature);
   end Parse_Any_Name;

   function Parse_Name (Allow_Indexes: Boolean := True) return Iir is
   begin
      return Parse_Any_Name (Allow_Indexes, False);
   end Parse_Name;

   function Parse_Signature_Name return Iir is
   begin
      return Parse_Any_Name (True, True);
   end Parse_Signature_Name;

   --  Emit an error message if MARK doesn't have the form of a type mark.
   function Check_Type_Mark (Mark : Iir) return Boolean is
   begin
      case Get_Kind (Mark) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            return True;
         when Iir_Kind_Attribute_Name =>
            --  For O'Subtype.
            return True;
         when others =>
            Error_Msg_Parse (+Mark, "type mark must be a name of a type");
            return False;
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

      if Check_Type_Mark (Res) then
         if Check_Paren and then Current_Token = Tok_Left_Paren then
            Error_Msg_Parse ("index constraint not allowed here");
            Old := Parse_Name_Suffix (Res, True);
         end if;
      else
         Res := Null_Iir;
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
   --  [ AMS-LRM17 6.5.2 ]
   --  interface_quantity_declaration ::=
   --      QUANTITY identifier_list : [ IN | OUT ] subtype_indication
   --          [ := /static/_expression ]
   --
   --  The default kind of interface declaration is DEFAULT.
   function Parse_Interface_Object_Declaration (Ctxt : Interface_Kind_Type)
                                               return Iir
   is
      Kind : Iir_Kind;
      Last : Iir;
      First : Iir;
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
                 ("file interface not allowed in generic or port clause");
            end if;
            Kind := Iir_Kind_Interface_File_Declaration;
         when Tok_Quantity =>
            Kind := Iir_Kind_Interface_Quantity_Declaration;
         when others =>
            --  Fall back in case of parse error.
            Kind := Iir_Kind_Interface_Variable_Declaration;
      end case;

      First := Create_Iir (Kind);

      if Flag_Elocations then
         Create_Elocations (First);
         Set_Start_Location (First, Get_Token_Location);
      end if;

      --  Comments for the interface.
      if Flag_Gather_Comments then
         Gather_Comments_Line (First);
      end if;

      if Current_Token = Tok_Identifier then
         Is_Default := True;
         Has_Class := False;
      else
         Is_Default := False;
         Has_Class := True;

         --  Skip 'signal', 'variable', 'constant' or 'file'.
         Scan;
      end if;

      --  Parse list of identifiers.
      Inter := First;
      Last := First;
      loop
         Scan_Identifier (Inter);

         exit when Current_Token /= Tok_Comma;

         --  Skip ','
         Scan;

         Inter := Create_Iir (Kind);

         if Flag_Elocations then
            Create_Elocations (Inter);
            Set_Start_Location (Inter, Get_Start_Location (First));
         end if;

         Set_Chain (Last, Inter);
         Last := Inter;
      end loop;

      if Flag_Elocations then
         Set_Colon_Location (First, Get_Token_Location);
      end if;

      --  Skip ':'
      Expect_Scan (Tok_Colon, "':' expected after interface identifier");

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
               Set_Identifier (N_Interface, Get_Identifier (O_Interface));

               if Flag_Elocations then
                  Create_Elocations (N_Interface);
                  Set_Start_Location
                    (N_Interface, Get_Start_Location (O_Interface));
                  Set_Colon_Location
                    (N_Interface, Get_Colon_Location (O_Interface));
               end if;

               if O_Interface = First then
                  First := N_Interface;
               else
                  Set_Chain (Last, N_Interface);
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
         when Iir_Kind_Interface_Quantity_Declaration =>
            case Interface_Mode is
               when Iir_Unknown_Mode =>
                  Interface_Mode := Iir_In_Mode;
               when Iir_In_Mode
                 | Iir_Out_Mode =>
                  null;
               when Iir_Inout_Mode
                 | Iir_Linkage_Mode
                 | Iir_Buffer_Mode =>
                  Error_Msg_Parse
                    ("mode must be 'in' or 'out' for a quantity");
                  Interface_Mode := Iir_In_Mode;
            end case;
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
         if Flag_Elocations then
            Set_Assign_Location (First, Get_Token_Location);
         end if;
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

      return First;
   end Parse_Interface_Object_Declaration;

   --  [ AMS-LRM17 6.5.2 ]
   --  interface_terminal_declaration ::=
   --      TERMINAL identifier_list : subnature_indication
   --
   --  The default kind of interface declaration is DEFAULT.
   function Parse_Interface_Terminal_Declaration (Ctxt : Interface_Kind_Type)
                                                 return Iir
   is
      Last : Iir;
      First : Iir;
      Inter: Iir;
      Interface_Nature: Iir;
      Default_Value: Iir;
   begin
      pragma Assert (Current_Token = Tok_Terminal);

      --  LRM08 6.5.2 Interface object declarations
      --  Interface obejcts include interface constants that appear as
      --  generics of a design entity, a component, a block, a package or
      --  a subprogram, or as constant parameter of subprograms; interface
      --  signals that appear as ports of a design entity, component or
      --  block, or as signal parameters of subprograms; interface variables
      --  that appear as variable parameter subprograms; interface files
      --  that appear as file parameters of subrograms.
      if Ctxt = Generic_Interface_List then
         Error_Msg_Parse ("terminal interface not allowed in generic clause");
      end if;

      First := Create_Iir (Iir_Kind_Interface_Terminal_Declaration);

      if Flag_Elocations then
         Create_Elocations (First);
         Set_Start_Location (First, Get_Token_Location);
      end if;

      --  Skip 'terminal'.
      Scan;

      --  Parse list of identifiers.
      Inter := First;
      Last := First;
      loop
         Scan_Identifier (Inter);

         exit when Current_Token /= Tok_Comma;

         --  Skip ','
         Scan;

         Inter := Create_Iir (Iir_Kind_Interface_Terminal_Declaration);

         if Flag_Elocations then
            Create_Elocations (Inter);
            Set_Start_Location (Inter, Get_Start_Location (First));
         end if;

         Set_Chain (Last, Inter);
         Last := Inter;
      end loop;

      if Flag_Elocations then
         Set_Colon_Location (First, Get_Token_Location);
      end if;

      --  Skip ':'
      Expect_Scan (Tok_Colon, "':' expected after interface identifier");

      case Current_Token is
         when Tok_In
           | Tok_Out
           | Tok_Inout
           | Tok_Linkage
           | Tok_Buffer =>
            Error_Msg_Parse ("mode not allowed for terminal interface");

            --  Skip mode.
            Scan;
         when others =>
            null;
      end case;

      Interface_Nature := Parse_Subnature_Indication;
      --  Subnature_Indication is set only on the first interface.
      Set_Subnature_Indication (First, Interface_Nature);

      if Current_Token = Tok_Assign then
         Error_Msg_Parse
              ("default expression not allowed for an interface terminal");

         --  Skip ':='
         Scan;

         Default_Value := Parse_Expression;
         pragma Unreferenced (Default_Value);
      end if;

      Inter := First;
      while Inter /= Null_Iir loop
         Set_Is_Ref (Inter, Inter /= First);
         Set_Has_Mode (Inter, False);
         Set_Has_Class (Inter, True);
         Set_Has_Identifier_List (Inter, Inter /= Last);
         Inter := Get_Chain (Inter);
      end loop;

      return First;
   end Parse_Interface_Terminal_Declaration;

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

      --  Skip 'package'.
      Scan;

      Scan_Identifier (Inter);

      --  Skip 'is'.
      Expect_Scan (Tok_Is);

      --  Skip 'new'.
      Expect_Scan (Tok_New);

      Set_Uninstantiated_Package_Name (Inter, Parse_Name (False));

      --  Skip 'generic'
      Expect_Scan (Tok_Generic);

      --  Skip 'map'
      Expect_Scan (Tok_Map);

      --  Skip '('
      Expect_Scan (Tok_Left_Paren);

      case Current_Token is
         when Tok_Box =>
            Map := Null_Iir;
            --  Skip '<>'
            Scan;
         when others =>
            Map := Parse_Association_List;
      end case;
      Set_Generic_Map_Aspect_Chain (Inter, Map);

      --  Skip ')'
      Expect_Scan (Tok_Right_Paren);

      return Inter;
   end Parse_Interface_Package_Declaration;

   --  Precond:  identifier or string
   --  Postcond: next token
   --
   --  [ 2.1 ]
   --  designator ::= identifier | operator_symbol
   procedure Parse_Subprogram_Designator (Subprg : Iir) is
   begin
      if Current_Token = Tok_Identifier then
         --  Skip identifier.
         Scan_Identifier (Subprg);
      elsif Current_Token = Tok_String then
         if Kind_In (Subprg, Iir_Kind_Procedure_Declaration,
                     Iir_Kind_Interface_Procedure_Declaration)
         then
            --  LRM93 2.1
            --  A procedure designator is always an identifier.
            Error_Msg_Parse ("a procedure name must be an identifier");
         end if;
         --  LRM93 2.1
         --  A function designator is either an identifier or an operator
         --  symbol.
         Set_Identifier (Subprg, Scan_To_Operator_Name (Get_Token_Location));
         Set_Location (Subprg);

         --  Skip string.
         Scan;
      else
         --  Just to display a parse error.
         Expect (Tok_Identifier);
      end if;
   end Parse_Subprogram_Designator;

   --  Emit an error message is function declaration SUBPRG has no return
   --  type mark.
   procedure Check_Function_Specification (Subprg : Iir) is
   begin
      if Get_Return_Type_Mark (Subprg) = Null_Iir then
         Error_Msg_Parse ("'return' expected");
         Set_Return_Type_Mark (Subprg, Create_Error_Node);
      end if;
   end Check_Function_Specification;

   --  Precond: '(' or return or any
   --  Postcond: next token
   procedure Parse_Subprogram_Parameters_And_Return
     (Subprg : Iir; Is_Func : Boolean; Required : Boolean)
   is
      Old : Iir;
      pragma Unreferenced (Old);
      Tm : Iir;
      Ret : Iir;
      Inters : Iir;
   begin
      if Current_Token = Tok_Parameter then
         Set_Has_Parameter (Subprg, True);

         --  Eat 'parameter'
         Scan;

         if Current_Token /= Tok_Left_Paren then
            Error_Msg_Parse
              ("'parameter' must be followed by a list of parameters");
         end if;
      end if;

      if Current_Token = Tok_Left_Paren then
         --  Parse the interface declaration.
         if Is_Func then
            Inters := Parse_Interface_List
              (Function_Parameter_Interface_List, Subprg);
         else
            Inters := Parse_Interface_List
              (Procedure_Parameter_Interface_List, Subprg);
         end if;
         Set_Interface_Declaration_Chain (Subprg, Inters);
      end if;

      if Current_Token = Tok_Return then
         if not Is_Func then
            Report_Start_Group;
            Error_Msg_Parse ("'return' not allowed for a procedure");
            Error_Msg_Parse ("(remove return part or declare a function)");
            Report_End_Group;

            --  Skip 'return'
            Scan;

            Old := Parse_Type_Mark;
         else
            --  Skip 'return'
            Scan;

            Tm := Parse_Type_Mark (Check_Paren => True);

            if Tm /= Null_Iir and then Current_Token = Tok_Of then
               if Vhdl_Std < Vhdl_19 then
                  Error_Msg_Parse
                    ("return identifier not allowed before vhdl 2019");
               elsif Get_Kind (Tm) /= Iir_Kind_Simple_Name then
                  Error_Msg_Parse ("return identifier must be an identifier");
               end if;
               Ret := Create_Iir (Iir_Kind_Subtype_Declaration);
               Location_Copy (Ret, Tm);
               Set_Identifier (Ret, Get_Identifier (Tm));
               if Get_Kind (Subprg) = Iir_Kind_Interface_Function_Declaration
               then
                  Error_Msg_Parse
                    ("return identifier not allowed in interface function");
               else
                  Set_Return_Identifier (Subprg, Ret);
               end if;
               Free_Iir (Tm);

               --  Skip 'of'
               Scan;

               Tm := Parse_Type_Mark (Check_Paren => True);
            end if;

            Set_Return_Type_Mark (Subprg, Tm);
         end if;
      else
         if Is_Func and Required then
            Check_Function_Specification (Subprg);
         end if;
      end if;
   end Parse_Subprogram_Parameters_And_Return;

   --  Precond:  PROCEDURE, FUNCTION, PURE, IMPURE
   --  Postcond: next token
   --
   --  LRM08 6.5.4 Interface subrpogram declarations
   --  interface_subprogram_declaration ::=
   --     interface_subprogram_specification
   --        [ IS interface_subprogram_default ]
   --
   --  interface_subrpogram_specification ::=
   --     interface_procedure_specification | interface_function_specification
   --
   --  interface_procedure_specification ::=
   --     PROCEDURE designator
   --     [ [ PARAMETER ] ( formal_parameter_list ) ]
   --
   --  interface_function_specification ::=
   --     [ PURE | IMPURE ] FUNCTION designator
   --       [ [ PARAMETER ] ( formal_parameter_list ) ] RETURN type_mark
   --
   --  interface_subprogram_default ::=
   --     /subprogram/_name | <>
   function Parse_Interface_Subprogram_Declaration return Iir
   is
      Kind : Iir_Kind;
      Subprg: Iir;
      Old : Iir;
      pragma Unreferenced (Old);
      Def : Iir;
   begin
      --  Create the node.
      case Current_Token is
         when Tok_Procedure =>
            Kind := Iir_Kind_Interface_Procedure_Declaration;
         when Tok_Function
           | Tok_Pure
           | Tok_Impure =>
            Kind := Iir_Kind_Interface_Function_Declaration;
         when others =>
            raise Internal_Error;
      end case;
      Subprg := Create_Iir (Kind);
      Set_Location (Subprg);

      case Current_Token is
         when Tok_Procedure =>
            --  Skip 'procedure'.
            Scan;
         when Tok_Function =>
            --  LRM93 2.1
            --  A function is impure if its specification contains the
            --  reserved word IMPURE; otherwise it is said to be pure.
            Set_Pure_Flag (Subprg, True);

            --  Skip 'function'.
            Scan;
         when Tok_Pure
           | Tok_Impure =>
            Set_Pure_Flag (Subprg, Current_Token = Tok_Pure);
            Set_Has_Pure (Subprg, True);

            --  Eat 'pure' or 'impure'.
            Scan;

            Expect_Scan
              (Tok_Function, "'function' must follow 'pure' or 'impure'");
         when others =>
            raise Internal_Error;
      end case;

      --  Designator.
      Parse_Subprogram_Designator (Subprg);

      Parse_Subprogram_Parameters_And_Return
        (Subprg, Kind = Iir_Kind_Interface_Function_Declaration, True);

      --  TODO: interface_subprogram_default
      if Current_Token = Tok_Is then
         --  Skip 'is'.
         Scan;

         if Current_Token = Tok_Box then
            Def := Create_Iir (Iir_Kind_Reference_Name);
            Set_Location (Def);

            --  Skip '<>'.
            Scan;
         else
            Def := Parse_Name;
         end if;

         Set_Default_Subprogram (Subprg, Def);
      end if;

      return Subprg;
   end Parse_Interface_Subprogram_Declaration;

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
      Prev_Loc := Get_Token_Location;

      --  Skip '('.
      Expect_Scan (Tok_Left_Paren);

      Res := Null_Iir;
      Last := Null_Iir;
      loop
         case Current_Token is
            when Tok_Identifier
              | Tok_Signal
              | Tok_Variable
              | Tok_Constant
              | Tok_File
              | Tok_Quantity =>
               --  An interface object.
               Inters := Parse_Interface_Object_Declaration (Ctxt);
            when Tok_Terminal =>
               Inters := Parse_Interface_Terminal_Declaration (Ctxt);
            when Tok_Package =>
               if Ctxt /= Generic_Interface_List then
                  Error_Msg_Parse
                    ("package interface only allowed in generic interface");
               else
                  Check_Vhdl_At_Least_2008 ("package interface");
               end if;
               Inters := Parse_Interface_Package_Declaration;
            when Tok_Type =>
               if Ctxt /= Generic_Interface_List then
                  Error_Msg_Parse
                    ("type interface only allowed in generic interface");
               else
                  Check_Vhdl_At_Least_2008 ("type interface");
               end if;
               Inters := Create_Iir (Iir_Kind_Interface_Type_Declaration);

               -- Skip 'type'.
               Scan;

               Scan_Identifier (Inters);
            when Tok_Procedure
              | Tok_Pure
              | Tok_Impure
              | Tok_Function =>
               if Ctxt /= Generic_Interface_List then
                  Error_Msg_Parse
                    ("subprogram interface only allowed in generic interface");
               else
                  Check_Vhdl_At_Least_2008 ("subprogram interface");
               end if;
               Inters := Parse_Interface_Subprogram_Declaration;
            when Tok_Right_Paren =>
               if Res = Null_Iir then
                  Error_Msg_Parse
                    (Prev_Loc, "empty interface list not allowed");
               else
                  Error_Msg_Parse
                    (Prev_Loc, "extra ';' at end of interface list");
               end if;

               --  Skip ')'.
               Scan;

               exit;
            when others =>
               Error_Msg_Parse ("interface declaration expected");
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

         Prev_Loc := Get_Token_Location;

         case Current_Token is
            when Tok_Comma =>
               Error_Msg_Parse
                 ("interfaces must be separated by ';' (found ',')");

               --  Skip ','.
               Scan;
            when Tok_Semi_Colon =>
               --  Skip ';'.
               Scan;
            when Tok_Right_Paren =>
               --  Skip ')'.
               Scan;

               exit;
            when others =>
               --  Try to resync; skip tokens until ';', ')'.  Handled nested
               --  parenthesis.
               Error_Msg_Parse ("';' or ')' expected after interface");

               if Resync_To_End_Of_Interface then
                  exit;
               end if;
         end case;
      end loop;

      return Res;
   end Parse_Interface_List;

   --  precond : PORT
   --  postcond: next token
   --
   --  [ LRM93 1.1.1 ]
   --  port_clause ::= PORT ( port_list ) ;
   --
   --  [ LRM93 1.1.1.2 ]
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
         case Get_Kind (El) is
            when Iir_Kind_Interface_Signal_Declaration
              | Iir_Kind_Interface_Terminal_Declaration
              | Iir_Kind_Interface_Quantity_Declaration =>
               null;
            when others =>
               if AMS_Vhdl then
                  Error_Msg_Parse
                    (+El, "port must be a signal, a terminal or a quantity");
               else
                  Error_Msg_Parse (+El, "port must be a signal");
               end if;
         end case;
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
   --  [ LRM93 1.1.1 ]
   --  entity_header ::=
   --      [ FORMAL_generic_clause ]
   --      [ FORMAL_port_clause ]
   --
   --  [ LRM93 4.5 ]
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

            if Flag_Elocations then
               Set_Generic_Location (Parent, Get_Token_Location);
            end if;

            Has_Generic := True;
            Parse_Generic_Clause (Parent);
         elsif Current_Token = Tok_Port then
            if Has_Port then
               Error_Msg_Parse ("at most one port clause is allowed");
            end if;

            if Flag_Elocations then
               Set_Port_Location (Parent, Get_Token_Location);
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

      --  LRM93 3.1.1
      --  The position number of the first listed enumeration literal is zero.
      Pos := 0;

      --  Eat '('.
      Scan;

      if Current_Token = Tok_Right_Paren then
         Error_Msg_Parse ("at least one literal must be declared");
      else
         loop
            if Current_Token = Tok_Identifier
              or Current_Token = Tok_Character
            then
               Enum_Lit := Create_Iir (Iir_Kind_Enumeration_Literal);
               Set_Identifier (Enum_Lit, Current_Identifier);
               Set_Parent (Enum_Lit, Parent);
               Set_Location (Enum_Lit);
               Set_Enum_Pos (Enum_Lit, Pos);

               --  Comments for the enumeration literal.
               if Flag_Gather_Comments then
                  Gather_Comments_Line (Enum_Lit);
               end if;

               --  LRM93 3.1.1
               --  the position number for each additional enumeration literal
               --  is one more than that if its predecessor in the list.
               Pos := Pos + 1;

               Append_Element (Enum_List, Enum_Lit);

               --  Skip identifier or character.
               Scan;
            else
               Error_Msg_Parse ("identifier or character expected");
            end if;

            exit when Current_Token /= Tok_Comma;

            --  Skip ','.
            Scan;

            if Current_Token = Tok_Right_Paren then
               Error_Msg_Parse ("extra ',' ignored");
               exit;
            end if;
         end loop;
      end if;

      --  Skip ')'.
      Expect_Scan (Tok_Right_Paren, "')' expected at end of enumeration type");

      Set_Enumeration_Literal_List (Enum_Type, List_To_Flist (Enum_List));

      return Enum_Type;
   end Parse_Enumeration_Type_Definition;

   --  Parse:
   --    ARRAY ( index_subtype_definition { , index_subtype_definition } ) OF
   --  | ARRAY index_constraint OF
   --
   --   index_subtype_definition ::= type_mark RANGE <>
   --
   --   index_constraint ::= ( discrete_range { , discrete_range } )
   --
   --   discrete_range ::= discrete_subtype_indication | range
   procedure Parse_Array_Indexes
     (Indexes : out Iir_Flist; Constrained : out Boolean)
   is
      First : Boolean;
      Index_List : Iir_List;
      Index_Constrained : Boolean;
      Array_Constrained : Boolean;
      Type_Mark : Iir;
      Def : Iir;
   begin
      --  Skip 'array'.
      Scan;

      --  Skip '('.
      Expect_Scan (Tok_Left_Paren);

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
         Type_Mark := Parse_Expression (Prio_Simple);

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
               Def := Parse_Range_Expression (Type_Mark);
            when others =>
               --  For a /range/_attribute_name
               Index_Constrained := True;
               Def := Type_Mark;
         end case;

         if First then
            Array_Constrained := Index_Constrained;
            First := False;
         else
            if Array_Constrained /= Index_Constrained then
               Error_Msg_Parse
                 ("cannot mix constrained and unconstrained index");
               Def := Create_Error_Node (Def);
            end if;
         end if;

         Append_Element (Index_List, Def);

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
      end loop;

      --  Skip ')' and 'of'
      Expect_Scan (Tok_Right_Paren);
      Expect_Scan (Tok_Of);

      Indexes := List_To_Flist (Index_List);
      Constrained := Array_Constrained;
   end Parse_Array_Indexes;

   --  precond : ARRAY
   --  postcond: ';'
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
   --  [ LRM08 5.3.2.1 ]
   --  array_type_definition ::= unbounded_array_definition
   --                          | constrained_array_definition
   --
   --   unbounded_array_definition ::=
   --      ARRAY ( index_subtype_definition { , index_subtype_definition } )
   --      OF element_subtype_indication
   function Parse_Array_Type_Definition return Iir
   is
      Array_Constrained : Boolean;
      Res_Type: Iir;
      Index_Flist : Iir_Flist;

      Loc : Location_Type;
      Element_Subtype : Iir;
   begin
      Loc := Get_Token_Location;

      Parse_Array_Indexes (Index_Flist, Array_Constrained);

      Element_Subtype := Parse_Subtype_Indication;

      if Array_Constrained then
         --  Sem_Type will create the array type.
         Res_Type := Create_Iir (Iir_Kind_Array_Subtype_Definition);
         Set_Array_Element_Constraint (Res_Type, Element_Subtype);
         Set_Index_Constraint_List (Res_Type, Index_Flist);
         Set_Index_Constraint_Flag (Res_Type, True);
      else
         Res_Type := Create_Iir (Iir_Kind_Array_Type_Definition);
         Set_Element_Subtype_Indication (Res_Type, Element_Subtype);
         Set_Index_Subtype_Definition_List (Res_Type, Index_Flist);
      end if;
      Set_Location (Res_Type, Loc);

      return Res_Type;
   end Parse_Array_Type_Definition;

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
      Res: Iir_Physical_Type_Definition;
      Unit: Iir_Unit_Declaration;
      Last : Iir_Unit_Declaration;
      Multiplier : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Physical_Type_Definition);
      Set_Location (Res);

      --  Skip 'units'
      Expect_Scan (Tok_Units);

      --  Parse primary unit.
      Unit := Create_Iir (Iir_Kind_Unit_Declaration);
      Set_Parent (Unit, Parent);

      Scan_Identifier (Unit);

      Scan_Semi_Colon ("primary physical unit");

      Set_Unit_Chain (Res, Unit);
      Last := Unit;

      --  Parse secondary units.
      while Current_Token = Tok_Identifier loop
         Unit := Create_Iir (Iir_Kind_Unit_Declaration);
         Set_Parent (Unit, Parent);

         Scan_Identifier (Unit);

         --  Skip '='.
         Expect_Scan (Tok_Equal);

         case Current_Token is
            when Tok_Integer
              | Tok_Identifier
              | Tok_Real =>
               Multiplier := Parse_Primary;
            when others =>
               Error_Msg_Parse
                 ("physical literal expected to define a secondary unit");
               Skip_Until_Semi_Colon;
               Multiplier := Null_Iir;
         end case;

         if Multiplier /= Null_Iir then
            Set_Physical_Literal (Unit, Multiplier);

            case Get_Kind (Multiplier) is
               when Iir_Kind_Simple_Name
                 | Iir_Kind_Selected_Name
                 | Iir_Kind_Physical_Int_Literal =>
                  null;
               when Iir_Kind_Physical_Fp_Literal =>
                  Error_Msg_Parse
                    ("secondary units may only be defined by an integer");
               when others =>
                  Error_Msg_Parse ("a physical literal is expected here");
                  Skip_Until_Semi_Colon;
            end case;
         end if;
         Set_Chain (Last, Unit);
         Last := Unit;

         Scan_Semi_Colon ("secondary physical unit");
      end loop;

      --  Skip 'end'.
      Expect_Scan (Tok_End);

      --  Skip 'units'.
      Expect_Scan (Tok_Units);
      Set_End_Has_Reserved_Id (Res, True);

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

      --  Skip 'record'
      Scan;

      if Current_Token = Tok_End then
         Error_Msg_Parse ("empty records are not allowed");
      else
         Pos := 0;
         First := Null_Iir;
         loop
            pragma Assert (First = Null_Iir);
            --  Parse identifier_list
            loop
               El := Create_Iir (Iir_Kind_Element_Declaration);
               Scan_Identifier (El);

               Set_Parent (El, Res);
               if First = Null_Iir then
                  First := El;
               end if;

               Append_Element (El_List, El);
               Set_Element_Position (El, Pos);
               Pos := Pos + 1;

               exit when Current_Token /= Tok_Comma;

               Set_Has_Identifier_List (El, True);

               --  Skip ','
               Scan;
            end loop;

            --  Comments attached to the first element.
            if Flag_Gather_Comments then
               Gather_Comments_Line (First);
            end if;

            --  Scan ':'.
            Expect_Scan (Tok_Colon);

            --  Parse element subtype indication.
            Subtype_Indication := Parse_Subtype_Indication;
            Set_Subtype_Indication (First, Subtype_Indication);

            First := Null_Iir;
            Scan_Semi_Colon_Declaration ("element declaration");
            exit when Current_Token /= Tok_Identifier;
         end loop;

         Set_Elements_Declaration_List (Res, List_To_Flist (El_List));
      end if;

      if Flag_Elocations then
         Create_Elocations (Res);
         Set_End_Location (Res, Get_Token_Location);
      end if;

      --  Skip 'end'
      Expect_Scan (Tok_End);
      Expect_Scan (Tok_Record);
      Set_End_Has_Reserved_Id (Res, True);

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
      Scan;
      Expect_Scan (Tok_Of);

      Type_Mark := Parse_Type_Mark (Check_Paren => True);
      if Type_Mark = Null_Iir
        or else Get_Kind (Type_Mark) not in Iir_Kinds_Denoting_Name
      then
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
   function Parse_Protected_Type_Definition
     (Ident : Name_Id; Loc : Location_Type) return Iir
   is
      Res : Iir;
      Decl : Iir;
   begin
      --  Skip 'protected'.
      Scan;

      if Current_Token = Tok_Body then
         Res := Create_Iir (Iir_Kind_Protected_Type_Body);

         --  Skip 'body'.
         Scan;

         Decl := Res;
      else
         Decl := Create_Iir (Iir_Kind_Type_Declaration);
         Res := Create_Iir (Iir_Kind_Protected_Type_Declaration);
         Set_Location (Res, Loc);
         Set_Type_Definition (Decl, Res);
         Set_Type_Declarator (Res, Decl);
      end if;
      Set_Identifier (Decl, Ident);
      Set_Location (Decl, Loc);

      Parse_Declarative_Part (Res, Res);

      --  Eat 'end'.
      Expect_Scan (Tok_End);

      if Flags.Vhdl_Std >= Vhdl_00 then
         Expect_Scan (Tok_Protected);
      else
         --  Avoid weird message: 'protected' expected instead of 'protected'.
         Expect_Scan (Tok_Identifier);
      end if;
      Set_End_Has_Reserved_Id (Res, True);
      if Get_Kind (Res) = Iir_Kind_Protected_Type_Body then
         Expect_Scan (Tok_Body);
      end if;
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
      Start_Loc : Location_Type;
   begin
      -- The current token must be type.
      pragma Assert (Current_Token = Tok_Type);
      Start_Loc := Get_Token_Location;

      --  Skip 'type'.
      Scan;

      -- Get the identifier
      Loc := Get_Token_Location;
      if Current_Token = Tok_Identifier then
         Ident := Current_Identifier;

         --  Skip identifier.
         Scan;
      else
         Expect (Tok_Identifier, "identifier is expected after 'type'");
         Ident := Null_Identifier;
      end if;


      if Current_Token = Tok_Semi_Colon then
         --  If there is a ';', this is an incomplete type declaration.
         Scan;

         Decl := Create_Iir (Iir_Kind_Type_Declaration);
         Set_Identifier (Decl, Ident);
         Set_Location (Decl, Loc);

         if Flag_Elocations then
            Create_Elocations (Decl);
            Set_Start_Location (Decl, Start_Loc);
         end if;

         return Decl;
      end if;

      Expect_Scan (Tok_Is, "'is' expected here");

      case Current_Token is
         when Tok_Left_Paren =>
            --  This is an enumeration.
            --  Create the type declaration now so that comments can be
            --  attached to it (and later comments to the literals).
            Decl := Create_Iir (Iir_Kind_Type_Declaration);

            --  Comments attached to the type.
            if Flag_Gather_Comments then
               Gather_Comments_Line (Decl);
            end if;

            Def := Parse_Enumeration_Type_Definition (Parent);
            Set_Type_Definition (Decl, Def);

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
                  Phys_Def : Iir;
               begin
                  Phys_Def := Parse_Physical_Type_Definition (Parent);
                  if Current_Token = Tok_Identifier then
                     if Flags.Vhdl_Std = Vhdl_87 then
                        Error_Msg_Parse
                          ("simple_name not allowed here in vhdl87");
                     end if;
                     Check_End_Name (Get_Identifier (Decl), Phys_Def);
                  end if;
                  Set_Range_Constraint (Phys_Def, Def);
                  Set_Type_Definition (Decl, Phys_Def);
                  Set_Type_Declarator (Phys_Def, Decl);
               end;
            end if;

         when Tok_Array =>
            Def := Parse_Array_Type_Definition;
            Decl := Null_Iir;

         when Tok_Record =>
            Decl := Create_Iir (Iir_Kind_Type_Declaration);
            Set_Identifier (Decl, Ident);
            Set_Location (Decl, Loc);

            --  Comments attached to the record.
            if Flag_Gather_Comments then
               Gather_Comments_Block (Decl);
            end if;

            Def := Parse_Record_Type_Definition;
            Set_Type_Definition (Decl, Def);
            Set_Type_Declarator (Def, Decl);
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
               Report_Start_Group;
               Error_Msg_Parse ("type %i cannot be defined from another type",
                                +Ident);
               Error_Msg_Parse ("(you should declare a subtype)");
               Report_End_Group;
               Decl := Create_Iir (Iir_Kind_Type_Declaration);
            end if;

         when Tok_Protected =>
            if Flags.Vhdl_Std < Vhdl_00 then
               Error_Msg_Parse ("protected type not allowed in vhdl87/93");
            end if;
            Decl := Parse_Protected_Type_Definition (Ident, Loc);

         when others =>
            Error_Msg_Parse ("missing type definition after 'is'");
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

         --  Comments attached to the type.
         if Flag_Gather_Comments then
            Gather_Comments_Line (Decl);
         end if;
      end if;
      Set_Identifier (Decl, Ident);
      Set_Location (Decl, Loc);

      -- ';' is expected after end of type declaration
      Scan_Semi_Colon_Declaration ("type declaration");

      if Flag_Elocations then
         Create_Elocations (Decl);
         Set_Start_Location (Decl, Start_Loc);
      end if;

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
                  Error_Msg_Parse (+Ind, "element name expected");
                  Id := Null_Identifier;
               end if;
               Free_Iir (Ind);

               Def := Create_Iir (Iir_Kind_Record_Resolution);
               Set_Location (Def, Loc);
               Chain_Init (First, Last);
               loop
                  El := Create_Iir (Iir_Kind_Record_Element_Resolution);
                  Set_Location (El, Loc);
                  Set_Identifier (El, Id);
                  Set_Resolution_Indication (El, Parse_Resolution_Indication);
                  Chain_Append (First, Last, El);
                  exit when Current_Token /= Tok_Comma;

                  --  Eat ','
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
         Expect_Scan (Tok_Right_Paren);

         return Def;
      else
         Error_Msg_Parse ("resolution indication expected");
         return Null_Iir;
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
   procedure Parse_Element_Constraint (Def : Iir)
   is
      El_Def : Iir;
      El : Iir;
      Index_List : Iir_List;
   begin
      --  Index_constraint.
      Set_Location (Def);
      Set_Index_Constraint_Flag (Def, True);
      Set_Has_Array_Constraint_Flag (Def, True);

      --  Eat '('.
      Scan;

      if Current_Token = Tok_Open then
         --  Eat 'open'.
         Scan;
      else
         Index_List := Create_Iir_List;
         --  index_constraint ::= (discrete_range {, discrete_range} )
         loop
            El := Parse_Discrete_Range;
            Append_Element (Index_List, El);

            exit when Current_Token /= Tok_Comma;

            --  Eat ','
            Scan;
         end loop;
         Set_Index_Constraint_List (Def, List_To_Flist (Index_List));
      end if;

      --  Eat ')'
      Expect_Scan (Tok_Right_Paren);

      if Current_Token = Tok_Left_Paren then
         El_Def := Create_Iir (Iir_Kind_Array_Subtype_Definition);
         Parse_Element_Constraint (El_Def);
         Set_Array_Element_Constraint (Def, El_Def);
         Set_Has_Element_Constraint_Flag (Def, True);
      end if;
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
   function Parse_Subtype_Indication (Name : Iir := Null_Iir) return Iir
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
         if Check_Type_Mark (Name) then
            Type_Mark := Name;
         else
            --  Not a type mark.  Ignore it.
            Type_Mark := Null_Iir;
         end if;
      else
         if Current_Token = Tok_Left_Paren then
            Check_Vhdl_At_Least_2008 ("resolution indication");
            Resolution_Indication := Parse_Resolution_Indication;
         end if;
         if Current_Token /= Tok_Identifier then
            Error_Msg_Parse ("type mark expected in a subtype indication");
            return Create_Error_Node;
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
            Def := Create_Iir (Iir_Kind_Array_Subtype_Definition);
            Parse_Element_Constraint (Def);
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
               if Type_Mark /= Null_Iir then
                  Location_Copy (Def, Type_Mark);
                  Set_Subtype_Type_Mark (Def, Type_Mark);
               end if;
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
   --  postcond: next token
   --
   --  [ LRM93 4.2 ]
   --  subtype_declaration ::= SUBTYPE identifier IS subtype_indication ;
   function Parse_Subtype_Declaration (Parent : Iir)
                                      return Iir_Subtype_Declaration
   is
      Decl: Iir_Subtype_Declaration;
      Def: Iir;
      Start_Loc : Location_Type;
   begin
      Decl := Create_Iir (Iir_Kind_Subtype_Declaration);
      Set_Parent (Decl, Parent);
      Start_Loc := Get_Token_Location;

      --  Eat 'subtype'.
      Scan;

      Scan_Identifier (Decl);

      --  Skip 'is'.
      Expect_Scan (Tok_Is);

      Def := Parse_Subtype_Indication;
      Set_Subtype_Indication (Decl, Def);

      --  Skip ';'.
      Scan_Semi_Colon_Declaration ("subtype decalaration");

      if Flag_Elocations then
         Create_Elocations (Decl);
         Set_Start_Location (Decl, Start_Loc);
      end if;

      return Decl;
   end Parse_Subtype_Declaration;

   --  [ LRM93 3.5.1 ]
   --  scalar_nature_definition ::= type_mark ACROSS
   --                               type_mark THROUGH
   --                               identifier REFERENCE
   --
   function Parse_Scalar_Nature_Definition return Iir
   is
      Def : Iir;
      Ref : Iir;
   begin
      Def := Create_Iir (Iir_Kind_Scalar_Nature_Definition);
      Set_Across_Type_Mark (Def, Parse_Type_Mark);
      Expect_Scan (Tok_Across, "'across' expected after type mark");
      Set_Through_Type_Mark (Def, Parse_Type_Mark);
      Expect_Scan (Tok_Through, "'through' expected after type mark");
      if Current_Token = Tok_Identifier then
         Ref := Create_Iir (Iir_Kind_Terminal_Declaration);
         Scan_Identifier (Ref);
         Set_Reference (Def, Ref);
         Set_Reference_Terminal_Flag (Ref, True);
         if Current_Token = Tok_Reference then
            Scan;
         else
            Expect (Tok_Reference, "'reference' expected");
            Skip_Until_Semi_Colon;
         end if;
      else
         Error_Msg_Parse ("reference identifier expected");
         Skip_Until_Semi_Colon;
      end if;

      return Def;
   end Parse_Scalar_Nature_Definition;

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
   function Parse_Subnature_Indication return Iir
   is
      Nature_Mark : Iir;
      Expr : Iir;
      Res : Iir;
   begin
      if Current_Token /= Tok_Identifier then
         Error_Msg_Parse ("nature mark expected in a subnature indication");
         return Null_Iir;
      end if;
      Res := Parse_Name (Allow_Indexes => False);

      if Current_Token = Tok_Left_Paren then
         Nature_Mark := Res;
         Res := Create_Iir (Iir_Kind_Array_Subnature_Definition);
         Parse_Element_Constraint (Res);
         Set_Subnature_Nature_Mark (Res, Nature_Mark);
      end if;

      if Current_Token = Tok_Tolerance then
         --  Skip 'tolerance'.
         Scan;

         Expr := Parse_Expression;

         Expect_Scan (Tok_Across, "'across' required after tolerance");

         Expr := Parse_Expression;

         Expect_Scan (Tok_Through, "'through' required after tolerance");
         pragma Unreferenced (Expr);
      end if;
      return Res;
   end Parse_Subnature_Indication;

   function Parse_Array_Nature_Definition return Iir
   is
      Loc : Location_Type;
      Index_Flist : Iir_Flist;
      Array_Constrained : Boolean;
      Element_Subnature : Iir;
      Res_Type : Iir;
   begin
      Loc := Get_Token_Location;

      Parse_Array_Indexes (Index_Flist, Array_Constrained);

      Element_Subnature := Parse_Subnature_Indication;

      if Array_Constrained then
         --  Sem_Type will create the array type.
         Res_Type := Create_Iir (Iir_Kind_Array_Subnature_Definition);
         Set_Array_Element_Constraint (Res_Type, Element_Subnature);
         Set_Index_Constraint_List (Res_Type, Index_Flist);
         Set_Index_Constraint_Flag (Res_Type, True);
      else
         Res_Type := Create_Iir (Iir_Kind_Array_Nature_Definition);
         Set_Element_Subnature_Indication (Res_Type, Element_Subnature);
         Set_Index_Subtype_Definition_List (Res_Type, Index_Flist);
      end if;
      Set_Location (Res_Type, Loc);

      return Res_Type;
   end Parse_Array_Nature_Definition;

   --  record_nature_definition ::=
   --     RECORD
   --        nature_element_declaration
   --        { nature_element_declaration }
   --     END RECORD [ /record_nature/_simple_name ]
   --
   function Parse_Record_Nature_Definition return Iir
   is
      Res : Iir;
      El_List : Iir_List;
      El : Iir;
      First : Iir;
      Pos: Iir_Index32;
      Subnature_Indication : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Record_Nature_Definition);
      Set_Location (Res);
      El_List := Create_Iir_List;

      --  Skip 'record'
      Scan;

      Pos := 0;
      First := Null_Iir;
      loop
         pragma Assert (First = Null_Iir);
         --  Parse identifier_list
         loop
            El := Create_Iir (Iir_Kind_Nature_Element_Declaration);
            Scan_Identifier (El);

            Set_Parent (El, Res);
            if First = Null_Iir then
               First := El;
            end if;

            Append_Element (El_List, El);
            Set_Element_Position (El, Pos);
            Pos := Pos + 1;

            exit when Current_Token /= Tok_Comma;

            Set_Has_Identifier_List (El, True);

            --  Skip ','
            Scan;
         end loop;

         --  Scan ':'.
         Expect_Scan (Tok_Colon);

         --  Parse element subnature indication.
         Subnature_Indication := Parse_Subnature_Indication;
         Set_Subnature_Indication (First, Subnature_Indication);

         First := Null_Iir;
         Scan_Semi_Colon_Declaration ("element declaration");
         exit when Current_Token /= Tok_Identifier;
      end loop;

      Set_Elements_Declaration_List (Res, List_To_Flist (El_List));

      if Flag_Elocations then
         Create_Elocations (Res);
         Set_End_Location (Res, Get_Token_Location);
      end if;

      --  Skip 'end'
      Expect_Scan (Tok_End);
      Expect_Scan (Tok_Record);
      Set_End_Has_Reserved_Id (Res, True);

      return Res;
   end Parse_Record_Nature_Definition;

   --  precond : NATURE
   --  postcond: a token
   --
   --  AMS-LRM17 6.11 Nature and subnature declarations
   --  nature_definition ::= scalar_nature_definition
   --                    | composite_nature_definition
   --
   --  [ LRM93 3.5.2 ]
   --  composite_nature_definition ::= array_nature_definition
   --                              | record_nature_definition
   function Parse_Nature_Declaration return Iir
   is
      Def : Iir;
      Loc : Location_Type;
      Ident : Name_Id;
      Decl : Iir;
   begin
      --  Skip 'nature'.
      pragma Assert (Current_Token = Tok_Nature);
      Scan;

      -- Get the identifier
      Expect (Tok_Identifier, "an identifier is expected after 'nature'");
      Loc := Get_Token_Location;
      Ident := Current_Identifier;

      Scan;

      --  Skip 'is'.
      Expect_Scan (Tok_Is);

      case Current_Token is
         when Tok_Array =>
            Def := Parse_Array_Nature_Definition;
            Set_Location (Def, Loc);
         when Tok_Record =>
            Def := Parse_Record_Nature_Definition;
            Set_Location (Def, Loc);
            if Current_Token = Tok_Identifier then
               Check_End_Name (Ident, Def);
            end if;
         when Tok_Identifier =>
            Def := Parse_Scalar_Nature_Definition;
            Set_Location (Def, Loc);
         when others =>
            Error_Msg_Parse ("nature definition expected here");
            Skip_Until_Semi_Colon;
      end case;

      Decl := Create_Iir (Iir_Kind_Nature_Declaration);
      Set_Nature (Decl, Def);
      Set_Identifier (Decl, Ident);
      Set_Location (Decl, Loc);

      -- ';' is expected after end of type declaration
      Scan_Semi_Colon_Declaration ("nature declaration");

      return Decl;
   end Parse_Nature_Declaration;

   --  AMS-LRM17 6.11 Nature and subnature declarations
   --  subnature_declaration ::=
   --    SUBNATURE identifier is subnature_indication ;
   function Parse_Subnature_Declaration return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Subnature_Declaration);
      Set_Location (Res);

      --  Skip 'subnature'.
      Scan;

      Scan_Identifier (Res);

      --  Skip 'is'.
      Expect_Scan (Tok_Is);

      Set_Subnature_Indication (Res, Parse_Subnature_Indication);

      -- ';' is expected after end of type declaration
      Scan_Semi_Colon_Declaration ("subnature declaration");

      return Res;
   end Parse_Subnature_Declaration;

   --  precond : TERMINAL
   --  postcond: next token.
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
      Chain_Init (First, Last);

      --  Skip 'terminal'.
      Scan;

      loop
         -- 'terminal' or "," was just scanned.
         Terminal := Create_Iir (Iir_Kind_Terminal_Declaration);

         Scan_Identifier (Terminal);

         Set_Parent (Terminal, Parent);

         Chain_Append (First, Last, Terminal);

         exit when Current_Token /= Tok_Comma;

         Set_Has_Identifier_List (Terminal, True);

         --  Skip ','.
         Scan;
      end loop;

      --  Skip ':'.
      Expect_Scan (Tok_Colon);

      Subnature := Parse_Subnature_Indication;

      Terminal := First;
      while Terminal /= Null_Iir loop
         -- Type definitions are factorized.  This is OK, but not done by
         -- sem.
         if Terminal = First then
            Set_Subnature_Indication (Terminal, Subnature);
         else
            Set_Subnature_Indication (Terminal, Null_Iir);
         end if;
         Terminal := Get_Chain (Terminal);
      end loop;

      --  Skip ';'.
      Scan_Semi_Colon_Declaration ("terminal declaration");

      return First;
   end Parse_Terminal_Declaration;

   --  precond : SPECTRUM
   --
   --  AMS-LRM17 6.4.2.7 Quantity declarations
   --  source_aspect ::=
   --     SPECTRUM magnitude_simple_expression , phase_simple_expression
   --   | NOISE power_simple_expression
   function Parse_Source_Quantity_Declaration
     (Old : Iir; Parent : Iir; Kind : Iir_Kinds_Source_Quantity_Declaration)
     return Iir
   is
      Object : Iir;
      New_Object : Iir;
      First, Last : Iir;
   begin
      --  Change declarations
      Object := Old;
      Chain_Init (First, Last);
      while Object /= Null_Iir loop
         New_Object := Create_Iir (Kind);
         Location_Copy (New_Object, Object);
         Set_Identifier (New_Object, Get_Identifier (Object));
         Set_Subtype_Indication (New_Object, Get_Subtype_Indication (Object));
         Set_Parent (New_Object, Parent);
         Set_Has_Identifier_List
           (New_Object, Get_Has_Identifier_List (Object));

         Chain_Append (First, Last, New_Object);

         New_Object := Get_Chain (Object);
         Free_Iir (Object);
         Object := New_Object;
      end loop;

      --  Skip 'spectrum'/'noise'
      Scan;

      case Kind is
         when Iir_Kind_Spectrum_Quantity_Declaration =>
            Set_Magnitude_Expression (First, Parse_Expression);

            Expect_Scan (Tok_Comma);

            Set_Phase_Expression (First, Parse_Expression);
         when Iir_Kind_Noise_Quantity_Declaration =>
            Set_Power_Expression (First, Parse_Expression);
      end case;

      return First;
   end Parse_Source_Quantity_Declaration;

   --  precond : QUANTITY
   --  postcond: next token.
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
      Chain_Init (First, Last);

      --  Eat 'quantity'
      Scan;

      loop
         --  Quantity or "," was just scanned.  We assume a free quantity
         --  declaration and will change to branch or source quantity if
         --  necessary.
         Object := Create_Iir (Iir_Kind_Free_Quantity_Declaration);

         Scan_Identifier (Object);

         Set_Parent (Object, Parent);

         Chain_Append (First, Last, Object);

         exit when Current_Token /= Tok_Comma;

         --  Eat ','
         Scan;

         Set_Has_Identifier_List (Object, True);
      end loop;

      case Current_Token is
         when Tok_Colon =>
            --  Either a free quantity (or a source quantity)
            --  TODO

            --  Skip ':'.
            Scan;

            Set_Subtype_Indication (First, Parse_Subtype_Indication);

            case Current_Token is
               when Tok_Spectrum =>
                  First := Parse_Source_Quantity_Declaration
                    (First, Parent, Iir_Kind_Spectrum_Quantity_Declaration);
               when Tok_Noise =>
                  First := Parse_Source_Quantity_Declaration
                    (First, Parent, Iir_Kind_Noise_Quantity_Declaration);
               when Tok_Assign =>
                  --  Skip ':='.
                  Scan;

                  Set_Default_Value (First, Parse_Expression);
               when others =>
                  null;
            end case;
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
                  Skip_Until_Semi_Colon;
                  return Null_Iir;
            end case;

            --  Eat across/through
            Scan;

            --  Change declarations
            Object := First;
            Chain_Init (First, Last);
            while Object /= Null_Iir loop
               New_Object := Create_Iir (Kind);
               Location_Copy (New_Object, Object);
               Set_Identifier (New_Object, Get_Identifier (Object));
               Set_Parent (New_Object, Parent);
               Set_Tolerance (New_Object, Tolerance);
               Set_Default_Value (New_Object, Default_Value);
               Set_Has_Identifier_List
                 (New_Object, Get_Has_Identifier_List (Object));

               Chain_Append (First, Last, New_Object);

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
                  if Get_Kind (First) = Iir_Kind_Through_Quantity_Declaration
                  then
                     Error_Msg_Parse ("terminal aspect expected");
                  end if;

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
                     Set_Has_Identifier_List (Last, True);
                     Chain_Append (First, Last, Object);
                     exit when Current_Token /= Tok_Comma;
                     --  Skip ','.
                     Scan;

                     Object := Create_Iir
                       (Iir_Kind_Through_Quantity_Declaration);
                     Scan_Identifier (Object);
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

            Set_Plus_Terminal_Name (First, Plus_Terminal);

            --  Parse minus terminal (if present)
            if Current_Token = Tok_To then
               --  Skip 'to'.
               Scan;

               Set_Minus_Terminal_Name (First, Parse_Name);
            end if;
         when others =>
            Error_Msg_Parse ("missing type or across/throught aspect "
                               & "in quantity declaration");
            Skip_Until_Semi_Colon;
            return Null_Iir;
      end case;

      --  Skip ';'.
      Scan_Semi_Colon_Declaration ("quantity declaration");

      return First;
   end Parse_Quantity_Declaration;

   --  precond : token (CONSTANT, SIGNAL, VARIABLE, FILE)
   --  postcond: next token.
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
      Start_Loc : Location_Type;
   begin
      Chain_Init (First, Last);

      --  Object keyword was just scanned.
      Start_Loc := Get_Token_Location;
      case Current_Token is
         when Tok_Signal =>
            Kind := Iir_Kind_Signal_Declaration;

            --  Skip 'signal'.
            Scan;

         when Tok_Constant =>
            Kind := Iir_Kind_Constant_Declaration;

            --  Skip 'constant'.
            Scan;

         when Tok_File =>
            Kind := Iir_Kind_File_Declaration;

            --  Skip 'file'.
            Scan;

         when Tok_Variable =>
            Kind := Iir_Kind_Variable_Declaration;
            Shared := False;

            --  Skip 'variable'.
            Scan;

         when Tok_Shared =>
            Kind := Iir_Kind_Variable_Declaration;
            Shared := True;

            --  Skip 'shared'.
            Scan;

            Expect_Scan (Tok_Variable);
         when others =>
            raise Internal_Error;
      end case;

      loop
         --  Object or "," was just scanned.
         Object := Create_Iir (Kind);
         if Kind = Iir_Kind_Variable_Declaration then
            Set_Shared_Flag (Object, Shared);
         end if;

         --  Comments attached to the object.
         if Flag_Gather_Comments then
            Gather_Comments_Line (Object);
         end if;

         Scan_Identifier (Object);

         Set_Parent (Object, Parent);

         if Flag_Elocations then
            Create_Elocations (Object);
            Set_Start_Location (Object, Start_Loc);
         end if;

         Chain_Append (First, Last, Object);

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
         Set_Has_Identifier_List (Object, True);
      end loop;

      --  Skip ':'.
      Expect_Scan (Tok_Colon);

      --  Skip unexpected mode, this could happen when the interface is
      --  copied.
      case Current_Token is
         when Tok_In | Tok_Out | Tok_Inout | Tok_Buffer | Tok_Linkage =>
            Error_Msg_Parse ("mode not allowed in object declaration");

            --  Skip mode.
            Scan;
         when others =>
            null;
      end case;

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
                  if Flags.Vhdl_Std /= Vhdl_87
                    and then not Flags.Flag_Relaxed_Files87
                  then
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
         Object := Get_Chain (Object);
      end loop;

      --  Skip ';'.
      Scan_Semi_Colon_Declaration ("object declaration");

      return First;
   end Parse_Object_Declaration;

   --  precond : COMPONENT
   --  postcond: next token.
   --
   --  [ LRM93 4.5 ]
   --  component_declaration ::=
   --      COMPONENT identifier [ IS ]
   --          [ LOCAL_generic_clause ]
   --          [ LOCAL_port_clause ]
   --      END COMPONENT [ COMPONENT_simple_name ] ;
   function Parse_Component_Declaration return Iir_Component_Declaration
   is
      Component : Iir_Component_Declaration;
   begin
      Component := Create_Iir (Iir_Kind_Component_Declaration);
      if Flag_Elocations then
         Create_Elocations (Component);
         Set_Start_Location (Component, Get_Token_Location);
      end if;

      --  Eat 'component'.
      pragma Assert (Current_Token = Tok_Component);
      Scan;

      Scan_Identifier (Component);

      if Current_Token = Tok_Is then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse ("""is"" keyword is not allowed here by vhdl 87");
         end if;
         Set_Has_Is (Component, True);

         --  Eat 'is'.
         Scan;
      end if;
      Parse_Generic_Port_Clauses (Component);

      if Flag_Elocations then
         Set_End_Location (Component, Get_Token_Location);
      end if;

      Check_End_Name (Tok_Component, Component);

      --  Skip ';'.
      Expect_Scan (Tok_Semi_Colon);

      return Component;
   end Parse_Component_Declaration;

   --  precond : '['
   --  postcond: next token after ']'
   --
   --  [ LRM93 2.3.2 ]
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
         loop
            Append_Element (List, Parse_Type_Mark (Check_Paren => True));
            exit when Current_Token /= Tok_Comma;

            --  Skip ','.
            Scan;
         end loop;
         Set_Type_Marks_List (Res, List_To_Flist (List));
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
   --  postcond: next token
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
      Start_Loc : Location_Type;
   begin
      Start_Loc := Get_Token_Location;

      --  Skip 'alias'.
      pragma Assert (Current_Token = Tok_Alias);
      Scan;

      Res := Create_Iir (Iir_Kind_Object_Alias_Declaration);
      Set_Location (Res);

      case Current_Token is
         when Tok_Identifier
           | Tok_Character =>
            Ident := Current_Identifier;

            --  Skip identifier/character.
            Scan;
         when Tok_String =>
            Ident := Scan_To_Operator_Name (Get_Token_Location);

            --  Skip operator.
            Scan;
            --  FIXME: vhdl87
            --  FIXME: operator symbol.
         when others =>
            Error_Msg_Parse ("alias designator expected");
            Ident := Null_Identifier;
      end case;
      Set_Identifier (Res, Ident);

      if Current_Token = Tok_Colon then
         --  Skip ':'.
         Scan;
         Set_Subtype_Indication (Res, Parse_Subtype_Indication);
      end if;

      if Current_Token = Tok_Assign then
         Error_Msg_Parse ("alias shall be defined with 'is', not ':='");
         Scan;
      else
         Expect_Scan (Tok_Is);
      end if;
      Set_Name (Res, Parse_Signature_Name);

      if Flag_Elocations then
         Create_Elocations (Res);
         Set_Start_Location (Res, Start_Loc);
      end if;

      --  Skip ';'.
      Scan_Semi_Colon_Declaration ("alias declaration");

      return Res;
   end Parse_Alias_Declaration;

   --  precond : FOR
   --  postcond: next token.
   --
   --  [ LRM93 5.2 ]
   --  configuration_specification ::=
   --      FOR component_specification binding_indication ;
   function Parse_Configuration_Specification
     return Iir_Configuration_Specification
   is
      Res : Iir_Configuration_Specification;
   begin
      Res := Create_Iir (Iir_Kind_Configuration_Specification);
      Set_Location (Res);

      --  Eat 'for'.
      Expect_Scan (Tok_For);

      Parse_Component_Specification (Res);
      Set_Binding_Indication (Res, Parse_Binding_Indication);

      --  Skip ';'.
      Scan_Semi_Colon_Declaration ("configuration specification");

      return Res;
   end Parse_Configuration_Specification;

   --  precond : next token
   --  postcond: next token
   --
   --  [ LRM93 5.2 ]
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
            Error_Msg_Parse ("%t is not a entity class", +Current_Token);
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
   --  [ LRM93 5.1 ]
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
            return Create_Error_Node;
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
   --  [ LRM93 5.1 ]
   --  entity_name_list ::= entity_designator { , entity_designator }
   --                     | OTHERS
   --                     | ALL
   procedure Parse_Entity_Name_List
     (Attribute : Iir_Attribute_Specification)
   is
      List : Iir_List;
      Flist : Iir_Flist;
      El : Iir;
   begin
      case Current_Token is
         when Tok_All =>
            Flist := Iir_Flist_All;

            --  Skip 'all'.
            Scan;

         when Tok_Others =>
            Flist := Iir_Flist_Others;

            --  Skip 'others'.
            Scan;

         when others =>
            List := Create_Iir_List;
            loop
               El := Parse_Entity_Designator;
               Append_Element (List, El);
               exit when Current_Token /= Tok_Comma;
               Scan;
            end loop;
            Flist := List_To_Flist (List);
      end case;
      Set_Entity_Name_List (Attribute, Flist);
      if Current_Token = Tok_Colon then
         Scan;
         Set_Entity_Class (Attribute, Parse_Entity_Class);
      else
         Error_Msg_Parse
           ("missing ':' and entity kind in attribute specification");
      end if;
   end Parse_Entity_Name_List;

   --  precond : ATTRIBUTE
   --  postcond: next token
   --
   --  [ 4.4 ]
   --  attribute_declaration ::= ATTRIBUTE identifier : type_mark ;
   --
   --  [ 5.1 ]
   --  attribute_specification ::=
   --     ATTRIBUTE attribute_designator OF entity_specification
   --       IS expression ;
   --
   --  entity_specification ::= entity_name_list : entity_class
   --
   function Parse_Attribute return Iir
   is
      Ident : Name_Id;
      Res : Iir;
      Designator : Iir;
      Loc, Start_Loc : Location_Type;
   begin
      Start_Loc := Get_Token_Location;

      --  Eat 'attribute'.
      pragma Assert (Current_Token = Tok_Attribute);
      Scan;

      Loc := Get_Token_Location;
      if Current_Token = Tok_Identifier then
         Ident := Current_Identifier;

         --  Skip identifier.
         Scan;
      else
         Expect (Tok_Identifier);
         Ident := Null_Identifier;
      end if;

      case Current_Token is
         when Tok_Colon =>
            Res := Create_Iir (Iir_Kind_Attribute_Declaration);
            Set_Location (Res, Loc);
            Set_Identifier (Res, Ident);

            --  Skip ':'.
            Scan;

            Set_Type_Mark (Res, Parse_Type_Mark (Check_Paren => True));
            Scan_Semi_Colon_Declaration ("attribute declaration");

         when Tok_Of =>
            Res := Create_Iir (Iir_Kind_Attribute_Specification);
            Set_Location (Res, Loc);
            Designator := Create_Iir (Iir_Kind_Simple_Name);
            Set_Location (Designator, Loc);
            Set_Identifier (Designator, Ident);
            Set_Attribute_Designator (Res, Designator);

            --  Skip 'of'.
            Scan;

            Parse_Entity_Name_List (Res);

            --  Skip 'is'.
            Expect_Scan (Tok_Is);

            Set_Expression (Res, Parse_Expression);
            Scan_Semi_Colon_Declaration ("attribute specification");

         when others =>
            Error_Msg_Parse ("':' or 'of' expected after identifier");
            return Null_Iir;
      end case;

      if Flag_Elocations then
         Create_Elocations (Res);
         Set_Start_Location (Res, Start_Loc);
      end if;

      return Res;
   end Parse_Attribute;

   --  precond : GROUP
   --  postcond: ';'
   --
   --  [ LRM93 4.6 ]
   --  group_template_declaration ::=
   --     GROUP identifier IS (entity_class_entry_list) ;
   --
   --  entity_class_entry_list ::= entity_class_entry { , entity_class_entry }
   --
   --  entity_class_entry ::= entity_class [ <> ]
   function Parse_Group return Iir
   is
      Loc : Location_Type;
      Ident : Name_Id;
   begin
      --  Skip 'group'.
      pragma Assert (Current_Token = Tok_Group);
      Scan;

      Loc := Get_Token_Location;
      if Current_Token = Tok_Identifier then
         Ident := Current_Identifier;

         --  Skip 'group'.
         Scan;
      else
         Ident := Null_Identifier;
         Expect (Tok_Identifier);
      end if;

      case Current_Token is
         when Tok_Is =>
            declare
               Res : Iir_Group_Template_Declaration;
               El : Iir_Entity_Class;
               First, Last : Iir_Entity_Class;
            begin
               Res := Create_Iir (Iir_Kind_Group_Template_Declaration);
               Set_Location (Res, Loc);
               Set_Identifier (Res, Ident);

               --  Skip 'is'.
               Scan;

               --  Skip '('.
               Expect_Scan (Tok_Left_Paren);

               Chain_Init (First, Last);
               loop
                  Chain_Append (First, Last, Parse_Entity_Class_Entry);
                  if Current_Token = Tok_Box then
                     El := Create_Iir (Iir_Kind_Entity_Class);
                     Set_Location (El);
                     Set_Entity_Class (El, Tok_Box);
                     Chain_Append (First, Last, El);

                     --  Skip '<>'.
                     Scan;

                     if Current_Token = Tok_Comma then
                        Error_Msg_Parse
                          ("'<>' is allowed only for the last "
                            & "entity class entry");
                     end if;
                  end if;
                  exit when Current_Token /= Tok_Comma;

                  --  Skip ','.
                  Scan;
               end loop;
               Set_Entity_Class_Entry_Chain (Res, First);

               --  Skip ')' ';'
               Expect_Scan (Tok_Right_Paren);
               Scan_Semi_Colon_Declaration ("group template");

               return Res;
            end;
         when Tok_Colon =>
            declare
               Res : Iir_Group_Declaration;
               List : Iir_List;
            begin
               Res := Create_Iir (Iir_Kind_Group_Declaration);
               Set_Location (Res, Loc);
               Set_Identifier (Res, Ident);

               --  Skip ':'.
               Scan;

               Set_Group_Template_Name
                 (Res, Parse_Name (Allow_Indexes => False));

               --  Skip '('.
               Expect_Scan (Tok_Left_Paren);

               List := Create_Iir_List;
               loop
                  Append_Element (List, Parse_Name (Allow_Indexes => False));
                  exit when Current_Token /= Tok_Comma;

                  --  Skip ','.
                  Scan;
               end loop;

               --  Skip ')' ';'.
               Expect_Scan (Tok_Right_Paren);
               Scan_Semi_Colon_Declaration ("group declaration");

               Set_Group_Constituent_List (Res, List_To_Flist (List));
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
   --  LRM93 5.4
   --  signal_list ::= signal_name { , signal_name }
   --                | OTHERS
   --                | ALL
   --
   --  AMS-LRM17 7.5 Step limit specification
   --  quantity_list ::=
   --      quantity_name { , quantity_name }
   --    | others
   --    | all
   function Parse_Name_List return Iir_Flist
   is
      Res : Iir_List;
   begin
      case Current_Token is
         when Tok_Others =>
            --  Skip 'others'.
            Scan;

            return Iir_Flist_Others;

         when Tok_All =>
            --  Skip 'all'.
            Scan;

            return Iir_Flist_All;

         when others =>
            Res := Create_Iir_List;
            loop
               Append_Element (Res, Parse_Name);
               exit when Current_Token /= Tok_Comma;

               --  Skip ','
               Scan;
            end loop;

            return List_To_Flist (Res);
      end case;
   end Parse_Name_List;

   --  precond : DISCONNECT
   --  postcond: next token.
   --
   --  [ LRM93 5.4 ]
   --  disconnection_specification ::=
   --      DISCONNECT guarded_signal_specification AFTER time_expression ;
   function Parse_Disconnection_Specification
     return Iir_Disconnection_Specification
   is
      Res : Iir_Disconnection_Specification;
   begin
      pragma Assert (Current_Token = Tok_Disconnect);

      Res := Create_Iir (Iir_Kind_Disconnection_Specification);
      Set_Location (Res);

      --  Skip 'disconnect'
      Scan;

      Set_Signal_List (Res, Parse_Name_List);

      --  Skip ':'
      Expect_Scan (Tok_Colon);

      Set_Type_Mark (Res, Parse_Type_Mark (Check_Paren => True));

      --  Skip 'after'
      Expect_Scan (Tok_After);

      Set_Expression (Res, Parse_Expression);

      --  Skip ';'.
      Scan_Semi_Colon_Declaration ("disconnection specification");

      return Res;
   end Parse_Disconnection_Specification;

   --  precond : LIMIT
   --  postcond: next token.
   --
   --  AMS-LRM17 7.5 Step limit specification
   --  step_limit_specification ::=
   --      LIMIT quantity_specification WITH real_expression ;
   function Parse_Step_Limit_Specification return Iir
   is
      Res : Iir;
   begin
      pragma Assert (Current_Token = Tok_Limit);

      Res := Create_Iir (Iir_Kind_Step_Limit_Specification);
      Set_Location (Res);

      --  Skip 'limit'
      Scan;

      Set_Quantity_List (Res, Parse_Name_List);

      --  Skip ':'
      Expect_Scan (Tok_Colon);

      Set_Type_Mark (Res, Parse_Type_Mark (Check_Paren => True));

      --  Skip 'with'
      Expect_Scan (Tok_With);

      Set_Expression (Res, Parse_Expression);

      --  Skip ';'.
      Scan_Semi_Colon_Declaration ("step limit specification");

      return Res;
   end Parse_Step_Limit_Specification;

   --  Parse PSL clock_declaration at 'clock'.
   function Parse_Psl_Default_Clock_Cont
     (Loc : Location_Type; Flag_Psl : Boolean) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Psl_Default_Clock);
      Set_Location (Res, Loc);
      Xrefs.Xref_Keyword (Get_Token_Location);

      --  Recognize PSL keywords.
      Vhdl.Scanner.Flag_Psl := True;

      --  Skip 'clock'.
      Expect_Scan (Tok_Psl_Clock);

      --  Skip 'is'.
      Expect_Scan (Tok_Is);

      Set_Psl_Boolean (Res, Parse_Psl.Parse_Psl_Boolean);

      Vhdl.Scanner.Flag_Scan_In_Comment := False;
      Vhdl.Scanner.Flag_Psl := Flag_Psl;

      Expect_Scan (Tok_Semi_Colon);

      return Res;
   end Parse_Psl_Default_Clock_Cont;

   --  1850-2005 A.4.2 PSL declarations
   --  clock_declaration ::= DEFAULT CLOCK IS clock_expression ;
   function Parse_Psl_Default_Clock (Flag_Psl : Boolean) return Iir
   is
      Loc : Location_Type;
   begin
      Loc := Get_Token_Location;

      --  Recognize PSL keywords.
      Vhdl.Scanner.Flag_Psl := True;

      --  Skip 'default'.
      Scan;

      return Parse_Psl_Default_Clock_Cont (Loc, Flag_Psl);
   end Parse_Psl_Default_Clock;

   function Parse_Psl_Declaration return Iir
   is
      Tok : constant Token_Type := Current_Token;
      Loc : constant Location_Type := Get_Token_Location;
      Res : Iir;
      Decl : PSL_Node;
      Id : Name_Id;
   begin
      --  Skip 'property', 'sequence' or 'endpoint'.
      Scan;

      if Current_Token /= Tok_Identifier then
         Error_Msg_Parse ("declaration name expected here");
         Id := Null_Identifier;
      else
         Id := Current_Identifier;
      end if;

      --  Parse PSL declaration.
      Vhdl.Scanner.Flag_Psl := True;
      Decl := Parse_Psl.Parse_Psl_Declaration (Tok);
      Vhdl.Scanner.Flag_Scan_In_Comment := False;
      Vhdl.Scanner.Flag_Psl := False;

      Expect_Scan (Tok_Semi_Colon);

      if Tok = Tok_Psl_Endpoint
        and then Parse_Psl.Is_Instantiated_Declaration (Decl)
      then
         --  Instantiated endpoint: make it visible from VHDL.
         Res := Create_Iir (Iir_Kind_Psl_Endpoint_Declaration);
      else
         --  Otherwise, it will be visible only from PSL.
         Res := Create_Iir (Iir_Kind_Psl_Declaration);
      end if;
      Set_Location (Res, Loc);
      Set_Identifier (Res, Id);
      Set_Psl_Declaration (Res, Decl);

      return Res;
   end Parse_Psl_Declaration;

   --  precond : next token
   --  postcond: next token
   --
   --  [ LRM08 3.2.3 Entity declarative part ]
   --  entity_declarative_item ::=
   --       subprogram_declaration
   --     | subprogram_body
   --     | subprogram_instantiation_declaration
   --     | package_declaration
   --     | package_body
   --     | package_instantiation_declaration
   --     | type_declaration
   --     | subtype_declaration
   --     | constant_declaration
   --     | signal_declaration
   --     | shared_variable_declaration
   --     | file_declaration
   --     | alias_declaration
   --     | attribute_declaration
   --     | attribute_specification
   --     | disconnection_specification
   --     | use_clause
   --     | group_template_declaration
   --     | group_declaration
   --     | PSL_property_declaration
   --     | PSL_sequence_declaration
   --     | PSL_clock_declaration
   --
   --  [ LRM08 3.3.2 Architecture declarative part ]
   --  block_declarative_item ::=
   --       subprogram_declaration
   --     | subprogram_body
   --     | subprogram_instantiation_declaration
   --     | package_declaration
   --     | package_body
   --     | package_instantiation_declaration
   --     | type_declaration
   --     | subtype_declaration
   --     | constant_declaration
   --     | signal_declaration
   --     | shared_variable_declaration
   --     | file_declaration
   --     | alias_declaration
   --     | component_declaration
   --     | attribute_declaration
   --     | attribute_specification
   --     | configuration_specification
   --     | disconnection_specification
   --     | use_clause
   --     | group_template_declaration
   --     | group_declaration
   --     | PSL_property_declaration
   --     | PSL_sequence_declaration
   --     | PSL_clock_declaration
   --
   --  [ LRM08 4.3 Subprogram bodies ]
   --  subprogram_declarative_item ::=
   --       subprogram_declaration
   --     | subprogram_body
   --     | subprogram_instantiation_declaration
   --     | package_declaration
   --     | package_body
   --     | package_instantiation_declaration
   --     | type_declaration
   --     | subtype_declaration
   --     | constant_declaration
   --     | variable_declaration
   --     | file_declaration
   --     | alias_declaration
   --     | attribute_declaration
   --     | attribute_specification
   --     | use_clause
   --     | group_template_declaration
   --     | group_declaration
   --
   --  [ LRM08 4.7 Package declarations ]
   --  package_declarative_item ::=
   --       subprogram_declaration
   --     | subprogram_instantiation_declaration
   --     | package_declaration
   --     | package_instantiation_declaration
   --     | type_declaration
   --     | subtype_declaration
   --     | constant_declaration
   --     | signal_declaration
   --     | variable_declaration
   --     | file_declaraton
   --     | alias_declaration
   --     | component_declaration
   --     | attribute_declaration
   --     | attribute_specification
   --     | disconnection_specification
   --     | use_clause
   --     | group_template_declaration
   --     | group_declaration
   --     | PSL_property_declaration
   --     | PSL_sequence_declaration
   --
   --  [ LRM08 4.8 Package bodies ]
   --  package_body_declarative_item ::=
   --       subprogram_declaration
   --     | subprogram_body
   --     | subprogram_instantiation_declaration
   --     | package_declaration
   --     | package_body
   --     | package_instantiation_declaration
   --     | type_declaration
   --     | subtype_declaration
   --     | constant_declaration
   --     | variable_declaration
   --     | file_declaration
   --     | alias_declaration
   --     | attribute_declaration
   --     | attribute_specification
   --     | use_clause
   --     | group_template_declaration
   --     | group_declaration
   --
   --  [ LRM08 5.6.2 Protected type declarations ]
   --  protected_type_declarative_item ::=
   --       subprogram_declaration
   --     | subprogram_instantiation_declaration
   --     | attribute_specification
   --     | use_clause
   --
   --  [ LRM08 5.6.3 Protected type bodies ]
   --  protected_type_body_declarative_item ::=
   --       subprogram_declaration
   --     | subprogram_body
   --     | subprogram_instantiation_declaration
   --     | package_declaration
   --     | package_body
   --     | package_instantiation_declaration
   --     | type_declaration
   --     | subtype_declaration
   --     | constant_declaration
   --     | variable_declaration
   --     | file_declaration
   --     | alias_declaration
   --     | attribute_declaration
   --     | attribute_specification
   --     | use_clause
   --     | group_template_declaration
   --     | group_declaration
   --
   --  [ LRM08 11.3 Process statement ]
   --  process_declarative_item ::=
   --       subprogram_declaration
   --     | subprogram_body
   --     | subprogram_instantiation_declaration
   --     | package_declaration
   --     | package_body
   --     | package_instantiation_declaration
   --     | type_declaration
   --     | subtype_declaration
   --     | constant_declaration
   --     | variable_declaration
   --     | file_declaration
   --     | alias_declaration
   --     | attribute_declaration
   --     | attribute_specification
   --     | use_clause
   --     | group_template_declaration
   --     | group_declaration
   --
   --  Some declarations are not allowed in every declarative part:
   --  - subprogram_body, package_body:
   --    not in package_declaration
   --  - signal_declaration, disconnection_specification:
   --    not in process, protected_type_body, package_body, subprogram
   --  - variable_declaration:
   --    shared in entity, block (*)
   --    not shared in subprogram, protected_type_body, process
   --    depends on parent for package and package_body
   --  - component_declaration:
   --    not in entity, subprogram, package_body, protected_type_body,
   --       process
   --    depends on parent for package
   --  - configuration_specification:
   --    not in entity, subprogram, package, package_body, protected_type_body,
   --       process
   --  - PSL_property_declaration, PSL_sequence_declaration:
   --    in entity and block (*)
   --    depends on parent for package
   --  - PSL_clock_declaration:
   --    in block (*)
   --
   --  Declarations for protected_type_declaration are handled in sem.
   --
   --  (*): block means block_declarative_item, ie: block_statement,
   --       architecture_body and generate_statement)
   --
   --  PACKAGE_PARENT is the parent for nested packages.
   function Parse_Declaration (Parent : Iir; Package_Parent : Iir) return Iir
   is
      Decl : Iir;
   begin
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
                     Error_Msg_Parse (+Decl, "protected type body not "
                                        & "allowed in package declaration");
                  when others =>
                     null;
               end case;
            end if;
         when Tok_Subtype =>
            Decl := Parse_Subtype_Declaration (Parent);
         when Tok_Nature =>
            Decl := Parse_Nature_Declaration;
         when Tok_Subnature =>
            Decl := Parse_Subnature_Declaration;
         when Tok_Terminal =>
            Decl := Parse_Terminal_Declaration (Parent);
         when Tok_Quantity =>
            Decl := Parse_Quantity_Declaration (Parent);
         when Tok_Signal =>
            --  LRM08 4.7 Package declarations
            --  For package declaration that appears in a subprogram body,
            --  a process statement, or a protected type body, [...]
            --  Moreover, it is an eror if [...] a signal declaration [...]
            --  appears as a package declarative item of such a package
            --  declaration.
            case Get_Kind (Package_Parent) is
               when Iir_Kind_Function_Body
                 | Iir_Kind_Procedure_Body =>
                  Error_Msg_Parse
                    ("signal declaration not allowed in subprogram body");
               when Iir_Kinds_Process_Statement =>
                  Error_Msg_Parse
                    ("signal declaration not allowed in process");
               when Iir_Kind_Protected_Type_Body
                 | Iir_Kind_Protected_Type_Declaration =>
                  Error_Msg_Parse
                    ("signal declaration not allowed in protected type");
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Architecture_Body
                 | Iir_Kind_Block_Statement
                 | Iir_Kind_Generate_Statement_Body
                 | Iir_Kind_Package_Declaration
                 | Iir_Kind_Package_Body
                 | Iir_Kind_Vunit_Declaration =>
                  if Get_Kind (Parent) = Iir_Kind_Package_Body then
                     Error_Msg_Parse
                       ("signal declaration not allowed in package body");
                  end if;
               when Iir_Kind_Simultaneous_Procedural_Statement =>
                  Error_Msg_Parse
                    ("signal declaration not allowed in procedural statement");
               when others =>
                  Error_Kind ("parse_declaration(1)", Package_Parent);
            end case;
            Decl := Parse_Object_Declaration (Parent);
         when Tok_Constant =>
            Decl := Parse_Object_Declaration (Parent);
         when Tok_Variable =>
            --  LRM93 4.3.1.3  Variable declarations
            --  Variable declared immediatly within entity declarations,
            --  architectures bodies, packages, packages bodies, and blocks
            --  must be shared variable.
            --  Variables declared immediatly within subprograms and
            --  processes must not be shared variables.
            --  Variables may appear in protected type bodies; such
            --  variables, which must not be shared variables, represent
            --  shared data.
            case Get_Kind (Package_Parent) is
               when Iir_Kind_Entity_Declaration
                  | Iir_Kind_Architecture_Body
                  | Iir_Kind_Block_Statement
                  | Iir_Kind_Generate_Statement_Body
                  | Iir_Kind_Package_Declaration
                  | Iir_Kind_Package_Body
                  | Iir_Kind_Protected_Type_Declaration =>
                  Error_Variable_Location (Get_Kind (Package_Parent));
               when Iir_Kind_Function_Body
                 | Iir_Kind_Procedure_Body
                 | Iir_Kinds_Process_Statement
                 | Iir_Kind_Protected_Type_Body
                 | Iir_Kind_Simultaneous_Procedural_Statement =>
                  null;
               when others =>
                  Error_Kind ("parse_declaration(2)", Package_Parent);
            end case;
            Decl := Parse_Object_Declaration (Parent);
         when Tok_Shared =>
            if Flags.Vhdl_Std <= Vhdl_87 then
               Error_Msg_Parse ("shared variable not allowed in vhdl 87");
            end if;
            --  LRM08 4.7 Package declarations
            --  For package declaration that appears in a subprogram body,
            --  a process statement, or a protected type body, it is an
            --  error if a variable declaration in the package declaratie
            --  part of the package declaration declares a shared variable.

            --  LRM08 4.8 Package bodies
            --  For a package body that appears in a subprogram body, a
            --  process statement or a protected type body, it is an error
            --  if a variable declaration in the package body declarative
            --  part of the package body declares a shared variable.

            --  LRM93 4.3.1.3  Variable declarations
            --  Variable declared immediatly within entity declarations,
            --  architectures bodies, packages, packages bodies, and blocks
            --  must be shared variable.
            --  Variables declared immediatly within subprograms and
            --  processes must not be shared variables.
            --  Variables may appear in proteted type bodies; such
            --  variables, which must not be shared variables, represent
            --  shared data.
            case Get_Kind (Package_Parent) is
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Architecture_Body
                 | Iir_Kind_Block_Statement
                 | Iir_Kind_Generate_Statement_Body
                 | Iir_Kind_Package_Declaration
                 | Iir_Kind_Package_Body
                 | Iir_Kind_Protected_Type_Declaration =>
                  null;
               when Iir_Kind_Function_Body
                 | Iir_Kind_Procedure_Body
                 | Iir_Kinds_Process_Statement
                 | Iir_Kind_Protected_Type_Body
                 | Iir_Kind_Simultaneous_Procedural_Statement =>
                  Error_Variable_Location (Get_Kind (Package_Parent));
               when others =>
                  Error_Kind ("parse_declarative_part(3)", Package_Parent);
            end case;
            Decl := Parse_Object_Declaration (Parent);
         when Tok_File =>
            Decl := Parse_Object_Declaration (Parent);
         when Tok_Function
           | Tok_Procedure
           | Tok_Pure
           | Tok_Impure =>
            Decl := Parse_Subprogram_Declaration;
            if Decl /= Null_Iir
              and then Get_Kind (Decl) in Iir_Kinds_Subprogram_Declaration
              and then Get_Subprogram_Body (Decl) /= Null_Iir
            then
               if Get_Kind (Parent) = Iir_Kind_Package_Declaration then
                  Error_Msg_Parse
                    (+Decl, "subprogram body not allowed in a package");
               end if;
            end if;
         when Tok_Alias =>
            Decl := Parse_Alias_Declaration;
         when Tok_Component =>
            case Get_Kind (Parent) is
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Procedure_Body
                 | Iir_Kind_Function_Body
                 | Iir_Kinds_Process_Statement
                 | Iir_Kind_Package_Body
                 | Iir_Kind_Protected_Type_Body
                 | Iir_Kind_Protected_Type_Declaration
                 | Iir_Kind_Simultaneous_Procedural_Statement =>
                  Error_Msg_Parse
                    ("component declaration are not allowed here");
               when Iir_Kind_Architecture_Body
                 | Iir_Kind_Block_Statement
                 | Iir_Kind_Generate_Statement_Body
                 | Iir_Kind_Package_Declaration =>
                  null;
               when others =>
                  Error_Kind ("parse_declarative_part(4)", Parent);
            end case;
            Decl := Parse_Component_Declaration;
         when Tok_For =>
            case Get_Kind (Parent) is
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Function_Body
                 | Iir_Kind_Procedure_Body
                 | Iir_Kinds_Process_Statement
                 | Iir_Kind_Package_Declaration
                 | Iir_Kind_Package_Body
                 | Iir_Kind_Protected_Type_Body
                 | Iir_Kind_Protected_Type_Declaration
                 | Iir_Kind_Simultaneous_Procedural_Statement
                 | Iir_Kind_Vunit_Declaration =>
                  Error_Msg_Parse
                    ("configuration specification not allowed here");
               when Iir_Kind_Architecture_Body
                 | Iir_Kind_Block_Statement
                 | Iir_Kind_Generate_Statement_Body =>
                  null;
               when others =>
                  Error_Kind ("parse_declarative_part(5)", Parent);
            end case;
            Decl := Parse_Configuration_Specification;
         when Tok_Attribute =>
            Decl := Parse_Attribute;
         when Tok_Disconnect =>
            --  LRM08 4.7 Package declarations
            --  For package declaration that appears in a subprogram body,
            --  a process statement, or a protected type body, [...]
            --  Moreover, it is an eror if [...] a disconnection
            --  specification [...] appears as a package declarative item
            --  of such a package declaration.
            case Get_Kind (Parent) is
               when Iir_Kind_Function_Body
                 | Iir_Kind_Procedure_Body
                 | Iir_Kinds_Process_Statement
                 | Iir_Kind_Protected_Type_Body
                 | Iir_Kind_Package_Body
                 | Iir_Kind_Protected_Type_Declaration
                 | Iir_Kind_Simultaneous_Procedural_Statement =>
                  Error_Msg_Parse
                    ("disconnect specification not allowed here");
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Architecture_Body
                 | Iir_Kind_Block_Statement
                 | Iir_Kind_Generate_Statement_Body
                 | Iir_Kind_Package_Declaration =>
                  null;
               when others =>
                  Error_Kind ("parse_declaration(6)", Parent);
            end case;
            Decl := Parse_Disconnection_Specification;
         when Tok_Limit =>
            Decl := Parse_Step_Limit_Specification;
         when Tok_Use =>
            Decl := Parse_Use_Clause;
         when Tok_Group =>
            Decl := Parse_Group;
         when Tok_Package =>
            Check_Vhdl_At_Least_2008 ("nested package");
            Decl := Parse_Package (Parent);
            if Decl /= Null_Iir
              and then Get_Kind (Decl) = Iir_Kind_Package_Body
            then
               if Get_Kind (Parent) = Iir_Kind_Package_Declaration then
                  Error_Msg_Parse
                    (+Decl, "package body not allowed in a package");
               end if;
            end if;

            if Current_Token = Tok_Semi_Colon then
               --  Skip ';'.
               Scan;
            end if;
         when Tok_Default =>
            --  This identifier is a PSL keyword.
            Xrefs.Xref_Keyword (Get_Token_Location);

            --  Check whether default clock are allowed in this region.
            case Get_Kind (Parent) is
               when Iir_Kind_Function_Body
                 | Iir_Kind_Procedure_Body
                 | Iir_Kinds_Process_Statement
                 | Iir_Kind_Protected_Type_Body
                 | Iir_Kind_Package_Declaration
                 | Iir_Kind_Package_Body
                 | Iir_Kind_Protected_Type_Declaration
                 | Iir_Kind_Simultaneous_Procedural_Statement =>
                  Error_Msg_Parse
                    ("PSL default clock declaration not allowed here");
               when Iir_Kind_Entity_Declaration
                  | Iir_Kind_Architecture_Body
                  | Iir_Kind_Block_Statement
                  | Iir_Kind_Generate_Statement_Body
                  | Iir_Kinds_Verification_Unit =>
                  null;
               when others =>
                  Error_Kind ("parse_declaration(7)", Parent);
            end case;
            Decl := Parse_Psl_Default_Clock (False);
         when Tok_Identifier =>
            Error_Msg_Parse
              ("object class keyword such as 'variable' is expected");
            Resync_To_End_Of_Declaration;
         when Tok_Semi_Colon =>
            Error_Msg_Parse ("';' (semi colon) not allowed alone");
            Scan;
         when Tok_Is =>
            Error_Msg_Parse ("duplicate 'is' in declarative part");
            Scan;
         when others =>
            null;
      end case;
      return Decl;
   end Parse_Declaration;

   procedure Parse_Declarative_Part (Parent : Iir; Package_Parent : Iir)
   is
      Last_Decl : Iir;
      Decl : Iir;
   begin
      Last_Decl := Null_Iir;
      loop
         Decl := Parse_Declaration (Parent, Package_Parent);
         exit when Decl = Null_Iir;
         loop
            Set_Parent (Decl, Parent);
            if Last_Decl = Null_Iir then
               Set_Declaration_Chain (Parent, Decl);
            else
               Set_Chain (Last_Decl, Decl);
            end if;
            Last_Decl := Decl;
            Decl := Get_Chain (Decl);
            exit when Decl = Null_Iir;
         end loop;
      end loop;
   end Parse_Declarative_Part;

   --  precond : ENTITY
   --  postcond: next token.
   --
   --  [ LRM93 1.1 ]
   --  entity_declaration ::=
   --      ENTITY identifier IS
   --          entiy_header
   --          entity_declarative_part
   --      [ BEGIN
   --          entity_statement_part ]
   --      END [ ENTITY ] [ ENTITY_simple_name ]
   --
   --  [ LRM93 1.1.1 ]
   --  entity_header ::=
   --      [ FORMAL_generic_clause ]
   --      [ FORMAL_port_clause ]
   procedure Parse_Entity_Declaration (Unit : Iir_Design_Unit)
   is
      Res: Iir_Entity_Declaration;
      Start_Loc : Location_Type;
      Begin_Loc : Location_Type;
      End_Loc : Location_Type;
   begin
      Expect (Tok_Entity);
      Res := Create_Iir (Iir_Kind_Entity_Declaration);
      Start_Loc := Get_Token_Location;

      if Flag_Elocations then
         Create_Elocations (Res);
         Set_Start_Location (Res, Start_Loc);
      end if;

      --  Skip 'entity'.
      pragma Assert (Current_Token = Tok_Entity);
      Scan;

      --  Get identifier.
      Scan_Identifier (Res);

      --  Skip 'is'.
      Expect_Scan (Tok_Is);

      --  Comments after 'entity' but before the first generic or port are
      --  attached to the entity.
      if Flag_Gather_Comments then
         Gather_Comments_Block (Res);
      end if;

      Parse_Generic_Port_Clauses (Res);

      Parse_Declarative_Part (Res, Res);

      if Current_Token = Tok_Begin then
         Begin_Loc := Get_Token_Location;
         Set_Has_Begin (Res, True);

         --  Skip 'begin'.
         Scan;

         Parse_Concurrent_Statements (Res);
      else
         Begin_Loc := No_Location;
      end if;

      --   end keyword is expected to finish an entity declaration
      End_Loc := Get_Token_Location;
      Expect_Scan (Tok_End);

      if Current_Token = Tok_Entity then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse ("'entity' keyword not allowed here by vhdl 87");
         end if;
         Set_End_Has_Reserved_Id (Res, True);
         Scan;
      end if;
      Check_End_Name (Res);
      Scan_Semi_Colon_Unit ("entity");

      Set_Library_Unit (Unit, Res);

      if Flag_Elocations then
         Set_Begin_Location (Res, Begin_Loc);
         Set_End_Location (Res, End_Loc);
      end if;
   end Parse_Entity_Declaration;

   --  [ LRM93 7.3.2 ]
   --  choice ::= simple_expression
   --           | discrete_range
   --           | ELEMENT_simple_name
   --           | OTHERS
   function Parse_A_Choice (Expr: Iir; Loc : Location_Type) return Iir
   is
      A_Choice: Iir;
      Expr1: Iir;
   begin
      if Expr = Null_Iir then
         if Current_Token = Tok_Others then
            A_Choice := Create_Iir (Iir_Kind_Choice_By_Others);
            Set_Location (A_Choice, Loc);

            --  Skip 'others'
            Scan;

            return A_Choice;
         else
            Expr1 := Parse_Expression;

            if Expr1 = Null_Iir then
               --  Handle parse error now.
               --  FIXME: skip until '=>'.
               A_Choice := Create_Iir (Iir_Kind_Choice_By_Expression);
               Set_Location (A_Choice, Loc);
               return A_Choice;
            end if;
         end if;
      else
         Expr1 := Expr;
      end if;

      if Is_Range_Attribute_Name (Expr1) then
         A_Choice := Create_Iir (Iir_Kind_Choice_By_Range);
         Set_Choice_Range (A_Choice, Expr1);
      elsif Current_Token = Tok_To or else Current_Token = Tok_Downto then
         A_Choice := Create_Iir (Iir_Kind_Choice_By_Range);
         Set_Choice_Range (A_Choice, Parse_Range_Expression (Expr1));
      elsif Current_Token = Tok_Range then
         A_Choice := Create_Iir (Iir_Kind_Choice_By_Range);
         Set_Choice_Range (A_Choice, Parse_Subtype_Indication (Expr1));
      else
         A_Choice := Create_Iir (Iir_Kind_Choice_By_Expression);
         Set_Choice_Expression (A_Choice, Expr1);
      end if;

      Set_Location (A_Choice, Loc);
      return A_Choice;
   end Parse_A_Choice;

   --  [ LRM93 7.3.2 ]
   --  choices ::= choice { | choice }
   --
   -- Leave tok_double_arrow as current token.
   procedure Parse_Choices (Expr: Iir;
                            First_Loc : Location_Type;
                            Chain : out Iir)
   is
      First, Last : Iir;
      A_Choice: Iir;
      Expr1 : Iir;
      Loc : Location_Type;
   begin
      Chain_Init (First, Last);
      Expr1 := Expr;
      Loc := First_Loc;
      loop
         A_Choice := Parse_A_Choice (Expr1, Loc);
         if First /= Null_Iir then
            Set_Same_Alternative_Flag (A_Choice, True);
            if Get_Kind (A_Choice) = Iir_Kind_Choice_By_Others then
               Error_Msg_Parse ("'others' choice must be alone");
            end if;
         end if;

         Chain_Append (First, Last, A_Choice);

         if Current_Token /= Tok_Bar then
            Chain := First;
            return;
         end if;
         Loc := Get_Token_Location;

         --  Skip '|'.
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
      Expr: Iir;
      Res: Iir;
      First, Last : Iir;
      Assoc: Iir;
      Loc, Right_Loc : Location_Type;
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
               --  This was just a braced expression.

               Right_Loc := Get_Token_Location;

               --  Skip ')'.
               Scan;

               if Expr /= Null_Iir
                 and then Get_Kind (Expr) = Iir_Kind_Aggregate
               then
                  --  Parenthesis around aggregate is useless and change the
                  --  context for array aggregate.
                  Warning_Msg_Sem
                    (Warnid_Parenthesis, +Expr,
                     "suspicious parenthesis around aggregate");
               elsif not Flag_Parse_Parenthesis then
                  return Expr;
               end if;

               --  Create a node for the parenthesis.
               Res := Create_Iir (Iir_Kind_Parenthesis_Expression);
               Set_Location (Res, Loc);
               Set_Expression (Res, Expr);

               if Flag_Elocations then
                  Create_Elocations (Res);
                  Set_Right_Paren_Location (Res, Right_Loc);
               end if;

               return Res;

            when Tok_Semi_Colon
               | Tok_Then
               | Tok_Is
               | Tok_Generate
               | Tok_Loop =>
               --  Surely a missing parenthesis.
               Error_Missing_Parenthesis(Loc);
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
      Chain_Init (First, Last);
      loop
         if Current_Token = Tok_Others and then Expr = Null_Iir then
            Assoc := Parse_A_Choice (Null_Iir, Loc);
            Expect (Tok_Double_Arrow);

            --  Eat '=>'
            Scan;

            Expr := Parse_Expression;
         else
            --  Not others: an expression (or a range).
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
                  Set_Location (Assoc, Loc);
               when others =>
                  Parse_Choices (Expr, Loc, Assoc);
                  Expect (Tok_Double_Arrow);

                  --  Eat '=>'.
                  Scan;

                  Expr := Parse_Expression;
            end case;
         end if;
         Set_Associated_Expr (Assoc, Expr);
         Chain_Append_Subchain (First, Last, Assoc);
         exit when Current_Token /= Tok_Comma;

         Loc := Get_Token_Location;

         --  Eat ','
         Scan;

         Expr := Null_Iir;
      end loop;
      Set_Association_Choices_Chain (Res, First);

      --  Eat ')'.
      Expect_Scan (Tok_Right_Paren);
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
      if Expr /= Null_Iir
        and then Get_Kind (Expr) /= Iir_Kind_Qualified_Expression
      then
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
   function Parse_Bit_String (Len : Int32) return Iir
   is
      Res : Iir;
      B : Number_Base_Type;
   begin
      Res := Create_Iir (Iir_Kind_String_Literal8);
      Set_Location (Res);
      Set_String8_Id (Res, Current_String_Id);
      Set_String_Length (Res, Current_String_Length);
      Set_Literal_Length (Res, Len + Get_Token_Length);
      case Get_Bit_String_Sign is
         when 's' =>
            Set_Has_Sign (Res, True);
            Set_Has_Signed (Res, True);
         when 'u' =>
            Set_Has_Sign (Res, True);
            Set_Has_Signed (Res, False);
         when others =>
            Set_Has_Sign (Res, False);
            Set_Has_Signed (Res, False);
      end case;

      case Get_Bit_String_Base is
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
                 (+Lit, "cannot expand an empty signed bit string");
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
                 (+Lit, "truncation of bit string changes the value");
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

   --  LRM93 3.1.3
   --  /unit/_name
   --
   --  A unit name is a name, but it must designate a unit declaration.  As
   --  a consequence, it can only be a simple_name or a selected name.
   function Parse_Unit_Name return Iir
   is
      Res : Iir;
   begin
      Res := Parse_Name (Allow_Indexes => False);
      case Get_Kind (Res) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            null;
         when others =>
            Error_Msg_Parse ("invalid unit name");
      end case;
      return Res;
   end Parse_Unit_Name;

   --  Precond : next token after tok_integer
   --  postcond: likewise
   --
   --  Return an integer_literal or a physical_literal.
   function Parse_Integer_Literal (Val : Int64; Len : Int32) return Iir
   is
      Res : Iir;
   begin
      if Current_Token = Tok_Identifier then
         -- physical literal
         Res := Create_Iir (Iir_Kind_Physical_Int_Literal);
         Set_Unit_Name (Res, Parse_Unit_Name);
      else
         -- integer literal
         Res := Create_Iir (Iir_Kind_Integer_Literal);
      end if;
      Set_Value (Res, Val);
      Set_Literal_Length (Res, Len);
      return Res;
   end Parse_Integer_Literal;

   function Parse_PSL_Builtin_Call (Kind : Iir_Kinds_Psl_Builtin) return Iir
   is
      Res : Iir;
      Expr : Iir;
   begin
      Res := Create_Iir (Kind);
      Set_Location (Res);

      --  Skip builtin.
      Scan;

      Expect_Scan (Tok_Left_Paren);

      Set_Expression (Res, Parse_Expression);

      if Current_Token = Tok_Comma then
         --  Skip ','.
         Scan;

         Expr := Parse_Expression;
         case Kind is
            when Iir_Kind_Psl_Fell
               | Iir_Kind_Psl_Rose
               | Iir_Kind_Psl_Stable =>
               Set_Clock_Expression (Res, Expr);
            when Iir_Kind_Psl_Prev =>
               Set_Count_Expression (Res, Expr);
            when others =>
               Error_Msg_Parse ("too many parameter for PSL builtin");
         end case;
      end if;

      if Current_Token = Tok_Comma then
         --  Skip ','.
         Scan;

         case Kind is
            when Iir_Kind_Psl_Prev =>
               Set_Clock_Expression (Res, Parse_Expression);
            when others =>
               Error_Msg_Parse ("too many parameter for PSL builtin");
         end case;
      end if;

      Expect_Scan (Tok_Right_Paren);

      return Res;
   end Parse_PSL_Builtin_Call;

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
      Int: Int64;
      Fp: Fp64;
      Loc: Location_Type;
      Len : Int32;
   begin
      case Current_Token is
         when Tok_Integer =>
            Int := Current_Iir_Int64;
            Loc := Get_Token_Location;
            Len := Get_Token_Length;

            --  Skip integer
            Scan;

            Res := Parse_Integer_Literal (Int, Len);
            Set_Location (Res, Loc);
            return Res;

         when Tok_Real =>
            Fp := Current_Iir_Fp64;
            Loc := Get_Token_Location;
            Len := Get_Token_Length;

            --  Skip real
            Scan;

            if Current_Token = Tok_Identifier then
               -- physical literal
               Res := Create_Iir (Iir_Kind_Physical_Fp_Literal);
               Set_Unit_Name (Res, Parse_Unit_Name);
            else
               -- real literal
               Res := Create_Iir (Iir_Kind_Floating_Point_Literal);
            end if;
            Set_Location (Res, Loc);
            Set_Fp_Value (Res, Fp);
            Set_Literal_Length (Res, Len);
            return Res;

         when Tok_Identifier
           | Tok_Double_Less =>
            Res := Parse_Name (Allow_Indexes => True);
            if Res /= Null_Iir
              and then Get_Kind (Res) = Iir_Kind_Signature
            then
               Error_Msg_Parse (+Res, "signature not allowed in expression");
               return Get_Signature_Prefix (Res);
            else
               return Res;
            end if;

         when Tok_Character =>
            Res := Create_Iir (Iir_Kind_Character_Literal);
            Set_Identifier (Res, Current_Identifier);
            Set_Location (Res);

            --  Skip character.
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
            if Parenthesis_Depth = Max_Parenthesis_Depth then
               Error_Msg_Parse
                 ("too many open parenthesis, skip to the matching one");
               Skip_Until_Closing_Parenthesis;
               return Null_Iir;
            else
               Parenthesis_Depth := Parenthesis_Depth + 1;
               Res := Parse_Aggregate;
               Parenthesis_Depth := Parenthesis_Depth - 1;
               return Res;
            end if;
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
            Len := Get_Token_Length;

            --  Skip integer
            Scan;

            if Current_Token = Tok_Bit_String then
               Res := Parse_Bit_String (Len);
               Set_Has_Length (Res, True);

               --  Skip bit string
               Scan;

               --  Resize.
               if Int > 2048 then
                  --  What is a reasonable limit ?
                  Error_Msg_Parse
                    (Get_Token_Location,
                     "bit string size is too large (> 2048)");
               else
                  Resize_Bit_String (Res, Nat32 (Int));
               end if;
            else
               Error_Msg_Parse
                 (Get_Token_Location,
                  "space is required between number and unit name");
               Res := Parse_Integer_Literal (Int, Len);
            end if;
            Set_Location (Res, Loc);
            return Res;

         when Tok_Bit_String =>
            Res := Parse_Bit_String (0);

            --  Skip bit string
            Scan;

            return Res;

         when Tok_Prev =>
            return Parse_PSL_Builtin_Call (Iir_Kind_Psl_Prev);
         when Tok_Stable =>
            return Parse_PSL_Builtin_Call (Iir_Kind_Psl_Stable);
         when Tok_Rose =>
            return Parse_PSL_Builtin_Call (Iir_Kind_Psl_Rose);
         when Tok_Fell =>
            return Parse_PSL_Builtin_Call (Iir_Kind_Psl_Fell);
         when Tok_Onehot =>
            return Parse_PSL_Builtin_Call (Iir_Kind_Psl_Onehot);
         when Tok_Onehot0 =>
            return Parse_PSL_Builtin_Call (Iir_Kind_Psl_Onehot0);

         when Tok_Minus
           | Tok_Plus =>
            Error_Msg_Parse
              ("'-' and '+' are not allowed in primary, use parenthesis");
            return Parse_Expression (Prio_Simple);

         when Tok_Comma
           | Tok_Semi_Colon
           | Tok_Right_Paren
           | Tok_Eof
           | Tok_End =>
            --  Token not to be skipped
            Error_Msg_Parse ("primary expression expected");
            return Create_Error_Node;

         when others =>
            Unexpected ("primary");
            return Create_Error_Node;
      end case;
   end Parse_Primary;

   --  [ LRM08 9 Expressions ]
   --
   --  expression ::=
   --      condition_operator primary
   --    | logical_expression
   --
   --  logical_expression ::=
   --      relation { and relation }
   --    | relation { or relation }
   --    | relation { xor relation }
   --    | relation [ nand relation ]
   --    | relation [ nor relation ]
   --    | relation { xnor relation }
   --
   --  relation ::=
   --    shift_expression [ relational_operator shift_expression ]
   --
   --  shift_expression ::=
   --    simple_expression [ shift_operator simple_expression ]
   --
   --  simple_expression ::=
   --    [ sign ] term { adding_operator term }
   --
   --  term ::=
   --    factor { multiplying_operator factor }
   --
   --  factor ::=
   --      primary [ ** primary ]
   --    | abs primary
   --    | not primary
   --    | logical_operator primary
   function Build_Unary_Factor (Op : Iir_Kind) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Op);
      Set_Location (Res);

      --  Skip operator.
      Scan;

      Set_Operand (Res, Parse_Primary);

      return Res;
   end Build_Unary_Factor;

   function Build_Unary_Simple (Op : Iir_Kind) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Op);
      Set_Location (Res);

      --  Skip operator.
      Scan;

      Set_Operand (Res, Parse_Expression (Prio_Term));

      return Res;
   end Build_Unary_Simple;

   function Build_Unary_Factor_08 (Op : Iir_Kind) return Iir is
   begin
      if Flags.Vhdl_Std < Vhdl_08 then
         Error_Msg_Parse ("missing left operand of logical expression");

         --  Skip operator
         Scan;

         return Parse_Primary;
      else
         return Build_Unary_Factor (Op);
      end if;
   end Build_Unary_Factor_08;

   function Parse_Unary_Expression return Iir
   is
      Res, Left : Iir_Expression;
   begin
      case Current_Token is
         when Tok_Plus =>
            return Build_Unary_Simple (Iir_Kind_Identity_Operator);
         when Tok_Minus =>
            return Build_Unary_Simple (Iir_Kind_Negation_Operator);

         when Tok_Abs =>
            return Build_Unary_Factor (Iir_Kind_Absolute_Operator);
         when Tok_Not =>
            return Build_Unary_Factor (Iir_Kind_Not_Operator);

         when Tok_And =>
            return Build_Unary_Factor_08 (Iir_Kind_Reduction_And_Operator);
         when Tok_Or =>
            return Build_Unary_Factor_08 (Iir_Kind_Reduction_Or_Operator);
         when Tok_Nand =>
            return Build_Unary_Factor_08 (Iir_Kind_Reduction_Nand_Operator);
         when Tok_Nor =>
            return Build_Unary_Factor_08 (Iir_Kind_Reduction_Nor_Operator);
         when Tok_Xor =>
            return Build_Unary_Factor_08 (Iir_Kind_Reduction_Xor_Operator);
         when Tok_Xnor =>
            return Build_Unary_Factor_08 (Iir_Kind_Reduction_Xnor_Operator);

         when Tok_Exclam_Mark =>
            Error_Msg_Parse ("'!' is not allowed here, replaced by 'not'");
            return Build_Unary_Factor (Iir_Kind_Not_Operator);

         when others =>
            Left := Parse_Primary;
            if Current_Token = Tok_Double_Star then
               Res := Create_Iir (Iir_Kind_Exponentiation_Operator);
               Set_Location (Res);

               --  Skip '**'.
               Scan;

               Set_Left (Res, Left);
               Set_Right (Res, Parse_Primary);
               return Res;
            else
               return Left;
            end if;
      end case;
   end Parse_Unary_Expression;

   --  Example: When PRIO is Prio_Simple, a simple expression will be returned.
   function Parse_Binary_Expression (Left : Iir; Prio : Prio_Type) return Iir
   is
      Res : Iir;
      Expr : Iir;
      Op : Iir_Kind;
      Op_Prio : Prio_Type;
      Op_Tok : Token_Type;
   begin
      Res := Left;
      loop
         Op_Tok := Current_Token;
         case Op_Tok is
            when Tok_Star =>
               Op := Iir_Kind_Multiplication_Operator;
               Op_Prio := Prio_Term;
            when Tok_Slash =>
               Op := Iir_Kind_Division_Operator;
               Op_Prio := Prio_Term;
            when Tok_Mod =>
               Op := Iir_Kind_Modulus_Operator;
               Op_Prio := Prio_Term;
            when Tok_Rem =>
               Op := Iir_Kind_Remainder_Operator;
               Op_Prio := Prio_Term;

            when Tok_Plus =>
               Op := Iir_Kind_Addition_Operator;
               Op_Prio := Prio_Simple;
            when Tok_Minus =>
               Op := Iir_Kind_Substraction_Operator;
               Op_Prio := Prio_Simple;
            when Tok_Ampersand =>
               Op := Iir_Kind_Concatenation_Operator;
               Op_Prio := Prio_Simple;

            when Tok_Sll =>
               Op := Iir_Kind_Sll_Operator;
               Op_Prio := Prio_Shift;
            when Tok_Sla =>
               Op := Iir_Kind_Sla_Operator;
               Op_Prio := Prio_Shift;
            when Tok_Srl =>
               Op := Iir_Kind_Srl_Operator;
               Op_Prio := Prio_Shift;
            when Tok_Sra =>
               Op := Iir_Kind_Sra_Operator;
               Op_Prio := Prio_Shift;
            when Tok_Rol =>
               Op := Iir_Kind_Rol_Operator;
               Op_Prio := Prio_Shift;
            when Tok_Ror =>
               Op := Iir_Kind_Ror_Operator;
               Op_Prio := Prio_Shift;

            when Tok_Equal =>
               Op := Iir_Kind_Equality_Operator;
               Op_Prio := Prio_Relation;
            when Tok_Not_Equal =>
               Op := Iir_Kind_Inequality_Operator;
               Op_Prio := Prio_Relation;
            when Tok_Less =>
               Op := Iir_Kind_Less_Than_Operator;
               Op_Prio := Prio_Relation;
            when Tok_Less_Equal =>
               Op := Iir_Kind_Less_Than_Or_Equal_Operator;
               Op_Prio := Prio_Relation;
            when Tok_Greater =>
               Op := Iir_Kind_Greater_Than_Operator;
               Op_Prio := Prio_Relation;
            when Tok_Greater_Equal =>
               Op := Iir_Kind_Greater_Than_Or_Equal_Operator;
               Op_Prio := Prio_Relation;
            when Tok_Match_Equal =>
               Op := Iir_Kind_Match_Equality_Operator;
               Op_Prio := Prio_Relation;
            when Tok_Match_Not_Equal =>
               Op := Iir_Kind_Match_Inequality_Operator;
               Op_Prio := Prio_Relation;
            when Tok_Match_Less =>
               Op := Iir_Kind_Match_Less_Than_Operator;
               Op_Prio := Prio_Relation;
            when Tok_Match_Less_Equal =>
               Op := Iir_Kind_Match_Less_Than_Or_Equal_Operator;
               Op_Prio := Prio_Relation;
            when Tok_Match_Greater =>
               Op := Iir_Kind_Match_Greater_Than_Operator;
               Op_Prio := Prio_Relation;
            when Tok_Match_Greater_Equal =>
               Op := Iir_Kind_Match_Greater_Than_Or_Equal_Operator;
               Op_Prio := Prio_Relation;

            when Tok_And =>
               Op := Iir_Kind_And_Operator;
               Op_Prio := Prio_Logical;
            when Tok_Or =>
               Op := Iir_Kind_Or_Operator;
               Op_Prio := Prio_Logical;
            when Tok_Xor =>
               Op := Iir_Kind_Xor_Operator;
               Op_Prio := Prio_Logical;
            when Tok_Nand =>
               Op := Iir_Kind_Nand_Operator;
               Op_Prio := Prio_Logical;
            when Tok_Nor =>
               Op := Iir_Kind_Nor_Operator;
               Op_Prio := Prio_Logical;
            when Tok_Xnor =>
               Op := Iir_Kind_Xnor_Operator;
               Op_Prio := Prio_Logical;

            when others =>
               return Res;
         end case;

         --  If the OP_PRIO is less than PRIO, the binary operator will apply
         --  to the whole expression.
         --  eg: A * B + C
         if Op_Prio < Prio then
            return Res;
         end if;

         Expr := Create_Iir (Op);
         Set_Location (Expr);
         Set_Left (Expr, Res);

         --  Skip operator.
         Scan;

         --  Catch errors for Ada programmers.
         if Current_Token = Tok_Then or Current_Token = Tok_Else then
            Report_Start_Group;
            Error_Msg_Parse ("""or else"" and ""and then"" sequences "
                               & "are not allowed in vhdl");
            Error_Msg_Parse ("""and"" and ""or"" are short-circuit "
                               & "operators for BIT and BOOLEAN types");
            Report_End_Group;
            Scan;
         end if;

         if Op_Prio >= Prio_Simple and then Current_Token in Token_Sign_Type
         then
            Error_Msg_Parse ("'-'/'+' can only appear before the first term");
         end if;

         --  Left association: A + B + C is (A + B) + C
         Set_Right (Expr, Parse_Expression (Prio_Type'Succ (Op_Prio)));
         Res := Expr;

         --  Only one relational_operator or shift_operator.
         if Op_Prio = Prio_Relation then
            if Current_Token in Token_Relational_Operator_Type then
               Error_Msg_Parse
                 ("use parenthesis for consecutive relational expressions");
            end if;
         elsif Op_Prio = Prio_Shift then
            --  Only one shift_operator.
            if Current_Token in Token_Shift_Operator_Type then
               Error_Msg_Parse
                 ("use parenthesis for consecutive shift expressions");
            end if;
         elsif Op_Prio = Prio_Logical then
            if Current_Token = Op_Tok then
               if Op_Tok = Tok_Nand or Op_Tok = Tok_Nor then
                  Report_Start_Group;
                  Error_Msg_Parse ("sequence of 'nor' or 'nand' not allowed");
                  Error_Msg_Parse ("('nor' and 'nand' are not associative)");
                  Report_End_Group;
               end if;
            elsif Current_Token in Token_Logical_Type then
               --  Expression is a sequence of relations, with the same
               --  operator.
               Error_Msg_Parse ("only one type of logical operators may be "
                                & "used to combine relation");
            end if;
         end if;
      end loop;
   end Parse_Binary_Expression;

   function Parse_Expression (Prio : Prio_Type := Prio_Expression) return Iir
   is
      Left : Iir;
      Res : Iir;
   begin
      if Current_Token = Tok_Condition then
         if Prio /= Prio_Expression then
            Error_Msg_Parse
              ("'??' must be the first operator of an expression");
         end if;
         Res := Create_Iir (Iir_Kind_Condition_Operator);
         Set_Location (Res);

         --  Skip '??'
         Scan;

         Set_Operand (Res, Parse_Primary);

         --  Improve error message for expressions like '?? a and b'; in
         --  particular it avoids cascaded errors.
         case Current_Token is
            when Token_Logical_Type
              | Token_Relational_Operator_Type
              | Token_Shift_Operator_Type
              | Token_Adding_Operator_Type =>
               Error_Msg_Parse
                 ("'??' cannot be followed by a binary expression");
               Res := Parse_Binary_Expression (Res, Prio);
            when others =>
               null;
         end case;
      else
         Left := Parse_Unary_Expression;
         Res := Parse_Binary_Expression (Left, Prio);
      end if;

      return Res;
   end Parse_Expression;

   --  Like Parse_Expression, but assumed the expression is followed by a
   --  reserved identifier.  As a result, it will diagnoses extra parentheses.
   function Parse_Expression_Keyword return Iir
   is
      Res : Iir;
   begin
      Res := Parse_Expression;

      if Current_Token = Tok_Right_Paren then
         Error_Msg_Parse ("extra ')' ignored");

         --  Skip ')'.
         Scan;
      end if;

      return Res;
   end Parse_Expression_Keyword;

   --  precond : next token
   --  postcond: next token.
   --
   --  [ 8.4 ]
   --  waveform ::= waveform_element { , waveform_element }
   --             | UNAFFECTED
   --
   --  [ 8.4.1 ]
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

         Res := Create_Iir (Iir_Kind_Unaffected_Waveform);
         Set_Location (Res);

         --  Skip 'unaffected'.
         Scan;
      else
         Chain_Init (Res, Last_We);
         loop
            We := Create_Iir (Iir_Kind_Waveform_Element);
            Chain_Append (Res, Last_We, We);
            Set_Location (We);

            --  Note: NULL is handled as a null_literal.
            Set_We_Value (We, Parse_Expression);

            if Current_Token = Tok_After then
               --  Skip 'after'.
               Scan;

               Set_Time (We, Parse_Expression);
            end if;

            exit when Current_Token /= Tok_Comma;

            --  Skip ','.
            Scan;
         end loop;
      end if;

      return Res;
   end Parse_Waveform;

   --  precond : next token
   --  postcond: next token
   --
   --  [ 8.4 ]
   --  delay_mechanism ::= TRANSPORT
   --                    | [ REJECT TIME_expression ] INERTIAL
   procedure Parse_Delay_Mechanism (Assign: Iir) is
   begin
      if Current_Token = Tok_Transport then
         Set_Delay_Mechanism (Assign, Iir_Transport_Delay);
         Set_Has_Delay_Mechanism (Assign, True);

         --  Skip 'transport'.
         Scan;
      else
         Set_Delay_Mechanism (Assign, Iir_Inertial_Delay);
         if Current_Token = Tok_Reject then
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse
                 ("'reject' delay mechanism not allowed in vhdl 87");
            end if;
            Set_Has_Delay_Mechanism (Assign, True);

            --  Skip 'reject'.
            Scan;

            Set_Reject_Time_Expression (Assign, Parse_Expression);

            --  Skip 'inertial'.
            Expect_Scan (Tok_Inertial);
         elsif Current_Token = Tok_Inertial then
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse
                 ("'inertial' keyword not allowed in vhdl 87");
            end if;
            Set_Has_Delay_Mechanism (Assign, True);

            --  Skip 'inertial'.
            Scan;
         end if;
      end if;
   end Parse_Delay_Mechanism;

   --  precond : next token
   --  postcond: next token
   --
   --  [ LRM93 9.5 ]
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
   --  postcond: next token (';')
   --
   --  [ LRM93 9.5.1 ]
   --  conditional_waveforms ::=
   --      { waveform WHEN condition ELSE }
   --      waveform [ WHEN condition ]
   function Parse_Conditional_Waveforms return Iir
   is
      Wf : Iir;
      Res : Iir;
      Cond_Wf, N_Cond_Wf : Iir_Conditional_Waveform;
   begin
      Wf := Parse_Waveform;
      if Current_Token /= Tok_When then
         return Wf;
      else
         Res := Create_Iir (Iir_Kind_Conditional_Waveform);
         Set_Location (Res);
         Set_Waveform_Chain (Res, Wf);

         Cond_Wf := Res;
         loop
            --  Skip 'when'.
            Scan;

            Set_Condition (Cond_Wf, Parse_Expression);

            if Current_Token /= Tok_Else then
               if Flags.Vhdl_Std = Vhdl_87 then
                  Error_Msg_Parse ("else missing in vhdl 87");
               end if;
               exit;
            end if;

            N_Cond_Wf := Create_Iir (Iir_Kind_Conditional_Waveform);
            Set_Location (N_Cond_Wf);
            Set_Chain (Cond_Wf, N_Cond_Wf);
            Cond_Wf := N_Cond_Wf;

            --  Eat 'else'
            Scan;

            Set_Waveform_Chain (Cond_Wf, Parse_Waveform);

            exit when Current_Token /= Tok_When;
         end loop;
         return Res;
      end if;
   end Parse_Conditional_Waveforms;

   --  precond : '<=' (or ':=')
   --  postcond: next token (after ';')
   --
   --  [ LRM93 9.5.1 ]
   --  concurrent_conditional_signal_assignment ::=
   --      target <= [ GUARDED ] [ delay_mechanism ] conditional_waveforms ;
   --
   --  [ LRM08 10.5.2.1 ]
   --  concurrent_simple_waveform_assignment ::=
   --      target <= [ GUARDED ] [ delay_mechanism ] waveform ;
   function Parse_Concurrent_Conditional_Signal_Assignment (Target: Iir)
                                                           return Iir
   is
      Res: Iir;
      Loc : Location_Type;
      N_Res : Iir;
      Wf : Iir;
   begin
      Loc := Get_Token_Location;
      case Current_Token is
         when Tok_Less_Equal =>
            --  Skip '<='.
            Scan;
         when Tok_Assign =>
            Error_Msg_Parse ("':=' not allowed in concurrent statement, "
                               & "replaced by '<='");
            --  Skip ':='.
            Scan;
         when others =>
            Expect (Tok_Less_Equal);
      end case;

      --  Assume simple signal assignment.
      Res := Create_Iir (Iir_Kind_Concurrent_Simple_Signal_Assignment);
      Parse_Options (Res);

      Wf := Parse_Conditional_Waveforms;
      if Wf /= Null_Iir
        and then Get_Kind (Wf) = Iir_Kind_Conditional_Waveform
      then
         N_Res :=
           Create_Iir (Iir_Kind_Concurrent_Conditional_Signal_Assignment);
         if Get_Guard (Res) /= Null_Iir then
            Set_Guard (N_Res, N_Res);
         end if;
         Set_Delay_Mechanism (N_Res, Get_Delay_Mechanism (Res));
         Set_Reject_Time_Expression (N_Res, Get_Reject_Time_Expression (Res));
         Free_Iir (Res);
         Res := N_Res;
         Set_Conditional_Waveform_Chain (Res, Wf);
      else
         Set_Waveform_Chain (Res, Wf);
      end if;
      Set_Location (Res, Loc);
      Set_Target (Res, Target);
      Expect_Scan (Tok_Semi_Colon, "';' expected at end of signal assignment");

      return Res;
   end Parse_Concurrent_Conditional_Signal_Assignment;

   --  Like Parse_Expression, but keep parentheses.
   --  Parentheses are significant in case expressions, because of
   --  LRM02 8.8 Case Statement.
   function Parse_Case_Expression return Iir
   is
      Prev_Flag : constant Boolean := Flag_Parse_Parenthesis;
      Res : Iir;
   begin
      Flag_Parse_Parenthesis := True;
      Res := Parse_Expression_Keyword;
      Flag_Parse_Parenthesis := Prev_Flag;

      return Res;
   end Parse_Case_Expression;

   --  precond : WITH
   --  postcond: next token
   --
   --  [ LRM93 9.5.2 ]
   --  selected_signal_assignment ::=
   --      WITH expresion SELECT
   --          target <= options selected_waveforms ;
   --
   --  [ LRM93 9.5.2 ]
   --  selected_waveforms ::=
   --      { waveform WHEN choices , }
   --      waveform WHEN choices
   function Parse_Selected_Signal_Assignment return Iir
   is
      Res : Iir;
      Assoc : Iir;
      Wf_Chain : Iir_Waveform_Element;
      Target : Iir;
      First, Last : Iir;
      When_Loc : Location_Type;
   begin
      --  Skip 'with'.
      Scan;

      Res := Create_Iir (Iir_Kind_Concurrent_Selected_Signal_Assignment);
      Set_Location (Res);
      Set_Expression (Res, Parse_Case_Expression);

      Expect_Scan (Tok_Select, "'select' expected after expression");

      if Current_Token = Tok_Left_Paren then
         Target := Parse_Aggregate;
      else
         Target := Parse_Name (Allow_Indexes => True);
      end if;
      Set_Target (Res, Target);
      Expect_Scan (Tok_Less_Equal);

      Parse_Options (Res);

      Chain_Init (First, Last);
      loop
         Wf_Chain := Parse_Waveform;
         Expect (Tok_When, "'when' expected after waveform");
         When_Loc := Get_Token_Location;

         --  Eat 'when'.
         Scan;

         Parse_Choices (Null_Iir, When_Loc, Assoc);
         Set_Associated_Chain (Assoc, Wf_Chain);
         Chain_Append_Subchain (First, Last, Assoc);
         exit when Current_Token /= Tok_Comma;
         --  Skip ','.
         Scan;
      end loop;
      Set_Selected_Waveform_Chain (Res, First);

      Expect_Scan (Tok_Semi_Colon, "';' expected at end of signal assignment");

      return Res;
   end Parse_Selected_Signal_Assignment;

   --  precond : next token
   --  postcond: next token.
   --
   --  [ LRM93 8.1 ]
   --  sensitivity_list ::= SIGNAL_name { , SIGNAL_name }
   function Parse_Sensitivity_List return Iir_List
   is
      List : Iir_List;
      El : Iir;
   begin
      List := Create_Iir_List;

      loop
         El := Parse_Name (Allow_Indexes => True);
         if El /= Null_Iir then
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
                  El := Create_Error_Node (El);
            end case;
            Append_Element (List, El);
         end if;

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
      end loop;

      return List;
   end Parse_Sensitivity_List;

   --  precond : ASSERT
   --  postcond: next token
   --  Note: this fill an sequential or a concurrent statement.
   --
   --  [ LRM93 9.4 ]
   --  concurrent_assertion_statement ::=
   --      [ label : ] [ POSTPONED ] assertion ;
   --
   --  [ LRM93 8.2 ]
   --  assertion ::= ASSERT condition
   --      [ REPORT expression ] [ SEVERITY expression ]
   procedure Parse_Assertion (Stmt: Iir) is
   begin
      Set_Location (Stmt);

      --  Skip 'assert'.
      Scan;

      Set_Assertion_Condition (Stmt, Parse_Expression);

      if Current_Token = Tok_Report then
         --  Skip 'report'.
         Scan;

         Set_Report_Expression (Stmt, Parse_Expression);
      end if;

      if Current_Token = Tok_Severity then
         --  Skip 'severity'.
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

      --  Skip 'report'.
      Scan;

      Set_Report_Expression (Res, Parse_Expression);

      if Current_Token = Tok_Severity then
         --  Skip 'severity'.
         Scan;

         Set_Severity_Expression (Res, Parse_Expression);
      end if;
      return Res;
   end Parse_Report_Statement;

   -- precond : WAIT
   -- postcond: ';'
   --
   --  [ LRM93 8.1 ]
   --  wait_statement ::=
   --      [ label : ] WAIT [ sensitivity_clause ] [ condition_clause ]
   --          [ timeout_clause ] ;
   --
   --  [ LRM93 8.1 ]
   --  sensitivity_clause ::= ON sensitivity_list
   --
   --  [ LRM93 8.1 ]
   --  condition_clause ::= UNTIL conditiion
   --
   --  [ LRM93 8.1 ]
   --  timeout_clause ::= FOR TIME_expression
   function Parse_Wait_Statement return Iir_Wait_Statement
   is
      Res: Iir_Wait_Statement;
      List: Iir_List;
   begin
      Res := Create_Iir (Iir_Kind_Wait_Statement);
      Set_Location (Res);

      --  Skip 'wait'.
      Scan;

      --  Sensitivity clause.
      case Current_Token is
         when Tok_On =>
            --  Skip 'on'.
            Scan;

            List := Parse_Sensitivity_List;
            Set_Sensitivity_List (Res, List);
         when Tok_Until =>
            null;
         when Tok_For =>
            null;
         when Tok_Semi_Colon =>
            return Res;
         when others =>
            Error_Msg_Parse ("'on', 'until', 'for' or ';' expected");
            Resync_To_End_Of_Statement;
            return Res;
      end case;

      --  Condition clause.
      case Current_Token is
         when Tok_On =>
            Error_Msg_Parse ("only one sensitivity is allowed");
            Resync_To_End_Of_Statement;
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
            Resync_To_End_Of_Statement;
            return Res;
      end case;

      --  Timeout clause.
      case Current_Token is
         when Tok_On =>
            Error_Msg_Parse ("only one sensitivity clause is allowed");
            Resync_To_End_Of_Statement;
            return Res;
         when Tok_Until =>
            Error_Msg_Parse ("only one condition clause is allowed");
            Resync_To_End_Of_Statement;
            return Res;
         when Tok_For =>
            Scan;
            Set_Timeout_Clause (Res, Parse_Expression);
            return Res;
         when Tok_Semi_Colon =>
            return Res;
         when others =>
            Error_Msg_Parse ("'for' or ';' expected");
            Resync_To_End_Of_Statement;
            return Res;
      end case;
   end Parse_Wait_Statement;

   --  precond : IF
   --  postcond: next token.
   --
   --  [ LRM93 8.7 ]
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
      Start_Loc, Then_Loc, End_Loc : Location_Type;
   begin
      Res := Create_Iir (Iir_Kind_If_Statement);
      Start_Loc := Get_Token_Location;
      Set_Location (Res, Start_Loc);
      Set_Parent (Res, Parent);

      --  Eat 'if'.
      Scan;

      Clause := Res;
      loop
         Set_Condition (Clause, Parse_Expression_Keyword);
         Then_Loc := Get_Token_Location;
         --  Eat 'then'.
         Expect_Scan (Tok_Then, "'then' is expected here");

         Set_Sequential_Statement_Chain
           (Clause, Parse_Sequential_Statements (Res));

         End_Loc := Get_Token_Location;

         if Flag_Elocations then
            Create_Elocations (Clause);
            Set_Start_Location (Clause, Start_Loc);
            Set_Then_Location (Clause, Then_Loc);
            Set_End_Location (Clause, End_Loc);
         end if;

         exit when Current_Token /= Tok_Else and Current_Token /= Tok_Elsif;

         N_Clause := Create_Iir (Iir_Kind_Elsif);
         Start_Loc := Get_Token_Location;
         Set_Location (N_Clause, Start_Loc);
         Set_Else_Clause (Clause, N_Clause);
         Clause := N_Clause;
         if Current_Token = Tok_Else then

            --  Skip 'else'.
            Scan;

            Set_Sequential_Statement_Chain
              (Clause, Parse_Sequential_Statements (Res));

            if Flag_Elocations then
               Create_Elocations (Clause);
               Set_Start_Location (Clause, Start_Loc);
               Set_End_Location (Clause, Get_Token_Location);
            end if;

            exit;
         else
            pragma Assert (Current_Token = Tok_Elsif);
            --  Skip 'elsif'.
            Scan;
         end if;
      end loop;

      --  Skip 'end' 'if'
      Expect_Scan (Tok_End);
      Expect_Scan (Tok_If);

      return Res;
   end Parse_If_Statement;

   function Parenthesis_Name_To_Procedure_Call (Name: Iir; Kind : Iir_Kind)
                                               return Iir
   is
      Res: Iir;
      Call : Iir_Procedure_Call;
      Prefix : Iir;
   begin
      Res := Create_Iir (Kind);
      if Name = Null_Iir then
         Set_Location (Res);
         return Res;
      end if;

      Location_Copy (Res, Name);
      Call := Create_Iir (Iir_Kind_Procedure_Call);
      Location_Copy (Call, Name);
      Set_Procedure_Call (Res, Call);
      case Get_Kind (Name) is
         when Iir_Kind_Parenthesis_Name =>
            Prefix := Get_Prefix (Name);
            if Get_Kind (Prefix) = Iir_Kind_Operator_Symbol then
               Error_Msg_Parse
                 (+Prefix, "operator cannot be used as procedure call");
            end if;
            Set_Prefix (Call, Prefix);
            Set_Parameter_Association_Chain
              (Call, Get_Association_Chain (Name));
            Free_Iir (Name);
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            Set_Prefix (Call, Name);
         when Iir_Kind_String_Literal8 =>
            Error_Msg_Parse
              ("string or operator cannot be used as procedure call");
         when Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Qualified_Expression
           | Iir_Kind_Attribute_Name
           | Iir_Kind_Operator_Symbol
           | Iir_Kind_Signature =>
            Error_Msg_Parse
              ("invalid name for a procedure call or missing assignment");
         when Iir_Kind_Error =>
            null;
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
      Set_Parent (Decl, Parent);

      --  Skip identifier
      Scan_Identifier (Decl);

      --  Skip 'in'
      Expect_Scan (Tok_In);

      Set_Discrete_Range (Decl, Parse_Discrete_Range);
      return Decl;
   end Parse_Parameter_Specification;

   --  precond:  delay_mechanism or waveform
   --  postcond: next token
   --
   --  [ LRM93 8.4 ]
   --  signal_assignment_statement ::=
   --    [ label : ] target <= [ delay_mechanism ] waveform ;
   --
   --  [ LRM08 10.5 Signal assignment statement ]
   --  simple_waveform_assignment ::=
   --    target <= [ delay_mechanism ] waveform ;
   function Parse_Signal_Waveform_Assignment
     (Target : Iir; Loc : Location_Type) return Iir
   is
      Stmt : Iir;
      N_Stmt : Iir;
      Wave_Chain : Iir;
   begin
      Stmt := Create_Iir (Iir_Kind_Simple_Signal_Assignment_Statement);
      Set_Location (Stmt, Loc);
      Set_Target (Stmt, Target);

      Parse_Delay_Mechanism (Stmt);

      Wave_Chain := Parse_Conditional_Waveforms;

      --  LRM 8.4 Signal assignment statement
      --  It is an error is the reserved word UNAFFECTED appears as a
      --  waveform in a (sequential) signal assignment statement.
      if Get_Kind (Wave_Chain) = Iir_Kind_Unaffected_Waveform then
         if Flags.Vhdl_Std < Vhdl_08 then
            Error_Msg_Parse
              ("'unaffected' is not allowed in a sequential statement");
         end if;
         Set_Waveform_Chain (Stmt, Wave_Chain);
      elsif Get_Kind (Wave_Chain) = Iir_Kind_Conditional_Waveform then
         Check_Vhdl_At_Least_2008
            ("conditional signal assignment in sequential statement");
         N_Stmt :=
           Create_Iir (Iir_Kind_Conditional_Signal_Assignment_Statement);
         Location_Copy (N_Stmt, Stmt);
         Set_Target (N_Stmt, Target);
         Set_Delay_Mechanism (N_Stmt, Get_Delay_Mechanism (Stmt));
         Set_Reject_Time_Expression
           (N_Stmt, Get_Reject_Time_Expression (Stmt));
         Set_Conditional_Waveform_Chain (N_Stmt, Wave_Chain);
         Free_Iir (Stmt);
         Stmt := N_Stmt;
      else
         Set_Waveform_Chain (Stmt, Wave_Chain);
      end if;

      return Stmt;
   end Parse_Signal_Waveform_Assignment;

   --  precond:  -
   --  postcond: next token
   --
   --  [ LRM08 10.5.2 Simple signal assignments ]
   --  force_mode ::= IN | OUT
   procedure Parse_Force_Mode_Opt (Stmt : Iir) is
   begin
      case Current_Token is
         when Tok_In =>
            Set_Force_Mode (Stmt, Iir_Force_In);
            Set_Has_Force_Mode (Stmt, True);
         when Tok_Out =>
            Set_Force_Mode (Stmt, Iir_Force_Out);
            Set_Has_Force_Mode (Stmt, True);
         when others =>
            null;
      end case;
   end Parse_Force_Mode_Opt;

   --  precond:  'force'
   --  postcond: next token
   --
   --  [ LRM08 10.5 Signal assignment statement ]
   --  simple_force_assignment ::=
   --    target <= FORCE [ force_mode ] expression ;
   function Parse_Signal_Force_Assignment
     (Target : Iir; Loc : Location_Type) return Iir
   is
      Stmt : Iir;
   begin
      Stmt := Create_Iir (Iir_Kind_Signal_Force_Assignment_Statement);
      Set_Location (Stmt, Loc);
      Set_Target (Stmt, Target);

      --  Skip 'force'.
      Scan;

      Parse_Force_Mode_Opt (Stmt);

      Set_Expression (Stmt, Parse_Expression);

      return Stmt;
   end Parse_Signal_Force_Assignment;

   --  precond:  'release'
   --  postcond: next token
   --
   --  [ LRM08 10.5 Signal assignment statement ]
   --  simple_release_assignment ::=
   --    target <= RELEASE [ force_mode ] expression ;
   function Parse_Signal_Release_Assignment
     (Target : Iir; Loc : Location_Type) return Iir
   is
      Stmt : Iir;
   begin
      Stmt := Create_Iir (Iir_Kind_Signal_Release_Assignment_Statement);
      Set_Location (Stmt, Loc);
      Set_Target (Stmt, Target);

      --  Skip 'release'.
      Scan;

      Parse_Force_Mode_Opt (Stmt);

      return Stmt;
   end Parse_Signal_Release_Assignment;

   --  precond:  '<='
   --  postcond: next token
   --
   --  [ LRM93 8.4 ]
   --  signal_assignment_statement ::=
   --      [ label : ] target <= [ delay_mechanism ] waveform ;
   --
   --  [ LRM08 10.5 Signal assignment statement ]
   --  signal_assignement_statement ::=
   --      [ label : ] simple_signal_assignement
   --    | [ label : ] conditional_signal_assignement
   --    | [ label : ] selected_signal_assignement
   --
   --  simple_signal_assignment ::=
   --      simple_waveform_assignment
   --    | simple_force_assignment
   --    | simple_release_assignment
   function Parse_Signal_Assignment_Statement (Target : Iir) return Iir
   is
      Loc : Location_Type;
   begin
      Loc := Get_Token_Location;

      --  Skip '<='.
      Scan;

      case Current_Token is
         when Tok_Force =>
            return Parse_Signal_Force_Assignment (Target, Loc);
         when Tok_Release =>
            return Parse_Signal_Release_Assignment (Target, Loc);
         when others =>
            return Parse_Signal_Waveform_Assignment (Target, Loc);
      end case;
   end Parse_Signal_Assignment_Statement;

   --  precond:  WHEN
   --  postcond: next token
   --
   --  [ LRM08 10.5.3 Conditional signal assignments ]
   --  conditional_expressions ::=
   --      expression WHEN condition
   --    { ELSE expression WHEN condition }
   --    [ ELSE expression ]
   function Parse_Conditional_Expression_Chain (Expr : Iir) return Iir
   is
      Res : Iir;
      El, N_El : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Conditional_Expression);
      Set_Location (Res);
      Set_Expression (Res, Expr);
      El := Res;

      loop
         --  Skip 'when'.
         Scan;

         Set_Condition (El, Parse_Expression);

         exit when Current_Token /= Tok_Else;

         N_El := Create_Iir (Iir_Kind_Conditional_Expression);
         Set_Location (N_El);
         Set_Chain (El, N_El);
         El := N_El;

         --  Skip 'else'.
         Scan;

         Set_Expression (N_El, Parse_Expression);

         exit when Current_Token /= Tok_When;
      end loop;

      return Res;
   end Parse_Conditional_Expression_Chain;

   --  precond:  ':='
   --  postcond: next token
   --
   --  [ LRM93 8.5 ]
   --  variable_assignment_statement ::=
   --      [ label : ] target := expression ;
   function Parse_Variable_Assignment_Statement (Target : Iir) return Iir
   is
      Stmt : Iir;
      Loc : Location_Type;
      Expr : Iir;
   begin
      Loc := Get_Token_Location;

      --  Skip ':='.
      Scan;

      Expr := Parse_Expression;

      if Current_Token = Tok_When then
         Check_Vhdl_At_Least_2008 ("conditional variable assignment");
         Stmt :=
           Create_Iir (Iir_Kind_Conditional_Variable_Assignment_Statement);
         Set_Location (Stmt, Loc);
         Set_Target (Stmt, Target);
         Set_Conditional_Expression_Chain
           (Stmt, Parse_Conditional_Expression_Chain (Expr));
      else
         Stmt := Create_Iir (Iir_Kind_Variable_Assignment_Statement);
         Set_Location (Stmt, Loc);
         Set_Target (Stmt, Target);
         Set_Expression (Stmt, Expr);
      end if;
      return Stmt;
   end Parse_Variable_Assignment_Statement;

   --  precond:  '<=', ':=' or ';'
   --  postcond: next token
   function Parse_Sequential_Assignment_Statement (Target : Iir) return Iir
   is
      Stmt : Iir;
   begin
      case Current_Token is
         when Tok_Less_Equal =>
            return Parse_Signal_Assignment_Statement (Target);
         when Tok_Assign =>
            return Parse_Variable_Assignment_Statement (Target);
         when Tok_Semi_Colon =>
            return Parenthesis_Name_To_Procedure_Call
              (Target, Iir_Kind_Procedure_Call_Statement);
         when others =>
            Error_Msg_Parse
              ("""<="" or "":="" expected instead of %t", +Current_Token);
            Stmt := Create_Iir (Iir_Kind_Variable_Assignment_Statement);
            Set_Expression (Stmt, Target);
            Resync_To_End_Of_Statement;
            return Stmt;
      end case;
   end Parse_Sequential_Assignment_Statement;

   --  precond:  CASE
   --  postcond: ';'
   --
   --  [ LRM08 10.9 ]
   --  case_statement ::=
   --      [ CASE_label : ]
   --          CASE [?] expression IS
   --              case_statement_alternative
   --              { case_statement_alternative }
   --          END CASE [?] [ CASE_label ] ;
   --
   --  [ LRM08 10.9]
   --  case_statement_alternative ::= WHEN choices => sequence_of_statements
   function Parse_Case_Statement (Label : Name_Id) return Iir
   is
      Stmt : Iir;
      Assoc: Iir;
      First_Assoc, Last_Assoc : Iir;
      When_Loc : Location_Type;
   begin
      Stmt := Create_Iir (Iir_Kind_Case_Statement);
      Set_Label (Stmt, Label);
      Set_Location (Stmt);

      --  Skip 'case'.
      Scan;

      if Flags.Vhdl_Std >= Vhdl_08 then
         --  Check ? for matching case
         if Current_Token = Tok_Question_Mark then
            --  Skip ?
            Scan;
            --  Mark the case as matching case statement
            Set_Matching_Flag (Stmt, True);
         end if;
      end if;

      --  Parse the Expression
      Set_Expression (Stmt, Parse_Case_Expression);

      --  Skip 'is'.
      Expect_Scan (Tok_Is);

      if Current_Token = Tok_End then
         Error_Msg_Parse ("missing alternative in case statement");
      end if;

      Chain_Init (First_Assoc, Last_Assoc);
      while Current_Token = Tok_When loop
         When_Loc := Get_Token_Location;

         --  Skip 'when'.
         Scan;

         Parse_Choices (Null_Iir, When_Loc, Assoc);

         --  Skip '=>'.
         Expect_Scan (Tok_Double_Arrow);

         Set_Associated_Chain (Assoc, Parse_Sequential_Statements (Stmt));
         Chain_Append_Subchain (First_Assoc, Last_Assoc, Assoc);
      end loop;
      Set_Case_Statement_Alternative_Chain (Stmt, First_Assoc);

      if Flag_Elocations then
         Create_Elocations (Stmt);
         Set_End_Location (Stmt, Get_Token_Location);
      end if;

      --  Skip 'end', 'case'.
      Expect_Scan (Tok_End);
      Expect_Scan (Tok_Case);

      if Get_Matching_Flag (Stmt) then
         --  Matching case statement must match the ?
         Expect_Scan (Tok_Question_Mark);
      end if ;

      if Flags.Vhdl_Std >= Vhdl_93 then
         Check_End_Name (Stmt);
      end if;

      return Stmt;
   end Parse_Case_Statement;

   --  precond:  FOR
   --  postcond: ';'
   --
   --  [ LRM93 8.9 ]
   --  loop_statement ::=
   --      [ LOOP_label : ]
   --          [ iteration_scheme ] LOOP
   --              sequence_of_statements
   --          END LOOP [ LOOP_label ] ;
   --
   --  [ LRM93 8.9 ]
   --  iteration_scheme ::= WHILE condition
   --                     | FOR LOOP_parameter_specification
   function Parse_For_Loop_Statement (Label : Name_Id) return Iir
   is
      Stmt : Iir;
      Start_Loc, Loop_Loc, End_Loc : Location_Type;
   begin
      Stmt := Create_Iir (Iir_Kind_For_Loop_Statement);
      Start_Loc := Get_Token_Location;
      Set_Location (Stmt, Start_Loc);
      Set_Label (Stmt, Label);

      --  Skip 'for'
      Scan;

      Set_Parameter_Specification
        (Stmt, Parse_Parameter_Specification (Stmt));

      --  Skip 'loop'
      Loop_Loc := Get_Token_Location;
      Expect (Tok_Loop);
      Scan;

      Set_Sequential_Statement_Chain
        (Stmt, Parse_Sequential_Statements (Stmt));

      --  Skip 'end'
      End_Loc := Get_Token_Location;
      Expect_Scan (Tok_End);

      --  Skip 'loop'
      Expect_Scan (Tok_Loop);

      Check_End_Name (Stmt);

      if Flag_Elocations then
         Create_Elocations (Stmt);
         Set_Start_Location (Stmt, Start_Loc);
         Set_Loop_Location (Stmt, Loop_Loc);
         Set_End_Location (Stmt, End_Loc);
      end if;

      return Stmt;
   end Parse_For_Loop_Statement;

   --  precond:  WHILE or LOOP
   --  postcond: ';'
   --
   --  [ 8.9 ]
   --  loop_statement ::=
   --      [ LOOP_label : ]
   --          [ iteration_scheme ] LOOP
   --              sequence_of_statements
   --          END LOOP [ LOOP_label ] ;
   function Parse_While_Loop_Statement (Label : Name_Id) return Iir
   is
      Stmt : Iir;
      Start_Loc, Loop_Loc, End_Loc : Location_Type;
   begin
      Stmt := Create_Iir (Iir_Kind_While_Loop_Statement);
      Start_Loc := Get_Token_Location;
      Set_Location (Stmt, Start_Loc);
      Set_Label (Stmt, Label);
      if Current_Token = Tok_While then
         --  Skip 'while'.
         Scan;

         Set_Condition (Stmt, Parse_Expression);
         Expect (Tok_Loop);
      end if;

      --  Skip 'loop'.
      Loop_Loc := Get_Token_Location;
      Scan;

      Set_Sequential_Statement_Chain
        (Stmt, Parse_Sequential_Statements (Stmt));

      End_Loc := Get_Token_Location;

      --  Skip 'end'.
      Expect_Scan (Tok_End);

      --  Skip 'loop'.
      Expect_Scan (Tok_Loop);

      Check_End_Name (Stmt);

      if Flag_Elocations then
         Create_Elocations (Stmt);
         Set_Start_Location (Stmt, Start_Loc);
         Set_Loop_Location (Stmt, Loop_Loc);
         Set_End_Location (Stmt, End_Loc);
      end if;

      return Stmt;
   end Parse_While_Loop_Statement;

   --  AMS-LRM17 10.15 Break statement
   --  break_list ::= break_element { , break_element }
   --
   --  break_element ::=
   --    [ break_selector_clause ] /quantity/_name => expression
   --
   --  break_selector_clause ::= FOR /quantity/_name USE

   function Parse_Break_List return Iir
   is
      First, Last : Iir;
      El : Iir;
      Sel : Iir;
   begin
      Chain_Init (First, Last);

      loop
         case Current_Token is
            when Tok_For =>
               --  break_selector_clause

               --  Skip 'for'.
               Scan;

               Sel := Parse_Name;

               --  Skip 'use'.
               Expect_Scan (Tok_Use, "'use' expected after quantity name");

            when Tok_Identifier =>
               --  No break_selector_clause.
               Sel := Null_Iir;

            when others =>
               --  No more break_element.
               exit;
         end case;

         El := Create_Iir (Iir_Kind_Break_Element);
         Set_Selector_Quantity (El, Sel);

         Set_Location (El);
         Set_Break_Quantity (El, Parse_Name);

         Expect_Scan (Tok_Double_Arrow, "'=>' expected after quantity name");
         Set_Expression (El, Parse_Expression);

         Chain_Append (First, Last, El);

         exit when Current_Token /= Tok_Comma;

         --  Eat ','
         Scan;
      end loop;

      return First;
   end Parse_Break_List;

   -- precond : BREAK
   -- postcond: ';'
   --
   --  AMS-LRM17 10.15 Break statement
   --  break_statement ::=
   --    [ label : ] BREAK [ break_list ] [ WHEN condition ] ;
   function Parse_Break_Statement return Iir
   is
      Res: Iir;
   begin
      Res := Create_Iir (Iir_Kind_Break_Statement);
      Set_Location (Res);

      --  Skip 'break'.
      Scan;

      Set_Break_Element (Res, Parse_Break_List);

      if Current_Token = Tok_When then
         --  Skip 'when'.
         Scan;

         Set_Condition (Res, Parse_Expression);
      end if;

      return Res;
   end Parse_Break_Statement;

   --  precond:  next token
   --  postcond: next token
   --
   --  [ LRM93 8 ]
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
   --                         | break_statement
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
   --  [ 8.2 ]
   --  assertion_statement ::= [ label : ] assertion ;
   --
   --  [ 8.3 ]
   --  report_statement ::= [ label : ] REPORT expression SEVERITY expression ;
   function Parse_Sequential_Statements (Parent : Iir) return Iir
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

            --  Skip identifier.
            Scan;

            if Current_Token = Tok_Colon then
               --  Skip ':'.
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

               --  Skip 'null'.
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
               if Flags.Vhdl_Std >= Vhdl_93 then
                  Check_End_Name (Stmt);
               end if;
            when Tok_Case =>
               Stmt := Parse_Case_Statement (Label);
            when Tok_Identifier
              | Tok_String
              | Tok_Double_Less =>
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

               --  Skip return.
               Scan;

               if Current_Token /= Tok_Semi_Colon then
                  Set_Expression (Stmt, Parse_Expression);
               end if;

            when Tok_For =>
               Stmt := Parse_For_Loop_Statement (Label);
               Set_Location (Stmt, Loc);

               --  A loop statement can have a label, even in vhdl87.
               Label := Null_Identifier;

            when Tok_While
              | Tok_Loop =>
               Stmt := Parse_While_Loop_Statement (Label);
               Set_Location (Stmt, Loc);

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

            when Tok_Wait =>
               Stmt := Parse_Wait_Statement;

            when Tok_Break =>
               Stmt := Parse_Break_Statement;

            when Tok_Semi_Colon =>
               Error_Msg_Parse ("extra ';' ignored");

               --  Eat ';'
               Scan;

               goto Again;
            when Tok_Constant
              | Tok_Variable
              | Tok_Signal
              | Tok_Alias
              | Tok_File
              | Tok_Attribute =>
               Error_Msg_Parse ("declaration not allowed within statements");
               Scan;
               Resync_To_End_Of_Declaration;
               goto Again;

            when Tok_Begin =>
               Error_Msg_Parse ("'begin' not allowed within statements");
               Scan;
               goto Again;

            when Tok_Tick =>
               Unexpected ("statement");
               Resync_To_End_Of_Statement;
               goto Again;

            when others =>
               return First_Stmt;
         end case;
         << Has_Stmt >> null;
         Set_Parent (Stmt, Parent);
         Set_Location (Stmt, Loc);
         if Label /= Null_Identifier then
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse
                 (+Stmt, "this statement can't have a label in vhdl 87");
            else
               Set_Label (Stmt, Label);
            end if;
         end if;

         if Current_Token = Tok_Semi_Colon then
            --  Skip ';'.
            Scan;
         else
            Error_Missing_Semi_Colon ("statement");
            Resync_To_End_Of_Statement;
            if Current_Token = Tok_Semi_Colon then
               --  Skip ';'.
               Scan;
            end if;
         end if;

         --  Append it to the chain.
         if First_Stmt = Null_Iir then
            First_Stmt := Stmt;
         else
            Set_Chain (Last_Stmt, Stmt);
         end if;
         Last_Stmt := Stmt;

         <<Again>> null;
      end loop;
   end Parse_Sequential_Statements;

   procedure Parse_Subprogram_Body (Subprg : Iir; Is_Loc : Location_Type)
   is
      Kind : constant Iir_Kind := Get_Kind (Subprg);
      Subprg_Body : Iir;
      Begin_Loc, End_Loc : Location_Type;
   begin
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

      Parse_Declarative_Part (Subprg_Body, Subprg_Body);

      --  Skip 'begin'.
      Begin_Loc := Get_Token_Location;
      Expect_Scan (Tok_Begin);

      Set_Sequential_Statement_Chain
        (Subprg_Body, Parse_Sequential_Statements (Subprg_Body));

      --  Skip 'end'.
      End_Loc := Get_Token_Location;
      Expect_Scan (Tok_End);

      if Flag_Elocations then
         Create_Elocations (Subprg_Body);
         Set_Is_Location (Subprg_Body, Is_Loc);
         Set_Begin_Location (Subprg_Body, Begin_Loc);
         Set_End_Location (Subprg_Body, End_Loc);
      end if;

      case Current_Token is
         when Tok_Function =>
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse ("'function' not allowed here by vhdl 87");
            end if;
            if Kind = Iir_Kind_Procedure_Declaration then
               Error_Msg_Parse ("'procedure' expected instead of 'function'");
            end if;
            Set_End_Has_Reserved_Id (Subprg_Body, True);

            --  Skip 'function'.
            Scan;

         when Tok_Procedure =>
            if Flags.Vhdl_Std = Vhdl_87 then
               Error_Msg_Parse ("'procedure' not allowed here by vhdl 87");
            end if;
            if Kind = Iir_Kind_Function_Declaration then
               Error_Msg_Parse ("'function' expected instead of 'procedure'");
            end if;
            Set_End_Has_Reserved_Id (Subprg_Body, True);

            --  Skip 'procedure'
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
               Error_Msg_Parse ("misspelling, %i expected", +Subprg);
            end if;
            Set_End_Has_Identifier (Subprg_Body, True);

            --  Skip string.
            Scan;

         when others =>
            null;
      end case;
      Scan_Semi_Colon_Declaration ("subprogram body");
   end Parse_Subprogram_Body;

   --  precond : NEW
   --
   --  LRM08 4.4 Subprogram instantiation declarations
   --  subprogram_instantiation_declaration ::=
   --    subprogram_kind designator IS
   --      NEW uninstantiated_subprogram_name [ signature ]
   --      [ generic_map_aspect ];
   function Parse_Subprogram_Instantiation (Subprg : Iir) return Iir
   is
      Res : Iir;
   begin
      case Iir_Kinds_Subprogram_Declaration (Get_Kind (Subprg)) is
         when Iir_Kind_Function_Declaration =>
            Res := Create_Iir (Iir_Kind_Function_Instantiation_Declaration);
            if Get_Has_Pure (Subprg) then
               Error_Msg_Parse
                 (+Subprg, "pure/impure not allowed for instantiations");
            end if;
            if Get_Return_Type_Mark (Subprg) /= Null_Iir then
               Error_Msg_Parse
                 (+Subprg, "return type not allowed for instantiations");
            end if;
         when Iir_Kind_Procedure_Declaration =>
            Res := Create_Iir (Iir_Kind_Procedure_Instantiation_Declaration);
      end case;
      Location_Copy (Res, Subprg);
      Set_Identifier (Res, Get_Identifier (Subprg));

      if Get_Interface_Declaration_Chain (Subprg) /= Null_Iir then
         Error_Msg_Parse
           (+Subprg, "interfaces not allowed for instantiations");
      end if;

      --  Skip 'new'.
      Scan;

      Set_Uninstantiated_Subprogram_Name (Res, Parse_Signature_Name);

      if Current_Token = Tok_Generic then
         Set_Generic_Map_Aspect_Chain (Res, Parse_Generic_Map_Aspect);
      end if;

      --  Skip ';'.
      Expect_Scan (Tok_Semi_Colon);

      return Res;
   end Parse_Subprogram_Instantiation;

   --  precond : PROCEDURE, FUNCTION, PURE or IMPURE.
   --  postcond: next token.
   --
   --  [ LRM93 2.1 ]
   --  subprogram_declaration ::= subprogram_specification ;
   --
   --  [ LRM93 2.1 ]
   --  subprogram_specification ::=
   --      PROCEDURE designator [ ( formal_parameter_list ) ]
   --    | [ PURE | IMPURE ] FUNCTION designator [ ( formal_parameter_list ) ]
   --          RETURN type_mark
   --
   --  [ LRM19 4.2 Subprogram declarations ]
   --  function_specification ::=
   --    [ PURE | IMPURE ] FUNCTION designator
   --      subprogram_header [ [ PARAMETER ] ( formal_parameter_list) ]
   --      RETURN [ return_identifier OF ] type_mark
   --
   --  [ LRM93 2.2 ]
   --  subprogram_body ::=
   --      subprogram_specification IS
   --          subprogram_declarative_part
   --      BEGIN
   --          subprogram_statement_part
   --      END [ subprogram_kind ] [ designator ] ;
   --
   --  [ LRM93 2.1 ]
   --  designator ::= identifier | operator_symbol
   --
   --  [ LRM93 2.1 ]
   --  operator_symbol ::= string_literal
   function Parse_Subprogram_Declaration return Iir
   is
      Kind : Iir_Kind;
      Subprg : Iir;
      Gen : Iir;
      Start_Loc, Is_Loc : Location_Type;
   begin
      --  Create the node.
      Start_Loc := Get_Token_Location;
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

      --  Comments for the subprogram.
      if Flag_Gather_Comments then
         Gather_Comments_Line (Subprg);
      end if;

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

      --  Designator.
      Parse_Subprogram_Designator (Subprg);

      if Current_Token = Tok_Generic then
         --  Eat 'generic'
         Scan;

         Gen := Parse_Interface_List (Generic_Interface_List, Subprg);
         Set_Generic_Chain (Subprg, Gen);
      end if;

      Parse_Subprogram_Parameters_And_Return
        (Subprg, Kind = Iir_Kind_Function_Declaration, False);

      if Flag_Elocations then
         Create_Elocations (Subprg);
         Set_Start_Location (Subprg, Start_Loc);
      end if;

      case Current_Token is
         when Tok_Is =>
            --  Skip 'is'.
            Is_Loc := Get_Token_Location;
            Scan;

            if Current_Token = Tok_New then
               return Parse_Subprogram_Instantiation (Subprg);
            end if;
         when Tok_Begin =>
            Error_Msg_Parse ("missing 'is' before 'begin'");
            Is_Loc := Get_Token_Location;
         when others =>
            if Kind = Iir_Kind_Function_Declaration then
               Check_Function_Specification (Subprg);
            end if;

            --  Skip ';'.
            Expect_Scan (Tok_Semi_Colon);

            return Subprg;
      end case;

      if Kind = Iir_Kind_Function_Declaration then
         Check_Function_Specification (Subprg);
      end if;

      --  The body.
      Parse_Subprogram_Body (Subprg, Is_Loc);
      return Subprg;
   end Parse_Subprogram_Declaration;

   --  precond:  PROCESS
   --  postcond: next token
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
      Start_Loc, Begin_Loc, End_Loc : Location_Type;
      Comments : Comments_Range;
   begin
      Start_Loc := Get_Token_Location;

      --  Attach comments now, as 'process' may appear alone, followed
      --  by a comment for the next declaration.
      if Flag_Gather_Comments then
         File_Comments.Save_Comments (Comments);
      end if;

      --  Skip 'process'
      Scan;

      if Current_Token = Tok_Left_Paren then
         Res := Create_Iir (Iir_Kind_Sensitized_Process_Statement);

         --  Comments for the process.
         if Flag_Gather_Comments then
            Gather_Comments_Block (Comments, Res);
         end if;

         --  Skip '('
         Scan;

         if Current_Token = Tok_All then
            Check_Vhdl_At_Least_2008 ("all sensitized process");
            Sensitivity_List := Iir_List_All;

            --  Skip 'all'
            Scan;
         else
            Sensitivity_List := Parse_Sensitivity_List;
         end if;
         Set_Sensitivity_List (Res, Sensitivity_List);

         --  Skip ')'
         Expect_Scan (Tok_Right_Paren);
      else
         Res := Create_Iir (Iir_Kind_Process_Statement);

         --  Comments for the process.
         if Flag_Gather_Comments then
            Gather_Comments_Block (Comments, Res);
         end if;
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

      --  Declarative part.
      Parse_Declarative_Part (Res, Res);

      --  Skip 'begin'.
      Begin_Loc := Get_Token_Location;
      Expect_Scan (Tok_Begin);

      Set_Sequential_Statement_Chain (Res, Parse_Sequential_Statements (Res));

      --  Skip 'end'.
      End_Loc := Get_Token_Location;
      Expect_Scan (Tok_End);

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

         --  Skip ';'.
         Scan;
      else
         Scan_End_Token (Tok_Process, Res);
         Check_End_Name (Res);
         Expect_Scan (Tok_Semi_Colon, "';' expected at end of process");
      end if;

      if Flag_Elocations then
         Create_Elocations (Res);
         Set_Start_Location (Res, Start_Loc);
         Set_Begin_Location (Res, Begin_Loc);
         Set_End_Location (Res, End_Loc);
      end if;

      return Res;
   end Parse_Process_Statement;

   function Check_Formal_Form (Formal : Iir) return Iir is
   begin
      if Formal = Null_Iir then
         return Formal;
      end if;

      case Get_Kind (Formal) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Selected_Name =>
            return Formal;
         when Iir_Kind_Parenthesis_Name =>
            --  Could be an indexed name, so nothing to check within the
            --  parenthesis.
            declare
               Assoc : constant Iir := Get_Association_Chain (Formal);
            begin
               if Assoc /= Null_Iir then
                  Set_In_Formal_Flag (Assoc, True);
               end if;
            end;
            return Formal;
         when Iir_Kind_String_Literal8 =>
            --  Operator designator
            return String_To_Operator_Symbol (Formal);
         when others =>
            Error_Msg_Parse (+Formal, "incorrect formal name ignored");
            return Null_Iir;
      end case;
   end Check_Formal_Form;

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
      Arrow_Loc : Location_Type;
      Comma_Loc : Location_Type;
   begin
      Chain_Init (Res, Last);

      if Current_Token = Tok_Right_Paren then
         Error_Msg_Parse ("empty association list is not allowed");
         return Res;
      end if;

      Nbr_Assocs := 1;
      loop
         --  Parse formal and actual.
         Loc := Get_Token_Location;
         Arrow_Loc := No_Location;
         Formal := Null_Iir;

         if Current_Token /= Tok_Open then
            Actual := Parse_Expression;
            case Current_Token is
               when Tok_To
                 | Tok_Downto =>
                  --  To/downto can appear in slice name.

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

               when Tok_Range =>
                  Actual := Parse_Subtype_Indication (Actual);

               when Tok_Double_Arrow =>
                  --  Check that FORMAL is a name and not an expression.
                  Formal := Check_Formal_Form (Actual);
                  Arrow_Loc := Get_Token_Location;

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

         if Flag_Elocations then
            Create_Elocations (El);
            Set_Arrow_Location (El, Arrow_Loc);
         end if;

         Chain_Append (Res, Last, El);
         exit when Current_Token /= Tok_Comma;

         -- Eat ','.
         Comma_Loc := Get_Token_Location;
         Scan;

         if Current_Token = Tok_Right_Paren then
            Error_Msg_Parse (Comma_Loc, "extra ',' ignored");
            exit;
         end if;

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
      Expect_Scan (Tok_Left_Paren);

      Res := Parse_Association_List;

      --  Skip ')'
      Expect_Scan (Tok_Right_Paren);

      return Res;
   end Parse_Association_List_In_Parenthesis;

   --  precond : GENERIC
   --  postcond: next token
   --
   --  [ LRM93 5.2.1.2, LRM08 6.5.7.2 ]
   --  generic_map_aspect ::= GENERIC MAP ( GENERIC_association_list )
   function Parse_Generic_Map_Aspect return Iir is
   begin
      --  Skip 'generic'.
      Expect_Scan (Tok_Generic);

      --  Skip 'map'.
      Expect_Scan (Tok_Map);

      return Parse_Association_List_In_Parenthesis;
   end Parse_Generic_Map_Aspect;

   --  precond : PORT
   --  postcond: next token
   --
   --  [ LRM93 5.2.1.2 ]
   --  port_map_aspect ::= PORT MAP ( PORT_association_list )
   function Parse_Port_Map_Aspect return Iir is
   begin
      --  Skip 'port'.
      Expect_Scan (Tok_Port);

      --  Skip 'map'.
      Expect_Scan (Tok_Map);

      return Parse_Association_List_In_Parenthesis;
   end Parse_Port_Map_Aspect;

   --  precond : COMPONENT | ENTIY | CONFIGURATION
   --  postcond : next_token
   --
   --  instantiated_unit ::=
   --        [ COMPONENT ] component_name
   --      | ENTITY entity_name [ ( architecture_identifier ) ]
   --      | CONFIGURATION configuration_name
   function Parse_Instantiated_Unit return Iir
   is
      Res : Iir;
   begin
      if Flags.Vhdl_Std = Vhdl_87 then
         Report_Start_Group;
         Error_Msg_Parse
           ("component instantiation using keyword 'component', 'entity',");
         Error_Msg_Parse (" or 'configuration' is not allowed in vhdl87");
         Report_End_Group;
      end if;

      case Current_Token is
         when Tok_Component =>
            --  Eat 'component'.
            Scan;

            return Parse_Name (False);

         when Tok_Entity =>
            Res := Create_Iir (Iir_Kind_Entity_Aspect_Entity);
            Set_Location (Res);

            --  Eat 'entity'.
            Scan;

            Set_Entity_Name (Res, Parse_Name (False));
            if Current_Token = Tok_Left_Paren then
               --  Skip '('.
               Scan;

               if Current_Token = Tok_Identifier then
                  Set_Architecture (Res, Parse_Simple_Name);
               else
                  Expect (Tok_Identifier, "identifier for architecture");
               end if;

               --  Skip ')'.
               Expect_Scan (Tok_Right_Paren);
            end if;
            return Res;

         when Tok_Configuration =>
            Res := Create_Iir (Iir_Kind_Entity_Aspect_Configuration);
            Set_Location (Res);

            --  Skip 'configuration.
            Scan;

            Expect (Tok_Identifier);
            Set_Configuration_Name (Res, Parse_Name (False));
            return Res;

         when others =>
            raise Internal_Error;
      end case;
   end Parse_Instantiated_Unit;

   --  precond : next token
   --  postcond: next token
   --
   --  component_instantiation_statement ::=
   --      INSTANTIATION_label :
   --          instantiated_unit [ generic_map_aspect ] [ port_map_aspect ] ;
   function Parse_Component_Instantiation (Name: Iir)
      return Iir_Component_Instantiation_Statement
   is
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
      Expect_Scan (Tok_Semi_Colon);
      return Res;
   end Parse_Component_Instantiation;

   --  precond : next token
   --  postcond: next token
   --
   --  [ LRM93 9.1 ]
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
   --  [ LRM93 9.1 ]
   --  block_statement ::=
   --      BLOCK_label :
   --          BLOCK [ ( GUARD_expression ) ] [ IS ]
   --              block_header
   --              block_declarative_part
   --          BEGIN
   --              block_statement_part
   --          END BLOCK [ BLOCK_label ] ;
   --
   --  [ LRM93 9.1 ]
   --  block_declarative_part ::= { block_declarative_item }
   --
   --  [ LRM93 9.1 ]
   --  block_statement_part ::= { concurrent_statement }
   function Parse_Block_Statement (Label: Name_Id; Loc : Location_Type)
     return Iir_Block_Statement
   is
      Res : Iir_Block_Statement;
      Guard : Iir_Guard_Signal_Declaration;
      Begin_Loc : Location_Type;
   begin
      if Label = Null_Identifier then
         Error_Msg_Parse ("a block statement must have a label");
      end if;

      -- block was just parsed.
      Res := Create_Iir (Iir_Kind_Block_Statement);
      Set_Location (Res, Loc);
      Set_Label (Res, Label);

      --  Eat 'block'.
      Scan;

      if Current_Token = Tok_Left_Paren then
         Guard := Create_Iir (Iir_Kind_Guard_Signal_Declaration);
         Set_Location (Guard);
         Set_Guard_Decl (Res, Guard);

         --  Eat '('.
         Scan;

         Set_Guard_Expression (Guard, Parse_Expression);

         --  Eat ')'.
         Expect_Scan (Tok_Right_Paren, "')' expected after guard expression");
      end if;

      if Current_Token = Tok_Is then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse ("'is' not allowed here in vhdl87");
         end if;

         Set_Has_Is (Res, True);

         --  Eat 'is'.
         Scan;
      end if;
      if Current_Token = Tok_Generic or Current_Token = Tok_Port then
         Set_Block_Header (Res, Parse_Block_Header);
      end if;
      if Current_Token /= Tok_Begin then
         Parse_Declarative_Part (Res, Res);
      end if;

      Begin_Loc := Get_Token_Location;

      --  Eat 'begin'.
      Expect_Scan (Tok_Begin);

      Parse_Concurrent_Statements (Res);

      if Flag_Elocations then
         Create_Elocations (Res);
         Set_Begin_Location (Res, Begin_Loc);
         Set_End_Location (Res, Get_Token_Location);
      end if;

      Check_End_Name (Tok_Block, Res);
      Expect_Scan (Tok_Semi_Colon);

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
   procedure Parse_Generate_Statement_Body (Parent : Iir;
                                            Label : Name_Id;
                                            Bod : out Iir;
                                            End_Loc : out Location_Type)
   is
      function Is_Early_End return Boolean is
      begin
         case Current_Token is
            when Tok_Elsif
              | Tok_Else =>
               if Get_Kind (Parent) = Iir_Kind_If_Generate_Statement then
                  return True;
               end if;
            when Tok_When =>
               if Get_Kind (Parent) = Iir_Kind_Case_Generate_Statement then
                  return True;
               end if;
            when others =>
               null;
         end case;
         return False;
      end Is_Early_End;
   begin
      Bod := Create_Iir (Iir_Kind_Generate_Statement_Body);
      Set_Location (Bod);
      Set_Parent (Bod, Parent);
      Set_Alternative_Label (Bod, Label);
      Set_Has_Label (Bod, Label /= Null_Identifier);
      End_Loc := No_Location;

      if Flag_Elocations then
         Create_Elocations (Bod);
      end if;

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
            Parse_Declarative_Part (Bod, Bod);
            Expect (Tok_Begin);
            Set_Has_Begin (Bod, True);

            if Flag_Elocations then
               Set_Begin_Location (Bod, Get_Token_Location);
            end if;

            --  Skip 'begin'
            Scan;
         when others =>
            null;
      end case;

      Parse_Concurrent_Statements (Bod);

      --  Return now if no 'end' (and not expected).
      if Is_Early_End then
         return;
      end if;

      --  Skip 'end'
      End_Loc := Get_Token_Location;
      Expect_Scan (Tok_End);

      if Vhdl_Std >= Vhdl_08 and then Current_Token /= Tok_Generate then
         --  This is the 'end' of the generate_statement_body.
         Set_Has_End (Bod, True);
         if Flag_Elocations then
            Set_End_Location (Bod, End_Loc);
         end if;

         Check_End_Name (Label, Bod);
         Scan_Semi_Colon ("generate statement body");

         --  Return now if no 'end' (and not expected).
         if Is_Early_End then
            return;
         end if;

         Expect (Tok_End);
         End_Loc := Get_Token_Location;

         --  Skip 'end'
         Scan;
      end if;
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
      Bod : Iir;
      Start_Loc, Generate_Loc, End_Loc : Location_Type;
   begin
      if Label = Null_Identifier then
         Error_Msg_Parse ("a generate statement must have a label");
      end if;
      Res := Create_Iir (Iir_Kind_For_Generate_Statement);
      Set_Location (Res, Loc);
      Set_Label (Res, Label);
      Start_Loc := Get_Token_Location;

      --  Skip 'for'
      Scan;

      Set_Parameter_Specification (Res, Parse_Parameter_Specification (Res));

      --  Skip 'generate'
      Expect (Tok_Generate);
      Generate_Loc := Get_Token_Location;
      Scan;

      Parse_Generate_Statement_Body (Res, Null_Identifier, Bod, End_Loc);
      Set_Generate_Statement_Body (Res, Bod);

      --  Skip 'generate'
      Expect_Scan (Tok_Generate);
      Set_End_Has_Reserved_Id (Res, True);

      --  LRM93 9.7
      --  If a label appears at the end of a generate statement, it must repeat
      --  the generate label.
      Check_End_Name (Res);
      Expect_Scan (Tok_Semi_Colon);

      if Flag_Elocations then
         Create_Elocations (Res);
         Set_Start_Location (Res, Start_Loc);
         Set_Generate_Location (Res, Generate_Loc);
         Set_End_Location (Res, End_Loc);
      end if;

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
      Start_Loc, Generate_Loc, End_Loc : Location_Type;
   begin
      Start_Loc := Get_Token_Location;

      --  Skip 'if'.
      Scan;

      Cond := Parse_Expression;

      --  AMS-VHDL simultaneous if statement.
      if Current_Token = Tok_Use then
         if not AMS_Vhdl then
            Error_Msg_Parse ("if/use is an AMS-VHDL statement");
         end if;
         return Parse_Simultaneous_If_Statement (Label, Loc, Start_Loc, Cond);
      end if;

      if Label = Null_Identifier then
         Error_Msg_Parse (Start_Loc, "a generate statement must have a label");
      end if;
      Res := Create_Iir (Iir_Kind_If_Generate_Statement);
      Set_Location (Res, Loc);
      Set_Label (Res, Label);

      Clause := Res;
      Last := Null_Iir;
      loop
         Alt_Label := Null_Identifier;
         if Current_Token = Tok_Colon then
            if Get_Kind (Cond) = Iir_Kind_Simple_Name then
               Check_Vhdl_At_Least_2008 ("alternative label");

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
         Generate_Loc := Get_Token_Location;
         case Current_Token is
            when Tok_Generate =>
               --  Skip 'generate'.
               Scan;
            when Tok_Then =>
               Expect_Error (Tok_Generate);
               --  Skip 'then'.
               Scan;
            when others =>
               Expect_Error (Tok_Generate);
         end case;

         Parse_Generate_Statement_Body (Res, Alt_Label, Bod, End_Loc);

         if Alt_Label /= Null_Identifier then
            --  Set location on the label, for xrefs.
            Set_Location (Bod, Alt_Loc);
         end if;

         Set_Generate_Statement_Body (Clause, Bod);

         --  Append clause to the generate statement.
         if Last /= Null_Iir then
            Set_Generate_Else_Clause (Last, Clause);
         end if;
         Last := Clause;

         if Flag_Elocations then
            Create_Elocations (Clause);
            Set_Start_Location (Clause, Start_Loc);
            Set_Generate_Location (Clause, Generate_Loc);
            Set_End_Location (Clause, End_Loc);
         end if;

         exit when Current_Token /= Tok_Elsif;

         --  Create new alternative.
         Clause := Create_Iir (Iir_Kind_If_Generate_Else_Clause);
         Set_Location (Clause, Loc);
         Start_Loc := Get_Token_Location;

         --  Skip 'elsif'
         Scan;

         Cond := Parse_Expression;
      end loop;

      if Current_Token = Tok_Else then
         Check_Vhdl_At_Least_2008 ("else generate");

         Clause := Create_Iir (Iir_Kind_If_Generate_Else_Clause);
         Start_Loc := Get_Token_Location;
         Set_Location (Clause, Start_Loc);

         --  Skip 'else'
         Scan;

         if Current_Token = Tok_Identifier then
            Alt_Label := Current_Identifier;
            Alt_Loc := Get_Token_Location;

            --  Skip identifier
            Scan;

            --  Skip ':'
            Expect_Scan (Tok_Colon);
         else
            Alt_Label := Null_Identifier;
         end if;

         --  Skip 'generate'
         Generate_Loc := Get_Token_Location;
         Expect_Scan (Tok_Generate);

         Parse_Generate_Statement_Body (Res, Alt_Label, Bod, End_Loc);
         if Alt_Label /= Null_Identifier then
            --  Set location on the label, for xrefs.
            Set_Location (Bod, Alt_Loc);
         end if;

         Set_Generate_Statement_Body (Clause, Bod);

         Set_Generate_Else_Clause (Last, Clause);

         if Flag_Elocations then
            Create_Elocations (Clause);
            Set_Start_Location (Clause, Start_Loc);
            Set_Generate_Location (Clause, Generate_Loc);
            Set_End_Location (Clause, End_Loc);
         end if;
      end if;

      --  Skip 'generate'
      case Current_Token is
         when Tok_Generate =>
            Scan;
            Set_End_Has_Reserved_Id (Res, True);
         when Tok_If =>
            Expect_Error (Tok_Generate);
            --  Skip 'then'.
            Scan;
            Set_End_Has_Reserved_Id (Res, True);
         when others =>
            Expect_Error (Tok_Generate);
      end case;

      --  LRM93 9.7
      --  If a label appears at the end of a generate statement, it must repeat
      --  the generate label.
      Check_End_Name (Res);
      Expect_Scan (Tok_Semi_Colon);
      return Res;
   end Parse_If_Generate_Statement;

   --  precond : WHEN
   --  postcond: ?
   --
   --  [ LRM08 11.8 ]
   --  case_generate_alternative ::=
   --     WHEN [ /alternative/_label : ] choices =>
   --        generate_statement_body
   procedure Parse_Case_Generate_Alternative (Parent : Iir; Assoc : out Iir)
   is
      Loc : Location_Type;
      Alt_Label : Name_Id;
      Bod : Iir;
      Expr : Iir;
      End_Loc : Location_Type;
   begin
      Loc := Get_Token_Location;

      --  Eat 'when'
      Expect (Tok_When);
      Scan;

      Alt_Label := Null_Identifier;
      if Current_Token = Tok_Double_Arrow then
         Error_Msg_Parse ("missing expression in alternative");
         Assoc := Create_Iir (Iir_Kind_Choice_By_Expression);
         Set_Location (Assoc);
      elsif Current_Token = Tok_Others then
         --  'others' is not an expression!
         Parse_Choices (Null_Iir, Loc, Assoc);
      else
         Expr := Parse_Expression;

         if Current_Token = Tok_Colon then
            if Get_Kind (Expr) = Iir_Kind_Simple_Name then
               --  In fact the parsed condition was an alternate label.
               Alt_Label := Get_Identifier (Expr);
               Loc := Get_Location (Expr);
               Free_Iir (Expr);
            else
               Error_Msg_Parse ("alternative label must be an identifier");
               Free_Iir (Expr);
            end if;

            Expr := Null_Iir;

            --  Skip ':'
            Scan;
         end if;

         Parse_Choices (Expr, Loc, Assoc);
      end if;

      --  Set location of label (if any, for xref) or location of 'when'.
      Set_Location (Assoc, Loc);

      --  Eat '=>'
      Expect_Scan (Tok_Double_Arrow);

      Parse_Generate_Statement_Body (Parent, Alt_Label, Bod, End_Loc);
      Set_Associated_Block (Assoc, Bod);
      if Alt_Label /= Null_Identifier then
         --  Set location on the label, for xrefs.
         Set_Location (Bod, Loc);
      end if;
   end Parse_Case_Generate_Alternative;

   --  precond : CASE
   --  postcond: ';'
   --
   --  [ LRM08 11.8 ]
   --  case_generate_statement ::=
   --     /generate/_label :
   --     CASE expression GENERATE
   --        case_generate_alternative
   --      { case_generate_alternative }
   --     END GENERATE [ /generate/_label ] ;
   function Parse_Case_Generate_Statement
     (Label : Name_Id; Loc : Location_Type) return Iir
   is
      Res : Iir;
      Alt : Iir;
      Last_Alt : Iir;
      Expr : Iir;
      Start_Loc : Location_Type;
   begin
      Start_Loc := Get_Token_Location;

      --  Skip 'case'.
      Scan;

      Expr := Parse_Case_Expression;

      if Current_Token = Tok_Use then
         if not AMS_Vhdl then
            Error_Msg_Parse ("if/use is an AMS-VHDL statement");
         end if;
         return Parse_Simultaneous_Case_Statement (Label, Loc, Expr);
      end if;

      if Label = Null_Identifier then
         Error_Msg_Parse (Start_Loc, "a generate statement must have a label");
      end if;

      Res := Create_Iir (Iir_Kind_Case_Generate_Statement);
      Set_Location (Res, Loc);
      Set_Label (Res, Label);
      Set_Expression (Res, Expr);

      --  Skip 'generate'
      Expect_Scan (Tok_Generate);

      if Current_Token = Tok_End then
         Error_Msg_Parse ("no generate alternative");
      end if;

      Last_Alt := Null_Iir;
      while Current_Token = Tok_When loop
         Parse_Case_Generate_Alternative (Res, Alt);
         if Last_Alt = Null_Iir then
            Set_Case_Statement_Alternative_Chain (Res, Alt);
         else
            Set_Chain (Last_Alt, Alt);
         end if;

         --  Skip until last choice of the choices list.
         loop
            Last_Alt := Alt;
            Alt := Get_Chain (Alt);
            exit when Alt = Null_Iir;
         end loop;
      end loop;

      --  Skip 'generate'
      Expect_Scan (Tok_Generate);
      Set_End_Has_Reserved_Id (Res, True);

      --  LRM93 9.7
      --  If a label appears at the end of a generate statement, it must repeat
      --  the generate label.
      Check_End_Name (Res);
      Expect_Scan (Tok_Semi_Colon);

      return Res;
   end Parse_Case_Generate_Statement;

   --  AMS-LRM17 11.10 Simple simultaneous statement
   --  simple_simultaneous_statement ::=
   --    [ label : ] simple_expression == simple_expression
   --      [ tolerance_aspect ] ;
   function Parse_Simple_Simultaneous_Statement (Name : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Simple_Simultaneous_Statement);
      Set_Simultaneous_Left
        (Res, Parse_Binary_Expression (Name, Prio_Simple));
      Set_Location (Res);
      Expect_Scan (Tok_Equal_Equal, "'==' expected after expression");
      Set_Simultaneous_Right (Res, Parse_Expression (Prio_Simple));
      Set_Tolerance (Res, Parse_Tolerance_Aspect_Opt);
      Expect_Scan (Tok_Semi_Colon);
      return Res;
   end Parse_Simple_Simultaneous_Statement;

   --  AMS-LRM17 11.13 Simultaneous procedural statement
   --  simultaneous_procedural_statement ::=
   --    [ procedural_label : ]
   --      PROCEDURAL [ IS ]
   --        procedural_declarative_part
   --      BEGIN
   --        procedural_statement_part
   --      END PROCEDURAL [ procedural_label ] ;
   function Parse_Simultaneous_Procedural_Statement (Label : Name_Id)
                                                    return Iir
   is
      Res: Iir;
      Start_Loc, Is_Loc, Begin_Loc, End_Loc : Location_Type;
   begin
      Start_Loc := Get_Token_Location;
      Res := Create_Iir (Iir_Kind_Simultaneous_Procedural_Statement);
      Set_Location (Res, Start_Loc);
      Set_Label (Res, Label);

      --  Skip 'procedural'.
      Scan;

      if Current_Token = Tok_Is then
         Is_Loc := Get_Token_Location;
         Set_Has_Is (Res, True);

         --  Skip 'is'.
         Scan;
      end if;

      Parse_Declarative_Part (Res, Res);

      --  Skip 'begin'.
      Begin_Loc := Get_Token_Location;
      Expect_Scan (Tok_Begin);

      Set_Sequential_Statement_Chain
        (Res, Parse_Sequential_Statements (Res));

      --  Skip 'end'.
      End_Loc := Get_Token_Location;
      Expect_Scan (Tok_End);

      --  Skip 'procedural'.
      Expect_Scan (Tok_Procedural, "missing 'procedural' after 'end'");
      Set_End_Has_Reserved_Id (Res, True);

      Check_End_Name (Res);

      if Flag_Elocations then
         Create_Elocations (Res);
         Set_Start_Location (Res, Start_Loc);
         Set_Is_Location (Res, Is_Loc);
         Set_Begin_Location (Res, Begin_Loc);
         Set_End_Location (Res, End_Loc);
      end if;

      Scan_Semi_Colon_Declaration ("procedural statement");

      return Res;
   end Parse_Simultaneous_Procedural_Statement;

   --  precond : NULL
   --
   --  AMS-LRM17 11.14 Simultaneous null statement
   --  simultaneous_null_statement ::=
   --    [ label : ] NULL ;
   function Parse_Simultaneous_Null_Statement
     (Label : Name_Id; Loc : Location_Type) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Simultaneous_Null_Statement);
      Set_Location (Res, Loc);
      Set_Label (Res, Label);

      --  Skip 'procedural'.
      Scan;

      Scan_Semi_Colon_Declaration ("null statement");

      return Res;
   end Parse_Simultaneous_Null_Statement;

   --  precond : first token
   --  postcond: next token
   --
   --  [ LRM93 9.3 ]
   --  concurrent_procedure_call_statement ::=
   --      [ label : ] [ POSTPONED ] procedure_call ;
   --
   --  [ LRM93 9.5 ]
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
            return Parse_Concurrent_Conditional_Signal_Assignment (Target);
         when Tok_Semi_Colon =>
            -- a procedure call or a component instantiation.
            -- Parse it as a procedure call, may be revert to a
            -- component instantiation during sem.
            Res := Parenthesis_Name_To_Procedure_Call
              (Target, Iir_Kind_Concurrent_Procedure_Call_Statement);

            --  Skip ';'.
            Scan;

            return Res;
         when Tok_Generic | Tok_Port =>
            -- or a component instantiation.
            if Get_Kind (Target) not in Iir_Kinds_Denoting_Name then
               Error_Msg_Parse (+Target, "component name expected");
            end if;
            return Parse_Component_Instantiation (Target);
         when others =>
            --  Catch PSL clock declaration.  Within comments, this is the
            --  right place (and handled as a concurrent statement).  After
            --  vhdl08, it is a declaration.
            if Get_Kind (Target) = Iir_Kind_Simple_Name
              and then Get_Identifier (Target) = Name_Default
              and then Current_Token = Tok_Identifier
              and then Current_Identifier = Name_Clock
            then
               Error_Msg_Parse (+Target, "PSL default clock is a declaration");

               Current_Token := Tok_Psl_Clock;
               Res := Parse_Psl_Default_Clock_Cont
                 (Get_Location (Target), False);

               return Res;
            end if;

            -- or a simple simultaneous statement
            if AMS_Vhdl then
               return Parse_Simple_Simultaneous_Statement (Target);
            else
               return Parse_Concurrent_Conditional_Signal_Assignment
                 (Parse_Binary_Expression (Target, Prio_Simple));
            end if;
      end case;
   end Parse_Concurrent_Assignment;

   function Parse_Name_From_Identifier (Name : Name_Id; Loc : Location_Type)
                                       return Iir
   is
      Target : Iir;
   begin
      Target := Create_Iir (Iir_Kind_Simple_Name);
      Set_Location (Target, Loc);
      Set_Identifier (Target, Name);
      return Parse_Name_Suffix (Target);
   end Parse_Name_From_Identifier;

   function Parse_Concurrent_Assignment_With_Name
     (Name : Name_Id; Loc : Location_Type) return Iir
   is
      Target : Iir;
   begin
      Target := Parse_Name_From_Identifier (Name, Loc);
      return Parse_Concurrent_Assignment (Target);
   end Parse_Concurrent_Assignment_With_Name;

   --  AMS-LRM17 11.9 Concurrent break statement
   --  concurrent_break_statement ::=
   --    [ label : ] BREAK [ break_list ] [ sensitivity_clause ]
   --      [ WHEN condition ] ;
   function Parse_Concurrent_Break_Statement (Label : Name_Id;
                                              Loc : Location_Type) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Concurrent_Break_Statement);
      Set_Location (Res, Loc);
      Set_Label (Res, Label);

      --  Skip 'break'.
      Scan;

      Set_Break_Element (Res, Parse_Break_List);

      if Current_Token = Tok_On then
         --  Sensitivity list.
         --  Skip 'on'.
         Scan;

         Set_Sensitivity_List (Res, Parse_Sensitivity_List);
      end if;

      if Current_Token = Tok_When then
         --  Condition.
         --  Skip 'when'.
         Scan;

         Set_Condition (Res, Parse_Expression);
      end if;

      --  Skip ';'.
      Expect_Scan (Tok_Semi_Colon);

      return Res;
   end Parse_Concurrent_Break_Statement;

   --  AMS-LRM17 11 Architecture statements
   --  simultaneous_statement ::=
   --      simple_simultaneous_statement
   --    | simultaneous_if_statement
   --    | simultaneous_case_statement
   --    | simultaneous_procedural_statement
   --    | simultaneous_null_statement
   --
   --  simultaneous_statement_part ::=
   --    { simultaneous_statement }
   function Parse_Simultaneous_Statements (Parent : Iir) return Iir
   is
      First_Stmt, Last_Stmt : Iir;
      Stmt: Iir;
      Label: Name_Id;
      Loc : Location_Type;
      Start_Loc : Location_Type;
      Expr : Iir;
   begin
      Chain_Init (First_Stmt, Last_Stmt);
      loop
         Stmt := Null_Iir;
         Label := Null_Identifier;
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
               Expr := Parse_Name_From_Identifier (Label, Loc);
               Stmt := Parse_Simple_Simultaneous_Statement (Expr);
               Label := Null_Identifier;
               goto Has_Stmt;
            end if;
         end if;

         case Current_Token is
            when Tok_End | Tok_Else | Tok_Elsif | Tok_When =>
               --  End of list.  'else', 'elseif' and 'when' can be used to
               --  separate statements in a generate statement.
               if Label /= Null_Identifier then
                  Error_Msg_Parse ("label is not allowed here");
               end if;
               return First_Stmt;
            when Tok_Identifier =>
               --  FIXME: sign, factor, parenthesis...
               Expr := Parse_Name (Allow_Indexes => True);
               Stmt := Parse_Simple_Simultaneous_Statement (Expr);
            when Tok_If =>
               Start_Loc := Get_Token_Location;

               --  Skip 'if'.
               Scan;

               Expr := Parse_Expression;

               Stmt := Parse_Simultaneous_If_Statement
                 (Label, Loc, Start_Loc, Expr);
            when Tok_Case =>
               --  Skip 'case'.
               Scan;

               Expr := Parse_Expression;

               Stmt := Parse_Simultaneous_Case_Statement (Label, Loc, Expr);
            when Tok_Null =>
               Stmt := Parse_Simultaneous_Null_Statement (Label, Loc);
            when Tok_Eof =>
               Error_Msg_Parse ("unexpected end of file, 'END;' expected");
               return First_Stmt;
            when others =>
               --  FIXME: improve message:
               Unexpected ("simultaneous statement list");
               Resync_To_End_Of_Statement;
               if Current_Token = Tok_Semi_Colon then
                  Scan;
               end if;
         end case;

         << Has_Stmt >> null;

         --  Stmt can be null in case of error.
         if Stmt /= Null_Iir then
            Set_Location (Stmt, Loc);
            if Label /= Null_Identifier then
               Set_Label (Stmt, Label);
            end if;
            Set_Parent (Stmt, Parent);
            --  Append it to the chain.
            Chain_Append (First_Stmt, Last_Stmt, Stmt);
         end if;
      end loop;
   end Parse_Simultaneous_Statements;

   --  AMS-LRM17 11.11 Simultaneous if statement
   --  simultaneous_if_statement ::=
   --    [ /if/_label : ]
   --      IF condition USE
   --        simultaneous_statement_part
   --      { ELSIF condition USE
   --        simultaneous_statement_part }
   --      [ ELSE
   --        simultaneous_statement_part ]
   --      END USE [ /if/_label ];
   function Parse_Simultaneous_If_Statement (Label : Name_Id;
                                             Label_Loc : Location_Type;
                                             If_Loc : Location_Type;
                                             First_Cond : Iir) return Iir
   is
      Res : Iir;
      Clause : Iir;
      N_Clause : Iir;
      Start_Loc, Use_Loc, End_Loc : Location_Type;
   begin
      Res := Create_Iir (Iir_Kind_Simultaneous_If_Statement);
      Set_Location (Res, Label_Loc);
      Set_Label (Res, Label);
      Set_Condition (Res, First_Cond);

      Start_Loc := If_Loc;
      Clause := Res;
      loop
         -- Set_Condition (Clause, Parse_Expression);
         Use_Loc := Get_Token_Location;
         if Current_Token = Tok_Use then
            --  Eat 'use'.
            Scan;
         else
            Expect_Error (Tok_Use, "'use' is expected here");
         end if;

         Set_Simultaneous_Statement_Chain
           (Clause, Parse_Simultaneous_Statements (Clause));

         End_Loc := Get_Token_Location;

         if Flag_Elocations then
            Create_Elocations (Clause);
            Set_Start_Location (Clause, Start_Loc);
            Set_Use_Location (Clause, Use_Loc);
            Set_End_Location (Clause, End_Loc);
         end if;

         exit when Current_Token /= Tok_Else and Current_Token /= Tok_Elsif;

         N_Clause := Create_Iir (Iir_Kind_Simultaneous_Elsif);
         Start_Loc := Get_Token_Location;
         Set_Location (N_Clause, Start_Loc);
         Set_Else_Clause (Clause, N_Clause);
         Clause := N_Clause;
         if Current_Token = Tok_Else then

            --  Skip 'else'.
            Scan;

            Set_Simultaneous_Statement_Chain
              (Clause, Parse_Simultaneous_Statements (Clause));

            if Flag_Elocations then
               Create_Elocations (Clause);
               Set_Start_Location (Clause, Start_Loc);
               Set_End_Location (Clause, Get_Token_Location);
            end if;

            exit;
         else
            pragma Assert (Current_Token = Tok_Elsif);
            --  Skip 'elsif'.
            Scan;

            Set_Condition (Clause, Parse_Expression);
         end if;
      end loop;

      --  Skip 'end' 'use'
      Expect_Scan (Tok_End);
      Expect_Scan (Tok_Use);

      Expect_Scan (Tok_Semi_Colon);

      return Res;
   end Parse_Simultaneous_If_Statement;

   --  simultaneous_case_statement ::=
   --     /case/_label :
   --     CASE expression USE
   --        simultaneous_alternative
   --      { simultaneous_alternative }
   --     END CASE [ /case/_label ] ;
   function Parse_Simultaneous_Case_Statement
     (Label : Name_Id; Loc : Location_Type; Expr : Iir) return Iir
   is
      Res : Iir;
      When_Loc : Location_Type;
      Assoc : Iir;
      First_Assoc, Last_Assoc : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Simultaneous_Case_Statement);
      Set_Location (Res, Loc);
      Set_Label (Res, Label);
      Set_Expression (Res, Expr);

      --  Skip 'use'
      Expect_Scan (Tok_Use);

      if Current_Token = Tok_End then
         Error_Msg_Parse ("no generate alternative");
      end if;

      Chain_Init (First_Assoc, Last_Assoc);
      while Current_Token = Tok_When loop
         When_Loc := Get_Token_Location;

         --  Skip 'when'.
         Scan;

         Parse_Choices (Null_Iir, When_Loc, Assoc);

         --  Skip '=>'.
         Expect_Scan (Tok_Double_Arrow);

         Set_Associated_Chain (Assoc, Parse_Simultaneous_Statements (Res));
         Chain_Append_Subchain (First_Assoc, Last_Assoc, Assoc);
      end loop;

      Set_Case_Statement_Alternative_Chain (Res, First_Assoc);

      --  Skip 'end', 'case'
      Expect_Scan (Tok_End);
      Expect_Scan (Tok_Case);

      --  LRM93 9.7
      --  If a label appears at the end of a generate statement, it must repeat
      --  the generate label.
      Check_End_Name (Res);
      Expect_Scan (Tok_Semi_Colon);

      return Res;
   end Parse_Simultaneous_Case_Statement;

   --  Parse end of PSL assert/cover statement.
   procedure Parse_Psl_Assert_Report_Severity
     (Stmt : Iir; Flag_Psl : Boolean) is
   begin
      --  No more PSL tokens after the property.
      Vhdl.Scanner.Flag_Psl := Flag_Psl;

      if Current_Token = Tok_Report then
         --  Skip 'report'
         Scan;

         Set_Report_Expression (Stmt, Parse_Expression);
      end if;

      if Current_Token = Tok_Severity then
         --  Skip 'severity'
         Scan;

         Set_Severity_Expression (Stmt, Parse_Expression);
      end if;

      Vhdl.Scanner.Flag_Scan_In_Comment := False;

      Expect_Scan (Tok_Semi_Colon);
   end Parse_Psl_Assert_Report_Severity;

   function Parse_Psl_Assert_Directive (Flag_Psl : Boolean) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Psl_Assert_Directive);
      Set_Location (Res);

      --  Accept PSL tokens
      if Flags.Vhdl_Std >= Vhdl_08 then
         Vhdl.Scanner.Flag_Psl := True;
      end if;

      --  Skip 'assert'
      Scan;

      Set_Psl_Property (Res, Parse_Psl.Parse_Psl_Property);

      Parse_Psl_Assert_Report_Severity (Res, Flag_Psl);

      return Res;
   end Parse_Psl_Assert_Directive;

   function Parse_Psl_Assume_Directive (Flag_Psl : Boolean) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Psl_Assume_Directive);
      Set_Location (Res);

      --  Accept PSL tokens
      Vhdl.Scanner.Flag_Psl := True;

      --  Skip 'assume'
      Scan;

      Set_Psl_Property (Res, Parse_Psl.Parse_Psl_Property);

      Vhdl.Scanner.Flag_Psl := Flag_Psl;
      Vhdl.Scanner.Flag_Scan_In_Comment := False;

      Expect_Scan (Tok_Semi_Colon);

      return Res;
   end Parse_Psl_Assume_Directive;

   function Parse_Psl_Cover_Directive (Flag_Psl : Boolean) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Psl_Cover_Directive);

      --  Accept PSL tokens
      Vhdl.Scanner.Flag_Psl := True;

      --  Skip 'cover'
      Scan;

      Set_Psl_Sequence (Res, Parse_Psl.Parse_Psl_Sequence);

      Parse_Psl_Assert_Report_Severity (Res, Flag_Psl);

      return Res;
   end Parse_Psl_Cover_Directive;

   function Parse_Psl_Restrict_Directive (Flag_Psl : Boolean) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Psl_Restrict_Directive);

      --  Accept PSL tokens
      Vhdl.Scanner.Flag_Psl := True;

      --  Skip 'restrict'
      Scan;

      Set_Psl_Sequence (Res, Parse_Psl.Parse_Psl_Sequence);

      --  No more PSL tokens after the sequence.
      Vhdl.Scanner.Flag_Psl := Flag_Psl;
      Vhdl.Scanner.Flag_Scan_In_Comment := False;

      Expect_Scan (Tok_Semi_Colon);
      return Res;
   end Parse_Psl_Restrict_Directive;

   --  precond : first token
   --  postcond: next token (end/else/when...)
   --
   --  [ LRM93 9 ]
   --  concurrent_statement ::= block_statement
   --                         | process_statement
   --                         | concurrent_procedure_call_statement
   --                         | concurrent_assertion_statement
   --                         | concurrent_signal_assignment_statement
   --                         | component_instantiation_statement
   --                         | generate_statement
   --
   function Parse_Concurrent_Statement (Parent : Iir; Prev_Label : Name_Id)
                                        return Iir
   is
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
      loop
         Stmt := Null_Iir;
         Label := Null_Identifier;
         Postponed := False;
         Loc := Get_Token_Location;

         -- Try to find a label.
         if Prev_Label /= Null_Identifier then
            Label := Prev_Label;
         elsif Current_Token = Tok_Identifier then
            Label := Current_Identifier;

            --  Skip identifier
            Scan;

            if Current_Token = Tok_Colon then
               --  The identifier is really a label.

               --  Skip ':'
               Scan;
            else
               --  This is not a label.  Assume a concurrent assignment.
               Stmt := Parse_Concurrent_Assignment_With_Name (Label, Loc);
               Label := Null_Identifier;
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
               return Null_Iir;
            when Tok_Identifier
              | Tok_Double_Less =>
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
                  Stmt := Parse_Concurrent_Conditional_Signal_Assignment (Id);
               else
                  Error_Msg_Parse ("'<=' expected after aggregate");
                  Skip_Until_Semi_Colon;
               end if;
            when Tok_Process =>
               Stmt := Parse_Process_Statement (Label, Loc, Postponed);
            when Tok_Assert =>
               if Vhdl_Std >= Vhdl_08
                 or else (Flag_Psl_Comment and then Flag_Scan_In_Comment)
               then
                  Stmt := Parse_Psl_Assert_Directive (False);
               else
                  Stmt := Create_Iir (Iir_Kind_Concurrent_Assertion_Statement);
                  Parse_Assertion (Stmt);
                  Expect_Scan (Tok_Semi_Colon);
               end if;
            when Tok_With =>
               Stmt := Parse_Selected_Signal_Assignment;
            when Tok_Block =>
               Postponed_Not_Allowed;
               Stmt := Parse_Block_Statement (Label, Loc);
            when Tok_For =>
               Postponed_Not_Allowed;
               Stmt := Parse_For_Generate_Statement (Label, Loc);
            when Tok_If =>
               Postponed_Not_Allowed;
               Stmt := Parse_If_Generate_Statement (Label, Loc);
            when Tok_Case =>
               Postponed_Not_Allowed;
               Check_Vhdl_At_Least_2008 ("case generate");
               Stmt := Parse_Case_Generate_Statement (Label, Loc);
            when Tok_Component
              | Tok_Entity
              | Tok_Configuration =>
               Postponed_Not_Allowed;
               declare
                  Unit : Iir;
                  Has_Component : constant Boolean :=
                    Current_Token = Tok_Component;
               begin
                  Unit := Parse_Instantiated_Unit;
                  Stmt := Parse_Component_Instantiation (Unit);
                  Set_Has_Component (Stmt, Has_Component);
               end;
            when Tok_Break =>
               Postponed_Not_Allowed;
               Stmt := Parse_Concurrent_Break_Statement (Label, Loc);
            when Tok_Procedural =>
               Postponed_Not_Allowed;
               Stmt := Parse_Simultaneous_Procedural_Statement (Label);
            when Tok_Null =>
               if not AMS_Vhdl then
                  Error_Msg_Parse ("concurrent null statement not allowed");
               else
                  Postponed_Not_Allowed;
               end if;
               Stmt := Parse_Simultaneous_Null_Statement (Label, Loc);
            when Tok_Default =>
               Postponed_Not_Allowed;
               Label_Not_Allowed;
               Stmt := Parse_Psl_Default_Clock (False);
            when Tok_Property
              | Tok_Sequence
              | Tok_Psl_Endpoint =>
               Postponed_Not_Allowed;
               Label_Not_Allowed;
               Stmt := Parse_Psl_Declaration;
            when Tok_Assume =>
               Postponed_Not_Allowed;
               Stmt := Parse_Psl_Assume_Directive (False);
            when Tok_Cover =>
               Postponed_Not_Allowed;
               Stmt := Parse_Psl_Cover_Directive (False);
            when Tok_Restrict =>
               Postponed_Not_Allowed;
               Stmt := Parse_Psl_Restrict_Directive (False);
            when Tok_Wait
              | Tok_Loop
              | Tok_While =>
               Error_Msg_Parse
                 ("sequential statement only allowed in processes");
               Stmt := Parse_Sequential_Statements (Parent);
               --  Continue.
               Stmt := Null_Iir;
            when Tok_Eof =>
               Error_Msg_Parse ("unexpected end of file, 'END;' expected");
               return Null_Iir;
            when others =>
               --  FIXME: improve message:
               --  instead of 'unexpected token 'signal' in conc stmt list'
               --  report: 'signal declarations are not allowed in conc stmt'
               Unexpected ("concurrent statement list");
               Resync_To_End_Of_Statement;
               if Current_Token = Tok_Semi_Colon then
                  Scan;
               end if;
               Stmt := Null_Iir;
         end case;

         << Has_Stmt >> null;

         --  Stmt can be null in case of error.
         if Stmt /= Null_Iir then
            Set_Location (Stmt, Loc);
            Set_Parent (Stmt, Parent);
            if Label /= Null_Identifier then
               Set_Label (Stmt, Label);
            end if;
            Set_Parent (Stmt, Parent);
            if Postponed then
               Set_Postponed_Flag (Stmt, True);
            end if;
            return Stmt;
         end if;
      end loop;
   end Parse_Concurrent_Statement;

   --  precond : first token
   --  postcond: next token (end/else/when...)
   procedure Parse_Concurrent_Statements (Parent : Iir)
   is
      Last_Stmt : Iir;
      Stmt      : Iir;
   begin
      -- begin was just parsed.
      Last_Stmt := Null_Iir;
      loop
         Stmt := Parse_Concurrent_Statement (Parent, Null_Identifier);
         exit when Stmt = Null_Iir;

         --  Append it to the chain.
         if Last_Stmt = Null_Iir then
            Set_Concurrent_Statement_Chain (Parent, Stmt);
         else
            Set_Chain (Last_Stmt, Stmt);
         end if;
         Last_Stmt := Stmt;
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
      Start_Loc : Location_Type;
   begin
      Chain_Init (First, Last);
      Expect (Tok_Library);
      loop
         Library := Create_Iir (Iir_Kind_Library_Clause);
         Start_Loc := Get_Token_Location;
         Chain_Append (First, Last, Library);

         --  Skip 'library' or ','.
         Scan;

         Scan_Identifier (Library);

         if Flag_Elocations then
            Create_Elocations (Library);
            Set_Start_Location (Library, Start_Loc);
         end if;

         exit when Current_Token /= Tok_Comma;

         Set_Has_Identifier_List (Library, True);
      end loop;

      --  Skip ';'.
      Scan_Semi_Colon ("library clause");

      return First;
   end Parse_Library_Clause;

   --  precond : USE
   --  postcond: next token (after ';').
   --
   --  [ LRM93 10.4 ]
   --  use_clause ::= USE selected_name { , selected_name }
   --
   --  FIXME: should be a list.
   function Parse_Use_Clause return Iir_Use_Clause
   is
      Use_Clause: Iir_Use_Clause;
      Loc : Location_Type;
      First, Last : Iir;
   begin
      First := Null_Iir;
      Last := Null_Iir;

      Loc := Get_Token_Location;

      --  Skip 'use'.
      Scan;

      loop
         Use_Clause := Create_Iir (Iir_Kind_Use_Clause);
         Set_Location (Use_Clause, Loc);
         Expect (Tok_Identifier);
         Set_Selected_Name (Use_Clause, Parse_Name);

         --  Chain use clauses.
         if First = Null_Iir then
            First := Use_Clause;
         else
            Set_Use_Clause_Chain (Last, Use_Clause);
         end if;
         Last := Use_Clause;

         exit when Current_Token /= Tok_Comma;
         Loc := Get_Token_Location;

         --  Skip ','.
         Scan;
      end loop;

      --  Skip ';'.
      Scan_Semi_Colon ("use clause");

      return First;
   end Parse_Use_Clause;

   --  precond : ARCHITECTURE
   --  postcond: ';'.
   --
   --  [ LRM93 1.2 ]
   --  architecture_body ::=
   --      ARCHITECTURE identifier OF ENTITY_name IS
   --          architecture_declarative_part
   --      BEGIN
   --          architecture_statement_part
   --      END [ ARCHITECTURE ] [ ARCHITECTURE_simple_name ] ;
   procedure Parse_Architecture_Body (Unit : Iir_Design_Unit)
   is
      Res : Iir_Architecture_Body;
      Start_Loc : Location_Type;
      Begin_Loc : Location_Type;
      End_Loc : Location_Type;
   begin
      Expect (Tok_Architecture);
      Res := Create_Iir (Iir_Kind_Architecture_Body);
      Start_Loc := Get_Token_Location;

      --  Skip 'architecture'.
      Scan;

      --  Comments after 'architecture' but before the first declaration are
      --  attached to the architecture.
      if Flag_Gather_Comments then
         Gather_Comments_Block (Res);
      end if;

      --  Identifier.
      Scan_Identifier (Res);

      --  Skip 'of'.
      Expect_Scan (Tok_Of);

      Set_Entity_Name (Res, Parse_Name (False));

      --  Skip 'is'.
      Expect_Scan (Tok_Is);

      Parse_Declarative_Part (Res, Res);

      --  Comments just before the 'begin' are attached to the last declaration
      --  or the architecture (if no declarations).
      if Flag_Gather_Comments then
         Gather_Comments_End;
      end if;

      --  Skip 'begin'.
      Begin_Loc := Get_Token_Location;
      Expect_Scan (Tok_Begin);

      Parse_Concurrent_Statements (Res);
      -- end was scanned.
      End_Loc := Get_Token_Location;

      --  Skip 'end'.
      Expect_Scan (Tok_End);

      if Current_Token = Tok_Architecture then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse
              ("'architecture' keyword not allowed here by vhdl 87");
         end if;
         Set_End_Has_Reserved_Id (Res, True);

         --  Skip 'architecture'.
         Scan;
      end if;
      Check_End_Name (Res);
      Scan_Semi_Colon_Unit ("architecture");

      Set_Library_Unit (Unit, Res);

      if Flag_Elocations then
         Create_Elocations (Res);
         Set_Start_Location (Res, Start_Loc);
         Set_Begin_Location (Res, Begin_Loc);
         Set_End_Location (Res, End_Loc);
      end if;
   end Parse_Architecture_Body;

   --  precond : next token
   --  postcond: a token
   --
   --  [ LRM93 5.2 ]
   --  instantiation_list ::= INSTANTIATION_label { , INSTANTIATION_label }
   --                       | OTHERS
   --                       | ALL
   --
   --  FIXME: merge with parse_signal_list ?
   function Parse_Instantiation_List return Iir_Flist
   is
      Res : Iir_List;
   begin
      case Current_Token is
         when Tok_All =>
            --  Skip 'all'.
            Scan;

            return Iir_Flist_All;

         when Tok_Others =>
            --  Skip 'others'.
            Scan;

            return Iir_Flist_Others;

         when Tok_Identifier =>
            Res := Create_Iir_List;
            loop
               Append_Element (Res, Parse_Simple_Name);

               exit when Current_Token /= Tok_Comma;

               --  Skip ','.
               Scan;

               if Current_Token /= Tok_Identifier then
                  Expect (Tok_Identifier);
                  exit;
               end if;
            end loop;
            return List_To_Flist (Res);

         when others =>
            Error_Msg_Parse ("instantiation list expected");
            return Null_Iir_Flist;
      end case;
   end Parse_Instantiation_List;

   --  precond : next token
   --  postcond: next token
   --
   --  [ LRM93 5.2 ]
   --  component_specification ::= instantiation_list : COMPONENT_name
   procedure Parse_Component_Specification (Res : Iir)
   is
      List : Iir_Flist;
   begin
      List := Parse_Instantiation_List;
      Set_Instantiation_List (Res, List);

      --  Skip ':'.
      Expect_Scan (Tok_Colon);

      Expect (Tok_Identifier);
      Set_Component_Name (Res, Parse_Name);
   end Parse_Component_Specification;

   --  precond : next token
   --  postcond: next token
   --
   --  [ LRM93 5.2.1.1 ]
   --  entity_aspect ::= ENTITY ENTITY_name [ ( ARCHITECTURE_identifier ) ]
   function Parse_Entity_Aspect_Entity return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Entity_Aspect_Entity);
      Set_Location (Res);

      if Current_Token = Tok_Entity then
         --  Eat 'entity' (but only if present).
         Scan;
      end if;

      Expect (Tok_Identifier);
      Set_Entity_Name (Res, Parse_Name (False));

      --  Optional architecture
      if Current_Token = Tok_Left_Paren then
         --  Skip '('.
         Scan;

         if Current_Token = Tok_Identifier then
            Set_Architecture (Res, Parse_Simple_Name);
         else
            Expect (Tok_Identifier);
         end if;

         Expect_Scan (Tok_Right_Paren);
      end if;

      return Res;
   end Parse_Entity_Aspect_Entity;

   --  precond : next token
   --  postcond: next token
   --
   --  [ LRM93 5.2.1.1 ]
   --  entity_aspect ::= ENTITY ENTITY_name [ ( ARCHITECTURE_identifier ) ]
   --                  | CONFIGURATION CONFIGURATION_name
   --                  | OPEN
   function Parse_Entity_Aspect return Iir
   is
      Res : Iir;
   begin
      case Current_Token is
         when Tok_Entity =>
            Res := Parse_Entity_Aspect_Entity;
         when Tok_Configuration =>
            Res := Create_Iir (Iir_Kind_Entity_Aspect_Configuration);
            Set_Location (Res);

            --  Skip 'configuration'.
            Scan;

            Expect (Tok_Identifier);
            Set_Configuration_Name (Res, Parse_Name (False));
         when Tok_Open =>
            Res := Create_Iir (Iir_Kind_Entity_Aspect_Open);
            Set_Location (Res);
            Scan;
         when others =>
            Error_Msg_Parse ("'entity', 'configuration' or 'open' expected");
            --  Assume 'entity' is missing (common case).
            Res := Parse_Entity_Aspect_Entity;
      end case;
      return Res;
   end Parse_Entity_Aspect;

   --  precond : next token
   --  postcond: next token
   --
   --  [ LRM93 5.2.1 ]
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
   --  postcond: next token.
   --
   --  [ LRM93 1.3.2 ]
   --  component_configuration ::=
   --      FOR component_specification
   --          [ binding_indication ; ]
   --          [ block_configuration ]
   --      END FOR ;
   function Parse_Component_Configuration (Loc : Location_Type;
                                           Inst_List : Iir_Flist)
     return Iir_Component_Configuration
   is
      Res : Iir_Component_Configuration;
   begin
      Res := Create_Iir (Iir_Kind_Component_Configuration);
      Set_Location (Res, Loc);

      --  Skip ':'.
      pragma Assert (Current_Token = Tok_Colon);
      Scan;

      --  Component specification.
      Set_Instantiation_List (Res, Inst_List);

      Expect (Tok_Identifier);
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
      end if;
      Expect_Scan (Tok_End);
      Expect_Scan (Tok_For);
      Expect_Scan (Tok_Semi_Colon);
      return Res;
   end Parse_Component_Configuration;

   --  precond : FOR
   --  postcond: next token.
   --
   --  [ LRM93 1.3.1 ]
   --  block_configuration ::=
   --      FOR block_specification
   --          { use_clause }
   --          { configuration_item }
   --      END FOR ;
   --
   --  [ LRM93 1.3.1 ]
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
            First, Last : Iir;
         begin
            Chain_Init (First, Last);

            while Current_Token = Tok_Use loop
               Chain_Append_Subchain (First, Last, Parse_Use_Clause);
            end loop;
            Set_Declaration_Chain (Res, First);
         end;
      end if;

      --  Parse configuration item list
      declare
         First, Last : Iir;
         Item : Iir;
      begin
         Chain_Init (First, Last);
         while Current_Token = Tok_For loop
            Item := Parse_Configuration_Item;
            exit when Item = Null_Iir;
            Chain_Append (First, Last, Item);
         end loop;
         Set_Configuration_Item_Chain (Res, First);
      end;
      Expect_Scan (Tok_End);
      Expect_Scan (Tok_For);
      Expect_Scan (Tok_Semi_Colon);
      return Res;
   end Parse_Block_Configuration_Suffix;

   function Parse_Block_Configuration return Iir_Block_Configuration
   is
      Loc : Location_Type;
   begin
      Loc := Get_Token_Location;

      --  Skip 'for'.
      Expect_Scan (Tok_For);

      return Parse_Block_Configuration_Suffix (Loc, Parse_Name);
   end Parse_Block_Configuration;

   --  precond : FOR
   --  postcond: next token.
   --
   --  [ LRM93 1.3.1 ]
   --  configuration_item ::= block_configuration
   --                       | component_configuration
   function Parse_Configuration_Item return Iir
   is
      Loc : Location_Type;
      List : Iir_List;
      Flist : Iir_Flist;
      El : Iir;
   begin
      Loc := Get_Token_Location;
      Expect_Scan (Tok_For);

      --  ALL and OTHERS are tokens from an instantiation list.
      --  Thus, the rule is a component_configuration.
      case Current_Token is
         when Tok_All =>
            --  Skip 'all'.
            Scan;

            return Parse_Component_Configuration (Loc, Iir_Flist_All);

         when Tok_Others =>
            --  Skip 'others'.
            Scan;

            return Parse_Component_Configuration (Loc, Iir_Flist_Others);

         when Tok_Identifier =>
            El := Parse_Simple_Name;

            case Current_Token is
               when Tok_Colon =>
                  --  The identifier was a label from an instantiation list.
                  Flist := Create_Iir_Flist (1);
                  Set_Nth_Element (Flist, 0, El);
                  return Parse_Component_Configuration (Loc, Flist);
               when Tok_Comma =>
                  --  The identifier was a label from an instantiation list.
                  List := Create_Iir_List;
                  Append_Element (List, El);
                  while Current_Token = Tok_Comma loop
                     --  Skip ','.
                     Scan;

                     if Current_Token = Tok_Identifier then
                        Append_Element (List, Parse_Simple_Name);
                     else
                        Expect (Tok_Identifier);
                        exit;
                     end if;
                  end loop;
                  Flist := List_To_Flist (List);
                  return Parse_Component_Configuration (Loc, Flist);
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
                  return Null_Iir;
            end case;
         when others =>
            Error_Msg_Parse ("configuration item expected");
            return Null_Iir;
      end case;
   end Parse_Configuration_Item;

   --  precond : next token
   --  postcond: next token
   --
   --  [ LRM93 1.3]
   --  configuration_declarative_part ::= { configuration_declarative_item }
   --
   --  [ LRM93 1.3]
   --  configuration_declarative_item ::= use_clause
   --                                   | attribute_specification
   --                                   | group_declaration
   --  FIXME: attribute_specification, group_declaration
   procedure Parse_Configuration_Declarative_Part (Parent : Iir)
   is
      First, Last : Iir;
      El : Iir;
   begin
      Chain_Init (First, Last);
      loop
         case Current_Token is
            when Tok_Invalid =>
               raise Internal_Error;
            when Tok_Use =>
               Chain_Append_Subchain (First, Last, Parse_Use_Clause);
            when Tok_Attribute =>
               El := Parse_Attribute;
               if El /= Null_Iir then
                  if Get_Kind (El) /= Iir_Kind_Attribute_Specification then
                     Error_Msg_Parse
                       ("attribute declaration not allowed here");
                  end if;
                  Set_Parent (El, Parent);
                  Chain_Append (First, Last, El);
               end if;
            when Tok_Group =>
               El := Parse_Group;
               if El /= Null_Iir then
                  if Get_Kind (El) /= Iir_Kind_Group_Declaration then
                     Error_Msg_Parse
                       ("group template declaration not allowed here");
                  end if;
                  Set_Parent (El, Parent);
                  Chain_Append (First, Last, El);
               end if;
            when others =>
               exit;
         end case;
      end loop;
      Set_Declaration_Chain (Parent, First);
   end Parse_Configuration_Declarative_Part;

   --  precond : CONFIGURATION
   --  postcond: next token.
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
      Start_Loc : Location_Type;
      End_Loc : Location_Type;
   begin
      pragma Assert (Current_Token = Tok_Configuration);
      Res := Create_Iir (Iir_Kind_Configuration_Declaration);
      Start_Loc := Get_Token_Location;

      --  Skip 'configuration'.
      pragma Assert (Current_Token = Tok_Configuration);
      Scan;

      --  Get identifier.
      Scan_Identifier (Res);

      --  Skip 'of'.
      Expect_Scan (Tok_Of);

      Set_Entity_Name (Res, Parse_Name (False));

      --  Skip 'is'.
      Expect_Scan (Tok_Is);

      --  Comments after 'context' but before the first clause are attached
      --  to the context.
      if Flag_Gather_Comments then
         Gather_Comments_Block (Res);
      end if;

      Parse_Configuration_Declarative_Part (Res);

      Set_Block_Configuration (Res, Parse_Block_Configuration);

      End_Loc := Get_Token_Location;
      --  Skip 'end'.
      Expect_Scan (Tok_End);

      if Current_Token = Tok_Configuration then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse
              ("'configuration' keyword not allowed here by vhdl 87");
         end if;
         Set_End_Has_Reserved_Id (Res, True);

         --  Skip 'configuration'.
         Scan;
      end if;

      --  LRM93 1.3
      --  If a simple name appears at the end of a configuration declaration,
      --  it must repeat the identifier of the configuration declaration.
      Check_End_Name (Res);
      Scan_Semi_Colon_Unit ("configuration");

      Set_Library_Unit (Unit, Res);

      if Flag_Elocations then
         Create_Elocations (Res);
         Set_Start_Location (Res, Start_Loc);
         Set_End_Location (Res, End_Loc);
      end if;
   end Parse_Configuration_Declaration;

   --  Return the parent of a nested package.  Used to check if some
   --  declarations are allowed in a package.
   function Get_Package_Parent (Decl : Iir) return Iir
   is
      Res : Iir;
      Parent : Iir;
   begin
      Res := Decl;
      loop
         case Get_Kind (Res) is
            when Iir_Kind_Package_Declaration
              | Iir_Kind_Package_Body =>
               Parent := Get_Parent (Res);
               if Get_Kind (Parent) = Iir_Kind_Design_Unit then
                  return Res;
               else
                  Res := Parent;
               end if;
            when others =>
               return Res;
         end case;
      end loop;
   end Get_Package_Parent;

   --  precond : generic
   --  postcond: next token
   --
   --  [ LRM08 4.7 ]
   --  package_header ::=
   --      [ generic_clause               -- LRM08 6.5.6.2
   --      [ generic_map aspect ; ] ]
   function Parse_Package_Header return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Package_Header);
      Set_Location (Res);
      Parse_Generic_Clause (Res);

      if Current_Token = Tok_Generic then
         Set_Generic_Map_Aspect_Chain (Res, Parse_Generic_Map_Aspect);
         Scan_Semi_Colon ("generic map aspect");
      end if;
      return Res;
   end Parse_Package_Header;

   --  precond : token (after 'IS')
   --  postcond: next token.
   --
   --  [ LRM93 2.5, LRM08 4.7 ]
   --  package_declaration ::=
   --      PACKAGE identifier IS
   --          package_header           -- LRM08
   --          package_declarative_part
   --      END [ PACKAGE ] [ PACKAGE_simple_name ] ;
   function Parse_Package_Declaration (Parent : Iir;
                                       Id : Name_Id;
                                       Loc : Location_Type;
                                       Comments : Comments_Range) return Iir
   is
      Res: Iir_Package_Declaration;
      End_Loc : Location_Type;
   begin
      Res := Create_Iir (Iir_Kind_Package_Declaration);
      Set_Location (Res, Loc);
      Set_Identifier (Res, Id);
      Set_Parent (Res, Parent);

      --  Comments after 'package' but before the first declaration are
      --  attached to the package.
      if Flag_Gather_Comments then
         Gather_Comments_Block (Comments, Res);
      end if;

      if Current_Token = Tok_Generic then
         Check_Vhdl_At_Least_2008 ("generic packages");
         Set_Package_Header (Res, Parse_Package_Header);
      end if;

      Parse_Declarative_Part (Res, Get_Package_Parent (Res));

      End_Loc := Get_Token_Location;

      --  Comments just before the 'end' are attached to the last declaration
      --  or the package (if no declarations).
      if Flag_Gather_Comments then
         Gather_Comments_End;
      end if;

      --  Skip 'end'
      Expect_Scan (Tok_End);

      if Current_Token = Tok_Package then
         if Flags.Vhdl_Std = Vhdl_87 then
            Error_Msg_Parse ("'package' keyword not allowed here by vhdl 87");
         end if;
         Set_End_Has_Reserved_Id (Res, True);

         --  Skip 'package'.
         Scan;
      end if;

      Check_End_Name (Res);
      Scan_Semi_Colon_Unit ("package declaration");

      if Flag_Elocations then
         Create_Elocations (Res);
         Set_End_Location (Res, End_Loc);
      end if;

      return Res;
   end Parse_Package_Declaration;

   --  precond : BODY
   --  postcond: next token.
   --
   --  [ LRM93 2.6, LRM08 4.8 ]
   --  package_body ::=
   --      PACKAGE BODY PACKAGE_simple_name IS
   --          package_body_declarative_part
   --      END [ PACKAGE BODY ] [ PACKAGE_simple_name ] ;
   function Parse_Package_Body (Parent : Iir) return Iir
   is
      Res : Iir;
      End_Loc : Location_Type;
   begin
      Res := Create_Iir (Iir_Kind_Package_Body);
      Set_Parent (Res, Parent);

      -- Get identifier.
      Scan_Identifier (Res);

      --  Skip 'is'.
      Expect_Scan (Tok_Is);

      Parse_Declarative_Part (Res, Get_Package_Parent (Res));

      End_Loc := Get_Token_Location;

      --  Skip 'end'
      Expect_Scan (Tok_End);

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
      Scan_Semi_Colon_Unit ("package body");

      if Flag_Elocations then
         Create_Elocations (Res);
         Set_End_Location (Res, End_Loc);
      end if;

      return Res;
   end Parse_Package_Body;

   --  precond : NEW
   --  postcond: ';'.
   --
   --  [ LRM08 4.9 ]
   --  package_instantiation_declaration ::=
   --      PACKAGE identifier IS NEW uninstantiated_package_name
   --         [ generic_map_aspect ] ;
   function Parse_Package_Instantiation_Declaration
     (Parent : Iir; Id : Name_Id; Loc : Location_Type) return Iir
   is
      Res: Iir;
   begin
      Res := Create_Iir (Iir_Kind_Package_Instantiation_Declaration);
      Set_Location (Res, Loc);
      Set_Identifier (Res, Id);
      Set_Parent (Res, Parent);

      --  Skip 'new'
      Scan;

      Set_Uninstantiated_Package_Name (Res, Parse_Name (False));

      if Current_Token = Tok_Generic then
         Set_Generic_Map_Aspect_Chain (Res, Parse_Generic_Map_Aspect);
      elsif Current_Token = Tok_Left_Paren then
         Error_Msg_Parse ("missing 'generic map'");
         Set_Generic_Map_Aspect_Chain
           (Res, Parse_Association_List_In_Parenthesis);
      end if;

      if Flag_Elocations then
         Create_Elocations (Res);
         Set_End_Location (Res, Get_Token_Location);
      end if;

      Scan_Semi_Colon_Unit ("package instantiation");

      return Res;
   end Parse_Package_Instantiation_Declaration;

   --  precond : PACKAGE
   --  postcond: next token.
   --
   --    package_declaration
   --  | package_body
   --  | package_instantiation_declaration
   function Parse_Package (Parent : Iir) return Iir
   is
      Loc : Location_Type;
      Id : Name_Id;
      Res : Iir;
      Start_Loc : Location_Type;
      Comments : Comments_Range;
   begin
      --  Skip 'package'
      Start_Loc := Get_Token_Location;
      Scan;

      if Current_Token = Tok_Body then
         --  Skip 'body'
         Scan;

         Res := Parse_Package_Body (Parent);
      else
         Loc := Get_Token_Location;
         if Current_Token = Tok_Identifier then
            Id := Current_Identifier;

            --  Skip identifier.
            Scan;
         else
            Id := Null_Identifier;
            Expect (Tok_Identifier);
         end if;

         if Flag_Gather_Comments then
            File_Comments.Save_Comments (Comments);
         end if;

         --  Skip 'is'.
         Expect_Scan (Tok_Is);

         if Current_Token = Tok_New then
            Res := Parse_Package_Instantiation_Declaration (Parent, Id, Loc);
            --  Note: there is no 'end' in instantiation.
         else
            Res := Parse_Package_Declaration (Parent, Id, Loc, Comments);
         end if;
      end if;

      if Flag_Elocations then
         Set_Start_Location (Res, Start_Loc);
      end if;

      return Res;
   end Parse_Package;

   --  1850-2005 7.2 Verification units
   --  inherit_spec ::=
   --    [ NONTRANSITIVE ] INHERIT vunit_Name { , vunit_name } ;
   function Parse_PSL_Inherit_Spec return Iir
   is
      N : Iir;
      First, Last : Iir;
      Name : Iir;
   begin
      First := Null_Iir;
      Last := Null_Iir;
      loop
         N := Create_Iir (Iir_Kind_PSL_Inherit_Spec);
         Set_Location (N);

         --  Append.
         if First = Null_Iir then
            First := N;
         else
            Set_Inherit_Spec_Chain (Last, N);
         end if;
         Last := N;

         -- Skip 'inherit' or ','.
         Scan;

         Name := Parse_Name;
         Set_Name (N, Name);

         exit when Current_Token /= Tok_Comma;
      end loop;

      Expect_Scan (Tok_Semi_Colon,
                   "';' expected at the end of an inherit spec");

      return First;
   end Parse_PSL_Inherit_Spec;

   --  1850-2005 7.2 Verification units
   --  verification_unit ::=
   --    vunit_type PSL_Identifier [ ( hierachical_hdl_name ) ] {
   --      { inherit_spec }
   --      { vunit_item }
   --    }
   procedure Parse_Verification_Unit (Unit : Iir_Design_Unit)
   is
      Kind            : constant Iir_Kind := Iir_Kind_Vunit_Declaration;
      Hier_Name       : Iir;
      Res             : Iir;
      Item, Last_Item : Iir;

      Label           : Name_Id;
      Loc             : Location_Type;
   begin
      Res := Create_Iir (Kind);
      Set_Parent (Res, Unit);

      --  Recognize PSL keywords.
      Vhdl.Scanner.Flag_Psl := True;

      --  Skip 'vunit'.
      Scan;

      --  Identifier.
      Scan_Identifier (Res);

      --  Hierarchical hdl name.
      if Current_Token = Tok_Left_Paren then
         --  Skip '('.
         Scan;

         Hier_Name := Create_Iir (Iir_Kind_Psl_Hierarchical_Name);
         Set_Location (Hier_Name);
         Set_Hierarchical_Name (Res, Hier_Name);

         Set_Entity_Name (Hier_Name, Parse_Simple_Name);

         if Current_Token = Tok_Left_Paren then
            --  Skip '('.
            Scan;

            Set_Architecture (Hier_Name, Parse_Simple_Name);

            --  Skip ')'.
            Expect_Scan (Tok_Right_Paren);
         end if;

         --  Skip ')'
         Expect_Scan (Tok_Right_Paren);
      end if;

      --  Skip '{'.
      Expect_Scan (Tok_Left_Curly);

      --  Vunit items.
      Last_Item := Null_Iir;
      loop
         --  Some parse subprograms clear the mode...
         Vhdl.Scanner.Flag_Psl := True;

         if Current_Token = Tok_Identifier then
            Label := Current_Identifier;
            Loc := Get_Token_Location;

            --  Skip label.
            Scan;

            if Current_Token = Tok_Colon then
               --  Skip ':'.
               Scan;
            else
               Item := Parse_Concurrent_Assignment_With_Name (Label, Loc);
               goto Has_Stmt;
            end if;
         else
            Label := Null_Identifier;
         end if;

         case Current_Token is
            when Tok_Type
               | Tok_Subtype
               | Tok_Signal
               | Tok_Constant
               | Tok_Variable
               | Tok_Shared
               | Tok_File
               | Tok_Function
               | Tok_Pure
               | Tok_Impure
               | Tok_Procedure
               | Tok_Alias
               | Tok_Attribute
               | Tok_Disconnect
               | Tok_Use
               | Tok_Group
               | Tok_Package
               | Tok_Default =>
               if Label /= Null_Identifier then
                  Error_Msg_Parse
                    (+Loc, "label not allowed before a declaration");
                  Label := Null_Identifier;
               end if;
               --  Do not recognize PSL keywords.  This is required for
               --  'boolean' which is a PSL keyword.
               Vhdl.Scanner.Flag_Psl := False;
               Item := Parse_Declaration (Res, Res);

            when Tok_For =>
               Vhdl.Scanner.Flag_Psl := False;
               if Label = Null_Identifier then
                  Item := Parse_Declaration (Res, Res);
               else
                  Item := Parse_Concurrent_Statement (Res, Label);
               end if;

            when Tok_End
               | Tok_Eof
               | Tok_Right_Curly =>
               exit;

            when Tok_Inherit =>
               if Label /= Null_Identifier then
                  Error_Msg_Parse
                    (+Loc, "label not allowed for inherit spec");
                  Label := Null_Identifier;
               end if;
               if Last_Item /= Null_Iir
                 and then Get_Kind (Last_Item) /= Iir_Kind_PSL_Inherit_Spec
               then
                  Error_Msg_Parse
                    ("inherit spec must be placed at the beginning");
               end if;
               Item := Parse_PSL_Inherit_Spec;

            when others =>
               --  Do not recognize PSL keywords.  This is required for
               --  'boolean' which is a PSL keyword.
               Vhdl.Scanner.Flag_Psl := False;
               Item := Parse_Concurrent_Statement (Res, Label);
               exit when Item = Null_Iir;
         end case;

         <<Has_Stmt>> null;

         while Item /= Null_Iir loop
            Set_Parent (Item, Res);
            if Last_Item = Null_Node then
               Set_Vunit_Item_Chain (Res, Item);
            else
               Set_Chain (Last_Item, Item);
            end if;
            Last_Item := Item;
            Item := Get_Chain (Item);
         end loop;
      end loop;

      --  Skip '}'.
      Expect_Scan (Tok_Right_Curly);

      --  Normal mode.
      Vhdl.Scanner.Flag_Psl := False;

      Set_Library_Unit (Unit, Res);
   end Parse_Verification_Unit;

   procedure Parse_Context_Declaration_Or_Reference
     (Unit : Iir_Design_Unit; Clause : out Iir);

   --  Precond:  next token
   --  Postcond: next token
   --
   --  [ LRM93 11.3, LRM08 13.4 Context clauses ]
   --  context_clause ::= { context_item }
   --
   --  context_item ::= library_clause | use_clause | context_reference
   procedure Parse_Context_Clause (Unit : Iir)
   is
      First, Last : Iir;
      Els : Iir;
   begin
      Chain_Init (First, Last);

      loop
         case Current_Token is
            when Tok_Library =>
               Els := Parse_Library_Clause;
            when Tok_Use =>
               Els := Parse_Use_Clause;
            when Tok_Context =>
               Parse_Context_Declaration_Or_Reference (Unit, Els);
               if Els = Null_Iir then
                  --  This was a context declaration.  No more clause.

                  --  LRM08 13.1 Design units
                  --  It is an error if the context clause preceding a library
                  --  unit that is a context declaration is not empty.
                  if Get_Context_Items (Unit) /= Null_Iir then
                     Error_Msg_Parse
                       (+Get_Context_Items (Unit),
                        "context declaration does not allow context "
                          & "clauses before it");
                  end if;

                  return;
               end if;
            when Tok_With =>
               --  Be Ada friendly.
               Error_Msg_Parse ("'with' not allowed in context clause "
                                  & "(try 'use' or 'library')");
               Els := Parse_Use_Clause;
            when others =>
               exit;
         end case;
         Chain_Append_Subchain (First, Last, Els);
      end loop;
      Set_Context_Items (Unit, First);
   end Parse_Context_Clause;

   --  Precond:  IS
   --
   --  [ LRM08 13.13 Context declarations ]
   --  context_declaration ::=
   --    CONTEXT identifier IS
   --       context_clause
   --    END [ CONTEXT ] [ /context/_simple_name ] ;
   procedure Parse_Context_Declaration (Unit : Iir; Decl : Iir)
   is
      End_Loc : Location_Type;
   begin
      if Get_Kind (Unit) = Iir_Kind_Context_Declaration then
         Error_Msg_Parse ("nested context declaration not allowed");
      else
         Set_Library_Unit (Unit, Decl);
      end if;

      --  Skip 'is'
      Scan;

      --  Comments after 'context' but before the first clause are attached
      --  to the context.
      if Flag_Gather_Comments then
         Gather_Comments_Block (Decl);
      end if;

      Parse_Context_Clause (Decl);

      Expect (Tok_End);
      End_Loc := Get_Token_Location;

      --  Skip 'end'
      Scan;

      if Current_Token = Tok_Context then
         Set_End_Has_Reserved_Id (Decl, True);

         --  Skip 'context'.
         Scan;
      end if;

      Check_End_Name (Decl);
      Scan_Semi_Colon_Unit ("context declaration");

      if Flag_Elocations then
         Create_Elocations (Decl);
         Set_End_Location (Decl, End_Loc);
      end if;
   end Parse_Context_Declaration;

   --  Precond:  next token after selected_name.
   --  Postcond: next token
   --
   --  [ LRM08 13.4 Context clauses ]
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

      while Current_Token = Tok_Comma loop
         --  Skip ','.
         Scan;

         Ref := Create_Iir (Iir_Kind_Context_Reference);
         Set_Location (Ref, Loc);
         Set_Selected_Name (Ref, Parse_Name);

         Set_Context_Reference_Chain (Last, Ref);
         Last := Ref;
      end loop;

      Scan_Semi_Colon_Unit ("context reference");

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
         if Get_Kind (Name) = Iir_Kind_Simple_Name then
            Location_Copy (Res, Name);
            Set_Identifier (Res, Get_Identifier (Name));
         else
            Set_Location (Res, Loc);
            Error_Msg_Parse (+Name, "identifier for context expected");
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
   --  [ LRM93 11.1 ]
   --  design_unit ::= context_clause library_unit
   function Parse_Design_Unit return Iir_Design_Unit
   is
      procedure Error_Empty is
      begin
         Error_Msg_Parse
           ("missing entity, architecture, package or configuration");
      end Error_Empty;

      Res: Iir_Design_Unit;
      Unit: Iir;
   begin
      pragma Assert (Parenthesis_Depth = 0);

      -- Create the design unit node.
      Res := Create_Iir (Iir_Kind_Design_Unit);
      Set_Location (Res);
      Set_Date_State (Res, Date_Extern);

      --  Attach comments to the design unit.
      if Flag_Gather_Comments then
         Gather_Comments_Block (Res);
      end if;

      Parse_Context_Clause (Res);

      if Get_Library_Unit (Res) = Null_Iir then
         --  Parse library unit.  Context declaration are already parsed.
         case Current_Token is
            when Tok_Entity =>
               Parse_Entity_Declaration (Res);
            when Tok_Architecture =>
               Parse_Architecture_Body (Res);
            when Tok_Package =>
               Set_Library_Unit (Res, Parse_Package (Res));
            when Tok_Configuration =>
               Parse_Configuration_Declaration (Res);
            when Tok_Vunit =>
               Parse_Verification_Unit (Res);
            when Tok_Identifier =>
               if Current_Identifier = Name_Context then
                  Check_Vhdl_At_Least_2008 ("context clause");
               else
                  Error_Empty;
               end if;
               Resync_To_Next_Unit;
               return Res;
            when others =>
               Error_Empty;
               Resync_To_Next_Unit;
               return Res;
         end case;
      end if;

      Unit := Get_Library_Unit (Res);
      Set_Design_Unit (Unit, Res);
      Set_Identifier (Res, Get_Identifier (Unit));
      Set_Date (Res, Date_Parsed);
      return Res;
   end Parse_Design_Unit;

   --  [ LRM93 11.1 ]
   --  design_file ::= design_unit { design_unit }
   function Parse_Design_File return Iir_Design_File
   is
      Res : Iir_Design_File;
      Design, Last_Design : Iir_Design_Unit;
   begin
      if Flag_Gather_Comments then
         File_Comments.Comment_Init_Scan (Get_Current_Source_File);
      end if;

      --  The first token.
      pragma Assert (Current_Token = Tok_Invalid);
      Scan;

      Res := Create_Iir (Iir_Kind_Design_File);
      Set_Location (Res);

      Last_Design := Null_Iir;
      while Current_Token /= Tok_Eof loop
         Design := Parse_Design_Unit;
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

      if Flag_Gather_Comments then
         File_Comments.Sort_Comments_By_Node;
         File_Comments.Comment_Close_Scan;
      end if;

      if Last_Design = Null_Iir then
         Error_Msg_Parse ("design file is empty (no design unit found)");
      end if;

      return Res;
   end Parse_Design_File;
end Vhdl.Parse;
