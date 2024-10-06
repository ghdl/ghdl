--  Verilog parser
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.

with Std_Names;
with Str_Table;
with Errorout; use Errorout;
with Verilog.Types; use Verilog.Types;
with Verilog.Tokens; use Verilog.Tokens;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Scans; use Verilog.Scans;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Flags; use Verilog.Flags;
with Verilog.Standard; use Verilog.Standard;
with Verilog.Bignums;
with Verilog.Sem_Utils;

package body Verilog.Parse is
      --  For a data type node: node + owner flag.
   type Type_Node is record
      Typ : Node;
      Own : Boolean;
   end record;

   --  Set Type and Type_Owner of N from DECL_TYPE; clear ownership of
   --  DECL_TYPE.
   procedure Set_Type_Node (N : Node; Decl_Type : in out Type_Node);

   --  For arrays.
   procedure Set_Element_Type_Node (N : Node; Decl_Type : in out Type_Node);

   --  For type parameters.
   procedure Set_Default_Type_Node (N : Node; Decl_Type : in out Type_Node);

   function Parse_Lvalue return Node;
   function Parse_Statement_Name (Name : Node) return Node;
   function Parse_Arguments (Allow_Blank : Boolean) return Node;
   function Parse_Module_Item (Parent : Node) return Node;
   procedure Parse_Module_Item (Constr : in out Items_Constr);
   function Parse_Module_Items_Rep (Parent : Node) return Node;
   function Parse_Class_Declaration return Node;
   function Parse_Data_Type_Or_Implicit return Type_Node;
   function Parse_Program_Declaration return Node;
   function Parse_Parameter_Value_Assignment return Node;
   procedure Parse_Block_Item_Declaration_Statement
     (Parent : Node; First_Decl : out Node; First_Stmt : out Node);
   function Parse_Bit_Select_Opt (Prefix : Node) return Node;
   function Parse_Constraint_Block return Node;
   function Parse_Constraint_Expression return Node;
   function Parse_Delay_Value return Node;

   type Type_Or_Expr_Choice is (Choice_Type, Choice_Expr);

   type Type_Or_Expr_Node (Choice : Type_Or_Expr_Choice := Choice_Expr)
      is record
      case Choice is
         when Choice_Type =>
            Typ : Node;
            Own : Boolean;
         when Choice_Expr =>
            Expr : Node;
      end case;
   end record;

   function Parse_Data_Type_Or_Expression return Type_Or_Expr_Node;

   --  Current timescale directive.
   Current_Timescale : Node := Null_Node;

   type Lifetime_State is record
      Life : Lifetime_Type;
      Has_Lifetime : Boolean;
   end record;

   procedure Set_Lifetime (N : Node; Lifetime : Lifetime_State) is
   begin
      if Lifetime.Has_Lifetime then
         Set_Has_Lifetime (N, True);
         Set_Lifetime (N, Lifetime.Life);
      end if;
   end Set_Lifetime;

   procedure Error_Msg_Parse (Msg : String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Start_Group;
      Report_Msg (Msgid_Error, Errorout.Parse, +Get_Token_Location,
                  Msg, Args);
      Scan_Report_Msg_Context;
      Report_End_Group;
   end Error_Msg_Parse;

   procedure Error_Msg_Parse
     (Loc : Location_Type; Msg : String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Start_Group;
      Report_Msg (Msgid_Error, Errorout.Parse, +Loc, Msg, Args);
      Scan_Report_Msg_Context;
      Report_End_Group;
   end Error_Msg_Parse;

   procedure Error_Msg_Parse (Msg : String; Arg : Earg_Type) is
   begin
      Error_Msg_Parse (Msg, Earg_Arr'(1 => Arg));
   end Error_Msg_Parse;

   procedure Warning_Msg_Parse (Msg : String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Start_Group;
      Report_Msg (Msgid_Warning, Errorout.Parse, +Get_Token_Location,
                  Msg, Args);
      Scan_Report_Msg_Context;
      Report_End_Group;
   end Warning_Msg_Parse;


   procedure Scan_Or_Error (Tok : Token_Type; Msg : String) is
   begin
      if Current_Token = Tok then
         Scan;
      else
         Error_Msg_Parse (Msg);
      end if;
   end Scan_Or_Error;

   procedure Scan_Statement_Semicolon is
   begin
      Scan_Or_Error (Tok_Semicolon, "';' expected at end of statement");
   end Scan_Statement_Semicolon;

   procedure Scan_Declaration_Semicolon is
   begin
      Scan_Or_Error (Tok_Semicolon, "';' expected at end of declaration");
   end Scan_Declaration_Semicolon;

   procedure Skip_Until_Semicolon is
   begin
      loop
         case Current_Token is
            when Tok_Semicolon =>
               Scan;
               exit;
            when Tok_Eof =>
               exit;
            when others =>
               null;
         end case;
         Scan;
      end loop;
   end Skip_Until_Semicolon;

   procedure Scan_Identifier (N : Node; Msg : String) is
   begin
      if Current_Token = Tok_Identifier then
         Set_Identifier (N, Current_Identifier);

         --  Skip identifier.
         Scan;
      else
         Error_Msg_Parse (Msg);
      end if;
   end Scan_Identifier;

   --  Make NEW_SCOPE the current scope and save the old one to SAVE.
   procedure Push_Scope (New_Scope : Node; Save : out Node) is
   begin
      Save := Current_Scope;
      Current_Scope := New_Scope;
   end Push_Scope;

   --  Restore current scope.
   procedure Pop_Scope (Old : Node) is
   begin
      Current_Scope := Old;
   end Pop_Scope;

   procedure Set_Token_Location (N : Node) is
   begin
      Set_Location (N, Get_Token_Location);
   end Set_Token_Location;

      --  Set Type and Type_Owner of N from DECL_TYPE; clear ownership of
   --  DECL_TYPE.
   procedure Set_Type_Node (N : Node; Decl_Type : in out Type_Node) is
   begin
      Set_Data_Type (N, Decl_Type.Typ);
      Set_Type_Owner (N, Decl_Type.Own);
      Decl_Type.Own := False;
   end Set_Type_Node;

   procedure Set_Element_Type_Node (N : Node; Decl_Type : in out Type_Node) is
   begin
      Set_Element_Data_Type (N, Decl_Type.Typ);
      Set_Type_Owner (N, Decl_Type.Own);
      Decl_Type.Own := False;
   end Set_Element_Type_Node;

   procedure Set_Default_Type_Node
     (N : Node; Decl_Type : in out Type_Node) is
   begin
      Set_Default_Type (N, Decl_Type.Typ);
      Set_Type_Owner (N, Decl_Type.Own);
      Decl_Type.Own := False;
   end Set_Default_Type_Node;

   --  Create a name from the current identifier, and skip the identifier.
   function Scan_Name return Node
   is
      Res : Node;
   begin
      pragma Assert (Current_Token = Tok_Identifier);

      Res := Create_Node (N_Name);
      Set_Token_Location (Res);
      Set_Identifier (Res, Current_Identifier);

      --  Skip identifier.
      Scan;

      return Res;
   end Scan_Name;

   function Scan_This return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_This);
      Set_Token_Location (Res);

      --  Skip 'this'.
      Scan;

      return Res;
   end Scan_This;

   --  1800-2017 11.3.2 Operator precedence
   type Prio_Type is (Prio_None,
                      Prio_Paren,
                      Prio_Cond,
                      Prio_Log_Or,
                      Prio_Log_And,
                      Prio_Bit_Or,
                      Prio_Bit_Xor,
                      Prio_Bit_And,
                      Prio_Equal,
                      Prio_Comp,
                      Prio_Shift,
                      Prio_Add,
                      Prio_Mul,
                      Prio_Exp,
                      Prio_Unary);

   --  Chain of implicit declarations.
   Implicit_First : Node := Null_Node;
   Implicit_Last : Node := Null_Node;
   pragma Unreferenced (Implicit_Last);

   --  Move implicits declaration to CONSTR.
   procedure Append_Implicits (Constr : in out Items_Constr);

   function Parse_Expression (Prio : Prio_Type := Prio_None) return Node;

   --  1800-2017 9.3.4 Block names
   --  Similarly, a matching block name may be specified after the following
   --  block end keywords, preceded by a colon:
   --    - endchecker
   --    - endclass
   --    - endclocking
   --    - endconfig
   --    - endfunction
   --    - endgroup
   --    - endinterface
   --    - endmodule
   --    - endpackage
   --    - endprimitive
   --    - endprogram
   --    - endproperty
   --    - endsequence
   --    - endtask
   procedure Parse_End_Name (N : Node)
   is
      Id : Name_Id;
   begin
      if Current_Token = Tok_Colon then
         --  Skip ':'
         Scan;

         if Current_Token = Tok_Identifier or else Current_Token = Tok_New
         then
            Set_Has_End_Name (N, True);

            Id := Get_Identifier (N);
            if Current_Identifier /= Id then
               Error_Msg_Parse ("name mistmatch, %i expected", +Id);
            end if;

            --  Skip identifier
            Scan;
         else
            Error_Msg_Parse ("matching name expected after ':'");
         end if;
      end if;
   end Parse_End_Name;

   --  1800-2017 5.12 Attributes
   --  attribute_instance ::= (* attr_spec { , attr_spec } *)
   --  attr_spec ::= attr_name [ = constant_expression ]
   --  attr_name ::= identifier
   --
   --  Parse { attribute_instance }.
   function Parse_Attribute_Instances_Rep return Node
   is
      Res : Node_Tuple;
      Attr : Node;
   begin
      Init_Chain (Res.First, Res.Last);

      while Current_Token = Tok_Paren_Star loop
         --  Skip '(*'.
         Scan;

         loop
            Attr := Create_Node (N_Attribute);
            Set_Token_Location (Attr);

            Scan_Identifier (Attr, "missing attribute name");

            if Current_Token = Tok_Equal then
               --  Skip '='.
               Scan;

               Set_Expression (Attr, Parse_Expression);
            end if;

            Append_Chain (Res.First, Res.Last, Attr);
            exit when Current_Token /= Tok_Comma;

            --  Skip ','.
            Scan;
         end loop;

         Scan_Or_Error (Tok_Star_Paren, "missing '*)' at end of attribute");
      end loop;
      return Res.First;
   end Parse_Attribute_Instances_Rep;

   --  1800-2017 11.4.14 Streaming operators (pack/unpack)
   --  streaming_concatenation ::=
   --    '{' stream_operator [ slice_size ] stream_concatenation '}'
   --
   --  stream_operator ::= >> | <<
   --
   --  slice_size ::= simple_type | constant_expression
   --
   --  stream_concatenation ::=
   --    '{' stream_expression { , stream_expression } '}'
   --
   --  stream_expression ::= expression [ with '[' array_range_expression ']' ]
   function Parse_Streaming_Concatenation
     (Type_Kind : Nkind; Expr_Kind : Nkind; Loc : Location_Type)
     return Node
   is
      Res : Node;
      TE : Type_Or_Expr_Node;
      First, Last : Node;
      El : Node;
   begin
      --  Skip '<<' or '>>'.
      Scan;

      --  [ slice_size ]
      if Current_Token = Tok_Left_Curly then
         Res := Create_Node (Expr_Kind);
      else
         TE := Parse_Data_Type_Or_Expression;

         case TE.Choice is
            when Choice_Type =>
               Res := Create_Node (Type_Kind);
               Set_Slice_Size_Type (Res, TE.Typ);
               Set_Type_Owner (Res, TE.Own);
            when Choice_Expr =>
               Res := Create_Node (Expr_Kind);
               Set_Expression (Res, TE.Expr);
         end case;
      end if;

      Set_Location (Res, Loc);

      --  Stream concatenation.
      Scan_Or_Error (Tok_Left_Curly, "'{' expected for stream concatenation");

      Init_Chain (First, Last);
      loop
         El := Create_Node (N_Stream_Expression);
         Set_Token_Location (El);

         Set_Expression (El, Parse_Expression);

         --  TODO: array_range_expression.

         Append_Chain (First, Last, El);

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
      end loop;

      Scan_Or_Error
        (Tok_Right_Curly, "'}' expected at end of stream expressions");

      Scan_Or_Error
        (Tok_Right_Curly, "'}' expected at end of stream concatenation");

      Set_Expressions (Res, First);

      return Res;
   end Parse_Streaming_Concatenation;

   --  precond:  '{'
   --  postcond: next token.
   function Parse_Concatenation return Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Res : Node;
      El : Node;
      Last_El : Node;
      Expr : Node;
   begin
      --  Skip '{'.
      Scan;

      case Current_Token is
         when Tok_Left_Lshift =>
            return Parse_Streaming_Concatenation
              (N_Left_Streaming_Type, N_Left_Streaming_Expr, Loc);
         when Tok_Right_Lshift =>
            return Parse_Streaming_Concatenation
              (N_Right_Streaming_Type, N_Right_Streaming_Expr, Loc);
         when Tok_Right_Curly =>
            --  Empty unpacked array concatenation.
            Res := Create_Node (N_Concatenation);
            Set_Location (Res, Loc);

            --  Skip '}'.
            Scan;

            return Res;
         when others =>
            null;
      end case;

      Res := Create_Node (N_Concatenation);
      Set_Location (Res, Loc);

      El := Create_Node (N_Element);
      Set_Token_Location (El);
      Set_Expressions (Res, El);

      Expr := Parse_Expression (Prio_None);
      case Current_Token is
         when Tok_Right_Curly =>
            Set_Expression (El, Expr);

            --  Skip '}'.
            Scan;

            return Res;
         when Tok_Left_Curly =>
            --  IEEE 4.1.14  Concatenations
            --  The repetition multiplier must be a constant.
            --  FIXME: check this.
            Set_Replication (Res, Expr);

            --  Skip '{'.
            Scan;

            Expr := Parse_Expression;
            Set_Expression (El, Expr);
         when Tok_Comma =>
            Set_Expression (El, Expr);
         when others =>
            Error_Msg_Parse ("'{' or ',' expected");
      end case;

      loop
         exit when Current_Token = Tok_Right_Curly;
         if Current_Token /= Tok_Comma then
            Error_Msg_Parse ("'}' or ',' expected");
         end if;

         --  Skip ','.
         Scan;

         Last_El := El;
         El := Create_Node (N_Element);
         Set_Token_Location (El);
         Set_Expression (El, Parse_Expression);
         Set_Chain (Last_El, El);
      end loop;

      if Get_Replication (Res) /= Null_Node then
         --  A second closing '}' is expected.

         --  Skip the first '}'.
         Scan;

         if Current_Token /= Tok_Right_Curly then
            Error_Msg_Parse ("missing '}' of multiple concatenation");
         end if;
      end if;

      --  Skip '}'.
      Scan;

      return Res;
   end Parse_Concatenation;

   --  precond:  unary operator
   --  postcond: next token
   function Parse_Unary_Expression (Op : Unary_Ops) return Node
   is
      Res : Node;
      Attrs : Node;
   begin
      Res := Create_Node (N_Unary_Op);
      Set_Unary_Op (Res, Op);
      Set_Token_Location (Res);

      --  Skip operator.
      Scan;

      Attrs := Parse_Attribute_Instances_Rep;
      Set_Op_Attributes (Res, Attrs);

      Set_Expression (Res, Parse_Expression (Prio_Unary));
      return Res;
   end Parse_Unary_Expression;

   --  1800-2017 5.7.1 Integer literal constants
   --  If the size of the unsigned number is smaller than the size specified
   --  for the literal constant, the unsigned number shall be padded left with
   --  zeros.  If the leftmost bit in the unsigned number is an x or a z, then
   --  an x or a z shall be used to pad to the left, respectively.  If the size
   --  of the unsigned number is larger than the size specified for the literal
   --  constant, the unsigned number shall be truncated from the left.
   procedure Reformat_Based_Number (Sz : Natural)
   is
      procedure Warning_Extra_Digits is
      begin
         Warning_Msg_Parse ("extra digits ignored");
      end Warning_Extra_Digits;

      Mask : Uns32;
      V, X : Uns32;
   begin
      if Current_Number_Len > Sz then
         --  More digits than the size, need to truncate.
         if Current_Number_Len <= 32 then
            if Shift_Right (Current_Number_Lo.Val, Sz) /= 0
              or else Shift_Right (Current_Number_Lo.Zx, Sz) /= 0
            then
               Warning_Extra_Digits;
            end if;
         elsif Current_Number_Len <= 64 then
            if Sz >= 32 then
               if Shift_Right (Current_Number_Hi.Val, Sz - 32) /= 0
                 or else Shift_Right (Current_Number_Hi.Zx, Sz - 32) /= 0
               then
                  Warning_Extra_Digits;
               end if;
            else
               if Shift_Right (Current_Number_Lo.Val, Sz) /= 0
                 or else Shift_Right (Current_Number_Lo.Zx, Sz) /= 0
                 or else Current_Number_Hi.Val /= 0
                 or else Current_Number_Hi.Zx /= 0
               then
                  Warning_Extra_Digits;
               end if;
            end if;
         else
            --  TODO.
            raise Program_Error;
         end if;
      end if;

      --  First, extend.
      if Current_Number_Len < Sz then
         --  Extend.
         if Current_Number_Len <= 32 then
            X := Shift_Left (Current_Number_Lo.Zx, 32 - Current_Number_Len);
            if (X and 16#8000_0000#) /= 0 then
               Current_Number_Lo.Zx :=
                 Shift_Right_Arithmetic (X, 32 - Current_Number_Len);
               Current_Number_Hi.Zx := Shift_Right_Arithmetic (X, 31);

               V := Shift_Left (Current_Number_Lo.Val,
                                32 - Current_Number_Len);
               Current_Number_Lo.Val :=
                 Shift_Right_Arithmetic (V, 32 - Current_Number_Len);
               Current_Number_Hi.Val := Shift_Right_Arithmetic (V, 31);
            end if;
         else
            X := Shift_Left (Current_Number_Hi.Zx, 64 - Current_Number_Len);
            if (X and 16#8000_0000#) /= 0 then
               Current_Number_Hi.Zx :=
                 Shift_Right_Arithmetic (X, 64 - Current_Number_Len);

               V := Shift_Left (Current_Number_Hi.Val,
                                64 - Current_Number_Len);
               Current_Number_Hi.Val :=
                 Shift_Right_Arithmetic (V and X, 64 - Current_Number_Len);
            end if;
         end if;
      end if;

      --  Then truncate.
      if Sz >= 64 then
         --  Will be truncated at run-time.
         null;
      elsif Sz >= 32 then
         Mask := not Shift_Left (16#ffff_ffff#, Sz - 32);
         Current_Number_Hi.Val := Current_Number_Hi.Val and Mask;
         Current_Number_Hi.Zx := Current_Number_Hi.Zx and Mask;
      else
         Mask := not Shift_Left (16#ffff_ffff#, Sz);
         Current_Number_Lo.Val := Current_Number_Lo.Val and Mask;
         Current_Number_Lo.Zx := Current_Number_Lo.Zx and Mask;
         Current_Number_Hi.Val := 0;
         Current_Number_Hi.Zx := 0;
      end if;
   end Reformat_Based_Number;

   --  Parse a based number; do not set location.
   function Parse_Based_Number (Width : Width_Type) return Node
   is
      Res : Node;
      Base : Base_Type;
      Signed : Boolean;
   begin
      case Toks_Base (Current_Token) is
         when Tok_Base_Hex
           | Tok_Base_Signed_Hex =>
            Base := Base_Hexa;
         when Tok_Base_Bin
           | Tok_Base_Signed_Bin =>
            Base := Base_Binary;
         when Tok_Base_Oct
           | Tok_Base_Signed_Oct =>
            Base := Base_Octal;
         when Tok_Base_Dec
           | Tok_Base_Signed_Dec =>
            Base := Base_Decimal;
      end case;

      case Toks_Base (Current_Token) is
         when Tok_Base_Hex
           | Tok_Base_Bin
           | Tok_Base_Oct
           | Tok_Base_Dec =>
            Signed := False;
         when Tok_Base_Signed_Hex
           | Tok_Base_Signed_Bin
           | Tok_Base_Signed_Oct
           | Tok_Base_Signed_Dec =>
            Signed := True;
      end case;

      --  Skip base.
      Scan;

      case Current_Token is
         when Tok_Number_32 =>
            Res := Create_Node (N_Number);
            Reformat_Based_Number (Natural (Width));
            Set_Number_Lo_Val (Res, Current_Number_Lo.Val);
            Set_Number_Lo_Zx (Res, Current_Number_Lo.Zx);
         when Tok_Number_64 =>
            Res := Create_Node (N_Number);
            Reformat_Based_Number (Natural (Width));
            Set_Number_Lo_Val (Res, Current_Number_Lo.Val);
            Set_Number_Lo_Zx (Res, Current_Number_Lo.Zx);
            Set_Number_Hi_Val (Res, Current_Number_Hi.Val);
            Set_Number_Hi_Zx (Res, Current_Number_Hi.Zx);
         when Tok_Dec_Number =>
            pragma Assert (Base = Base_Decimal);
            Res := Create_Node (N_Number);
            Set_Number_Lo_Val (Res, Current_Number_Lo.Val);
            Set_Number_Lo_Zx (Res, 0);
            Set_Number_Hi_Val (Res, Current_Number_Hi.Val);
            Set_Number_Hi_Zx (Res, 0);
         when Tok_Bignum
           | Tok_Dec_Bignum =>
            Res := Create_Node (N_Bignum);
            Set_Bignum_Index (Res, Current_Bignum);
            Set_Bignum_Len (Res, Uns32 (Current_Number_Len));
         when others =>
            Error_Msg_Parse ("missing number value after base");
            Res := Create_Node (N_Number);
            Set_Number_Lo_Val (Res, 0);
            Set_Number_Lo_Zx (Res, 0);

            Set_Number_Base (Res, Base);
            Set_Signed_Flag (Res, Signed);
            Set_Number_Size (Res, Width);

            --  Do not eat token.
            return Res;
      end case;

      Set_Number_Base (Res, Base);
      Set_Signed_Flag (Res, Signed);
      Set_Number_Size (Res, Width);

      --  Skip number.
      Scan;

      return Res;
   end Parse_Based_Number;

   --  precond:  tok_dec_number
   --  postcond: next token.
   function Parse_Unsigned_Number return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Number);
      Set_Token_Location (Res);
      Set_Number_Base (Res, Base_Decimal);
      Set_Number_Size (Res, 0);
      Set_Number_Lo_Val (Res, Current_Number_Lo.Val);
      Set_Number_Hi_Val (Res, Current_Number_Hi.Val);
      Set_Number_Lo_Zx (Res, 0);
      Set_Number_Hi_Zx (Res, 0);
      Scan;
      return Res;
   end Parse_Unsigned_Number;

   --  precond: '('
   --
   --  ( expression )
   function Parse_Parenthesis_Expression return Node
   is
      Res : Node;
   begin
      Scan_Or_Error (Tok_Left_Paren, "'(' expected before expression");
      Res := Parse_Expression;
      Scan_Or_Error (Tok_Right_Paren, "')' expected after expression");
      return Res;
   end Parse_Parenthesis_Expression;

   function Convert_Arguments_To_Iterator (Args : Node) return Node
   is
      Port : Node;
      Expr : Node;
      Res : Node;
   begin
      --  Argument is optionnal.
      if Args = Null_Node then
         return Null_Node;
      end if;

      if Get_Chain (Args) /= Null_Node then
         Error_Msg_Sem
           (+Args, "only one argument allowed for array method call");
         --  TODO: free chain.
      end if;

      Port := Get_Port (Args);
      if Port /= Null_Node then
         Error_Msg_Sem
           (+Port, "argument identifier not allowed for array method call");
         Free_Node (Port);
      end if;

      Expr := Get_Expression (Args);
      if Expr = Null_Node then
         if Port = Null_Node then
            Error_Msg_Sem
              (+Args, "iterator argument expected for array method call");
         end if;
         return Null_Node;
      end if;

      if Get_Kind (Expr) /= N_Name then
         Error_Msg_Sem
           (+Expr,
            "iterator argument must be an identifier in array method call");
         return Null_Node;
      end if;

      Res := Create_Node (N_Iterator_Argument);
      Set_Identifier (Res, Get_Identifier (Expr));
      Set_Location (Res, Get_Location (Expr));
      Free_Node (Expr);

      return Res;
   end Convert_Arguments_To_Iterator;

   --  tf_call ::=
   --    ps_or_hierarchical_tf_identifier { attribute_instance }
   --      [ ( list_of_arguments ) ]
   --
   --  1800-2017 7.12 Array manipulation methods
   --  array_method_call ::=
   --    expression . array_method_name { attribute_instance }
   --      [ ( iterator_argument ) ] [ WITH ( expression ) ]
   --
   --  1800-2017 18.7 In-line constraints -- randomize() with
   --  randomize_call ::=
   --    randomize { attribute_instance }
   --      [ ( [ variable_identifier_list | NULL ] ) ]
   --      [ WITH [ ( identifier_list ] ) ] constraint_block ]
   function Parse_Tf_Call (Name : Node) return Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Res : Node;
      Args : Node;
      Wexpr : Node;
      Has_Args : Boolean;
   begin
      if Current_Token = Tok_Left_Paren then
         Has_Args := True;
         Args := Parse_Arguments (False);
      else
         Has_Args := False;
         Args := Null_Node;
      end if;

      if Current_Token = Tok_With then
         --  Skip 'with'.
         Scan;

         if Current_Token = Tok_Left_Paren then
            Wexpr := Parse_Parenthesis_Expression;
         else
            Wexpr := Null_Node;
            if Current_Token /= Tok_Left_Curly then
               Error_Msg_Parse ("expression within parentheses or "
                                  & "contraint block expected after 'with'");
            end if;
         end if;
         if Current_Token = Tok_Left_Curly then
            Res := Create_Node (N_Randomize_Call);
            Set_Arguments (Res, Args);
            Set_Constraint_Block_Chain (Res, Parse_Constraint_Block);
         else
            Res := Create_Node (N_Array_Method_Call);

            --  Convert argument to iterator_argument
            if Args /= Null_Node then
               Args := Convert_Arguments_To_Iterator (Args);
            else
               --  1800-2017 7.12 Array manipulation methods
               --  If it is not specified, the name ITEM is used by default.
               Args := Create_Node (N_Iterator_Argument);
               Set_Identifier (Args, Std_Names.Name_Item);
               Set_Location (Args, Loc);
            end if;
            Set_Iterator_Argument (Res, Args);
         end if;
         Set_With_Expression (Res, Wexpr);
      else
         Res := Create_Node (N_Call);
         Set_Arguments (Res, Args);
      end if;

      Set_Location (Res, Loc);
      Set_Subroutine (Res, Name);
      Set_Has_Parenthesis (Res, Has_Args);

      return Res;
   end Parse_Tf_Call;

   --  Convert time unit name to the power of 10 of second.
   function Parse_Time_Unit return Int32
   is
      use Std_Names;
   begin
      case Current_Identifier is
         when Name_S =>
            return 0;
         when Name_Ms =>
            return -3;
         when Name_Us =>
            return -6;
         when Name_Ns =>
            return -9;
         when Name_Ps =>
            return -12;
         when Name_Fs =>
            return -15;
         when others =>
            Error_Msg_Parse ("invalid time unit");
            return 0;
      end case;
   end Parse_Time_Unit;

   --  1800-2017 8.8 Typed constructor calls
   --  class_new ::=
   --      [ class_scope ] NEW [ ( list_of_arguments ) ]
   --    | NEW expression
   function Parse_Class_New return Node
   is
      Res : Node;
      Loc : Location_Type;
   begin
      Loc := Get_Token_Location;

      --  Skip 'new'.
      Scan;

      case Current_Token is
         when Tok_Left_Paren =>
            Res := Create_Node (N_New_Call);
            Set_Has_Parenthesis (Res, True);
            Set_Arguments (Res, Parse_Arguments (False));
         when Tok_Semicolon
           | Tok_Comma =>
            Res := Create_Node (N_New_Call);
         when Tok_Left_Brack =>
            Res := Create_Node (N_Dynamic_Array_New);

            --  Skip '['.
            Scan;

            Set_Size_Expression (Res, Parse_Expression);

            --  Skip ']'.
            Scan_Or_Error (Tok_Right_Brack, "']' expected after expression");

            if Current_Token = Tok_Left_Paren then
               Set_Init_Expression (Res, Parse_Parenthesis_Expression);
            end if;
         when others =>
            Res := Create_Node (N_New_Expression);
            Set_Expression (Res, Parse_Expression);
      end case;
      Set_Location (Res, Loc);

      return Res;
   end Parse_Class_New;

   function Parse_Unbased_Number (Val : Uns32; Zx : Uns32) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Unbased_Literal);
      Set_Token_Location (Res);
      Set_Number_Lo_Val (Res, Val);
      Set_Number_Lo_Zx (Res, Zx);

      --  Skip unbased literal.
      Scan;

      return Res;
   end Parse_Unbased_Number;

   function Parse_Default_Case return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Default);
      Set_Token_Location (Res);

      --  Skip 'default'.
      Scan;

      return Res;
   end Parse_Default_Case;

   --  1800-2017 10.9 Assignment patterns
   --  assignment_pattern ::=
   --      '{ expression { , expression } }
   --    | '{ structure_pattern_key : expression
   --         { , structure_pattern_key : expression } }
   --    | '{ array_pattern_key : expression
   --         { , array_pattern_key : expression } }
   --    | '{ constant_expression '{' expression { , expression } '}' }
   function Parse_Aggregate_Literal return Node
   is
      First, Last : Node;
      Res : Node;
      Expr : Node;
      El : Node;
   begin
      Res := Create_Node (N_Aggregate_Literal);
      Set_Token_Location (Res);

      --  Skip ''{'.
      Scan;

      if Current_Token = Tok_Right_Curly then
         --  Unpacked array concatenation can be empty.
         Error_Msg_Parse ("empty aggregate is not allowed, use {} instead");

         --  Skip '}'.
         Scan;

         return Res;
      end if;

      if Current_Token = Tok_Default then
         Expr := Parse_Default_Case;
      else
         Expr := Parse_Expression;

         if Current_Token = Tok_Left_Curly then
            Set_Replication (Res, Expr);

            --  Skip '{'
            Scan;

            Expr := Parse_Expression;
         end if;
      end if;

      Init_Chain (First, Last);
      loop
         El := Create_Node (N_Aggregate_Element);
         Set_Token_Location (El);

         if Current_Token = Tok_Colon then
            Set_Pattern_Key (El, Expr);

            --  Skip ':'.
            Scan;

            Set_Expression (El, Parse_Expression);
         else
            Set_Expression (El, Expr);
         end if;

         Append_Chain (First, Last, El);

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;

         if Current_Token = Tok_Default then
            Expr := Parse_Default_Case;
         else
            Expr := Parse_Expression;
         end if;
      end loop;

      Scan_Or_Error
        (Tok_Right_Curly, "'}' required at end of pattern");

      if Get_Replication (Res) /= Null_Node then
         Scan_Or_Error
           (Tok_Right_Curly, "'}' required at end of pattern replication");
      end if;

      Set_Elements (Res, First);

      return Res;
   end Parse_Aggregate_Literal;

   --  1800-2017 5.7 Numbers
   --  time_literal ::=
   --      unsigned_number time_unit
   --    | fixed_point_number time_unit
   function Parse_Time_Literal return Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Val : constant Fp64 := Current_Real;
      Res : Node;

   begin
      --  Skip number.
      Scan;

      if Current_Token = Tok_Identifier
        and then Current_Identifier = Std_Names.Name_Step
      then
         --  FIXME: catch 1.0step or 01step.
         if Val /= 1.0 then
            Error_Msg_Parse ("'step' delay can only be '1step'");
         end if;
         Res := Create_Node (N_1step_Literal);
         Set_Location (Res, Loc);

         --  Skip identifier.
         Scan;

         return Res;
      end if;

      Res := Create_Node (N_Time_Literal);
      Set_Location (Res, Loc);
      if Current_Timescale /= Null_Node then
         Set_Timescale (Res, Current_Timescale);
      else
         Set_Timescale (Res, Default_Timescale);
      end if;
      Set_Real_Number (Res, Val);

      Set_Time_Unit (Res, Parse_Time_Unit);

      --  Skip identifier.
      --  FIXME: even in case of error ?
      Scan;

      return Res;
   end Parse_Time_Literal;

   function Parse_Pre_Inc_Or_Dec (Kind : Nkind) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (Kind);
      Set_Token_Location (Res);

      --  Skip '++' or '--'.
      Scan;

      Set_Lvalue (Res, Parse_Lvalue);

      return Res;
   end Parse_Pre_Inc_Or_Dec;

   function Parse_Post_Inc_Or_Dec (Kind : Nkind; Lvalue : Node) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (Kind);
      Set_Token_Location (Res);
      Set_Lvalue (Res, Lvalue);

      --  Skip '++' or '--'.
      Scan;

      return Res;
   end Parse_Post_Inc_Or_Dec;

   procedure Parse_Cast_Expr (N : Node) is
   begin
      pragma Assert (Current_Token = Tok_Tick);

      --  Skip '''.
      Scan;

      --  Skip '('.
      Set_Expression (N, Parse_Parenthesis_Expression);
   end Parse_Cast_Expr;

   function Parse_Type_Cast (Typ : Type_Node) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Type_Cast);
      Set_Token_Location (Res);

      Set_Cast_Data_Type (Res, Typ.Typ);
      Set_Type_Owner (Res, Typ.Own);

      Parse_Cast_Expr (Res);

      return Res;
   end Parse_Type_Cast;

   function Parse_Size_Cast (Sz : Node) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Size_Cast);
      Set_Token_Location (Res);
      Set_Size_Expression (Res, Sz);

      Parse_Cast_Expr (Res);

      return Res;
   end Parse_Size_Cast;

   --  1800-2017 20.6.2 Expression size system function
   --  size_function ::=
   --      $bits ( expression )
   --    | $bits ( data_type )
   --
   --  FIXME: generalize, add arg_type (+ owner)
   function Parse_System_Call_Type_Expr return Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      TE : Type_Or_Expr_Node;
      Res : Node;
   begin
      --  Skip system function identifier.
      Scan;

      if Current_Token = Tok_Left_Paren then
         --  Skip '('
         Scan;

         TE := Parse_Data_Type_Or_Expression;

         case TE.Choice is
            when Choice_Type =>
               Res := Create_Node (N_Bits_Type);
               Set_Type_Argument (Res, TE.Typ);
               Set_Type_Owner (Res, TE.Own);
            when Choice_Expr =>
               Res := Create_Node (N_Bits_Expr);
               Set_Expression (Res, TE.Expr);
         end case;

         Scan_Or_Error (Tok_Right_Paren,
                        "')' expected after argument");
      else
         Error_Msg_Parse ("data_type or expression argument expected");
         Res := Create_Node (N_Bits_Expr);
      end if;

      Set_Location (Res, Loc);

      return Res;
   end Parse_System_Call_Type_Expr;

   --  1800-2017 11.4.13 Set membership operator
   --
   --  Parse: { open_range_list }
   --
   --  open_range_list ::= open_value_range { , open_value_range }
   --
   --  open_value_range ::= value_range
   --
   --  value_range ::=
   --      expression
   --    | [ expression : expression ]
   function Parse_Bracketed_Range_List return Node
   is
      First, Last : Node;
      Rng : Node;
      El : Node;
   begin
      Init_Chain (First, Last);

      Scan_Or_Error (Tok_Left_Curly, "'{' expected before range_list");
      loop
         El := Create_Node (N_Element);
         Set_Token_Location (El);

         if Current_Token = Tok_Left_Brack then
            Rng := Create_Node (N_Value_Range);
            Set_Token_Location (Rng);

            --  Skip '['.
            Scan;

            Set_Msb (Rng, Parse_Expression);

            --  Skip ':'.
            Scan_Or_Error (Tok_Colon, "':' expected in value range");

            Set_Lsb (Rng, Parse_Expression);

            --  Skip ']'.
            Scan_Or_Error (Tok_Right_Brack, "']' expected");
         else
            Rng := Parse_Expression;
         end if;

         Set_Expression (El, Rng);
         Append_Chain (First, Last, El);

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
      end loop;
      Scan_Or_Error (Tok_Right_Curly, "'}' expected after range_list");

      return First;
   end Parse_Bracketed_Range_List;

   function Parse_Infinity return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Infinity);
      Set_Token_Location (Res);

      --  Skip '$'.
      Scan;

      return Res;
   end Parse_Infinity;

   function Parse_Primary_Expression return Node
   is
      use Std_Names;
      Res : Node;
      Width : Width_Type;
      Loc : Location_Type;
   begin
      case Current_Token is
         when Tok_Dec_Number =>
            Res := Parse_Unsigned_Number;
            if Current_Token in Toks_Base then
               --  1800-2017 5.7.1 Integer literal constants
               --  The first token, a size constant, shall specify the size of
               --  the integer literal constant in terms of its exact number
               --  of bits.
               Width := Width_Type (Get_Number_Lo_Val (Res));
               if Width = 0 then
                  --  1800-2017 5.7.1 Integer literal constants
                  --  It shall be specified as a nonzero unsigned decimal
                  --  number.
                  Error_Msg_Parse ("size of a number cannot be 0");
                  Width := 1;
               end if;
               Loc := Get_Location (Res);
               Free_Node (Res);
               Res := Parse_Based_Number (Width);
               Set_Location (Res, Loc);
            else
               Set_Number_Base (Res, Base_Decimal);
               --  1800-2017 5.7.1 Integer literal constants
               --  Simple decimal numbers without the size and the base format
               --  shall be treated a signed integers, [...]
               Set_Signed_Flag (Res, True);
               if False and then Current_Token = Tok_Tick then
                  Error_Msg_Parse ("missing base specifier ('h, 'b or 'o)");
               end if;
            end if;
            if Current_Token = Tok_Tick then
               Res := Parse_Size_Cast (Res);
            end if;
         when Toks_Base =>
            Loc := Get_Token_Location;
            Res := Parse_Based_Number (32);
            Set_Location (Res, Loc);
            --  Not sized.
            Set_Number_Size (Res, 0);

         when Tok_Time_Literal =>
            Res := Parse_Time_Literal;

         when Tok_Real_Number =>
            Res := Create_Node (N_Real_Number);
            Set_Token_Location (Res);
            Set_Real_Number (Res, Current_Real);

            --  Skip real number.
            Scan;

         when Tok_Scale_Number =>
            Res := Create_Node (N_Scale_Number);
            Set_Token_Location (Res);
            Set_Real_Number (Res, Current_Real);
            Set_Scale_Factor (Res, Bignums.To_Int32 (Current_Number_Lo.Val));

            --  Skip scale number.
            Scan;

         when Tok_Dollar =>
            Res := Parse_Infinity;

         when Tok_Unbased_1 =>
            Res := Parse_Unbased_Number (Val => not 0, Zx => 0);
         when Tok_Unbased_0 =>
            Res := Parse_Unbased_Number (Val => 0, Zx => 0);
         when Tok_Unbased_X =>
            Res := Parse_Unbased_Number (Val => not 0, Zx => not 0);
         when Tok_Unbased_Z =>
            Res := Parse_Unbased_Number (Val => 0, Zx => not 0);

         when Tok_Left_Paren =>
            declare
               Expr : Node;
            begin
               if Flag_Keep_Parentheses then
                  Res := Create_Node (N_Parenthesis_Expr);
                  Set_Token_Location (Res);
               end if;

               --  Skip '('.
               Scan;

               Expr := Parse_Expression (Prio_Paren);

               --  Skip ')'.
               Scan_Or_Error (Tok_Right_Paren, "missing ')'");

               if Flag_Keep_Parentheses then
                  Set_Expression (Res, Expr);
               else
                  Res := Expr;
               end if;

               if Current_Token = Tok_Tick then
                  Res := Parse_Size_Cast (Res);
               end if;
            end;

         when Tok_Identifier | Tok_This | Tok_Super =>
            Res := Parse_Lvalue;
            case Current_Token is
               when Tok_Tick =>
                  Res := Parse_Type_Cast ((Res, True));
               when Tok_Plus_Plus =>
                  Res := Parse_Post_Inc_Or_Dec (N_Post_Increment, Res);
               when Tok_Minus_Minus =>
                  Res := Parse_Post_Inc_Or_Dec (N_Post_Decrement, Res);
               when others =>
                  null;
            end case;

         when Tok_String | Tok_Real | Tok_Shortreal | Tok_Int =>
            declare
               Ctype : Type_Node;
            begin
               Ctype := Parse_Data_Type_Or_Implicit;
               if Current_Token = Tok_Tick then
                  Res := Parse_Type_Cast (Ctype);
               else
                  Error_Msg_Parse ("''' expected after data type");
                  Res := Null_Node;
               end if;
            end;
         when Tok_System =>
            case Current_Identifier is
               when Name_Bits =>
                  Res := Parse_System_Call_Type_Expr;
               when others =>
                  Res := Create_Node (N_System_Call);
                  Set_Token_Location (Res);
                  Set_Identifier (Res, Current_Identifier);
                  Set_Call_Scope (Res, Current_Scope);

                  --  Skip system function identifier.
                  Scan;

                  if Current_Token = Tok_Left_Paren then
                     Set_Has_Parenthesis (Res, True);
                     Set_Arguments (Res, Parse_Arguments (True));
                  end if;
            end case;
         when Tok_Minus =>
            Res := Parse_Unary_Expression (Unop_Minus);
         when Tok_Plus =>
            Res := Parse_Unary_Expression (Unop_Plus);
         when Tok_Bit_Neg =>
            Res := Parse_Unary_Expression (Unop_Bit_Neg);
         when Tok_Logic_Neg =>
            Res := Parse_Unary_Expression (Unop_Logic_Neg);
         when Tok_Bit_Or =>
            Res := Parse_Unary_Expression (Unop_Red_Or);
         when Tok_Red_Nor =>
            Res := Parse_Unary_Expression (Unop_Red_Nor);
         when Tok_Bit_And =>
            Res := Parse_Unary_Expression (Unop_Red_And);
         when Tok_Red_Nand =>
            Res := Parse_Unary_Expression (Unop_Red_Nand);
         when Tok_Bit_Xor =>
            Res := Parse_Unary_Expression (Unop_Red_Xor);
         when Tok_Bit_Xnor =>
            Res := Parse_Unary_Expression (Unop_Red_Xnor);
         when Tok_Bit_Nxor =>
            Res := Parse_Unary_Expression (Unop_Red_Nxor);
         when Tok_Left_Curly =>
            Res := Parse_Concatenation;
         when Tok_Tick_Curly =>
            Res := Parse_Aggregate_Literal;
         when Tok_String_Literal =>
            Res := Create_Node (N_String_Literal);
            Set_Token_Location (Res);
            Set_String_Id (Res, Current_String);
            Set_String_Size (Res, Uns32 (Current_String_Len));

            --  Skip string
            Scan;

            return Res;
         when Tok_New =>
            Res := Parse_Class_New;
         when Tok_Null =>
            Res := Create_Node (N_Null);
            Set_Token_Location (Res);

            --  Skip 'null'.
            Scan;
         when Tok_Minus_Minus =>
            Res := Parse_Pre_Inc_Or_Dec (N_Pre_Decrement);
         when Tok_Plus_Plus =>
            Res := Parse_Pre_Inc_Or_Dec (N_Pre_Increment);

         when others =>
            Error_Msg_Parse ("expression expected instead of token %t",
                             +Current_Token);
            return Null_Node;
      end case;

      return Res;
   end Parse_Primary_Expression;

   --  precond : next token
   --  postcond: next token
   function Parse_Expression_With_Primary
     (Prim : Node; Prio : Prio_Type := Prio_None) return Node
   is
      Res : Node;
      Left : Node;
      N_Prio : Prio_Type;
      Op : Binary_Ops;
      Attrs : Node;
   begin
      Res := Prim;

      loop
         Left := Res;

         case Current_Token is
            when Tok_Less =>
               Op := Binop_Ult;
               N_Prio := Prio_Comp;
            when Tok_Less_Equal =>
               Op := Binop_Ule;
               N_Prio := Prio_Comp;
            when Tok_Greater_Equal =>
               Op := Binop_Uge;
               N_Prio := Prio_Comp;
            when Tok_Greater =>
               Op := Binop_Ugt;
               N_Prio := Prio_Comp;

            when Tok_Logic_Eq =>
               Op := Binop_Log_Eq;
               N_Prio := Prio_Equal;
            when Tok_Logic_Ne =>
               Op := Binop_Log_Ne;
               N_Prio := Prio_Equal;
            when Tok_Case_Ne =>
               Op := Binop_Case_Ne;
               N_Prio := Prio_Equal;
            when Tok_Case_Eq =>
               Op := Binop_Case_Eq;
               N_Prio := Prio_Equal;

            when Tok_Left_Lshift =>
               Op := Binop_Left_Lshift;
               N_Prio := Prio_Shift;
            when Tok_Left_Ashift =>
               Op := Binop_Left_Ashift;
               N_Prio := Prio_Shift;
            when Tok_Right_Lshift =>
               Op := Binop_Right_Lshift;
               N_Prio := Prio_Shift;
            when Tok_Right_Ashift =>
               Op := Binop_Right_Ashift;
               N_Prio := Prio_Shift;

            when Tok_Minus =>
               Op := Binop_Sub;
               N_Prio := Prio_Add;
            when Tok_Plus =>
               Op := Binop_Add;
               N_Prio := Prio_Add;

            when Tok_Star =>
               Op := Binop_Umul;
               N_Prio := Prio_Mul;
            when Tok_Slash =>
               Op := Binop_Udiv;
               N_Prio := Prio_Mul;
            when Tok_Modulo =>
               Op := Binop_Umod;
               N_Prio := Prio_Mul;

            when Tok_Star_Star =>
               Op := Binop_Exp;
               N_Prio := Prio_Exp;

            when Tok_Bit_And =>
               Op := Binop_Bit_And;
               N_Prio := Prio_Bit_And;
            when Tok_Bit_Or =>
               Op := Binop_Bit_Or;
               N_Prio := Prio_Bit_Or;

            when Tok_Bit_Xor =>
               Op := Binop_Bit_Xor;
               N_Prio := Prio_Bit_Xor;
            when Tok_Bit_Xnor =>
               Op := Binop_Bit_Xnor;
               N_Prio := Prio_Bit_Xor;
            when Tok_Bit_Nxor =>
               Op := Binop_Bit_Nxor;
               N_Prio := Prio_Bit_Xor;

            when Tok_Logic_And =>
               Op := Binop_Logic_And;
               N_Prio := Prio_Log_And;
            when Tok_Logic_Or =>
               Op := Binop_Logic_Or;
               N_Prio := Prio_Log_Or;

            when Tok_Question =>
               if Prio > Prio_Cond then
                  return Res;
               else
                  Res := Create_Node (N_Cond_Op);
                  Set_Token_Location (Res);
                  Set_Condition (Res, Left);

                  --  Skip '?'.
                  Scan;

                  Attrs := Parse_Attribute_Instances_Rep;
                  Set_Op_Attributes (Res, Attrs);

                  Set_Cond_True (Res, Parse_Expression (Prio_Cond));

                  --  Skip ':'.
                  Scan_Or_Error (Tok_Colon, "':' expected");

                  Set_Cond_False (Res, Parse_Expression (Prio_Cond));

                  goto Done;
               end if;

            when Tok_Inside =>
               --  1800-2017 11.4.13 Set membership operator
               --  inside_expression ::= expression INSIDE { open_range_list }
               if Prio > Prio_Comp then
                  return Res;
               else
                  --  Skip 'inside'.
                  Scan;

                  Res := Create_Node (N_Membership);
                  Set_Token_Location (Res);

                  Set_Expression (Res, Left);

                  Set_Expressions (Res, Parse_Bracketed_Range_List);

                  goto Done;
               end if;

            when Tok_Colon =>
               --  Only allow mintypmax inside parenthesis.
               --  ':' is also used for ranges and conditionnal expressions.
               if Prio /= Prio_Paren then
                  return Res;
               end if;
               Res := Create_Node (N_Mintypmax);
               Set_Location (Res, Get_Location (Left));
               Set_Min_Expr (Res, Left);
               Scan;
               Set_Typ_Expr (Res, Parse_Expression (Prio_None));
               Scan_Or_Error (Tok_Colon, "':' expected in mintypmax");
               Set_Max_Expr (Res, Parse_Expression (Prio_None));
               return Res;

            when Tok_Left_Paren
              | Tok_Semicolon =>
               return Res;
            when others =>
               return Res;
         end case;

         --  All operators shall associate left to right, with the exception
         --  of the conditional operator which shall associate right to left.
         if Prio >= N_Prio then
            return Res;
         end if;

         if Op in Binary_Short_Circuit_Ops then
            Res := Create_Node (N_Short_Circuit_Op);
         else
            Res := Create_Node (N_Binary_Op);
         end if;
         Set_Binary_Op (Res, Op);
         Set_Token_Location (Res);

         --  Skip operator.
         Scan;

         Set_Left (Res, Left);

         Attrs := Parse_Attribute_Instances_Rep;
         Set_Op_Attributes (Res, Attrs);

         Set_Right (Res, Parse_Expression (N_Prio));

         << Done >> null;
      end loop;
   end Parse_Expression_With_Primary;

   --  precond : next token
   --  postcond: next token
   function Parse_Expression (Prio : Prio_Type := Prio_None) return Node
   is
      Prim : Node;
   begin
      Prim := Parse_Primary_Expression;

      if Prim = Null_Node then
         return Null_Node;
      end if;

      return Parse_Expression_With_Primary (Prim, Prio);
   end Parse_Expression;

   function Parse_Part_Select
     (Kind : Nkinds_Part_Select; Pfx : Node; Msb : Node) return Node
   is
      Res : Node;
      Lsb : Node;
   begin
      Res := Create_Node (Kind);
      Set_Token_Location (Res);
      Set_Name (Res, Pfx);

      --  Skip ':' '+:' '-:'
      Scan;

      Lsb := Parse_Expression;

      case Kind is
         when N_Part_Select =>
            Set_Msb (Res, Msb);
            Set_Lsb (Res, Lsb);
         when Nkinds_Indexed_Part_Select =>
            Set_Base_Expr (Res, Msb);
            Set_Width_Expr (Res, Lsb);
      end case;

      return Res;
   end Parse_Part_Select;

   --  precond:  '['
   --  postcond: next token
   function Parse_Range (Kind : Nkind; El_Type : Type_Node) return Type_Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Msb : Node;
      Res : Node;
   begin
      --  Skip '['.
      Scan_Or_Error
        (Tok_Left_Brack, "'[' is expected at beginning of a range");

      Msb := Parse_Expression;

      case Current_Token is
         when Tok_Plus_Colon =>
            Res := Parse_Part_Select (N_Plus_Part_Select, El_Type.Typ, Msb);
         when Tok_Minus_Colon =>
            Res := Parse_Part_Select (N_Minus_Part_Select, El_Type.Typ, Msb);
         when others =>
            Res := Create_Node (Kind);
            Set_Location (Res, Loc);
            Set_Element_Data_Type (Res, El_Type.Typ);
            Set_Type_Owner (Res, El_Type.Own);
            Set_Msb (Res, Msb);

            if Current_Token = Tok_Colon then
               --  Skip ':'.
               Scan;
               Set_Lsb (Res, Parse_Expression);
            end if;
      end case;

      --  Skip ']'.
      Scan_Or_Error (Tok_Right_Brack, "']'  is expected at end of range");

      return (Res, True);
   end Parse_Range;

   function Parse_Range (Kind : Nkind) return Node
   is
      Res : Type_Node;
   begin
      Res := Parse_Range (Kind, (Null_Node, False));
      return Res.Typ;
   end Parse_Range;

   --  Parse bit_select or range_select.
   --  bit_select ::=
   --    { [ expression ] }
   function Parse_Bit_Select_Opt (Prefix : Node) return Node
   is
      Res : Node;
      Pfx : Node;
      Msb : Node;
   begin
      Pfx := Prefix;
      while Current_Token = Tok_Left_Brack loop
         --  Skip '['
         Scan;

         Msb := Parse_Expression;
         case Current_Token is
            when Tok_Colon =>
               Res := Parse_Part_Select (N_Part_Select, Pfx, Msb);
            when Tok_Plus_Colon =>
               Res := Parse_Part_Select (N_Plus_Part_Select, Pfx, Msb);
            when Tok_Minus_Colon =>
               Res := Parse_Part_Select (N_Minus_Part_Select, Pfx, Msb);
            when others =>
               Res := Create_Node (N_Bit_Select);
               Set_Token_Location (Res);
               Set_Expression (Res, Msb);
               Set_Name (Res, Pfx);
         end case;
         if Current_Token = Tok_Right_Brack then
            --  Skip ']'.
            Scan;
         else
            Error_Msg_Parse ("']' expected after bit/part reference");
         end if;
         Pfx := Res;
      end loop;
      return Pfx;
   end Parse_Bit_Select_Opt;

   function Parse_Dotted_Name (Prefix : Node) return Node
   is
      Res : Node;
   begin
      --  FIXME: check for no space before.
      Res := Create_Node (N_Dotted_Name);
      Set_Token_Location (Res);
      Set_Name (Res, Prefix);

      --  Skip '.'.
      Scan;

      --  Skip identifier.
      case Current_Token is
         when Tok_Identifier
           | Tok_Unique
           | Tok_New =>
            Set_Identifier (Res, Current_Identifier);

            --  Skip identifier.
            Scan;
         when others =>
            Error_Msg_Parse ("identifier expected after '.'");
      end case;

      return Res;
   end Parse_Dotted_Name;

   function Parse_Scoped_Name (Prefix : Node) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Scoped_Name);
      Set_Token_Location (Res);
      Set_Name (Res, Prefix);

      --  Skip '::'.
      Scan;

      --  Skip identifier.
      if Current_Token /= Tok_Identifier and Current_Token /= Tok_New then
         Error_Msg_Parse ("identifier expected after '::'");
      else
         Set_Identifier (Res, Current_Identifier);

         --  Skip identifier.
         Scan;
      end if;

      return Res;
   end Parse_Scoped_Name;

   --  '#' was scanned after a scoped or hierarchical name.
   function Parse_Class_Instance (Base : Node) return Node
   is
      Res : Node;
   begin
      pragma Assert (Current_Token = Tok_Sharp);

      --  Skip '#'.
      Scan;

      if Current_Token = Tok_Left_Paren then
         Res := Create_Node (N_Class_Instance);
         Set_Location (Res, Get_Location (Base));
         Set_Class_Name (Res, Base);
         Set_Parameter_Values (Res, Parse_Parameter_Value_Assignment);
         return Res;
      else
         --  A delay.
         Res := Create_Node (N_Primitive_Instance);
         Set_Location (Res, Get_Location (Base));
         Set_Module (Res, Base);
         Set_Gate_Delay (Res, Parse_Delay_Value);
         return Res;
      end if;
   end Parse_Class_Instance;

   function Parse_Scoped_Or_Hierarchical_Name return Node
   is
      Res : Node;
   begin
      --  Skip 'this'/identifier.
      case Current_Token is
         when Tok_Identifier =>
            Res := Scan_Name;
            if Current_Token = Tok_Sharp then
               Res := Parse_Class_Instance (Res);
            end if;
         when Tok_This =>
            Res := Scan_This;
         when others =>
            raise Internal_Error;
      end case;

      loop
         case Current_Token is
            when Tok_Dot =>
               Res := Parse_Dotted_Name (Res);
            when Tok_Colon_Colon =>
               Res := Parse_Scoped_Name (Res);
            when others =>
               return Res;
         end case;
      end loop;
   end Parse_Scoped_Or_Hierarchical_Name;

   function Parse_Name (Prefix : Node) return Node
   is
      Res : Node;
   begin
      Res := Prefix;

      loop
         case Current_Token is
            when Tok_Dot =>
               Res := Parse_Dotted_Name (Res);
            when Tok_Left_Brack =>
               Res := Parse_Bit_Select_Opt (Res);
            when Tok_Left_Paren =>
               Res := Parse_Tf_Call (Res);
            when Tok_With =>
               Res := Parse_Tf_Call (Res);
               exit;
            when others =>
               exit;
         end case;
      end loop;
      return Res;
   end Parse_Name;

   --  precond:  an identifier
   --  postcond: next token
   function Parse_Lvalue return Node
   is
      Res : Node;
   begin
      case Current_Token is
         when Tok_Identifier =>
            --  Skip identifier.
            Res := Parse_Scoped_Or_Hierarchical_Name;

            Res := Parse_Name (Res);

         when Tok_This =>
            Res := Scan_This;

            Res := Parse_Name (Res);

         when Tok_Super =>
            Res := Create_Node (N_Super);
            Set_Token_Location (Res);

            --  Skip 'super'.
            Scan;

            Res := Parse_Name (Res);

         when Tok_Left_Curly =>
            declare
               Last : Node;
               El : Node;
            begin
               Res := Create_Node (N_Concatenation);
               Set_Token_Location (Res);

               --  Skip '{'.
               Scan;

               El := Create_Node (N_Element);
               Set_Token_Location (El);
               Set_Expressions (Res, El);

               Set_Expression (El, Parse_Lvalue);

               Last := El;
               loop
                  exit when Current_Token /= Tok_Comma;

                  El := Create_Node (N_Element);
                  Set_Token_Location (El);

                  --  Skip ','.
                  Scan;

                  Set_Expression (El, Parse_Lvalue);
                  Set_Chain (Last, El);
                  Last := El;
               end loop;

               --  Skip '}'.
               Scan_Or_Error
                 (Tok_Right_Curly, "missing '}' to close concatenation");
            end;
         when others =>
            Error_Msg_Parse ("name expected");
            Res := Null_Node;
      end case;

      return Res;
   end Parse_Lvalue;

   procedure Append_Implicits (Constr : in out Items_Constr) is
   begin
      while Implicit_First /= Null_Node loop
         Append_Node (Constr, Implicit_First);
         Implicit_First := Get_Chain (Implicit_First);
      end loop;
      Implicit_Last := Null_Node;
   end Append_Implicits;

   --  1800-2017 6.7 Net declarations
   --  delay_value ::=
   --      unsigned_number
   --    | real_number
   --    | ps_identifier
   --    | time_literal
   --    | 1step
   --
   --  GHDL: cannot use directly parse_expression as a delay_value can be
   --  followed by an expression.  So the delay of '#5 +a' is not (5+a).
   --  Likewise, "#4 'h8" is a delay of 4 and a value of 'h8.
   function Parse_Delay_Value return Node is
   begin
      case Current_Token is
         when Tok_Identifier =>
            return Parse_Scoped_Or_Hierarchical_Name;
         when Tok_Dec_Number =>
            return Parse_Unsigned_Number;
         when Tok_Time_Literal
           | Tok_Real_Number =>
            return Parse_Primary_Expression;
         when others =>
            Error_Msg_Parse ("delay value expected");
            return Null_Node;
      end case;
   end Parse_Delay_Value;

   --  precond:  #
   --  postcond: next token
   --
   --  1800-2017 6.7 Net declarations
   --  delay3 ::=
   --      # delay_value
   --    | # ( mintypmax_expression
   --          [ , mintypmax_expression [ , mintypmax_expression ] ] )
   --
   --  delay2 ::=
   --      # delay_value
   --    | # ( mintypmax_expression [ , mintypmax_expression ] )
   function Parse_Delay2_3 (Highz_Allowed : Boolean) return Node
   is
      Loc : Location_Type;
      Res : Node;
   begin
      Loc := Get_Token_Location;

      --  Skip '#'.
      Scan;

      if Current_Token = Tok_Left_Paren then
         Res := Create_Node (N_Delay);
         Set_Location (Res, Loc);

         --  Skip '('.
         Scan;

         Set_Rising_Delay (Res, Parse_Expression (Prio_Paren));

         if Current_Token = Tok_Comma then
            --  Skip ','.
            Scan;

            Set_Falling_Delay (Res, Parse_Expression (Prio_Paren));

            if Current_Token = Tok_Comma then
               --  Skip ','.
               Scan;

               if not Highz_Allowed then
                  Error_Msg_Parse ("highz delay not allowed here");
               end if;
               Set_Highz_Delay (Res, Parse_Expression (Prio_Paren));
            end if;
         end if;

         --  Skip ')'.
         Scan_Or_Error (Tok_Right_Paren, "')' expected at end of delay3");
      else
         --  Delay value.
         Res := Parse_Delay_Value;
      end if;

      return Res;
   end Parse_Delay2_3;

   subtype Nat_01 is Natural range 0 .. 1;
   procedure Parse_Strength (D : out Drive_Strength_Type;
                             N : out Nat_01)
   is
   begin
      case Current_Token is
         when Tok_Highz1 =>
            D := Drive_Highz;
            N := 1;
         when Tok_Highz0 =>
            D := Drive_Highz;
            N := 0;
         when Tok_Weak0 =>
            D := Drive_Weak;
            N := 0;
         when Tok_Weak1 =>
            D := Drive_Weak;
            N := 1;
         when Tok_Pull0 =>
            D := Drive_Pull;
            N := 0;
         when Tok_Pull1 =>
            D := Drive_Pull;
            N := 1;
         when Tok_Strong0 =>
            D := Drive_Strong;
            N := 0;
         when Tok_Strong1 =>
            D := Drive_Strong;
            N := 1;
         when Tok_Supply0 =>
            D := Drive_Supply;
            N := 0;
         when Tok_Supply1 =>
            D := Drive_Supply;
            N := 1;
         when others =>
            D := Drive_Unknown;
            N := 0;
      end case;
   end Parse_Strength;

   --  precond:  first strength (after '(').
   --  postcond: next token
   function Parse_Drive_Strength return Int32
   is
      type Drive_Strength_Array is array (Nat_01) of Drive_Strength_Type;
      Ds : Drive_Strength_Array;
      D : Drive_Strength_Type;
      N : Nat_01;
   begin
      Ds := (Drive_Unknown, Drive_Unknown);
      Parse_Strength (D, N);
      if D = Drive_Unknown then
         Error_Msg_Parse ("strength expected");
      else
         Ds (N) := D;
      end if;
      Scan;

      if Current_Token = Tok_Comma then
         --  Skip ','.
         Scan;

         Parse_Strength (D, N);
         if D = Drive_Unknown then
            Error_Msg_Parse ("strength expected");
         else
            if Ds (N) /= Drive_Unknown then
               Error_Msg_Parse ("strength for the polarity already specified");
            else
               Ds (N) := D;
            end if;
         end if;
         if Ds (0) = Drive_Highz and Ds (1) = Drive_Highz then
            Error_Msg_Parse
              ("drive strength cannot be highz for both polarities");
         end if;
         Scan;
      end if;

      --  Skip ')'.
      Scan_Or_Error (Tok_Right_Paren, "')' expected after drive strength");

      return Drive_Strength_To_Int32 (Ds (0), Ds (1));
   end Parse_Drive_Strength;

   --  precond:  '('
   --  postcond: next token
   function Parse_Charge_Strength return Int32
   is
      D : Drive_Strength_Type;
   begin
      if Current_Token /= Tok_Left_Paren then
         raise Internal_Error;
      end if;
      Scan;
      case Current_Token is
         when Tok_Small =>
            D := Charge_Small;
         when Tok_Medium =>
            D := Charge_Medium;
         when Tok_Large =>
            D := Charge_Large;
         when others =>
            D := Drive_Unknown;
            Error_Msg_Parse ("charge strength expected");
      end case;
      Scan;

      --  Skip ')'.
      Scan_Or_Error (Tok_Right_Paren, "')' expected after charge strength");

      return Drive_Strength_To_Int32 (D, Drive_Unknown);
   end Parse_Charge_Strength;

   procedure Parse_Continuous_Assignment (Constr : in out Items_Constr)
   is
      Res : Node;
      Lvalue : Node;
      Delay3 : Node;
      Strength : Int32;
   begin
      --  Skip 'assign'.
      Scan;

      --  Drive_strength
      if Current_Token = Tok_Left_Paren then
         --  Skip '('
         Scan;

         Strength := Parse_Drive_Strength;
      else
         Strength := 0;
      end if;

      --  Delay3
      if Current_Token = Tok_Sharp then
         Delay3 := Parse_Delay2_3 (True);
      else
         Delay3 := Null_Node;
      end if;

      loop
         Lvalue := Parse_Lvalue;

         if Current_Token /= Tok_Equal then
            Error_Msg_Parse
              ("'=' expected after lvalue for a continous assignment");
         end if;

         Res := Create_Node (N_Assign);
         Set_Token_Location (Res);
         Set_Lvalue (Res, Lvalue);
         Set_Assign_Delay (Res, Delay3);
         Set_Drive_Strength (Res, Strength);

         --  Skip '='.
         Scan;

         Set_Expression (Res, Parse_Expression);

         Append_Node (Constr, Res);

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
      end loop;

      --  Skip ';'.
      Scan_Statement_Semicolon;
   end Parse_Continuous_Assignment;

   --  1800-2005 4.2 Data type syntax
   --   [ signing ]
   function Parse_Signing_Opt (Uns_Type : Type_Node;
                               Sgn_Type : Type_Node;
                               Def_Type : Type_Node) return Type_Node
   is
      Res : Type_Node;
   begin
      case Current_Token is
         when Tok_Unsigned =>
            Res := Uns_Type;

            --  Skip 'unsigned'.
            Scan;

         when Tok_Signed =>
            Res := Sgn_Type;

            --  Skip 'signed'.
            Scan;

         when others =>
            Res := Def_Type;
      end case;

      return Res;
   end Parse_Signing_Opt;

   --  Parse: { packed_dimension }
   function Parse_Packed_Dimensions (Btype : Type_Node) return Type_Node
   is
      Res : Node;
      El : Type_Node;
   begin
      if Current_Token /= Tok_Left_Brack then
         return Btype;
      end if;

      Res := Parse_Range (N_Packed_Array);

      if Current_Token = Tok_Left_Brack then
         if Verilog.Flags.Std in Verilog_Standard then
            --  FIXME: to be moved.
            Error_Msg_Parse
              ("multi-dimensional packed arrays not allowed by Verilog");
         end if;

         El := Parse_Packed_Dimensions (Btype);
      else
         El := Btype;
      end if;

      if Get_Kind (Res) = N_Packed_Array then
         Set_Element_Type_Node (Res, El);
      else
         --  For plus/minus part select
         --  FIXME: handle owner.
         Set_Name (Res, El.Typ);
      end if;

      return (Res, True);
   end Parse_Packed_Dimensions;

   --  1800-2005 4.2 Data type syntax
   --  data_type ::=
   --      integer_vector_type [ signing ] { packed_dimension }
   --    | ...
   --
   --  1800-2017 7.4.1 Packed arrays
   --  If a packed array is declared as signed, then the array viewed as a
   --  single vector shall be signed.  The individual elements of the array
   --  are unsigned unless they are of a named type declared as signed.
   function Parse_Signing_Packed_Dimension
     (Uns_Type : Type_Node; Sgn_Type : Type_Node; Def_Type : Type_Node)
     return Type_Node
   is
      Res : Type_Node;
      Sign : Boolean;
      Has_Sign : Boolean;
   begin
      case Current_Token is
         when Tok_Unsigned =>
            Has_Sign := True;
            Sign := False;

            --  Skip 'unsigned'.
            Scan;

         when Tok_Signed =>
            Has_Sign := True;
            Sign := True;

            --  Skip 'signed'.
            Scan;

         when others =>
            Has_Sign := False;
            Sign := False;
      end case;

      if Current_Token = Tok_Left_Brack then
         Res := Parse_Packed_Dimensions (Def_Type);
         Set_Signed_Flag (Res.Typ, Sign);
         Set_Has_Sign (Res.Typ, Has_Sign);
      else
         if Has_Sign then
            if Sign then
               Res := Sgn_Type;
            else
               Res := Uns_Type;
            end if;
         else
            Res := Def_Type;
         end if;
      end if;

      return Res;
   end Parse_Signing_Packed_Dimension;

   --  1800-2005 4.2 Data type syntax
   --  data_type ::=
   --      integer_vector_type [ signing ] { packed_dimension }
   --    | ...
   function Parse_Integer_Vector_Data_Type
     (Uns_Type : Node; Sgn_Type : Node; Def_Type : Node) return Type_Node is
   begin
      --  Skip type.
      Scan;

      return Parse_Signing_Packed_Dimension
        ((Uns_Type, False), (Sgn_Type, False), (Def_Type, False));
   end Parse_Integer_Vector_Data_Type;

   --  1800-2005 4.2 Data type syntax
   --  data_type ::=
   --      ...
   --    | integer_atom_type [ signing ]
   --    | ...
   function Parse_Integer_Atom_Data_Type
     (Uns_Type : Node; Sgn_Type : Node; Def_Type : Node) return Type_Node is
   begin
      --  Skip type.
      Scan;

      return Parse_Signing_Opt
        ((Uns_Type, False), (Sgn_Type, False), (Def_Type, False));
   end Parse_Integer_Atom_Data_Type;

   --  1800-2012 7.10 Queues
   --  variable_dimension ::=
   --      unsized_dimension
   --    | unpacked_dimension
   --    | associative_dimension
   --    | queue_dimension
   --
   --  queue_dimension ::= [ $ [ : constant_expression ] ]
   --
   --  unsized_dimension ::= [ ]
   function Parse_Variable_Dimension_Rep (Decl_Type : Type_Node)
                                         return Type_Node
   is
      Ntype : Node;
      El_Type : Type_Node;
      Itype : Type_Node;
   begin
      --  Skip '['.
      Scan;

      case Current_Token is
         when Tok_Dollar =>
            --  A queue.
            Ntype := Create_Node (N_Queue);
            Set_Token_Location (Ntype);

            --  Skip '$'.
            Scan;

            if Current_Token = Tok_Colon then

               --  Skip ':'.
               Scan;

               Set_Maximum_Size_Expr (Ntype, Parse_Expression);
            end if;

         when Tok_Star =>
            --  An associative array with a wildcard.
            --  Data type are parsed as normal array.
            Ntype := Create_Node (N_Associative_Array);

            Set_Token_Location (Ntype);

            --  Skip '*'.
            Scan;

         when Tok_String | Tok_Int =>
            Ntype := Create_Node (N_Associative_Array);
            Set_Token_Location (Ntype);
            Itype := Parse_Data_Type_Or_Implicit;

            Set_Index_Data_Type (Ntype, Itype.Typ);
            Set_Type_Owner_2 (Ntype, Itype.Own);

         when Tok_Right_Brack =>
            Ntype := Create_Node (N_Dynamic_Array);
            Set_Token_Location (Ntype);

         when others =>
            --  An array.
            Ntype := Create_Node (N_Array);
            Set_Token_Location (Ntype);

            Set_Msb (Ntype, Parse_Expression);

            if Current_Token = Tok_Colon then
               --  Skip ':'.
               Scan;

               Set_Lsb (Ntype, Parse_Expression);
            end if;
      end case;

      --  Skip ']'.
      Scan_Or_Error (Tok_Right_Brack, "missing ']' at end of array");

      if Current_Token = Tok_Left_Brack then
         El_Type := Parse_Variable_Dimension_Rep (Decl_Type);
      else
         El_Type := Decl_Type;
      end if;
      Set_Element_Type_Node (Ntype, El_Type);

      return (Ntype, True);
   end Parse_Variable_Dimension_Rep;

   procedure Parse_Variable_Dimension_Rep (Decl : Node)
   is
      Decl_Type : Type_Node;
   begin
      --  7.4.5 Multidimensional arrays
      --  In a list of dimensions, the righmost one varies most rapidely, as
      --  in C.

      if Current_Token /= Tok_Left_Brack then
         return;
      end if;

      Decl_Type := (Get_Data_Type (Decl), Get_Type_Owner (Decl));
      Decl_Type := Parse_Variable_Dimension_Rep (Decl_Type);

      if Flags.Std <= Verilog_2005 then
         --  FIXME: do this check after parse.
         case Get_Kind (Get_Element_Data_Type (Decl_Type.Typ)) is
            when N_Log_Packed_Array_Cst
              | N_Packed_Array
              | N_Array =>
               null;
            when N_Typedef =>
               --  Doesn't exist in verilog, so this is a logic type.
               null;
            when N_Logic_Type =>
               null;
            when N_Real_Type
              | N_Shortreal_Type =>
               --  FIXME: added in 2001 ?
               null;
            when others =>
               Error_Msg_Parse
                 ("memory allowed only for reg, integer or time");
         end case;
      end if;

      Set_Type_Node (Decl, Decl_Type);
   end Parse_Variable_Dimension_Rep;

   --  1800-2012 6.8 Variable Declarations
   --  list_of_variable_decl_assignments ::=
   --    variable_decl_assignment { , variable_decl_assignment }
   --
   --  variable_decl_assignment ::=
   --      variable_identifier { variable_dimension } [ = expression ]
   --    | dynamic_array_variable_identifier unsized_dimension
   --        { variable_dimension } [ = dynamic_array_new ]
   --    | class_variable_identifier [ = class_new ]
   procedure Parse_List_Of_Variable_Decl_Assignments
     (Constr : in out Items_Constr;
      Decl_Type : in out Type_Node;
      Kind : Nkind)
   is
      Res : Node;
   begin
      loop
         Res := Create_Node (Kind);
         Set_Token_Location (Res);
         Set_Type_Node (Res, Decl_Type);

         --  Skip identifier.
         Scan_Identifier (Res, "identifier expected");

         --  variable_dimension
         Parse_Variable_Dimension_Rep (Res);

         if Current_Token = Tok_Equal then
            --  Skip '='.
            Scan;

            Set_Expression (Res, Parse_Expression);
         end if;

         Append_Node (Constr, Res);

         exit when Current_Token /= Tok_Comma;

         Set_Has_Identifier_List (Res, True);

         --  Skip ','.
         Scan;
      end loop;
   end Parse_List_Of_Variable_Decl_Assignments;

   --  1800-2017 7.2 Structures
   --  struct_union_member ::=
   --    [ random_qualifier ] data_type_or_void
   --      list_of_variable_decl_assignment ;
   function Parse_Members (Kind : Nkind; Parent : Node) return Node
   is
      Constr : Items_Constr;
      Decl_Type : Type_Node;
   begin
      Init_Constr (Constr, Parent);

      Scan_Or_Error
        (Tok_Left_Curly, "'{' required before struct or union members");

      while Current_Token /= Tok_Right_Curly loop
         Decl_Type := Parse_Data_Type_Or_Implicit;

         if Decl_Type.Typ = Null_Node then
            Error_Msg_Parse ("data type required for member declaration");
            exit;
         end if;

         Parse_List_Of_Variable_Decl_Assignments (Constr, Decl_Type, Kind);

         --  Skip ';'.
         Scan_Declaration_Semicolon;
      end loop;

      Scan_Or_Error
        (Tok_Right_Curly, "'}' required after struct or union members");

      return Get_Constr_Chain (Constr);
   end Parse_Members;

   procedure Parse_Struct_Union_Packed (Atype : Node) is
   begin
      --  Skip 'packed'.
      Scan;

      --  Signing.
      case Current_Token is
         when Tok_Signed =>
            Set_Signed_Flag (Atype, True);
            Set_Has_Sign (Atype, True);

            --  Skip 'signed'.
            Scan;
         when Tok_Unsigned =>
            Set_Signed_Flag (Atype, False);
            Set_Has_Sign (Atype, True);

            --  Skip 'unsigned'.
            Scan;
         when others =>
            --  1800-2017 7.2.1 Packed structures
            --  ...  with unsigned being the default.

            --  1800-2017 7.3.1 Packed unions
            --  ...  with unsigned being the default.
            Set_Signed_Flag (Atype, False);
      end case;
   end Parse_Struct_Union_Packed;

   --  Precond: next token (after STRUCT)
   --
   --  1800-2017 7.2 Structures
   --  data_type ::=
   --      ...
   --    | struct_union [ PACKED [ signing ] ]
   --        '{' struct_union_member { struct_union_member } '}'
   --        { packed_dimension }
   function Parse_Struct_Data_Type return Type_Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Mkind : Nkind;
      Res : Node;
   begin
      if Current_Token = Tok_Packed then
         Res := Create_Node (N_Packed_Struct_Type);
         Mkind := N_Packed_Member;

         Parse_Struct_Union_Packed (Res);
      else
         Res := Create_Node (N_Struct_Type);
         Mkind := N_Member;
         if Current_Token = Tok_Signed or Current_Token = Tok_Unsigned then
            Error_Msg_Parse ("signing not allowed for unpacked structures");

            --  Skip 'signed'/'unsigned'.
            Scan;
         end if;
      end if;
      Set_Location (Res, Loc);

      Set_Members (Res, Parse_Members (Mkind, Res));

      return (Res, True);
   end Parse_Struct_Data_Type;

   --  1800-2017 7.3 Unions
   --  data_type ::=
   --      ...
   --    | struct_union [ PACKED [ signing ] ]
   --        '{' struct_union_member { struct_union_member } '}'
   --        { packed_dimension }
   function Parse_Union_Data_Type return Type_Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Mkind : Nkind;
      Res : Node;
   begin
      if Current_Token = Tok_Packed then
         Res := Create_Node (N_Packed_Union_Type);
         Mkind := N_Packed_Member;

         Parse_Struct_Union_Packed (Res);
      else
         --  1800-2017 7.3 Unions
         --  By default, a union is unpacked, meaning there is no required
         --  representation for how members of the union are stored.
         Res := Create_Node (N_Union_Type);
         Mkind := N_Member;

         if Current_Token = Tok_Signed or Current_Token = Tok_Unsigned then
            Error_Msg_Parse ("signing not allowed for unpacked unions");

            --  Skip 'signed'/'unsigned'.
            Scan;
         end if;
      end if;
      Set_Location (Res, Loc);

      Set_Members (Res, Parse_Members (Mkind, Res));

      return (Res, True);
   end Parse_Union_Data_Type;

   --  1800-2017 6.19 Enumerations
   --  enum_name_declaration ::=
   --    enum_identifier [ '[' integral_number [ ':' integral_number ] ']'
   --      [ = constant_expression ]
   function Parse_Enum_Name (Parent : Node) return Node
   is
      Res : Node;
      Loc : Location_Type;
      Id : Name_Id;
   begin
      pragma Assert (Current_Token = Tok_Identifier);
      Id := Current_Identifier;
      Loc := Get_Token_Location;

      --  Skip identifier.
      Scan;

      if Current_Token = Tok_Left_Brack then
         raise Program_Error;
      else
         Res := Create_Node (N_Enum_Name);
         Set_Parent (Res, Parent);
      end if;

      Set_Identifier (Res, Id);
      Set_Location (Res, Loc);

      if Current_Token = Tok_Equal then
         --  Skip '='.
         Scan;

         Set_Expression (Res, Parse_Expression);
      end if;

      return Res;
   end Parse_Enum_Name;

   --  1800-2017 6.19 Enumerations
   --  data_type ::=
   --    ...
   --    |  ENUM [ enum_base_type ] '{' enum_name_declaration
   --              { , enum_name_declaration } '}' { packed_dimension }
   function Parse_Enum_Data_Type return Type_Node
   is
      Res : Node;
      Base_Type : Type_Node;
      First, Last : Node;
      El : Node;
   begin
      Res := Create_Node (N_Enum_Type);
      Set_Token_Location (Res);

      --  Skip 'enum'
      Scan;

      Base_Type := Parse_Data_Type_Or_Implicit;
      Set_Enum_Base_Data_Type (Res, Base_Type.Typ);
      Set_Type_Owner (Res, Base_Type.Own);

      Scan_Or_Error (Tok_Left_Curly, "'{' expected for enum declaration");

      Init_Chain (First, Last);
      while Current_Token = Tok_Identifier loop
         El := Parse_Enum_Name (Res);
         Append_Chain (First, Last, El);
         exit when Current_Token /= Tok_Comma;

         --  Skip ','
         Scan;
      end loop;

      Scan_Or_Error
        (Tok_Right_Curly, "'}' expected at end of enum declaration");

      Set_Enum_Names (Res, First);

      return (Res, True);
   end Parse_Enum_Data_Type;

   --  1800-2017 25.9 Virtual Interfaces
   --  data_type ::=
   --      ...
   --    | VIRTUAL [ INTERFACE ] interface_identifier
   --        [ parameter_value_assignment ] [ . modport_identifier ]
   --    | ...
   function Parse_Virtual_Interface return Type_Node
   is
      Res : Node;
      Name : Node;
   begin
      Res := Create_Node (N_Virtual_Interface);
      Set_Token_Location (Res);

      --  Skip 'virtual'.
      Scan;

      if Current_Token = Tok_Interface then
         --  Skip 'interface'.
         Scan;
      end if;

      if Current_Token = Tok_Identifier then
         Name := Scan_Name;
      else
         Error_Msg_Parse ("interface identifier expected after 'virtual'");
         Name := Null_Node;
      end if;

      if Current_Token = Tok_Sharp then
         --  Skip '#'.
         Scan;

         Set_Parameter_Values (Res, Parse_Parameter_Value_Assignment);
      end if;

      if Current_Token = Tok_Dot then
         --  Skip '.'.
         Scan;

         Name := Parse_Dotted_Name (Name);
      end if;

      Set_Interface (Res, Name);

      return (Res, True);
   end Parse_Virtual_Interface;

   --  1800-2017 6.8 Variable declarations
   --  1800-2005 4.2 Data type syntax
   --  data_type ::=
   --      integer_vector_type [ signing ] { packed_dimension }
   --    | integer_atom_type [ signing ]
   --    | non_integer_type
   --    | ...
   --    | STRING
   --    | ...
   --
   --  integer_vector_type ::= BIT | LOGIC | REG
   --
   --  integer_atom_type ::= BYTE | SHORTINT | INT | LONGINT | INTEGER | TIME
   --
   --  implicit_data_type ::= [ signing ] { packed_dimension }
   --
   --  data_type_or_implicit ::=
   --      data_type
   --    | implicit_data_type
   function Parse_Data_Type_Or_Implicit return Type_Node is
   begin
      case Current_Token is
         when Tok_Bit =>
            return Parse_Integer_Vector_Data_Type
              (Unsigned_Bit_Typedef, Signed_Bit_Typedef, Unsigned_Bit_Typedef);
         when Tok_Logic =>
            return Parse_Integer_Vector_Data_Type
              (Unsigned_Logic_Typedef, Signed_Logic_Typedef,
               Unsigned_Logic_Typedef);
         when Tok_Reg =>
            return Parse_Integer_Vector_Data_Type
              (Reg_Typedef, Signed_Reg_Typedef, Reg_Typedef);
         when Tok_Byte =>
            return Parse_Integer_Atom_Data_Type
              (Unsigned_Byte_Typedef, Signed_Byte_Typedef, Byte_Typedef);
         when Tok_Shortint =>
            return Parse_Integer_Atom_Data_Type
              (Unsigned_Shortint_Typedef, Signed_Shortint_Typedef,
               Signed_Shortint_Type);
         when Tok_Int =>
            return Parse_Integer_Atom_Data_Type
              (Unsigned_Int_Typedef, Int_Typedef, Int_Typedef);
         when Tok_Longint =>
            return Parse_Integer_Atom_Data_Type
              (Unsigned_Longint_Typedef, Longint_Typedef,
               Longint_Typedef);
         when Tok_Integer =>
            return Parse_Integer_Atom_Data_Type
              (Unsigned_Integer_Typedef, Integer_Typedef, Integer_Typedef);
         when Tok_Time =>
            return Parse_Integer_Atom_Data_Type
              (Unsigned_Time_Typedef, Signed_Time_Typedef, Time_Typedef);
         when Tok_Realtime =>
            --  Skip 'realtime'.
            Scan;

            return (Realtime_Typedef, False);
         when Tok_Real =>
            --  Skip 'real'.
            Scan;

            return (Real_Typedef, False);
         when Tok_Shortreal =>
            --  Skip 'shortreal'.
            Scan;

            return (Shortreal_Typedef, False);
         when Tok_Event =>
            --  Skip 'event'.
            Scan;

            return (Event_Typedef, False);
         when Tok_String =>
            --  Skip 'string'.
            Scan;

            return (String_Typedef, False);
         when Tok_Signed | Tok_Unsigned | Tok_Left_Brack =>
            return Parse_Signing_Packed_Dimension
              ((Implicit_Unsigned_Typedef, False),
               (Implicit_Signed_Typedef, False),
               (Implicit_Typedef, False));
         when Tok_Identifier =>
            --  Assume type_identifier { packed_dimension }
            declare
               Res : Type_Node;
            begin
               --  FIXME: in whichs case a data_type can be a
               --    hierarchical name ?
               --  For a modport ?
               Res.Typ := Parse_Scoped_Or_Hierarchical_Name;
               Res.Own := True;

               case Current_Token is
                  when Tok_Left_Brack =>
                     Res := Parse_Packed_Dimensions (Res);
                  when Tok_Sharp =>
                     Res.Typ := Parse_Class_Instance (Res.Typ);
                  when others =>
                     null;
               end case;
               return Res;
            end;
         when Tok_Struct =>
            --  Skip 'struct'
            Scan;

            return Parse_Struct_Data_Type;
         when Tok_Union =>
            --  Skip 'union'
            Scan;

            return Parse_Union_Data_Type;
         when Tok_Enum =>
            return Parse_Enum_Data_Type;
         when Tok_Chandle =>
            --  Skip 'chandle'.
            Scan;

            return (Chandle_Typedef, False);
         when Tok_Void =>
            --  Skip 'void'.
            Scan;

            return (Void_Typedef, False);
         when Tok_Virtual =>
            return Parse_Virtual_Interface;
         when others =>
            return (Null_Node, False);
      end case;
   end Parse_Data_Type_Or_Implicit;

   function Is_Implicit_Type (Atype : Node) return Boolean
   is
      Base : Node;
   begin
      if Atype = Null_Node then
         return True;
      end if;

      if Get_Kind (Atype) = N_Packed_Array then
         Base := Get_Element_Data_Type (Atype);
      else
         Base := Atype;
      end if;
      return Base = Implicit_Typedef
        or Base = Implicit_Signed_Typedef
        or Base = Implicit_Unsigned_Typedef;
   end Is_Implicit_Type;

   --  Convert a data_type to an identifier + type.
   --  Notations like T or T[a:b] are ambiguous: they can represent a
   --  type (T being a typedef or a class) or declaration of T with an
   --  implicit type.
   procedure Data_Type_To_Identifier (Id : out Name_Id;
                                      Decl_Type : in out Type_Node)
   is
      pragma Assert (Decl_Type.Own);
      Typ : constant Node := Decl_Type.Typ;
   begin
      case Get_Kind (Typ) is
         when N_Name =>
            Id := Get_Identifier (Typ);
            Free_Node (Typ);
            Decl_Type := (Implicit_Typedef, False);
         when N_Packed_Array =>
            declare
               Pfx : constant Node := Get_Element_Data_Type (Typ);
               Pfx_Type : Type_Node;
               Res : Node;
            begin
               --  If the element type is not owned, then this is certainly
               --  a default type; so identifier is missing.
               if not Get_Type_Owner (Typ) then
                  Error_Msg_Parse (+Typ, "missing identifier");
                  Id := Null_Identifier;
                  return;
               end if;
               Pfx_Type := (Pfx, True);
               Data_Type_To_Identifier (Id, Pfx_Type);
               Res := Create_Node (N_Array);
               Location_Copy (Res, Typ);
               Set_Msb (Res, Get_Msb (Typ));
               Set_Lsb (Res, Get_Lsb (Typ));
               Set_Element_Data_Type (Res, Pfx_Type.Typ);
               Set_Type_Owner (Res, Pfx_Type.Own);
               Free_Node (Typ);
               Decl_Type := (Res, True);
            end;
         when others =>
            Error_Kind ("data_type_to_identifier", Decl_Type.Typ);
      end case;
   end Data_Type_To_Identifier;

   procedure Data_Type_To_Identifier
     (Decl : Node; Decl_Type : in out Type_Node)
   is
      Id : Name_Id;
   begin
      Data_Type_To_Identifier (Id, Decl_Type);
      Set_Identifier (Decl, Id);
   end Data_Type_To_Identifier;

   --  1800-2017 6.18 User-defined types
   --  type_declaration ::=
   --      TYPEDEF data_type type_identifier { variable_dimension } ;
   --    | TYPEDEF interface_instance_identifier constant_bit_select .
   --        type_identifier type_identifier ;
   --    | TYPEDEF [ ENUM | STRUCT | UNION | CLASS | INTERFACE CLASS ]
   --        type_identifier ;
   function Parse_Type_Declaration return Node
   is
      Res : Node;
      Decl_Type : Type_Node;
   begin
      --  Skip 'typedef'.
      Scan;

      case Current_Token is
         when Tok_Struct =>
            --  Skip 'struct'.
            Scan;

            if Current_Token = Tok_Identifier then
               Res := Create_Node (N_Typedef_Struct);
            else
               Res := Create_Node (N_Typedef);
               Decl_Type := Parse_Struct_Data_Type;
            end if;

         when Tok_Class =>
            --  Skip 'class'.
            Scan;

            Res := Create_Node (N_Typedef_Class);

         when others =>
            Res := Create_Node (N_Typedef);

            Decl_Type := Parse_Data_Type_Or_Implicit;

            if Current_Token = Tok_Semicolon
              and then Get_Kind (Decl_Type.Typ) = N_Name
            then
               Res := Create_Node (N_Typedef_Forward);
               Location_Copy (Res, Decl_Type.Typ);
               Data_Type_To_Identifier (Res, Decl_Type);

               --  Skip ';'.
               Scan;

               return Res;
            end if;
      end case;

      Set_Token_Location (Res);
      Scan_Identifier (Res, "type identifier expected");

      if Get_Kind (Res) = N_Typedef then
         if Current_Token = Tok_Left_Brack then
            Decl_Type := Parse_Variable_Dimension_Rep (Decl_Type);
         end if;
         Set_Type_Node (Res, Decl_Type);
      end if;

      Scan_Or_Error (Tok_Semicolon, "';' expected at end of typedef");

      return Res;
   end Parse_Type_Declaration;

   procedure Parse_Value_Range_Range (Rng : Node; Lsb_Include : Boolean) is
   begin
      Set_Lsb_Include_Flag (Rng, Lsb_Include);

      --  Skip '['/'('.
      Scan;

      Set_Lsb (Rng, Parse_Expression);

      Scan_Or_Error (Tok_Colon, "':' expected between range end points");

      Set_Msb (Rng, Parse_Expression);

      case Current_Token is
         when Tok_Right_Paren =>
            Set_Msb_Include_Flag (Rng, False);

            --  Skip ')'.
            Scan;
         when Tok_Right_Brack =>
            Set_Msb_Include_Flag (Rng, True);

            --  Skip ']'.
            Scan;
         when others =>
            Error_Msg_Parse ("')' or ']' expected at end of value range");
      end case;
   end Parse_Value_Range_Range;

   function Parse_Value_Range return Node
   is
      First, Last : Node;
      Rng : Node;
   begin
      Init_Chain (First, Last);

      loop
         case Current_Token is
            when Tok_From =>
               Rng := Create_Node (N_From_Range);
            when Tok_Exclude =>
               Rng := Create_Node (N_Exclude_Range);
            when others =>
               exit;
         end case;
         Set_Token_Location (Rng);

         --  Skip 'from'/'exclude'.
         Scan;

         case Current_Token is
            when Tok_Left_Paren =>
               Parse_Value_Range_Range (Rng, False);
            when Tok_Left_Brack =>
               Parse_Value_Range_Range (Rng, True);
            when Tok_Tick_Curly =>
               Set_Lsb (Rng, Parse_Expression);
            when others =>
               --  Only exclude ?
               Set_Lsb (Rng, Parse_Expression);
         end case;
         Append_Chain (First, Last, Rng);
      end loop;

      return First;
   end Parse_Value_Range;

   --  1800-2017 6.20.1 Parameter declaration syntax
   --  param_assignment ::=
   --    parameter_identifier { unpacked_dimension }
   --      [ = constant_param_expression ]
   --  AMS:
   --   value_range
   procedure Parse_Param_Assignment
     (Kind : Nkind; Decl_Type : in out Type_Node; Param : out Node) is
   begin
      Param := Create_Node (Kind);
      Set_Token_Location (Param);

      if Current_Token = Tok_Identifier then
         Set_Type_Node (Param, Decl_Type);
         Set_Identifier (Param, Current_Identifier);

         --  Skip identifier
         Scan;
      elsif Decl_Type.Typ /= Null_Node then
         Data_Type_To_Identifier (Param, Decl_Type);
      else
         Error_Msg_Parse ("missing parameter identifier");
      end if;

      --  FIXME: only unpacked dimension.
      Parse_Variable_Dimension_Rep (Param);

      if Current_Token = Tok_Equal then
         --  Skip '='.
         Scan;

         Set_Expression (Param, Parse_Expression);

         if Flag_AMS then
            Set_Value_Range (Param, Parse_Value_Range);
         end if;
      end if;
   end Parse_Param_Assignment;

   --  1800-2017 6.20.1 Parameter declaration syntax
   --  parameter_declaration ::=
   --      PARAMETER data_type_or_implicit list_of_param_assignments
   --    | PARAMETER TYPE list_of_type_assignments
   --
   --  local_parameter_declaration ::=
   --      LOCALPARAM data_type_or_implicit list_of_param_assignments
   --    | LOCALPARAM TYPE list_of_type_assignments
   --
   --  list_of_param_assignments ::= param_assignment { , param_assignment }
   procedure Parse_Parameter_Declaration (Kind : Nkind;
                                          Constr : in out Items_Constr)
   is
      Res : Node;
      Decl_Typ : Type_Node;
   begin
      pragma Assert (Current_Token = Tok_Parameter
                       or Current_Token = Tok_Localparam);

      --  Skip 'parameter'.
      Scan;

      Decl_Typ := Parse_Data_Type_Or_Implicit;

      loop
         Parse_Param_Assignment (Kind, Decl_Typ, Res);

         Append_Node (Constr, Res);

         exit when Current_Token /= Tok_Comma;

         --  Skip ','
         Scan;
      end loop;

      --  Skip ';'.
      Scan_Declaration_Semicolon;
   end Parse_Parameter_Declaration;

   --  1800-2017 6.20.1 Parameter declaration syntax
   --  type_assignment ::= type_identifier [ = data_type ]
   procedure Parse_Type_Assignment (Kind : Nkind; Param : out Node)
   is
      Decl_Typ : Type_Node;
   begin
      Param := Create_Node (Kind);
      Set_Token_Location (Param);

      Scan_Identifier (Param, "type parameter identifier expected");

      if Current_Token = Tok_Equal then
         --  Skip '='.
         Scan;

         Decl_Typ := Parse_Data_Type_Or_Implicit;
         Set_Default_Type_Node (Param, Decl_Typ);
      end if;
   end Parse_Type_Assignment;

   --  1800-2017 13.3 Tasks
   --  lifetime ::= STATIC | AUTOMATIC
   function Parse_Lifetime return Lifetime_State is
   begin
      case Current_Token is
         when Tok_Automatic =>
            --  Skip 'automatic'.
            Scan;

            return (Life_Automatic, True);

         when Tok_Static =>
            --  Skip 'static'.
            Scan;

            return (Life_Static, True);

         when others =>
            return (Life_Static, False);
      end case;
   end Parse_Lifetime;

   --  1800-2012 6.8 Variable Declarations
   --  data_declaration ::=
   --       [ CONST ] [ VAR ] [ lifetime ] data_type_of_implicit
   --         list_of_variable_decl_assignments ;
   --     | ...
   --
   --  1364-2005 3.2.2 Variable declarations
   --  reg_declaration ::=
   --     REG [ SIGNED ] [ range ] list_of_variable_identifiers ;
   procedure Parse_Variable_Declarations (Constr : in out Items_Constr;
                                          Decl_Type : in out Type_Node;
                                          Has_Var : Boolean;
                                          Is_Const : Boolean;
                                          Lifetime : Lifetime_State)
   is
      Local_Constr : Items_Constr;
      N : Node;
   begin
      Init_Constr (Local_Constr, Get_Parent (Constr));
      Parse_List_Of_Variable_Decl_Assignments (Local_Constr, Decl_Type, N_Var);

      N := Get_Constr_Chain (Local_Constr);
      while N /= Null_Node loop
         Set_Is_Const (N, Is_Const);
         Set_Has_Var (N, Has_Var);
         Set_Lifetime (N, Lifetime);

         N := Get_Chain (N);
      end loop;

      Append_Constr (Constr, Local_Constr);

      --  Skip ';'.
      Scan_Declaration_Semicolon;
   end Parse_Variable_Declarations;

   procedure Parse_Const_Opt (Is_Const : out Boolean) is
   begin
      if Current_Token = Tok_Const then
         Is_Const := True;

         --  Skip 'const'.
         Scan;
      else
         Is_Const := False;
      end if;
   end Parse_Const_Opt;

   procedure Parse_Var_Opt (Has_Var : out Boolean) is
   begin
      if Current_Token = Tok_Var then
         Has_Var := True;

         --  Skip 'var'.
         Scan;
      else
         Has_Var := False;
      end if;
   end Parse_Var_Opt;

   procedure Parse_Variable_Declarations (Constr : in out Items_Constr)
   is
      Decl_Type : Type_Node;
      Is_Const : Boolean;
      Has_Var : Boolean;
      Lifetime : Lifetime_State;
   begin
      Parse_Const_Opt (Is_Const);
      Parse_Var_Opt (Has_Var);

      Lifetime := Parse_Lifetime;

      Decl_Type := Parse_Data_Type_Or_Implicit;

      Parse_Variable_Declarations
        (Constr, Decl_Type, Has_Var, Is_Const, Lifetime);
   end Parse_Variable_Declarations;

   --  1800-2017 6.7 Net declarations
   --  net_declaration ::=
   --      net_type [ drive_strength | charge_strength ]
   --        [ VECTORED | SCALARED ] data_type_or_implicit [ delay3 ]
   --        list_of_net_decl_assignments ;
   --    | net_type_identifier [ delay_control ] list_of_net_decl_assignments ;
   --    | ...
   --
   --  net_type ::= SUPPLY0 | SUPPLY1 | TRI | TRIAND | TRIOR | TRIREG
   --    | TRI0 | TRI1 | UWIRE | WIRE | WAND | WOR
   procedure Parse_Net_Declarations (Constr : in out Items_Constr;
                                     Orig_Kind : Nkind)
   is
      Res : Node;
      Decl_Type : Type_Node;
      Dly : Node;
      Strength : Int32;
      Net_Kind : Nkind;
   begin
      Net_Kind := Orig_Kind;

      --  Skip net_type or 'trireg'.
      Scan;

      --  Parse drive_strength / charge_strength.
      if Current_Token = Tok_Left_Paren then
         if Net_Kind = N_Trireg then
            Strength := Parse_Charge_Strength;
         else
            --  Skip '('
            Scan;

            Strength := Parse_Drive_Strength;
         end if;
      else
         --  No strength.
         Strength := 0;
      end if;

      Decl_Type := Parse_Data_Type_Or_Implicit;

      Dly := Null_Node;

      case Current_Token is
         when Tok_Equal
           | Tok_Comma
           | Tok_Semicolon
           | Tok_Left_Brack =>
            --  Oops, there was no type.
            if Orig_Kind = N_Wire and then Strength = 0 then
               --  Use N_Wire_Direct to save space over N_Wire.
               Net_Kind := N_Wire_Direct;
            end if;
            Res := Create_Node (Net_Kind);
            Location_Copy (Res, Decl_Type.Typ);
            Data_Type_To_Identifier (Res, Decl_Type);

         when others =>
            --  Parse delay3.
            if Current_Token = Tok_Sharp then
               Dly := Parse_Delay2_3 (True);
            elsif Strength = 0 and then Orig_Kind = N_Wire then
               --  Use N_Wire_Direct to save space over N_Wire.
               Net_Kind := N_Wire_Direct;
            end if;

            Res := Create_Node (Net_Kind);
            Set_Token_Location (Res);

            --  Skip identifier.
            Scan_Identifier (Res, "net identifier expected");
      end case;

      --  Parse list of net decl.
      loop
         Set_Type_Node (Res, Decl_Type);

         if Dly /= Null_Node or else Strength /= 0 then
            Set_Net_Delay (Res, Dly);
            Set_Net_Drive_Strength (Res, Strength);
         end if;

         Parse_Variable_Dimension_Rep (Res);

         if Current_Token = Tok_Equal then
            --  Skip '='.
            Scan;

            Set_Expression (Res, Parse_Expression);
         end if;

         Append_Node (Constr, Res);

         exit when Current_Token /= Tok_Comma;

         Set_Has_Identifier_List (Res, True);

         --  Skip ','.
         Scan;

         Res := Create_Node (Net_Kind);
         Set_Token_Location (Res);
         Set_Type_Node (Res, Decl_Type);

         --  Skip identifier.
         Scan_Identifier (Res, "net identifier expected");
      end loop;

      --  Skip ';'.
      Scan_Declaration_Semicolon;
   end Parse_Net_Declarations;


   function Data_Type_To_Name_Inner (N : Node) return Node is
   begin
      case Get_Kind (N) is
         when N_Name
           | N_Dotted_Name | N_Scoped_Name
           | N_Bit_Select | N_Part_Select
           | N_Plus_Part_Select
           | N_Minus_Part_Select =>
            --  Already a name.
            return N;

         when N_Packed_Array =>
            --  Convert to Bit_Select / Part_Select.
            declare
               Prefix : constant Node :=
                 Data_Type_To_Name_Inner (Get_Element_Data_Type (N));
               Res : Node;
            begin
               if Get_Lsb (N) = Null_Node then
                  Res := Create_Node (N_Bit_Select);
                  Set_Expression (Res, Get_Msb (N));
               else
                  Res := Create_Node (N_Part_Select);
                  Set_Lsb (Res, Get_Lsb (N));
                  Set_Msb (Res, Get_Msb (N));
               end if;
               Set_Name (Res, Prefix);
               Set_Location (Res, Get_Location (N));
               Free_Node (N);
               return Res;
            end;
         when N_Array | N_Queue =>
            --  Missing identifier or extract it
            raise Program_Error;
         when others =>
            --  Ditto.
            raise Program_Error;
      end case;
   end Data_Type_To_Name_Inner;

   function Data_Type_To_Name (Decl_Type : Type_Node) return Node
   is
      N : Node;
   begin
      pragma Assert (Decl_Type.Own);

      N := Data_Type_To_Name_Inner (Decl_Type.Typ);

      case Get_Kind (N) is
         when N_Name
           | N_Dotted_Name | N_Scoped_Name
           | N_Bit_Select | N_Part_Select
           | N_Plus_Part_Select
           | N_Minus_Part_Select =>
            if Current_Token = Tok_Dot then
               return Parse_Name (N);
            else
               return N;
            end if;

         when others =>
            --  Ditto.
            raise Program_Error;
      end case;
   end Data_Type_To_Name;

   --  precond:  BEGIN/FORK
   --  postcond: END/JOIN
   --
   --  1364-2005 9.8.1 Sequential blocks
   --  seq_block ::=
   --    BEGIN [ : block_identifier { block_item_declaration } ]
   --      { statement } END
   --
   --  1364-2005 9.8.2 Parallel blocks
   --  par_block ::=
   --    FORK [ : block_identifier { block_item_declaration } ]
   --      { statement } JOIN
   procedure Parse_Block (Res : Node)
   is
      Old_Scope : Node;
      First_Stmt : Node;
      First_Decl : Node;
   begin
      Set_Token_Location (Res);

      --  Skip 'begin' or 'fork'.
      Scan;

      if Current_Token = Tok_Colon then
         --  Skip ':'.
         Scan;

         Scan_Identifier (Res, "block identifier expected after ':'");

         if Get_Identifier (Res) /= No_Name_Id then
            --  IEEE1364 2005 12.5 Hierarchical names
            --  The hierarchy of names can be viewed as a tree structure, where
            --  each module instance, generate block instance, task, function
            --  or *named* begin-end or fork-join block defines a new
            --  hierarchical level, or scope, in a particular branch of the
            --  tree.
            --  GHDL: an anonymous block does not create a scope.
            Push_Scope (Res, Old_Scope);
         end if;
      end if;

      Parse_Block_Item_Declaration_Statement (Res, First_Decl, First_Stmt);
      Set_Block_Item_Declaration_Chain (Res, First_Decl);
      Set_Statements_Chain (Res, First_Stmt);

      Parse_End_Name (Res);

      if Get_Identifier (Res) /= Null_Identifier then
         Pop_Scope (Old_Scope);
      end if;
   end Parse_Block;

   --  1800-2017 9.3.1 Sequential blocks
   --  seq_block ::=
   --    BEGIN [ : block_identifier ]
   --      { block_item_declaration } { statement_or_null }
   --    END [ : block_identifier ]
   --
   --  precond:  BEGIN
   --  postcond: next token
   function Parse_Seq_Block return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Seq_Block);
      Parse_Block (Res);

      --  Skip 'end'.
      Scan_Or_Error (Tok_End, "'end' expected at end of sequential block");

      Parse_End_Name (Res);

      return Res;
   end Parse_Seq_Block;

   --  1800-2017 9.3.2 Parallel blocks
   --  par_block ::=
   --    FORK [ : block_identifier ]
   --      { block_item_declaration } { statement_or_null }
   --    join_keyword [ : block_identifier ]
   --
   --  join_keyword ::= JOIN | JOIN_ANY | JOIN_NONE
   --
   --  precond:  FORK
   --  postcond: next token
   function Parse_Par_Block return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Par_Block);
      Parse_Block (Res);
      case Current_Token is
         when Tok_Join =>
            Set_Join_Option (Res, Join_All);

            --  Skip 'join'.
            Scan;

         when Tok_Join_None =>
            Set_Join_Option (Res, Join_None);

            --  Skip 'join_none'.
            Scan;

         when Tok_Join_Any =>
            Set_Join_Option (Res, Join_Any);

            --  Skip 'join_any'.
            Scan;

         when others =>
            Error_Msg_Parse ("'join' expected at end of parallel block");
      end case;

      Parse_End_Name (Res);

      return Res;
   end Parse_Par_Block;

   --  precond:  next token (or ';')
   --  postcond: next token
   --
   --  1800-2017 12.3 Syntax
   --  statement_or_null ::=
   --      statement
   --    | { attribute_instance } ;
   function Parse_Statement_Or_Null (Parent : Node) return Node is
   begin
      if Current_Token = Tok_Semicolon then
         --  Skip ';'.
         Scan;

         return Null_Node;
      else
         return Parse_Statement (Parent);
      end if;
   end Parse_Statement_Or_Null;

   --  precond:  IF
   --  postcond: next token
   function Parse_Conditional_Statement
     (Parent : Node; Violation : Violation_Type) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_If);
      Set_Token_Location (Res);
      Set_Parent (Res, Parent);
      Set_Violation (Res, Violation);

      --  Skip 'if'.
      Scan;

      Set_Condition (Res, Parse_Parenthesis_Expression);

      Set_True_Stmt (Res, Parse_Statement_Or_Null (Res));

      if Current_Token = Tok_Else then
         --  Skip 'else'.
         Scan;

         Set_False_Stmt (Res, Parse_Statement_Or_Null (Res));
      end if;

      return Res;
   end Parse_Conditional_Statement;

   --  precond:  next token
   --  postcond: next token
   --
   --  1800-2017 9.4 Procedural timing control
   function Parse_Event_Expression return Node
   is
      Left : Node;
      Res : Node;
      New_Res : Node;
   begin
      Res := Null_Node;
      loop
         if Current_Token = Tok_Posedge then
            Left := Create_Node (N_Posedge);
            Set_Token_Location (Left);

            --  Skip 'posedge'.
            Scan;

            Set_Expression (Left, Parse_Expression);
         elsif Current_Token = Tok_Negedge then
            Left := Create_Node (N_Negedge);
            Set_Token_Location (Left);

            --  Skip 'negedge'.
            Scan;

            Set_Expression (Left, Parse_Expression);
         else
            Left := Parse_Expression;
         end if;

         if Res = Null_Node then
            Res := Left;
         else
            Set_Right (Res, Left);
         end if;

         case Current_Token is
            when Tok_Or =>
               null;
            when Tok_Comma =>
               if Std < Verilog_2001 then
                  Error_Msg_Parse
                    ("',' can only be used here in verilog 2001 or later");
               end if;
            when others =>
               exit;
         end case;

         New_Res := Create_Node (N_Or);
         Set_Token_Location (New_Res);
         Set_Left (New_Res, Res);
         Res := New_Res;

         --  Skip 'or' or ','.
         Scan;
      end loop;
      return Res;
   end Parse_Event_Expression;

   --  precond:  *
   --  postcond: next token
   function Parse_Implicit_Event return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Implicit_Event);
      Set_Token_Location (Res);

      --  Skip '*'
      Scan;

      return Res;
   end Parse_Implicit_Event;

   --  precond:  AT
   --  postcond: next token
   --
   --  1800-2017 9.4 Procedural timing control
   --  event_control ::=
   --      @ hierachical_event_identifier
   --    | @ ( event_expression )
   --    | @*
   --    | @(*)
   --    | @ ps_or_hierarchical_sequence_identifier
   function Parse_Event_Control return Node
   is
      Loc : Location_Type;
      Res : Node;
      Expr : Node;
   begin
      Res := Create_Node (N_Event_Control);
      Set_Token_Location (Res);

      --  Skip '@'.
      Scan;

      case Current_Token is
         when Tok_Left_Paren =>
            Loc := Get_Token_Location;

            --  Skip '('.
            Scan;

            if Current_Token = Tok_Star then
               Expr := Parse_Implicit_Event;
            else
               Expr := Parse_Event_Expression;

               if Flag_Keep_Parentheses then
                  declare
                     Paren : Node;
                  begin
                     Paren := Create_Node (N_Parenthesis_Expr);
                     Set_Location (Paren, Loc);
                     Set_Expression (Paren, Expr);
                     Expr := Paren;
                  end;
               end if;
            end if;

            --  Skip ')'.
            Scan_Or_Error (Tok_Right_Paren,
                           "')' expected at end of event control");

         when Tok_Identifier =>
            Expr := Parse_Lvalue;
         when Tok_Star =>
            Expr := Parse_Implicit_Event;
         when Tok_Paren_Star =>
            Expr := Parse_Implicit_Event;
            Scan_Or_Error (Tok_Right_Paren,
                           "')' expected after implicit event");
         when others =>
            Error_Msg_Parse ("event expression expected");
            Expr := Null_Node;
      end case;

      Set_Expression (Res, Expr);

      return Res;
   end Parse_Event_Control;

   --  1800-2017 9.4 Procedural timing control
   --  delay_control ::=
   --      # delay_value
   --    | # ( mintypmax_expression )
   function Parse_Delay_Control return Node
   is
      Res : Node;
      Expr : Node;
   begin
      Res := Create_Node (N_Delay_Control);
      Set_Token_Location (Res);
      Scan;
      if Current_Token = Tok_Left_Paren then
         Expr := Parse_Expression;
      else
         Expr := Parse_Delay_Value;
      end if;
      Set_Expression (Res, Expr);
      return Res;
   end Parse_Delay_Control;

   --  1800-2017 9.4 Procedural timing control
   --  14.11 Cycle delay: ##
   --  cycle_delay ::=
   --      ## integral_number
   --    | ## identifier
   --    | ## ( expression )
   function Parse_Cycle_Delay return Node
   is
      Res : Node;
      Expr : Node;
   begin
      Res := Create_Node (N_Cycle_Delay);
      Set_Token_Location (Res);

      --  Skip '##'.
      Scan;

      case Current_Token is
         when Tok_Left_Paren =>
            Expr := Parse_Expression;
         when Tok_Identifier =>
            Expr := Scan_Name;
         when Tok_Dec_Number =>
            Expr := Parse_Unsigned_Number;
         when others =>
            Error_Msg_Parse ("cycle delay value expected");
      end case;

      Set_Expression (Res, Expr);
      return Res;
   end Parse_Cycle_Delay;

   --  1800-2017 9.4 Procedural timing control
   --  delay_or_event_control ::=
   --      delay_control
   --    | event_control
   --    | REPEAT ( expression ) event_control
   function Parse_Delay_Or_Event_Control return Node is
   begin
      case Current_Token is
         when Tok_At =>
            return Parse_Event_Control;
         when Tok_Sharp =>
            return Parse_Delay_Control;
         when Tok_Repeat =>
            declare
               Res : Node;
            begin
               Res := Create_Node (N_Repeat_Control);
               Set_Token_Location (Res);

               --  Skip 'repeat'.
               Scan;

               --  Skip '('.
               Scan_Or_Error (Tok_Left_Paren, "'(' required after 'repeat'");

               Set_Expression (Res, Parse_Expression);

               --  Skip ')'.
               Scan_Or_Error
                 (Tok_Right_Paren, "')' required after repeat expression");

               if Current_Token = Tok_At then
                  Set_Control (Res, Parse_Event_Control);
               end if;
               return Res;
            end;
         when others =>
            return Null_Node;
      end case;
   end Parse_Delay_Or_Event_Control;

   --  1800-2017 10.4.2 Nonblocking procedural assignments
   --  nonblocking_assignment ::=
   --    variable_lvalue <= [ delay_or_event_control ] expression
   function Parse_Non_Blocking_Assignment (Lvalue : Node) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Noblk_Assign);
      Set_Token_Location (Res);

      Set_Lvalue (Res, Lvalue);

      --  Skip '<='.
      Scan;

      Set_Control (Res, Parse_Delay_Or_Event_Control);

      Set_Expression (Res, Parse_Expression);

      return Res;
   end Parse_Non_Blocking_Assignment;

   --  1800-2017 10.4.1 Blocking procedural assignments
   --  blocking_assignment ::=
   --      variable_lvalue = delay_or_event_control expression
   --    | nonrange_variable_lvalue = dynamic_array_new
   --    | [ implicit_class_handle . | class_scope | package_scope ]
   --      hierarchical_variable_identifier select = class_new
   --    | operator_assignment
   --
   --  precond: '='
   function Parse_Blocking_Assignment (Lvalue : Node) return Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Control : Node;
      Expr : Node;
      Res : Node;
   begin
      --  Skip '='.
      Scan;

      Control := Parse_Delay_Or_Event_Control;
      Expr := Parse_Expression;

      if Get_Kind (Lvalue) in Nkinds_Streaming then
         if Expr /= Null_Node and then Get_Kind (Expr) in Nkinds_Streaming then
            Res := Create_Node (N_Pack_Unpack_Assign);
         else
            Res := Create_Node (N_Unpack_Assign);
         end if;
      else
         if Expr /= Null_Node and then Get_Kind (Expr) in Nkinds_Streaming then
            Res := Create_Node (N_Pack_Assign);
         else
            Res := Create_Node (N_Blocking_Assign);
         end if;
      end if;

      Set_Location (Res, Loc);
      Set_Lvalue (Res, Lvalue);
      Set_Control (Res, Control);
      Set_Expression (Res, Expr);

      return Res;
   end Parse_Blocking_Assignment;

   --  1800-2017 10.4.1 Blcoking procedural assignments
   --  operator_assignment ::=
   --    variable_lvalue assignment_operator expression
   --
   --  assignment_operators ::=
   --    = | += | -= | *= | /= | %= | &= | |= | ^= | <<= | >>= | <<<= | >>>=
   function Parse_Assignment_Operator (Lvalue : Node; Op : Binary_Ops)
                                      return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Assign_Operator);
      Set_Token_Location (Res);
      Set_Lvalue (Res, Lvalue);
      Set_Binary_Op (Res, Op);

      --  Skip assign operator.
      Scan;

      Set_Expression (Res, Parse_Expression);

      return Res;
   end Parse_Assignment_Operator;

   --  Verilog-AMS 2.4.0 5.6 Contribution statements
   --  contribution_statement ::= branch_lvalue <+ analog_expression ;
   function Parse_Contribution_Statement (Lval : Node) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Contribution);
      Set_Token_Location (Res);

      Set_Lvalue (Res, Lval);

      --  Skip '<+'.
      Scan;

      Set_Expression (Res, Parse_Expression);

      return Res;
   end Parse_Contribution_Statement;

   --  Handle the common part between case_item and case_generate_item, ie:
   --
   --      expression { , expression } :
   --    | default [ : ]
   procedure Parse_Case_Item
     (First_Item : in out Node; Last_Item : in out Node)
   is
      Item : Node;
   begin
      if Current_Token = Tok_Default then
         Item := Create_Node (N_Default_Case_Item);
         Set_Token_Location (Item);

         --  Skip 'default'.
         Scan;

         if Current_Token = Tok_Colon then
            --  Skip ':'.
            Scan;
         end if;
         --  FIXME: verilog 95/2001 ?
         --  Error_Msg_Parse ("missing ':' after 'default'");
         Append_Chain (First_Item, Last_Item, Item);
      else
         loop
            Item := Create_Node (N_Case_Item);
            Set_Token_Location (Item);
            Set_Expression (Item, Parse_Expression);
            Append_Chain (First_Item, Last_Item, Item);
            exit when Current_Token /= Tok_Comma;

            --  Skip ','
            Scan;

            Set_Same_Case_Flag (Item, True);
         end loop;
         if Current_Token /= Tok_Colon then
            Error_Msg_Parse ("missing ':' after case item expression");
         else
            --  Skip ':'.
            Scan;
         end if;
      end if;
   end Parse_Case_Item;

   --  precond:  CASE, CASEX or CASEZ
   --  postcond: next token
   --
   --  1364-2005 9.5 Case statement
   --  case_statement ::=
   --      CASE ( expression )
   --        case_item { case_item } ENDCASE
   --    | CASEZ ( expression )
   --        case_item { case_item } ENDCASE
   --    | CASEX ( expression )
   --        case_item { case_item } ENDCASE
   --
   --  case_item ::=
   --      expression { , expression } : statement_or_null
   --    | default [ : ] statement_or_null
   function Parse_Case_Statement
     (Kind : Nkind; Attrs : Node; Violation : Violation_Type) return Node
   is
      use Std_Names;
      Res : Node;
      First_Item, Last_Item : Node;
   begin
      Res := Create_Node (Kind);
      Set_Token_Location (Res);
      Set_Violation (Res, Violation);

      --  Skip 'case', 'casex' or 'casez'.
      Scan;

      Set_Expression (Res, Parse_Parenthesis_Expression);

      --  Handle pragma comments
      if Current_Token = Tok_Pragma_Comment then
         loop
            Scan;
            exit when Current_Token = Tok_Pragma_End_Comment;
            if Current_Token = Tok_Identifier then
               case Current_Identifier is
                  when Name_Full_Case =>
                     Set_Attribute_Full (Res, True);

                  when Name_Parallel_Case =>
                     Set_Attribute_Parallel (Res, True);

                  when others =>
                     Warning_Msg_Parse
                       ("ignored pragma %i", (1 => +Current_Identifier));
               end case;
            else
               Error_Msg_Parse ("non-identifier in pragma comment");
            end if;
         end loop;

         --  Skip end comment.
         Scan;
      end if;

      --  Parse case items.
      First_Item := Null_Node;
      Last_Item := Null_Node;
      loop
         exit when Current_Token = Tok_Endcase;
         exit when Current_Token = Tok_End;
         Parse_Case_Item (First_Item, Last_Item);
         Set_Statement (Last_Item, Parse_Statement_Or_Null (Res));
      end loop;

      Set_Case_Items (Res, First_Item);
      if Last_Item = Null_Node then
         Error_Msg_Parse ("at least one case item is required");
      end if;

      if Current_Token /= Tok_Endcase then
         Error_Msg_Parse ("'endcase' expected");
      else
         Scan;
      end if;

      --  Check attributes.
      declare
         Attr : Node;
         Has_Others : Boolean;
      begin
         Has_Others := False;
         Attr := Attrs;
         while Attr /= Null_Node loop
            case Get_Identifier (Attr) is
               when Name_Full_Case =>
                  if Get_Expression (Attr) = Null_Node then
                     Set_Attribute_Full (Res, True);
                  else
                     Has_Others := True;
                  end if;
               when Name_Parallel_Case =>
                  if Get_Expression (Attr) = Null_Node then
                     Set_Attribute_Parallel (Res, True);
                  else
                     Has_Others := True;
                  end if;
               when others =>
                  Has_Others := True;
            end case;
            if Has_Others then
               Set_Other_Attributes (Res, True);
               exit;
            end if;
            Attr := Get_Chain (Attr);
         end loop;
      end;

      return Res;
   end Parse_Case_Statement;

   function Parse_Variable_Assignment (Lvalue : Node) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Blocking_Assign);
      Set_Token_Location (Res);
      Set_Lvalue (Res, Lvalue);

      Scan_Or_Error (Tok_Equal, "'=' required after lvalue in assignment");
      Set_Expression (Res, Parse_Expression);
      return Res;
   end Parse_Variable_Assignment;

   --  1800-2017 12.7 Loop statements
   --  for_step ::=
   --    for_step_assignment { , for_step_assignment }
   --
   --  for_step_assignment ::=
   --      operator_assignment
   --    | inc_or_dec_expression
   --    | function_subroutine_call
   function Parse_For_Step return Node
   is
      First, Last : Node;
      Res : Node;
      Lvalue : Node;
   begin
      Init_Chain (First, Last);

      loop
         case Current_Token is
            when Tok_Plus_Plus =>
               Res := Parse_Pre_Inc_Or_Dec (N_Pre_Increment);
            when Tok_Minus_Minus =>
               Res := Parse_Pre_Inc_Or_Dec (N_Pre_Decrement);
            when Tok_Left_Curly =>
               Lvalue := Parse_Concatenation;
               Res := Parse_Statement_Name (Lvalue);
            when Tok_Identifier =>
               Lvalue := Parse_Lvalue;
               Res := Parse_Statement_Name (Lvalue);
            when others =>
               Error_Msg_Parse ("step assignment expected");
               exit;
         end case;
         Append_Chain (First, Last, Res);
         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
      end loop;
      return First;
   end Parse_For_Step;

   --  1800-2017 27.3 Generate construct syntax
   --  genvar_initialization ::=
   --    [ GENVAR ] genvar_identifier = constant_expression
   function Parse_Genvar_Initialization (Parent : Node) return Node
   is
      Res : Node;
   begin
      pragma Assert (Current_Token = Tok_Genvar);

      Res := Create_Node (N_Genvar);
      Set_Token_Location (Res);
      Set_Parent (Res, Parent);

      --  Eat 'genvar'.
      Scan;

      --  Scan identifier.
      Scan_Identifier (Res, "identifier expected after genvar");

      if Current_Token = Tok_Equal then
         --  Eat '='.
         Scan;

         Set_Expression (Res, Parse_Expression);
      else
         Error_Msg_Parse ("missing '=', genvar must be initialized");
      end if;

      return Res;
   end Parse_Genvar_Initialization;

   --  1800-2017 12.7 Loop statements
   --  loop_statement ::=
   --      [...]
   --    | FOR ( [ for_initialization ] ; [ expression ] ; [ for_step ] )
   --        statement_or_null
   --
   --  for_initialization ::=
   --      list_of_variable_assignments
   --    | for_variable_declaration { , for_variable_declaration }
   --
   --  for_variable_declaration ::=
   --    [ VAR ] data_type variable_identifier = expression
   --      { , variable_identifier = expression }
   --
   --  list_of_variable_assignments ::=
   --    variable_assignment { , variable_assignment }
   function Parse_For_Initialization (Parent : Node) return Node
   is
      Constr : Items_Constr;
      Decl_Type : Type_Node;
      Name : Node;
      Stmt : Node;
   begin
      Init_Constr (Constr, Parent);

      case Current_Token is
         when Toks_Verilog_Types | Toks_SV30_Types | Toks_SV31_Types
           | Tok_Var =>
            Decl_Type := Parse_Data_Type_Or_Implicit;
            Parse_List_Of_Variable_Decl_Assignments (Constr, Decl_Type, N_Var);
         when Tok_Identifier =>
            Decl_Type := Parse_Data_Type_Or_Implicit;
            if Current_Token = Tok_Identifier then
               Parse_List_Of_Variable_Decl_Assignments
                 (Constr, Decl_Type, N_Var);
            else
               Name := Data_Type_To_Name (Decl_Type);
               loop
                  Stmt := Parse_Variable_Assignment (Name);
                  if Stmt /= Null_Node then
                     Append_Node (Constr, Stmt);
                  end if;
                  exit when Current_Token /= Tok_Comma;
                  Name := Parse_Lvalue;
               end loop;
            end if;
         when Tok_Genvar =>
            return Parse_Genvar_Initialization (Parent);
         when Tok_Semicolon =>
            null;
         when others =>
            Error_Msg_Parse ("variable assignment or declaration expected");
            Skip_Until_Semicolon;
      end case;
      return Get_Constr_Chain (Constr);
   end Parse_For_Initialization;

   --  precond:  FOR
   --  postcond: next token after ')'
   --
   --  1364-2005 9.6 Looping statements
   --  FOR ( variable_assignment ; expression ; variable_assignment )
   --
   --  1800-2017 12.7 Loop statements
   --  loop_statement ::=
   --      [...]
   --    | FOR ( [ for_initialization ] ; [ expression ] ; [ for_step ] )
   --        statement_or_null
   --
   --  for_initialization ::=
   --      list_of_variable_asignments
   --    | for_variable_declaration { , for_variable_declaration }
   --
   --  for_variable_declaration ::=
   --    [ VAR ] data_type variable_identifier = expression
   --      { , variable_identifier = expression }
   procedure Parse_For (N : Node) is
   begin
      --  Skip 'for'
      Scan;

      --  Skip '('.
      Scan_Or_Error (Tok_Left_Paren, "'(' required after 'for'");

      Set_For_Initialization (N, Parse_For_Initialization (N));

      --  Skip ';'.
      Scan_Or_Error (Tok_Semicolon, "';' required after initial assignment");

      Set_Condition (N, Parse_Expression);

      --  Skip ';'.
      Scan_Or_Error (Tok_Semicolon, "';' required after for expression");

      if Current_Token /= Tok_Right_Paren then
         Set_Step_Assign (N, Parse_For_Step);
      end if;

      --  Skip ')'.
      Scan_Or_Error
        (Tok_Right_Paren, "')' required after iteration assignment");
   end Parse_For;

   --  precond:  FOR
   --  postcond: next token
   --
   --  1800-2007 12.7.2 The for-loop
   --  loop_statement ::=
   --      for ( [ for_initialization ] ; [ expression ] ; [ for_step )
   --        statement_or_null
   --    | ...
   --
   --  1364-2005 9.6 Looping statements
   --  FOR ( variable_assignment ; expression ; variable_assignment )
   --    statement
   function Parse_For_Statement return Node
   is
      Res : Node;
      Stmt : Node;
   begin
      Res := Create_Node (N_For);
      Set_Token_Location (Res);

      Parse_For (Res);

      Stmt := Parse_Statement_Or_Null (Res);
      Set_Statement (Res, Stmt);

      return Res;
   end Parse_For_Statement;

   --  precond:  WHILE/REPEAT
   --  postcond: next token
   --  1800-2017 12.7 Loop statements
   --  loop_statement ::=
   --       ...
   --    |  REPEAT ( expression ) statement_or_null
   --    |  WHILE ( expression ) statement_or_null
   --    |  ...
   function Parse_While_Repeat_Statement return Node
   is
      Res : Node;
   begin
      case Current_Token is
         when Tok_While =>
            Res := Create_Node (N_While);
         when Tok_Repeat =>
            Res := Create_Node (N_Repeat);
         when others =>
            raise Internal_Error;
      end case;
      Set_Token_Location (Res);

      --  Skip 'while'/'repeat'.
      Scan;

      Set_Condition (Res, Parse_Parenthesis_Expression);

      Set_Statement (Res, Parse_Statement_Or_Null (Res));
      return Res;
   end Parse_While_Repeat_Statement;

   --  1800-2017 12.7 Loop statements
   --  Parse:
   --    FOREACH ( ps_or_hierarchical_array_identifier '[' loop_variables ']' )
   --
   --  loop_variables ::=
   --    [ index_variable_identifier ] { , [ index_variable_identifier ] }
   function Parse_Foreach (Kind : Nkind) return Node
   is
      Res : Node;
      First_Var, Last_Var : Node;
      Var : Node;
      Idx, V : Node;
   begin
      Res := Create_Node (Kind);
      Set_Token_Location (Res);

      --  Skip 'foreach'.
      Scan;

      Scan_Or_Error (Tok_Left_Paren, "'(' expected after foreach");

      --  TODO: handle hierarchical name...
      Var := Parse_Lvalue;
      if Var /= Null_Node then
         --  TODO: support multiple variables, support no variables
         Set_Foreach_Array (Res, Var);

         --  Extract loop variables.
         if Get_Kind (Var) /= N_Bit_Select then
            Error_Msg_Parse ("missing loop variables in foreach array");
         else
            Set_Foreach_Array (Res, Get_Name (Var));
            Idx := Get_Expression (Var);
            if Get_Kind (Idx) /= N_Name then
               Error_Msg_Sem (+Idx, "index variable must be an identifier");
            else
               V := Create_Node (N_Foreach_Variable);
               Set_Location (V, Get_Location (Idx));
               Set_Parent (V, Res);
               Set_Identifier (V, Get_Identifier (Idx));
               Set_Foreach_Variables (Res, V);
               Free_Node (Idx);
            end if;
            Free_Node (Var);
         end if;
      end if;

      if False then
         if Current_Token /= Tok_Right_Brack then
            Init_Chain (First_Var, Last_Var);
            loop
               Var := Create_Node (N_Foreach_Variable);
               Set_Token_Location (Var);

               Scan_Identifier (Var, "index variable identifier expected");
               exit when Current_Token /= Tok_Comma;

               --  Skip ','.
               Scan;
            end loop;
         end if;
      end if;

      Scan_Or_Error (Tok_Right_Paren, "')' expected after expression");
      return Res;
   end Parse_Foreach;

   --  1800-2017 12.7 Loop statements
   --  loop_statement ::=
   --      ...
   --    | FOREACH ( ps_or_hierarchical_array_identifier
   --                  '[' loop_variables ']' )
   --        statement
   --    | ...
   --
   --  loop_variables ::=
   --    [ index_variable_identifier ] { , [ index_variable_identifier ] }
   function Parse_Foreach_Statement return Node
   is
      Res : Node;
      Stmt : Node;
   begin
      Res := Parse_Foreach (N_Foreach);

      Stmt := Parse_Statement (Res);
      Set_Statement (Res, Stmt);

      return Res;
   end Parse_Foreach_Statement;

   --  1800-2017 12.7 Loop statements
   --  loop_statement ::=
   --      ...
   --    | DO statement_or_null WHILE ( expression ) ;
   --    | ...
   function Parse_Do_While_Statement return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Do_While);
      Set_Token_Location (Res);

      --  Skip 'do'.
      Scan;

      Set_Statement (Res, Parse_Statement_Or_Null (Res));

      Scan_Or_Error (Tok_While, "'while' expected after statement");

      Set_Condition (Res, Parse_Parenthesis_Expression);

      return Res;
   end Parse_Do_While_Statement;

   --  precond:  FOREVER
   --  postcond: next token
   function Parse_Forever_Statement return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Forever);
      Set_Token_Location (Res);

      --  Skip 'forever'.
      Scan;

      Set_Statement (Res, Parse_Statement (Res));

      return Res;
   end Parse_Forever_Statement;

   --  precond:  ASSIGN
   --  postcond: next token
   --
   --  1800-2017 10.6 Procedural continuous assignments
   --  procedure_continuous_assignment ::=
   --      ASSIGN variable_assignment
   --    | DEASSIGN variable_lvalue
   --    | FORCE variable_assignment
   --    | FORCE net_assignment
   --    | RELEASE variable_lvalue
   --    | RELEASE net_lvalue
   --
   --  variable_assignment ::= variable lvalue = expression
   --  net_assignment ::= net_lvalue = expression
   function Parse_Procedural_Continuous_Assignment (Kind : Nkind) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (Kind);
      Set_Token_Location (Res);

      --  Skip 'assign'.
      Scan;

      Set_Lvalue (Res, Parse_Lvalue);
      case Kind is
         when N_Proc_Assign
           | N_Force_Assign =>
            if Current_Token /= Tok_Equal then
               Error_Msg_Parse ("'=' required after lvalue in assignment");
            else
               Scan;
            end if;
            Set_Expression (Res, Parse_Expression);
         when N_Proc_Deassign
           | N_Release =>
            null;
         when others =>
            raise Internal_Error;
      end case;

      --  Skip ';'.
      Scan_Statement_Semicolon;

      return Res;
   end Parse_Procedural_Continuous_Assignment;

   --  precond: '('.
   --  postcond: next token.
   --
   --  1800-2017
   --  list_of_arguments ::=
   --      [ expression ] { , [ expression ] }
   --        { , . identifier ( [ expression ] ) }
   --    | . identifier ( [ expression ] )
   --        { , . identifier ( [ expression ] ) }
   function Parse_Arguments (Allow_Blank : Boolean) return Node
   is
      pragma Unreferenced (Allow_Blank);
      First : Node;
      Last : Node;
      Arg : Node;
      Expr : Node;
      Name : Node;
   begin
      --  Skip '('.
      Scan;

      First := Null_Node;
      Last := Null_Node;

      if Current_Token /= Tok_Right_Paren then
         loop
            Arg := Create_Node (N_Argument);
            Set_Token_Location (Arg);
            case Current_Token is
               when Tok_Comma | Tok_Right_Paren =>
                  Expr := Null_Node;
               when Tok_Dot =>
                  --  Skip '.'
                  Scan;

                  Name := Create_Node (N_Name);
                  Set_Token_Location (Name);
                  Scan_Identifier (Name, "missing identifier after '.'");
                  Set_Port (Arg, Name);

                  Scan_Or_Error
                    (Tok_Left_Paren, "'(' expected after identifier");

                  if Current_Token /= Tok_Right_Paren then
                     Expr := Parse_Expression;
                  else
                     Expr := Null_Node;
                  end if;

                  Scan_Or_Error
                    (Tok_Right_Paren, "')' expected after expression");

               when others =>
                  Expr := Parse_Expression;
            end case;
            Set_Expression (Arg, Expr);
            if First = Null_Node then
               First := Arg;
            else
               Set_Chain (Last, Arg);
            end if;
            Last := Arg;
            exit when Current_Token /= Tok_Comma;

            --  Skip ','.
            Scan;
         end loop;
      end if;

      --  Skip ')'.
      Scan_Or_Error (Tok_Right_Paren, "')' expected at end of arguments");

      return First;
   end Parse_Arguments;

   --  1800-2017 9.4.3 Level-sensitive event control
   --  wait_statement ::=
   --      WAIT ( expression ) statement_or_null
   --    | WAIT FORK ;
   --    | WAIT_ORDER ( hierarchical_identifier
   --        { , hierarchical_identifier } ) action_block
   function Parse_Wait_Statement return Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Res : Node;
   begin
      --  Skip 'wait'.
      Scan;

      if Current_Token = Tok_Fork then
         --  Skip 'fork'.
         Scan;

         Res := Create_Node (N_Wait_Fork);

         --  Skip ';'.
         Scan_Statement_Semicolon;
      else
         Res := Create_Node (N_Wait);
         Set_Condition (Res, Parse_Parenthesis_Expression);

         Set_Statement (Res, Parse_Statement_Or_Null (Res));
      end if;

      Set_Location (Res, Loc);

      return Res;
   end Parse_Wait_Statement;

   function Parse_Event_Trigger return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Trigger);
      Set_Token_Location (Res);

      --  Skip '->'.
      Scan;

      if Current_Token /= Tok_Identifier then
         Error_Msg_Parse ("event identifier expected");
      end if;
      Set_Event (Res, Parse_Lvalue);

      --  Skip ';'.
      Scan_Statement_Semicolon;

      return Res;
   end Parse_Event_Trigger;

   --  1800-2017 9.6 Process control
   --  disable_statement ::=
   --      DISABLE hierarchical_task_identifier ;
   --    | DISABLE hierarchical_block_identifier ;
   --    | DISABLE FORK ;
   function Parse_Disable_Statement return Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Res : Node;
   begin
      --  Skip 'disable'.
      Scan;

      if Current_Token = Tok_Fork then
         Res := Create_Node (N_Disable_Fork);

         --  Skip 'join'.
         Scan;
      else
         Res := Create_Node (N_Disable);

         if Current_Token /= Tok_Identifier then
            Error_Msg_Parse ("block or task identifier expected");
         end if;
         Set_Statement (Res, Parse_Lvalue);
      end if;

      Set_Location (Res, Loc);

      --  Skip ';'.
      Scan_Statement_Semicolon;

      return Res;
   end Parse_Disable_Statement;

   function Parse_Subroutine_Call_Stmt return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Subroutine_Call_Stmt);
      Set_Token_Location (Res);

      if Current_Token = Tok_Void then
         --  Skip 'void'.
         Scan;

         Scan_Or_Error (Tok_Tick, "single quote expected after void");
         Scan_Or_Error (Tok_Left_Paren, "'(' expected for cast");

         Set_Has_Void_Cast (Res, True);

         Set_Call (Res, Parse_Expression);

         Scan_Or_Error (Tok_Right_Paren, "')' expected at end of cast");
      else
         Set_Call (Res, Parse_Expression);
      end if;

      --  Skip ';'.
      Scan_Statement_Semicolon;

      return Res;
   end Parse_Subroutine_Call_Stmt;

   --  1800-2017 9.3.5 Statement labels
   function Parse_Statement_Label (Name : Node) return Node
   is
      Stmt : Node;
      Label : Name_Id;
      Sub : Node;
   begin
      if Get_Kind (Name) /= N_Name then
         Error_Msg_Parse ("statemenent label must be an identifier");
         Label := Null_Identifier;
      else
         Label := Get_Identifier (Name);
      end if;

      Stmt := Create_Node (N_Label_Stmt);
      Location_Copy (Stmt, Name);
      Set_Identifier (Stmt, Label);

      Free_Node (Name);

      --  Skip ':'.
      Scan;

      Sub := Parse_Statement (Stmt);
      if Get_Kind (Sub) = N_Label_Stmt then
         Error_Msg_Sem (+Stmt, "double label not allowed");
         Sub := Get_Statement (Sub);
      end if;
      Set_Statements_Chain (Stmt, Sub);

      return Stmt;
   end Parse_Statement_Label;

   function Parse_Statement_Name (Name : Node) return Node
   is
      Stmt : Node;
   begin
      case Current_Token is
         when Tok_Less_Equal =>
            Stmt := Parse_Non_Blocking_Assignment (Name);

         when Tok_Equal =>
            Stmt := Parse_Blocking_Assignment (Name);

         when Tok_Less_Plus =>
            Stmt := Parse_Contribution_Statement (Name);

         when Tok_Plus_Plus =>
            Stmt := Parse_Post_Inc_Or_Dec (N_Post_Increment, Name);
         when Tok_Minus_Minus =>
            Stmt := Parse_Post_Inc_Or_Dec (N_Post_Decrement, Name);

         when Tok_Plus_Asgn =>
            Stmt := Parse_Assignment_Operator (Name, Binop_Add);
         when Tok_Minus_Asgn =>
            Stmt := Parse_Assignment_Operator (Name, Binop_Sub);

         when Tok_Shl_Asgn =>
            Stmt := Parse_Assignment_Operator (Name, Binop_Left_Lshift);
         when Tok_Shr_Asgn =>
            Stmt := Parse_Assignment_Operator (Name, Binop_Right_Lshift);

         when Tok_And_Asgn =>
            Stmt := Parse_Assignment_Operator (Name, Binop_Bit_And);
         when Tok_Or_Asgn =>
            Stmt := Parse_Assignment_Operator (Name, Binop_Bit_Or);
         when Tok_Xor_Asgn =>
            Stmt := Parse_Assignment_Operator (Name, Binop_Bit_Xor);

         when Tok_Left_Paren
           | Tok_Semicolon =>
            declare
               Call : Node;
            begin
               Stmt := Create_Node (N_Subroutine_Call_Stmt);
               Set_Location (Stmt, Get_Location (Name));
               if Current_Token = Tok_Left_Paren
                 or else Get_Kind (Name) not in Nkinds_Call
               then
                  Call := Parse_Tf_Call (Name);
                  if Current_Token = Tok_Less_Plus then
                     --  AMS
                     Free_Node (Stmt);
                     Stmt := Parse_Contribution_Statement (Call);
                  else
                     Set_Call (Stmt, Call);
                  end if;
               else
                  Set_Call (Stmt, Name);
               end if;
            end;

         when Tok_Colon =>
            --  A label.
            Stmt := Parse_Statement_Label (Name);

         when others =>
            if Get_Kind (Name) = N_Call then
               Error_Msg_Parse ("missing semicolon after call");
            else
               Error_Msg_Parse ("assignment expected");
            end if;
            return Null_Node;
      end case;

      return Stmt;
   end Parse_Statement_Name;

   function Parse_Assignment return Node
   is
      Lvalue : Node;
      Res : Node;
   begin
      case Current_Token is
         when Tok_Identifier
           | Tok_This
           | Tok_Super =>
            Lvalue := Parse_Lvalue;
         when Tok_Left_Curly =>
            Lvalue := Parse_Concatenation;
         when others =>
            raise Internal_Error;
      end case;

      Res := Parse_Statement_Name (Lvalue);

      if Res /= Null_Node and then Get_Kind (Res) /= N_Label_Stmt then
         --  Skip ';'.
         Scan_Statement_Semicolon;
      end if;

      return Res;
   end Parse_Assignment;

   --  1800-2017 16 Assertions
   --  action_block ::=
   --      statement_or_null
   --    | [ statement ] ELSE statement_or_null
   procedure Parse_Action_Block (Stmt : Node) is
   begin
      --  Parse action_block
      if Current_Token /= Tok_Else then
         Set_Pass_Stmt (Stmt, Parse_Statement_Or_Null (Stmt));
      end if;
      if Current_Token = Tok_Else then
         --  Skip 'else'.
         Scan;

         Set_Else_Stmt (Stmt, Parse_Statement_Or_Null (Stmt));
      end if;
   end Parse_Action_Block;

   --  1800-2017 16 Assertions
   --  simple_immediate_assert_statement ::=
   --    ASSERT ( expression ) action_block
   function Parse_Assertion_Statement return Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Stmt : Node;
   begin
      --  Skip 'assert'.
      Scan;

      Stmt := Create_Node (N_Simple_Immediate_Assert);
      Set_Location (Stmt, Loc);

      Set_Condition (Stmt, Parse_Parenthesis_Expression);

      Parse_Action_Block (Stmt);

      return Stmt;
   end Parse_Assertion_Statement;

   function Parse_Unique_Priority
     (Parent : Node; Attrs : Node; Violation : Violation_Type) return Node is
   begin
      --  Skip violation.
      Scan;

      case Current_Token is
         when Tok_Unique | Tok_Unique0 | Tok_Priority =>
            Error_Msg_Parse ("only one unique/priority indicator is allowed");
            return Parse_Unique_Priority (Parent, Attrs, Violation);
         when Tok_If =>
            return Parse_Conditional_Statement (Parent, Violation);
         when Tok_Case =>
            return Parse_Case_Statement (N_Case, Attrs, Violation);
         when Tok_Casex =>
            return Parse_Case_Statement (N_Casex, Attrs, Violation);
         when Tok_Casez =>
            return Parse_Case_Statement (N_Casez, Attrs, Violation);
         when others =>
            Error_Msg_Parse
              ("if or case statement expected after unique/priority");
            return Null_Node;
      end case;
   end Parse_Unique_Priority;

   procedure Apply_Attributes (Attrs : Node; Item : Node)
   is
      Attr : Node;
      Last_Attr : Node;
      Attr_Parent : Node;
   begin
      if Attrs = Null_Node then
         return;
      end if;

      --  So ITEM has attributes.
      Set_Has_Attribute (Item, True);

      --  Set the parent of each attribute.
      Attr := Attrs;
      loop
         Set_Attribute_Item (Attr, Item);
         Last_Attr := Attr;
         Attr := Get_Chain (Attr);
         exit when Attr = Null_Node;
      end loop;

      --  Insert attributes.
      Attr_Parent := Sem_Utils.Get_Attribute_Parent (Item);
      Set_Chain (Last_Attr, Get_Attributes_Chain (Attr_Parent));
      Set_Attributes_Chain (Attr_Parent, Attrs);
   end Apply_Attributes;

   --  precond:  next token
   --  postcond: next token
   --
   --  1800-2017 12 Procedural programming statements
   --  statement_item ::=
   --      blocking_assignment ;
   --    | nonblocking_assignment ;
   --    | procedural_continuous_assignment ;
   --    | case_statement
   --    | conditional_statement
   --    | inc_or_dec_expression ;
   --    | subroutine_call_statement
   --    | disable_statement
   --    | event_trigger
   --    | loop_statement
   --    | jump_statement
   --    | par_block
   --    | procedural_timing_control_statement
   --    | seq_block
   --    | wait_statenement
   --    | procedural_assertion_statement
   --    | clocking_drive ;
   --    | randsequence_statement
   --    | randcase_statement
   --    | expect_property_statement
   --
   --  1800-2017 9.4 Procedure timing controls
   --  procedural_timing_control_statement ::=
   --    procedure_timing_control statement_or_null
   function Parse_Statement (Parent : Node) return Node
   is
      Stmt : Node;
      Attrs : Node;
   begin
      Attrs := Parse_Attribute_Instances_Rep;

      case Current_Token is
         when Tok_Begin =>
            Stmt := Parse_Seq_Block;
         when Tok_Unique =>
            Stmt := Parse_Unique_Priority (Parent, Attrs, Violation_Unique);
         when Tok_Unique0 =>
            Stmt := Parse_Unique_Priority (Parent, Attrs, Violation_Unique0);
         when Tok_Priority =>
            Stmt := Parse_Unique_Priority (Parent, Attrs, Violation_Priority);
         when Tok_If =>
            Stmt := Parse_Conditional_Statement (Parent, Violation_None);
         when Tok_Case =>
            Stmt := Parse_Case_Statement (N_Case, Attrs, Violation_None);
         when Tok_Casex =>
            Stmt := Parse_Case_Statement (N_Casex, Attrs, Violation_None);
         when Tok_Casez =>
            Stmt := Parse_Case_Statement (N_Casez, Attrs, Violation_None);
         when Tok_Do =>
            Stmt := Parse_Do_While_Statement;
         when Tok_For =>
            Stmt := Parse_For_Statement;
         when Tok_Foreach =>
            Stmt := Parse_Foreach_Statement;
         when Tok_Void
           | Tok_System =>
            Stmt := Parse_Subroutine_Call_Stmt;
         when Tok_Sharp =>
            Stmt := Parse_Delay_Control;
            Set_Statement (Stmt, Parse_Statement_Or_Null (Stmt));
         when Tok_At =>
            Stmt := Parse_Event_Control;
            Set_Statement (Stmt, Parse_Statement_Or_Null (Stmt));
         when Tok_Sharp_Sharp =>
            Stmt := Parse_Cycle_Delay;
            Set_Statement (Stmt, Parse_Statement_Or_Null (Stmt));
         when Tok_Repeat =>
            Stmt := Parse_Delay_Or_Event_Control;
            if Get_Control (Stmt) = Null_Node then
               declare
                  N_Stmt : Node;
               begin
                  N_Stmt := Create_Node (N_Repeat);
                  Set_Location (N_Stmt, Get_Location (Stmt));
                  Set_Expression (N_Stmt, Get_Expression (Stmt));
                  Free_Node (Stmt);
                  Stmt := N_Stmt;
               end;
               Set_Statement (Stmt, Parse_Statement (Stmt));
            else
               Set_Statement (Stmt, Parse_Statement_Or_Null (Stmt));
            end if;
         when Tok_While =>
            Stmt := Parse_While_Repeat_Statement;
         when Tok_Forever =>
            Stmt := Parse_Forever_Statement;
         when Tok_Wait =>
            Stmt := Parse_Wait_Statement;
         when Tok_Trigger =>
            Stmt := Parse_Event_Trigger;
         when Tok_Disable =>
            Stmt := Parse_Disable_Statement;
         when Tok_Assign =>
            Stmt := Parse_Procedural_Continuous_Assignment (N_Proc_Assign);
         when Tok_Force =>
            Stmt := Parse_Procedural_Continuous_Assignment (N_Force_Assign);
         when Tok_Deassign =>
            Stmt := Parse_Procedural_Continuous_Assignment (N_Proc_Deassign);
         when Tok_Release =>
            Stmt := Parse_Procedural_Continuous_Assignment (N_Release);
         when Tok_Fork =>
            Stmt := Parse_Par_Block;
         when Tok_Assert =>
            Stmt := Parse_Assertion_Statement;
         when Tok_Continue =>
            Stmt := Create_Node (N_Continue_Stmt);
            Set_Token_Location (Stmt);

            --  Skip 'continue'.
            Scan;

            Scan_Statement_Semicolon;
         when Tok_Break =>
            Stmt := Create_Node (N_Break_Stmt);
            Set_Token_Location (Stmt);

            --  Skip 'break'.
            Scan;

            Scan_Statement_Semicolon;
         when Tok_Return =>
            Stmt := Create_Node (N_Return_Stmt);
            Set_Token_Location (Stmt);

            --  Skip 'return'.
            Scan;

            if Current_Token /= Tok_Semicolon then
               Set_Expression (Stmt, Parse_Expression);
            end if;

            Scan_Statement_Semicolon;
         when Tok_Minus_Minus =>
            Stmt := Parse_Pre_Inc_Or_Dec (N_Pre_Decrement);
            Scan_Statement_Semicolon;
         when Tok_Plus_Plus =>
            Stmt := Parse_Pre_Inc_Or_Dec (N_Pre_Increment);
            Scan_Statement_Semicolon;

         when Tok_Identifier
           | Tok_Left_Curly
           | Tok_This
           | Tok_Super =>
            Stmt := Parse_Assignment;

         when others =>
            Error_Msg_Parse ("statement expected "
                               & Token_Type'Image (Current_Token));
            Skip_Until_Semicolon;
            Stmt := Null_Node;
      end case;
      if Stmt /= Null_Node then
         Set_Parent (Stmt, Parent);
         Apply_Attributes (Attrs, Stmt);
      end if;
      return Stmt;
   end Parse_Statement;

   procedure Parse_Initial_Always_Statement
     (Constr : in out Items_Constr; Kind : Nkind)
   is
      Res : Node;
   begin
      Res := Create_Node (Kind);
      Set_Token_Location (Res);

      --  Skip 'always' or 'initial'.
      Scan;

      Set_Statement (Res, Parse_Statement (Res));
      Append_Node (Constr, Res);
   end Parse_Initial_Always_Statement;

   --  Parse for a node that can designate either a type or an expression.
   function Parse_Data_Type_Or_Expression return Type_Or_Expr_Node
   is
      Name : Node;
      Ptype : Type_Node;
   begin
      case Current_Token is
         when Toks_Verilog_Types | Toks_SV30_Types | Toks_SV31_Types
           | Tok_Event =>
            Ptype := Parse_Data_Type_Or_Implicit;
            return (Choice => Choice_Type, Typ => Ptype.Typ, Own => Ptype.Own);
         when Tok_Identifier =>
            null;
         when others =>
            --  FIXME: handle identifier
            return (Choice => Choice_Expr, Expr => Parse_Expression);
      end case;

      --  Identifier: can be an expression or a type...
      Name := Parse_Scoped_Or_Hierarchical_Name;

      case Current_Token is
         when Tok_Sharp =>
            Name := Parse_Class_Instance (Name);
         when Tok_Left_Brack =>
            Name := Parse_Bit_Select_Opt (Name);
         when others =>
            null;
      end case;

      if Current_Token = Tok_Comma or Current_Token = Tok_Right_Paren then
         --  End of param expression, assume expression unless class instance.
         if Get_Kind (Name) = N_Class_Instance then
            return (Choice => Choice_Type, Typ => Name, Own => True);
         else
            return (Choice => Choice_Expr, Expr => Name);
         end if;
      else
         --  Continue to parse as an expression.
         Name := Parse_Name (Name);
         Name := Parse_Expression_With_Primary (Name);
         return (Choice => Choice_Expr, Expr => Name);
      end if;
   end Parse_Data_Type_Or_Expression;

   --  1800-2017 23.3.2 Module instantiation syntax
   --  param_expression ::= mintypmax_expression | data_type | $
   function Parse_Param_Expression return Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      TE : Type_Or_Expr_Node;
      Res : Node;
   begin
      TE := Parse_Data_Type_Or_Expression;
      case TE.Choice is
         when Choice_Type =>
            Res := Create_Node (N_Parameter_Value_Type);
            Set_Data_Type (Res, TE.Typ);
            Set_Type_Owner (Res, TE.Own);
         when Choice_Expr =>
            Res := Create_Node (N_Parameter_Value_Expr);
            Set_Expression (Res, TE.Expr);
      end case;
      Set_Location (Res, Loc);
      return Res;
   end Parse_Param_Expression;

   --  1800-2017 23.3.2 Module instantiation syntax
   --  parameter_value_assignment ::=
   --    # ( [ list_of_parameter_assignments ] )
   --
   --  list_of_parameter_assignemnts ::=
   --      ordered_parameter_assignment { , ordered_parameter_assignment }
   --    | named_parameter_assignment { , named_parameter_assignment }
   --
   --  ordered_parameter_assignment ::=
   --    param_expression
   --
   --  named_parameter_assignment ::=
   --    . parameter_identifier ( [ param_expression ] )
   --
   --  GHDL: '#' was already scanned.
   function Parse_Parameter_Value_Assignment return Node
   is
      Expr : Node;
      pragma Unreferenced (Expr);
      Conn : Node;
      First_Conn, Last_Conn : Node;
      Loc : Location_Type;
      Id : Name_Id;
   begin
      if Current_Token /= Tok_Left_Paren then
         --  Assume a delay.
         Error_Msg_Parse
           ("'(' expected at beginning of parameter assignment");
         Expr := Parse_Expression;

         return Null_Node;
      end if;

      --  Skip '('.
      Scan;

      if Current_Token = Tok_Right_Paren then
         --  Skip ')'.
         Scan;

         return Null_Node;
      end if;

      Init_Chain (First_Conn, Last_Conn);
      loop
         Loc := Get_Token_Location;

         if Current_Token = Tok_Dot then
            --  Named assignment

            --  Skip '.'.
            Scan;

            --  Skip identifier.
            if Current_Token = Tok_Identifier then
               Id := Current_Identifier;
               Scan;
            else
               Error_Msg_Parse ("parameter identifier expected after '.'");
               Id := No_Name_Id;
            end if;

            --  Skip '('.
            Scan_Or_Error (Tok_Left_Paren,
                           "'(' expected in named parameter assignment");

            if Current_Token /= Tok_Right_Paren then
               Conn := Parse_Param_Expression;
            else
               Conn := Create_Node (N_Parameter_Value_Expr);
            end if;
            Set_Identifier (Conn, Id);

            --  Skip ')'.
            Scan_Or_Error (Tok_Right_Paren,
                           "')' expected after named parameter assignment");
         else
            --  Ordered assignment
            Conn := Parse_Param_Expression;
         end if;

         Set_Location (Conn, Loc);

         Append_Chain (First_Conn, Last_Conn, Conn);
         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
      end loop;

      --  Skip ')'.
      Scan_Or_Error (Tok_Right_Paren,
                     "missing ')' at end of parameter assignment");

      return First_Conn;
   end Parse_Parameter_Value_Assignment;

   --  A module instantiation was parsed, but in fact this is a variable
   --  declaration.
   procedure Parse_Variable_Declarations_From_Module
     (Constr : in out Items_Constr;
      Id : Name_Id;
      Loc : Location_Type;
      Dtype : Type_Node)
   is
      Var : Node;
      Decl_Type : Type_Node;
   begin
      --  Create the variable from the instantiation.
      Var := Create_Node (N_Var);
      Set_Location (Var, Loc);
      Set_Identifier (Var, Id);

      Decl_Type := Dtype;
      Set_Type_Node (Var, Decl_Type);

      if Current_Token = Tok_Equal then
         --  Skip '='.
         Scan;

         Set_Expression (Var, Parse_Expression);
      end if;

      Append_Node (Constr, Var);

      if Current_Token = Tok_Comma then
         --  Skip ','.
         Scan;

         Set_Has_Identifier_List (Var, True);

         Parse_Variable_Declarations
           (Constr, Decl_Type, False, False, (Life_Static, False));
      else
         --  Skip ';'.
         Scan_Declaration_Semicolon;
      end if;
   end Parse_Variable_Declarations_From_Module;

   --  postcond: next token after '('
   procedure Parse_UDP_Terminal_List (Gate : Node)
   is
      Conn : Node;
      Last_Conn : Node;
   begin
      --  Connections.
      Last_Conn := Null_Node;
      loop
         exit when Current_Token = Tok_Right_Paren;
         Conn := Create_Node (N_Port_Connection);
         Set_Token_Location (Conn);
         Set_Expression (Conn, Parse_Expression);

         if Last_Conn = Null_Node then
            Set_Gate_Terminals (Gate, Conn);
         else
            -- FIXME: check same.
            Set_Chain (Last_Conn, Conn);
         end if;
         Last_Conn := Conn;
         exit when Current_Token = Tok_Right_Paren;
         if Current_Token /= Tok_Comma then
            Error_Msg_Parse ("',' between terminals");
            exit;
         end if;
         Scan;
      end loop;
      Scan;
   end Parse_UDP_Terminal_List;

   --  Common between udp instance and gates.
   --  Parse:
   --  [ identifier { unpacked_dimension } ] ( terminal { , terminal } )
   procedure Parse_Instance_Terminal (Res : Node) is
   begin
      --  Optional identifier and range.
      if Current_Token = Tok_Identifier then
         Set_Identifier (Res, Current_Identifier);
         Scan;
         if Current_Token = Tok_Left_Brack then
            Set_Range (Res, Parse_Range (N_Array));
         end if;
      end if;

      --  Skip '('.
      Scan_Or_Error
        (Tok_Left_Paren, "'(' expected before list of terminals");

      Parse_UDP_Terminal_List (Res);
   end Parse_Instance_Terminal;

   --  1800-2017 29.8 UDP instances
   --  udp_instantiation ::=
   --    udp_identifier [ drive_strength ] [ delay2 ]
   --      udp_instance { , udp_instance } ;
   --
   --  udp_instance ::=
   --    [ name_of_instance ]
   --      ( output_terminal, input_terminal { , input_terminal } )
   --
   --  name_of_instance ::= instance_identifier { unpacked_dimension }
   procedure Parse_Udp_Instantiation (Constr : in out Items_Constr;
                                      Udp_Name : Node;
                                      Udp_Delay : Node)
   is
      Res : Node;
   begin
      loop
         Res := Create_Node (N_Primitive_Instance);
         Set_Module (Res, Udp_Name);
         Set_Gate_Delay (Res, Udp_Delay);

         Set_Token_Location (Res);

         Parse_Instance_Terminal (Res);

         Append_Node (Constr, Res);
         exit when Current_Token /= Tok_Comma;
         Scan;
      end loop;

      --  Skip ';'.
      Scan_Declaration_Semicolon;
   end Parse_Udp_Instantiation;

   --  14.3 Clocking block declaration
   --  clocking_event ::=
   --      @ identifier
   --    | @ ( event_expression )
   function Parse_Clocking_Event return Node
   is
      Event : Node;
   begin
      pragma Assert (Current_Token = Tok_At);

      --  Skip '@'.
      Scan;

      if Current_Token = Tok_Identifier then
         Event := Scan_Name;
      else
         Scan_Or_Error
           (Tok_Left_Paren, "'(' expected before event expression");
         Event := Parse_Event_Expression;
         Scan_Or_Error
           (Tok_Right_Paren, "')' expected after event expression");
      end if;

      return Event;
   end Parse_Clocking_Event;

   --  1800-2017 16.9.1 Operator precedence
   type Prio_Seq_Type is (Prio_Seq_Paren,
   --                         Prio_Seq_Repeat,        --  [* ]  [= ]  [-> ]
                          Prio_Seq_Concat,      --  ##
                          Prio_Seq_Throughout);
   --                         Prio_Seq_Within,
   --                         Prio_Seq_Intersect,
   --                         Prio_Seq_And,
   --                         Prio_Seq_Or);

   function Parse_Sequence_Expr (Prio : Prio_Seq_Type) return Node;

   --  1800-2017 16.7 Sequences
   --  consecutive_repetition ::=
   --      [* const_or_range_expression ]
   --    | ...
   --
   --  const_or_range_expression ::=
   --      constant_expression
   --    | cycle_delay_const_range_expression
   --
   --  cycle_delay_const_range_expression ::=
   --      constant_expression : constant_expression
   --    | constant_expression : $
   function Parse_Sequence_Const_Repeat (Expr : Node) return Node
   is
      Res : Node;
   begin
      pragma Assert (Current_Token = Tok_Brack_Star);

      Res := Create_Node (N_Seq_Repeat);
      Set_Token_Location (Res);
      Set_Sequence (Res, Expr);

      --  Skip '[*'.
      Scan;

      Set_Msb (Res, Parse_Expression);

      if Current_Token = Tok_Colon then
         --  Skip ':'.
         Scan;

         if Current_Token = Tok_Dollar then
            Set_Lsb (Res, Parse_Infinity);
         else
            Set_Lsb (Res, Parse_Expression);
         end if;
      end if;

      --  Skip ']'.
      Scan_Or_Error (Tok_Right_Brack, "']' expected");

      return Res;
   end Parse_Sequence_Const_Repeat;

   --  1800-2017 16.7 Sequences
   function Parse_Sequence_Expr_Left return Node
   is
      Left : Node;
      Res : Node;
   begin
      case Current_Token is
         when Tok_Sharp_Sharp =>
            Res := Create_Node (N_Seq_Const_Concat);
            Set_Token_Location (Res);

            --  Skip '##'.
            Scan;

            Set_Repeat_Expression (Res, Parse_Primary_Expression);

            Set_Right (Res, Parse_Sequence_Expr (Prio_Seq_Paren));

         when Tok_Left_Paren =>
            declare
               Loc : constant Location_Type := Get_Token_Location;
               Seq : Node;
            begin
               --  Skip '('.
               Scan;

               Seq := Parse_Sequence_Expr (Prio_Seq_Paren);

               --  Skip ')'.
               Scan_Or_Error (Tok_Right_Paren, "missing ')'");

               --  Maybe the sequence was in fact an expression, try to
               --  continue parsing as an expression.
               if Get_Kind (Seq) in Nkinds_Seq_Expr then
                  Res := Create_Node (N_Seq_Parenthesis);
                  Set_Location (Res, Loc);
                  Set_Sequence (Res, Seq);
               else
                  Res := Create_Node (N_Parenthesis_Expr);
                  Set_Location (Res, Loc);
                  Set_Expression (Res, Seq);
                  Res := Parse_Expression_With_Primary (Res);
               end if;
            end;

         when others =>
            Res := Parse_Expression;

            case Current_Token is
               when Tok_Brack_Star =>
                  --  Boolean_abbrev consecutive_repetition
                  Res := Parse_Sequence_Const_Repeat (Res);

               when Tok_Throughout =>
                  Left := Res;
                  Res := Create_Node (N_Seq_Throughout);
                  Set_Token_Location (Res);
                  Set_Left (Res, Left);

                  --  Skip 'throughout'.
                  Scan;

                  Set_Right (Res, Parse_Sequence_Expr (Prio_Seq_Throughout));

               when others =>
                  null;
            end case;
      end case;

      return Res;
   end Parse_Sequence_Expr_Left;

   function Parse_Sequence_Expr_With_Left
     (Left : Node; Prio : Prio_Seq_Type) return Node
   is
      Left1 : Node;
      Res : Node;
      Kind : Nkind;
      N_Prio : Prio_Seq_Type;
   begin
      Res := Left;

      loop
         case Current_Token is
            when Tok_Sharp_Sharp =>
               Kind := N_Seq_Const_Concat;
               N_Prio := Prio_Seq_Concat;
            when Tok_Sharp_Star_Concat =>
               Kind := N_Seq_Star_Concat;
               N_Prio := Prio_Seq_Concat;
            when Tok_Sharp_Plus_Concat =>
               Kind := N_Seq_Plus_Concat;
               N_Prio := Prio_Seq_Concat;
            when others =>
               return Res;
         end case;

         --  Note: use '>' for left associativity, '>=' for right
         --  associativity.
         if Prio > N_Prio then
            return Res;
         end if;

         Left1 := Res;
         Res := Create_Node (Kind);
         Set_Token_Location (Res);
         Set_Left (Res, Left1);

         --  Skip operator.
         Scan;

         case Kind is
            when N_Seq_Const_Concat =>
               Set_Repeat_Expression (Res, Parse_Primary_Expression);
            when N_Seq_Star_Concat
              | N_Seq_Plus_Concat =>
               null;
            when others =>
               raise Internal_Error;
         end case;

         Set_Right (Res, Parse_Sequence_Expr (N_Prio));
      end loop;
   end Parse_Sequence_Expr_With_Left;

   function Parse_Sequence_Expr (Prio : Prio_Seq_Type) return Node
   is
      Left : Node;
   begin
      Left := Parse_Sequence_Expr_Left;
      return Parse_Sequence_Expr_With_Left (Left, Prio);
   end Parse_Sequence_Expr;

   type Prio_Prop_Type is (Prio_Prop_None,
                           Prio_Prop_Paren,
                           Prio_Prop_Not,
                           Prio_Prop_And,
                           Prio_Prop_Or,
                           Prio_Prop_Iff,
                           Prio_Prop_Until,
                           Prio_Prop_Imp);
   pragma Unreferenced (Prio_Prop_Iff);

   function Parse_Property_Expr (Prio : Prio_Prop_Type) return Node;

   --  1800-2017 16.12 Declaring properties
   function Parse_Property_Expr_Left return Node
   is
      Res : Node;
   begin
      case Current_Token is
         when Tok_Not =>
            Res := Create_Node (N_Prop_Not);
            Set_Token_Location (Res);

            --  Skip 'not'.
            Scan;

            Set_Expression (Res, Parse_Property_Expr (Prio_Prop_Not));

         when Tok_Left_Paren =>
            --  Skip '('.
            Scan;

            Res := Parse_Property_Expr (Prio_Prop_Paren);

            --  Skip ')'.
            Scan_Or_Error (Tok_Right_Paren, "missing ')'");

            if Get_Kind (Res) not in Nkinds_Prop_Expr then
               if Get_Kind (Res) not in Nkinds_Seq_Expr then
                  Res := Parse_Expression_With_Primary (Res);
               end if;
               Res := Parse_Sequence_Expr_With_Left (Res, Prio_Seq_Paren);
            end if;

         when others =>
            Res := Parse_Sequence_Expr (Prio_Seq_Paren);
      end case;

      return Res;
   end Parse_Property_Expr_Left;

   function Parse_Property_Expr (Prio : Prio_Prop_Type) return Node
   is
      Res : Node;
      Kind : Nkind;
      N_Prio : Prio_Prop_Type;
      Left : Node;
   begin
      Res := Parse_Property_Expr_Left;

      loop
         case Current_Token is
            when Tok_Or =>
               Kind := N_Prop_Or;
               N_Prio := Prio_Prop_Or;
            when Tok_And =>
               Kind := N_Prop_And;
               N_Prio := Prio_Prop_And;
            when Tok_Bar_Arrow =>
               Kind := N_Prop_Overlap_Imp;
               N_Prio := Prio_Prop_Imp;
            when Tok_Bar_Double_Arrow =>
               Kind := N_Prop_Non_Overlap_Imp;
               N_Prio := Prio_Prop_Imp;
            when Tok_Until =>
               Kind := N_Prop_Until;
               N_Prio := Prio_Prop_Until;
               --  FIXME: associativity.
            when others =>
               return Res;
         end case;
         if Prio >= N_Prio then
            return Res;
         end if;

         Left := Res;
         Res := Create_Node (Kind);
         Set_Token_Location (Res);

         --  Skip operator.
         Scan;

         Set_Left (Res, Left);
         Set_Right (Res, Parse_Property_Expr (N_Prio));
      end loop;
   end Parse_Property_Expr;

   --  1800-2017 16 Assertions
   --  property_spec ::=
   --    [ clocking_event ] [ disable iff ( expression_or_dist ) ]
   --      property_expr
   procedure Parse_Property_Spec (N : Node) is
   begin
      if Current_Token = Tok_At then
         Set_Clocking_Event (N, Parse_Clocking_Event);
      end if;
      if Current_Token = Tok_Disable then
         --  Skip 'disable'.
         Scan;

         --  Skip 'iff'.
         Scan_Or_Error (Tok_Iff, "'iff' expected after 'disable'");

         Set_Disable_Expression (N, Parse_Parenthesis_Expression);
      end if;

      Set_Property_Expression (N, Parse_Property_Expr (Prio_Prop_None));
   end Parse_Property_Spec;

   procedure Parse_Parenthesis_Property_Spec (N : Node) is
   begin
      Scan_Or_Error (Tok_Left_Paren, "'(' expected before property spec");
      Parse_Property_Spec (N);
      Scan_Or_Error (Tok_Right_Paren, "')' expected after property spec");
   end Parse_Parenthesis_Property_Spec;

   --  1800-2017 16 Assertions
   --  assert_property_statement ::=
   --    assert property ( property_spec ) action_block
   --
   --  simple_immediate_assert_statement ::=
   --    assert ( expression ) action_block
   --
   --  deferred_immediate_assert_statement ::=
   --      assert #0 ( expression ) action_block
   --    | assert final ( expression ) action_block
   function Parse_Assert return Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Res : Node;
   begin
      --  Skip 'assert'.
      Scan;

      case Current_Token is
         when Tok_Property =>
            --  Skip 'property'.
            Scan;

            Res := Create_Node (N_Assert_Property);
            Set_Location (Res, Loc);
            Parse_Parenthesis_Property_Spec (Res);
            Parse_Action_Block (Res);
--         when Tok_Left_Paren =>
--            Res := Create_Node (N_Assert_Simple);
--            Set_Expression (Res, Parse_Parenthesis_Expression);
--            Parse_Action_Block (Res);
         when others =>
            --  TODO: deferred.
            raise Internal_Error;
      end case;

      return Res;
   end Parse_Assert;

   function Parse_Assume return Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Res : Node;
   begin
      --  Skip 'assert'.
      Scan;

      case Current_Token is
         when Tok_Property =>
            --  Skip 'property'.
            Scan;

            Res := Create_Node (N_Assume_Property);
            Set_Location (Res, Loc);
            Parse_Parenthesis_Property_Spec (Res);
            Parse_Action_Block (Res);
         when others =>
            --  TODO: deferred.
            raise Internal_Error;
      end case;

      return Res;
   end Parse_Assume;

   function Parse_Assertion_Item (Block_Id : Name_Id;
                                  Id_Loc : Location_Type) return Node
   is
      Res : Node;
   begin
      case Current_Token is
         when Tok_Assert =>
            Res := Parse_Assert;
         when Tok_Assume =>
            Res := Parse_Assume;
         when Tok_Cover =>
            raise Internal_Error;
         when Tok_Restrict =>
            raise Internal_Error;
         when others =>
            raise Internal_Error;
      end case;

      if Block_Id /= Null_Identifier then
         Set_Identifier (Res, Block_Id);
         Set_Location (Res, Id_Loc);
      end if;

      return Res;
   end Parse_Assertion_Item;

   --  precond:  identifier
   --
   --  1800-2017 23.3.2 Module instantiation syntax
   --
   --  1364-2005 12.2
   --    module_instantiation ::=
   --      module_identifier [ parameter_value_assignment ]
   --        module_instance { , module_instance } ;
   --
   --    parameter_value_assignment ::=
   --      # ( list_of_parameter_assignments )
   procedure Parse_Module_Instantiation (Constr : in out Items_Constr)
   is
      Module : Node;
      Inst : Node;
      Conn : Node;
      Last_Conn : Node;
      Expr : Node;
      Decl_Type : Type_Node;
      Is_First : Boolean;
      Id : Name_Id;
      Loc : Location_Type;
   begin
      --  The first token is an identifier, so this can be a variable
      --  declaration, a module instantiation or an interface instantiation.
      --  Start with the worst case: a data type.
      Decl_Type := Parse_Data_Type_Or_Implicit;

      Module := Decl_Type.Typ;
      if Module /= Null_Node then
         case Get_Kind (Module) is
            when N_Name =>
               if Current_Token = Tok_Colon then
                  declare
                     Block_Id : constant Name_Id := Get_Identifier (Module);
                     Loc : constant Location_Type := Get_Location (Module);
                  begin
                     Free_Node (Module);

                     --  Skip ':'.
                     Scan;

                     Append_Node (Constr,
                                  Parse_Assertion_Item (Block_Id, Loc));
                     return;
                  end;
               else
                  --  Continue.
                  null;
               end if;
            when N_Class_Instance =>
               null;
            when N_Primitive_Instance =>
               declare
                  Udp : constant Node := Get_Module (Module);
                  Del : constant Node := Get_Gate_Delay (Module);
               begin
                  Free_Node (Module);
                  Parse_Udp_Instantiation (Constr, Udp, Del);
                  return;
               end;
            when others =>
               --  Certainly a packed array.
               Parse_Variable_Declarations
                 (Constr, Decl_Type, False, False, (Life_Static, False));
               return;
         end case;
      end if;

      Is_First := True;
      loop
         Loc := Get_Token_Location;

         --  Module identifier.
         --  Note: name of udp instance is optionnal.
         if Current_Token = Tok_Identifier then
            Id := Current_Identifier;
            Loc := Get_Token_Location;

            --  Skip identifier.
            Scan;

            --  Range.
            if Current_Token = Tok_Left_Brack then
               Decl_Type := Parse_Variable_Dimension_Rep (Decl_Type);
            end if;
         else
            Id := Null_Identifier;
         end if;

         if Is_First
           and then Current_Token /= Tok_Left_Paren
         then
            --  A variable declaration was parsed!
            Parse_Variable_Declarations_From_Module
              (Constr, Id, Loc, Decl_Type);
            return;
         end if;

         Is_First := False;

         Inst := Create_Node (N_Module_Instance);
         Set_Location (Inst, Loc);
         Set_Identifier (Inst, Id);

         if Decl_Type.Typ /= Null_Node then
            declare
               Typ : Node;
               El_Typ : Node;
            begin
               Typ := Decl_Type.Typ;
               loop
                  case Get_Kind (Typ) is
                     when N_Array =>
                        if Get_Range (Inst) = Null_Node then
                           Set_Range (Inst, Typ);
                        end if;
                        El_Typ := Get_Element_Data_Type (Typ);
                        if Get_Kind (El_Typ) /= N_Array then
                           Set_Element_Data_Type (Typ, Null_Node);
                        end if;
                        Typ := El_Typ;
                     when N_Name =>
                        Set_Module (Inst, Typ);
                        exit;
                     when N_Class_Instance =>
                        Set_Module (Inst, Get_Class_Name (Typ));
                        Set_Parameter_Values
                          (Inst, Get_Parameter_Values (Typ));
                        Free_Node (Typ);
                        exit;
                     when others =>
                        --  Incorrect range.  TODO.
                        raise Program_Error;
                  end case;
               end loop;
               Decl_Type := (Null_Node, False);
            end;
         end if;

         --  Skip '('.
         Scan_Or_Error (Tok_Left_Paren, "'(' expected after name of instance");

         --  Connections.
         Last_Conn := Null_Node;
         loop
            exit when Current_Token = Tok_Right_Paren;

            if Current_Token = Tok_Dot_Star then
               Conn := Create_Node (N_Wildcard_Connection);
               Set_Token_Location (Conn);

               --  Skip '.*'.
               Scan;
            else
               Conn := Create_Node (N_Port_Connection);
               Set_Token_Location (Conn);
               Expr := Null_Node;
               if Current_Token = Tok_Dot then
                  --  Skip '.'.
                  Scan;

                  --  Skip identifier.
                  Scan_Identifier (Conn, "port identifier expected after '.'");

                  --  Skip '('.
                  Scan_Or_Error (Tok_Left_Paren,
                                 "'(' expected after port identifier");

                  if Current_Token /= Tok_Right_Paren then
                     Expr := Parse_Expression;
                  end if;

                  Scan_Or_Error
                    (Tok_Right_Paren,
                     "')' expected after expression in port connection");
               else
                  if Current_Token /= Tok_Comma then
                     Expr := Parse_Expression;
                  end if;
               end if;
               Set_Expression (Conn, Expr);
            end if;

            if Last_Conn = Null_Node then
               Set_Connections (Inst, Conn);
            else
               -- FIXME: check same.
               Set_Chain (Last_Conn, Conn);
            end if;
            Last_Conn := Conn;
            exit when Current_Token = Tok_Right_Paren;
            if Current_Token /= Tok_Comma then
               Error_Msg_Parse ("',' between connections");
               exit;
            end if;
            Scan;
         end loop;
         Append_Implicits (Constr);
         Append_Node (Constr, Inst);
         if Current_Token = Tok_Right_Paren then
            Scan;
         end if;
         exit when Current_Token /= Tok_Comma;
         Scan;
      end loop;

      --  Skip ';'.
      Scan_Declaration_Semicolon;
   end Parse_Module_Instantiation;

   function Parse_Terminal (Kind : Nkind) return Node
   is
      Term : Node;
   begin
      Term := Create_Node (Kind);
      Set_Token_Location (Term);
      Set_Expression (Term, Parse_Expression);
      return Term;
   end Parse_Terminal;

   procedure Scan_Comma_After_Terminal is
   begin
      if Current_Token /= Tok_Comma then
         Error_Msg_Parse ("',' required between terminals");
      else
         --  Skip ','.
         Scan;
      end if;
   end Scan_Comma_After_Terminal;

   --  postcond: next token after '('
   procedure Parse_Gate_Terminal_List (Gate : Node)
   is
      Kind : constant Nkinds_Gate := Get_Kind (Gate);
      Term_Kind : Nkinds_Terminal;
      First, Last : Node;
      Term, Expr : Node;
      Loc : Location_Type;
      Term_Idx : Positive;
   begin
      Init_Chain (First, Last);

      --  First terminal.
      case Kind is
         when Nkinds_Input_Gate
           | Nkinds_Output_Gate
           | Nkinds_Enable_Gate
           | Nkinds_Mos_Switch
           | Nkinds_Cmos_Switch
           | Nkinds_Pull_Gate =>
            Term_Kind := N_Output_Terminal;
         when Nkinds_Pass_Switch
           | Nkinds_Pass_En_Switch =>
            Term_Kind := N_Inout_Terminal;
      end case;
      Term := Parse_Terminal (Term_Kind);
      Term_Idx := 2;

      Set_Gate_Terminals (Gate, Term);
      Append_Chain (First, Last, Term);

      if Kind in Nkinds_Pull_Gate then
         if Current_Token /= Tok_Right_Paren then
            Error_Msg_Parse ("pull gates have only one terminal");
         else
            --  Skip ')'.
            Scan;
         end if;
         return;
      else
         if Current_Token = Tok_Right_Paren then
            Error_Msg_Parse ("input terminal required");

            --  Skip ')'.
            Scan;

            return;
         end if;
      end if;

      --  Skip ','.
      Scan_Comma_After_Terminal;

      --  The other terminals.
      loop
         Loc := Get_Token_Location;
         Expr := Parse_Expression;

         case Kind is
            when Nkinds_Input_Gate =>
               Term_Kind := N_Input_Terminal;
            when Nkinds_Output_Gate =>
               --  Only the last terminal is an input.
               if Current_Token = Tok_Comma then
                  Term_Kind := N_Output_Terminal;
               else
                  Term_Kind := N_Input_Terminal;
               end if;
            when Nkinds_Enable_Gate
              | Nkinds_Mos_Switch
              | Nkinds_Cmos_Switch =>
               case Term_Idx is
                  when 2 =>
                     Term_Kind := N_Input_Terminal;
                  when others =>
                     Term_Kind := N_Control_Terminal;
               end case;
            when Nkinds_Pass_Switch =>
               Term_Kind := N_Inout_Terminal;
            when Nkinds_Pass_En_Switch =>
               if Current_Token = Tok_Comma then
                  Term_Kind := N_Inout_Terminal;
               else
                  Term_Kind := N_Control_Terminal;
               end if;
            when Nkinds_Pull_Gate =>
               raise Program_Error;
         end case;

         Term := Create_Node (Term_Kind);
         Set_Location (Term, Loc);
         Set_Expression (Term, Expr);

         Append_Chain (First, Last, Term);

         exit when Current_Token = Tok_Right_Paren;

         --  Skip ','.
         Scan_Comma_After_Terminal;
      end loop;

      --  Skip ')'.
      Scan;
   end Parse_Gate_Terminal_List;

   --  precond:  gate (OR, NMOS, TRANS ...)
   --
   --  1800-2017 28.3 Gate and switch declaration syntax
   procedure Parse_Gate_Instantiation (Constr : in out Items_Constr;
                                       Kind : Nkind)
   is
      Gate : Node;
      Del : Node;
      Strength : Int32;
      pragma Unreferenced (Strength);
   begin
      --  Skip gate type.
      Scan;

      Del := Null_Node;

      --  Drive strength.
      if Current_Token = Tok_Left_Paren then
         --  Skip '('.
         Scan;

         if Current_Token in Toks_Strength then
            Strength := Parse_Drive_Strength;
         else
            --  No strength.  Assume no name of gate too.
            Strength := 0;
            Gate := Create_Node (Kind);
            Set_Token_Location (Gate);
            Parse_Gate_Terminal_List (Gate);
            if Current_Token = Tok_Comma then
               Scan;
               goto Instances;
            else
               goto Semicol;
            end if;
         end if;
      end if;

      --  Delay3
      if Current_Token = Tok_Sharp then
         Del := Parse_Delay2_3 (False);
      end if;

      << Instances >> null;
      loop
         Gate := Create_Node (Kind);
         Set_Token_Location (Gate);
         Set_Gate_Delay (Gate, Del);

         --  Optional identifier and range.
         if Current_Token = Tok_Identifier then
            Set_Identifier (Gate, Current_Identifier);
            Scan;
            if Current_Token = Tok_Left_Brack then
               Set_Range (Gate, Parse_Range (N_Array));
            end if;
         end if;

         --  Skip '('.
         Scan_Or_Error
           (Tok_Left_Paren, "'(' expected before list of terminals");

         Parse_Gate_Terminal_List (Gate);

         Append_Node (Constr, Gate);
         exit when Current_Token /= Tok_Comma;
         Scan;
      end loop;
      << Semicol >> null;

      --  Skip ';'.
      Scan_Declaration_Semicolon;
   end Parse_Gate_Instantiation;

   --  precond:  INPUT, OUTPUT or INOUT
   --  postcond: next token
   --
   --  Note: this is a port declaration using the non-ANSI style as a module
   --    item.  See Parse_Ports_List for ports in headers.
   --
   --  1364-2005 12.3.3 Port declarations
   --  inout_declaration ::=
   --    INOUT [ net_type ] [ SIGNED ] [ range ] list_of_port_identifiers
   --  input_declaration ::=
   --    INPUT [ net_type ] [ SIGNED ] [ range ] list_of_port_identifiers
   --  output_declaration ::=
   --      OUTPUT [ net_type ] [ SIGNED ] [ range ] list_of_port_identifiers
   --    | OUTPUT REG [ SIGNED ] [ range ] list_of_variable_port_identifiers
   --    | OUTPUT output_variable_type list_of_variable_port_identifiers
   --
   --  1800-2017 23.2.2 Port declarations
   --  port_declaration ::=
   --      inout_declaration
   --    | input_declaration
   --    | output_declaration
   --    | ref_declaration
   --    | interface_port_declaration
   --
   --  inout_declaration ::=
   --    INOUT net_port_type list_of_port_identifiers
   --
   --  input_declaration ::=
   --      INPUT net_port_type list_of_port_identifiers
   --    | INPUT variable_port_type list_of_variable_port_identifiers
   --
   --  output_declaration ::=
   --      OUTPUT net_port_type list_of_port_identifiers
   --    | OUTPUT variable_port_type list_of_variable_port_identifiers
   --
   --  ref_declaration ::=
   --    REF variable_port_type list_of_variables_identifiers
   --
   --  interface_port_declaration ::=
   --      interface_identifier list_of_interface_identifiers
   --    | interface_identifier . modport_identifier
   --        list_of_interface_identifiers
   procedure Parse_Port_Declaration (Constr : in out Items_Constr;
                                     Kind : Nkind;
                                     End_Comma : out Boolean)
   is
      --use Name_Table;
      N : Node;
      Decl_Type : Type_Node;
      Net_Type : Nkind;
      Net : Node;
   begin
      --  Skip 'input', 'inout' or 'output'.
      Scan;

      case Current_Token is
         when Tok_Wire =>
            Net_Type := N_Wire;

            --  Skip 'wire'.
            Scan;

         when others =>
            Net_Type := N_Error;
            null;
      end case;

      Decl_Type := Parse_Data_Type_Or_Implicit;

      --  Build node.
      N := Create_Node (Kind);
      Set_Token_Location (N);
      Set_Has_Direction (N, True);

      --  FIXME: factorize.
      if Current_Token = Tok_Identifier then
         Set_Identifier (N, Current_Identifier);

         --  Skip identifier.
         Scan;
      elsif Decl_Type.Typ /= Null_Node then
         Data_Type_To_Identifier (N, Decl_Type);
      else
         Error_Msg_Parse ("missing port identifier");
      end if;

      if Net_Type = N_Error
        and then not Is_Implicit_Type (Decl_Type.Typ)
      then
         Net_Type := N_Var;
      end if;

      loop
         Set_Type_Node (N, Decl_Type);
         Parse_Variable_Dimension_Rep (N);
         Append_Node (Constr, N);

         if Net_Type /= N_Error then
            --  Complete declaration.
            --  FIXME: is there a place to put that declaration ?
            --  Will be put in module items by sem.
            Net := Create_Node (Net_Type);
            Set_Location (Net, Get_Location (N));
            Set_Identifier (Net, Get_Identifier (N));
            Set_Redeclaration (N, Net);
            Set_Redeclaration_Flag (Net, True);
            Set_Parent (Net, Get_Parent (Constr));
         end if;

         if Current_Token /= Tok_Comma then
            End_Comma := False;
            exit;
         end if;

         --  Skip ','.
         Scan;

         if Current_Token /= Tok_Identifier then
            End_Comma := True;
            exit;
         end if;
         Set_Has_Identifier_List (N, True);

         --  Build node.
         N := Create_Node (Kind);
         Set_Token_Location (N);
         Set_Identifier (N, Current_Identifier);
         Set_Has_Direction (N, True);

         --  Skip identifier.
         Scan;
      end loop;
   end Parse_Port_Declaration;

   procedure Parse_Port_Declaration_Semicolon (Constr : in out Items_Constr;
                                               Kind : Nkind)
   is
      Comma : Boolean;
   begin
      Parse_Port_Declaration (Constr, Kind, Comma);
      if Comma then
         Error_Msg_Parse ("extra comma ignored");
      end if;

      --  Skip ';'.
      Scan_Declaration_Semicolon;
   end Parse_Port_Declaration_Semicolon;

   --  port_direction ::=
   --    INPUT | OUTPUT | INOUT | REF
   --
   --  Return N_Error is there is no port direction.
   function Parse_Port_Direction_Opt return Nkind
   is
      Dir : Nkind;
   begin
      case Current_Token is
         when Tok_Input =>
            Dir := N_Input;

            --  Skip 'input'.
            Scan;

         when Tok_Output =>
            Dir := N_Output;

            --  Skip 'output'.
            Scan;

         when Tok_Inout =>
            Dir := N_Inout;

            --  Skip 'inout'.
            Scan;

         when Tok_Ref =>
            Dir := N_Tf_Ref;

            --  Skip 'ref'.
            Scan;

         when Tok_Const =>
            Dir := N_Tf_Const_Ref;

            --  Skip 'const'.
            Scan;

            Scan_Or_Error (Tok_Ref, "'ref' expected after 'const'");

         when others =>
            Dir := N_Error;
      end case;

      return Dir;
   end Parse_Port_Direction_Opt;

   --  precond:  '('
   --  postcond: next token
   --
   --  parse: ( [ tf_port_list ] )
   --
   --  1800-2017 13.3 Tasks
   --  tf_port_list ::=
   --    tf_port_item { , tf_port_item }
   --
   --  tf_port_item ::=
   --    { attribute_instance }
   --      [ tf_port_direction ] [ VAR ] data_type_or_implicit
   --      [ port_identifier { variable_dimension } [ = expression ] ]
   --
   --  tf_port_direction ::= port_direction | CONST REF
   function Parse_Tf_Port_List (Parent : Node) return Node
   is
      Port : Node;
      Dir : Nkind;
      Has_Dir : Boolean;
      Data_Type : Type_Node;
      Constr : Items_Constr;
   begin
      pragma Assert (Current_Token = Tok_Left_Paren);

      --  Skip '('.
      Scan;

      if Current_Token = Tok_Right_Paren then
         --  Skip ')'.
         Scan;

         return Null_Node;
      end if;

      --  Direction is not set.
      Dir := N_Error;

      Init_Constr (Constr, Parent);
      loop
         --  port direction
         Dir := Parse_Port_Direction_Opt;
         if Dir = N_Error then
            --  1800-2017 13.3 Tasks
            --  There is a default direction of INPUT if no direction has been
            --  specified.  Once a direction is given, subsequent formals
            --  default to the same direction.
            if Flags.Std not in Systemverilog_Standard then
               Error_Msg_Parse ("missing port direction");
            end if;
            Dir := N_Tf_Input;
            Has_Dir := False;
         else
            case Dir is
               when N_Input  => Dir := N_Tf_Input;
               when N_Output => Dir := N_Tf_Output;
               when N_Inout  => Dir := N_Tf_Inout;
               when others => null;
            end case;
            Has_Dir := True;
         end if;

         if Current_Token = Tok_Var then
            --  Skip 'var'.
            Scan;
         end if;

         Port := Create_Node (Dir);
         Set_Token_Location (Port);
         Set_Has_Direction (Port, Has_Dir);

         Data_Type := Parse_Data_Type_Or_Implicit;
         Set_Type_Node (Port, Data_Type);

         if Current_Token = Tok_Identifier then
            Set_Identifier (Port, Current_Identifier);

            --  Skip identifier.
            Scan;
         else
            if Data_Type.Typ /= Null_Node
              and then Get_Kind (Data_Type.Typ) = N_Name
            then
               --  No identifier, but data_type was an identifier.
               Set_Identifier (Port, Get_Identifier (Data_Type.Typ));
               Set_Type_Owner (Port, False);
               Set_Data_Type (Port, Null_Node);
               Free_Node (Data_Type.Typ);
            end if;
         end if;

         Parse_Variable_Dimension_Rep (Port);

         if Current_Token = Tok_Equal then
            --  Skip '='
            Scan;

            Set_Default_Value (Port, Parse_Expression);
         end if;

         Append_Node (Constr, Port);

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
      end loop;

      --  Skip ')'.
      Scan_Or_Error (Tok_Right_Paren, "')' expected after port list");

      return Get_Constr_Chain (Constr);
   end Parse_Tf_Port_List;

   --  1800-2017 13.3 Tasks
   --  tf_port_declaration ::=
   --    tf_port_direction [ VAR ] data_type_or_implicit
   --      list_of_tf_variable_identifier ;
   --
   --  list_of_tf_variable_identifiers ::=
   --    port_identifier { variable_dimension } [ = expression ]
   --    { , port_identifier { variable_dimension } [ = expression ] }
   procedure Parse_Tf_Port_Declaration (Constr : in out Items_Constr;
                                        Kind : Nkind)
   is
      Port : Node;
      Ptype : Type_Node;
   begin
      --  Skip direction
      Scan;

      if Current_Token = Tok_Var then
         --  Skip 'var'.
         Scan;
      end if;

      Port := Create_Node (Kind);
      Set_Token_Location (Port);
      Append_Node (Constr, Port);

      Ptype := Parse_Data_Type_Or_Implicit;

      if Current_Token = Tok_Identifier then
         Set_Token_Location (Port);
         Set_Identifier (Port, Current_Identifier);

         --  Skip identifier.
         Scan;

         Set_Type_Node (Port, Ptype);
      elsif Ptype.Typ /= Null_Node then
         Data_Type_To_Identifier (Port, Ptype);
         Ptype := (Null_Node, False);
      else
         Error_Msg_Parse ("missing port identifier");
      end if;

      loop
         Parse_Variable_Dimension_Rep (Port);

         if Current_Token = Tok_Equal then
            --  Skip '='.
            Scan;

            Set_Expression (Port, Parse_Expression);
         end if;

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;

         Port := Create_Node (Kind);
         Set_Token_Location (Port);

         Append_Node (Constr, Port);

         Scan_Identifier (Port, "missing port identifier");
      end loop;

      --  Skip ';'.
      Scan_Declaration_Semicolon;
   end Parse_Tf_Port_Declaration;

   --  For: sequential blocks, parallel blocks, tasks, functions.
   --  Need to parse declaration and statement at the same time, as an
   --  identifier can starts a variable declaration or a variable assignment.
   --
   --  Parse:
   --    { block_item_declaration }
   --    { statement_or_null }
   --
   --  1800-2017 9.3 Block statements
   --  block_item_declaration ::=
   --      data_declaration
   --    | local_parameter_declaration
   --    | parameter_declaration
   --    | let_declaration
   procedure Parse_Block_Item_Declaration_Statement (Parent : Node;
                                                     First_Decl : out Node;
                                                     First_Stmt : out Node)
   is
      Last_Stmt : Node;
      Decl_Constr : Items_Constr;
      Stmt : Node;
      Decl_Type : Type_Node;
   begin
      Init_Chain (First_Stmt, Last_Stmt);

      --  block_item_declaration.
      Init_Constr (Decl_Constr, Parent);
      loop
         case Current_Token is
            when Tok_Input =>
               Parse_Tf_Port_Declaration (Decl_Constr, N_Tf_Input);
            when Tok_Output =>
               Parse_Tf_Port_Declaration (Decl_Constr, N_Tf_Output);
            when Tok_Inout =>
               Parse_Tf_Port_Declaration (Decl_Constr, N_Tf_Inout);
            when Tok_Parameter =>
               Parse_Parameter_Declaration (N_Parameter, Decl_Constr);
            when Tok_Localparam =>
               Parse_Parameter_Declaration (N_Localparam, Decl_Constr);
            when Toks_Verilog_Types | Toks_SV30_Types | Toks_SV31_Types
              | Tok_Var | Tok_Const | Tok_Event
              | Tok_Static | Tok_Automatic =>
               Parse_Variable_Declarations (Decl_Constr);
            when Tok_Typedef =>
               Append_Node (Decl_Constr, Parse_Type_Declaration);
            when Tok_Identifier =>
               --  1800-2017 6.8 Variable declarations
               --  Note 10) In a data declaration, it shall be illegal to omit
               --  the explicit data_type before a
               --  list_of_variable_decl_assignments unless the VAR keyword is
               --  used.
               Decl_Type := Parse_Data_Type_Or_Implicit;

               case Current_Token is
                  when Tok_Identifier =>
                     --  A declaration.
                     Parse_Variable_Declarations
                       (Decl_Constr, Decl_Type,
                        False, False, (Life_Static, False));
                  when Tok_Less_Equal
                    | Tok_Plus_Plus
                    | Tok_Minus_Minus
                    | Tok_Equal
                    | Toks_Op_Asgn
                    | Tok_Less_Plus
                    | Tok_Left_Paren
                    | Tok_Dot
                    | Tok_Semicolon =>
                     Stmt := Parse_Statement_Name
                       (Data_Type_To_Name (Decl_Type));

                     --  Skip ';'.
                     Scan_Statement_Semicolon;

                     if Stmt /= Null_Node then
                        Set_Parent (Stmt, Parent);
                        Append_Chain (First_Stmt, Last_Stmt, Stmt);
                     end if;
                     exit;
                  when Tok_Colon =>
                     Stmt := Parse_Statement_Label
                       (Data_Type_To_Name (Decl_Type));

                     if Stmt /= Null_Node then
                        Set_Parent (Stmt, Parent);
                        Append_Chain (First_Stmt, Last_Stmt, Stmt);
                     end if;
                     exit;
                  when others =>
                     Error_Msg_Parse
                       ("variable declaration or assignment expected");
                     Skip_Until_Semicolon;
               end case;
            when others =>
               exit;
         end case;
      end loop;
      First_Decl := Get_Constr_Chain (Decl_Constr);

      --  Statements.
      loop
         case Current_Token is
            when Tok_End
              | Tok_Join
              | Tok_Join_None
              | Tok_Join_Any
              | Tok_Endtask
              | Tok_Endfunction
              | Tok_Eof =>
               exit;
            when others =>
               null;
         end case;
         Stmt := Parse_Statement_Or_Null (Parent);
         if Stmt /= Null_Node then
            Append_Chain (First_Stmt, Last_Stmt, Stmt);
         end if;
      end loop;
   end Parse_Block_Item_Declaration_Statement;

   procedure Parse_Tf_Items (Parent : Node)
   is
      First_Decl : Node;
      First_Stmt : Node;
   begin
      Parse_Block_Item_Declaration_Statement (Parent, First_Decl, First_Stmt);
      Set_Tf_Item_Declaration_Chain (Parent, First_Decl);
      Set_Statements_Chain (Parent, First_Stmt);
   end Parse_Tf_Items;

   --  1800-2017 13.3 Tasks
   --    ... [ interface_identifier . | class_scope ] ...
   function Parse_Subroutine_Name (Kind : Nkind; OOB_Kind : Nkind) return Node
   is
      Res : Node;
      Loc : Location_Type;
      Prefix : Node;
      Id : Name_Id;
   begin
      if Current_Token /= Tok_Identifier and then Current_Token /= Tok_New
      then
         Error_Msg_Parse
           ("identifier required for a task/function declaration");
         Res := Create_Node (Kind);
         Set_Location (Res, Get_Token_Location);
         return Res;
      end if;

      Id := Current_Identifier;
      Loc := Get_Token_Location;

      --  Skip identifier
      Scan;

      case Current_Token is
         when Tok_Dot =>
            --  TODO: N_Interface_Function/N_Interface_Task
            raise Internal_Error;
         when Tok_Colon_Colon =>
            Res := Create_Node (OOB_Kind);
         when others =>
            Res := Create_Node (Kind);
            Set_Location (Res, Loc);
            Set_Identifier (Res, Id);
            return Res;
      end case;

      --  Create prefix node.
      Prefix := Create_Node (N_Name);
      Set_Identifier (Prefix, Id);
      Set_Location (Prefix, Loc);

      Set_OOB_Prefix (Res, Prefix);

      --  Skip '.' or '::'.
      Scan;

      if Current_Token = Tok_Identifier or Current_Token = Tok_New then
         Set_Identifier (Res, Current_Identifier);
         Set_Token_Location (Res);

         --  Skip identifier.
         Scan;
      else
         Error_Msg_Parse ("task/function identifier expected");
      end if;

      return Res;
   end Parse_Subroutine_Name;

   --  precond:  TASK
   --  postcond: next token
   --
   --  1800-2017 13.3 Tasks
   function Parse_Task_Declaration (Prototype : Boolean := False) return Node
   is
      Res : Node;
      Lifetime : Lifetime_State;
   begin
      --  Skip 'task'.
      Scan;

      Lifetime := Parse_Lifetime;

      Res := Parse_Subroutine_Name (N_Task, N_OOB_Task);
      Set_Lifetime (Res, Lifetime);

      if Current_Token = Tok_Left_Paren then
         Set_Ansi_Port_Flag (Res, True);

         Set_Tf_Ports_Chain (Res, Parse_Tf_Port_List (Res));
      end if;

      Scan_Or_Error (Tok_Semicolon,
                     "';' expected before task item declarations");

      if Prototype then
         return Res;
      end if;

      --  task item declarations.
      Parse_Tf_Items (Res);

      --  Skip 'endtask'.
      Scan_Or_Error (Tok_Endtask,
                     "'endtask' expected at end of task");
      Parse_End_Name (Res);

      return Res;
   end Parse_Task_Declaration;

   --  precond:  FUNCTION
   --  postcond: next token
   --
   --  1800-2017 13.4 Functions
   --  function_declaration ::=
   --    FUNCTION [ lifetime ] function_body_declaration
   --
   --  function_body_declaration ::=
   --      function_data_type_or_implicit
   --        [ interface_identifier . | class_scope ] function_identifier ;
   --      { tf_item_declaration }
   --      { function_statement_or_null }
   --      ENDFUNCTION [ : function_identifier
   --    | funtion_data_type_or_implicit
   --        [ interface_identifier . | class_scope ] function_identifier
   --        ( [ tf_port_list ] ) ;
   --      { block_item_declaration }
   --      { function_statement_or_null }
   --      ENDFUNCTION [ : function_identifier ]
   --
   --  function_data_type_or_implicit ::=
   --      data_type_or_void
   --    | implicit_data_type
   function Parse_Function_Declaration (Prototype : Boolean := False)
                                       return Node
   is
      Res : Node;
      Lifetime : Lifetime_State;
      Atype : Type_Node;
      Pfx : Node;
   begin
      --  Skip 'function'.
      Scan;

      Lifetime := Parse_Lifetime;

      Atype := Parse_Data_Type_Or_Implicit;

      case Current_Token is
         when Tok_Identifier
           | Tok_New =>
            Res := Parse_Subroutine_Name (N_Function, N_OOB_Function);

         when others =>
            --  No type.
            --  1800-2017 13.4 Functions
            --  When the implicit syntax is used, the return type is the same
            --  as if the implicit syntax had been immediately preceded by the
            --  LOGIC keyword.  In particular, the implicit syntax can be
            --  empty, in which case the return type is a LOGIC scalar.
            if Atype.Typ /= Null_Node then
               case Get_Kind (Atype.Typ) is
                  when N_Name =>
                     Res := Create_Node (N_Function);
                     Location_Copy (Res, Atype.Typ);
                     Data_Type_To_Identifier (Res, Atype);
                  when N_Scoped_Name =>
                     Res := Create_Node (N_OOB_Function);
                     Location_Copy (Res, Atype.Typ);
                     Set_Identifier (Res, Get_Identifier (Atype.Typ));
                     Pfx := Get_Name (Atype.Typ);
                     pragma Assert (Get_Kind (Pfx) = N_Name);
                     Set_OOB_Prefix (Res, Pfx);
                     Free_Node (Atype.Typ);
                  when others =>
                     raise Internal_Error;
               end case;
               Atype := (Implicit_Typedef, False);
            else
               Error_Msg_Parse
                 ("identifier required for a function declaration");
               Res := Create_Node (N_Function);
               Set_Token_Location (Res);
            end if;
      end case;

      Set_Type_Node (Res, Atype);
      Set_Lifetime (Res, Lifetime);

      if Current_Token = Tok_Left_Paren then
         Set_Ansi_Port_Flag (Res, True);

         Set_Tf_Ports_Chain (Res, Parse_Tf_Port_List (Res));
      end if;

      --  Skip ';'.
      Scan_Or_Error (Tok_Semicolon,
                     "';' expected before function item declarations");

      if Prototype then
         return Res;
      end if;

      Parse_Tf_Items (Res);

      --  Skip 'endfunction'.
      Scan_Or_Error (Tok_Endfunction,
                     "'endfunction' expected at end of function");
      Parse_End_Name (Res);

      return Res;
   end Parse_Function_Declaration;

   --  1800-2017 35.5.4 Import declarations
   --  dpi_string_string ::= "DPI-C" | "DPI"
   function Parse_DPI_Spec_String return DPI_Spec_Type
   is
      Str : constant String :=
        Str_Table.String_String8 (Current_String, Int32 (Current_String_Len));
      Res : DPI_Spec_Type;
   begin
      if Str = "DPI-C" then
         Res := DPI_DPI_C;
      elsif Str = "DPI" then
         Res := DPI_DPI;
      else
         Error_Msg_Parse
           ("DPI spec string must be either ""DPI-C"" or ""DPI""");
         Res := DPI_Unknown;
      end if;

      --  Skip string.
      Scan;

      return Res;
   end Parse_DPI_Spec_String;

   --  1800-2017 35.5.4 Import declarations
   --  dpi_import_export ::=
   --      IMPORT dpi_spec_string [ dpi_function_import_property ]
   --        [ c_identifier = ] dpi_function_proto
   --    | IMPORT dpi_spec_string [ dpi_task_import_property ]
   --        [ c_identifier = ] dpi_task_proto
   --    | ...
   function Parse_DPI_Import_Declaration return Node
   is
      Res : Node;
      Spec : DPI_Spec_Type;
      Context : Boolean;
      Pure : Boolean;
      C_Id : Name_Id;
      Atype : Type_Node;
   begin
      pragma Assert (Current_Token = Tok_String_Literal);

      Spec := Parse_DPI_Spec_String;

      --  Import property.
      Context := False;
      Pure := False;
      case Current_Token is
         when Tok_Context =>
            Context := True;

            --  Skip 'context'.
            Scan;
         when Tok_Pure =>
            Pure := True;

            --  Skip 'pure'.
            Scan;
         when others =>
            null;
      end case;

      --  C identifier
      if Current_Token = Tok_Identifier then
         C_Id := Current_Identifier;

         --  Skip identifier.
         Scan;

         Scan_Or_Error (Tok_Equal, "'=' expected after c_identifier");
      else
         C_Id := Null_Identifier;
      end if;

      case Current_Token is
         when Tok_Function =>
            Res := Create_Node (N_Import_DPI_Function);
            Set_C_Identifier (Res, C_Id);
            Set_DPI_Spec (Res, Spec);
            Set_Pure_Property (Res, Pure);
            Set_Context_Property (Res, Context);

            --  Skip 'function'.
            Scan;

            Atype := Parse_Data_Type_Or_Implicit;
            Set_Type_Node (Res, Atype);

         when Tok_Task =>
            raise Program_Error;

         when others =>
            Error_Msg_Parse ("'function' or 'task' expected");
            Skip_Until_Semicolon;
            return Null_Node;
      end case;

      if Current_Token = Tok_Identifier then
         Set_Identifier (Res, Current_Identifier);
         Set_Token_Location (Res);

         --  Skip identifier.
         Scan;
      else
         Error_Msg_Parse ("function/task identifier required");
      end if;

      Set_Ansi_Port_Flag (Res, True);

      if Current_Token = Tok_Left_Paren then
         Set_Tf_Ports_Chain (Res, Parse_Tf_Port_List (Res));
      end if;

      --  Skip ';'.
      Scan_Or_Error
        (Tok_Semicolon, "';' expected at end of import declaration");

      return Res;
   end Parse_DPI_Import_Declaration;

   --  1800-2017 26.3 Referencing data in packages
   --  package_import_declaration ::=
   --    IMPORT package_import_item { , package_import_item } ;
   --
   --  package_import_item ::=
   --      package_identifier :: identifier
   --    | package_identifier :: *
   procedure Parse_Package_Import_Declaration (Constr : in out Items_Constr)
   is
      Decl : Node;
      Prefix, Name : Node;
   begin
      loop
         Decl := Create_Node (N_Package_Import);
         Set_Token_Location (Decl);

         if Current_Token = Tok_Identifier then
            Prefix := Scan_Name;
         else
            Error_Msg_Parse ("identifier expected for package import");
            Prefix := Null_Node;
         end if;

         Scan_Or_Error
           (Tok_Colon_Colon, "'::' expected after package identifier");

         if Current_Token = Tok_Star then
            Name := Create_Node (N_Wildcard_Name);
            Set_Token_Location (Name);
            Set_Name (Name, Prefix);

            --  Skip '*'.
            Scan;
         elsif Current_Token = Tok_Identifier then
            Name := Create_Node (N_Scoped_Name);
            Set_Token_Location (Name);
            Set_Identifier (Name, Current_Identifier);
            Set_Name (Name, Prefix);

            --  Skip identifier.
            Scan;
         else
            Error_Msg_Parse ("identifier or '*' expected after '::'");
            Name := Null_Node;
         end if;

         Set_Item_Name (Decl, Name);
         Append_Node (Constr, Decl);

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
      end loop;

      Scan_Declaration_Semicolon;
   end Parse_Package_Import_Declaration;

   --  For module/interface/program header.
   function Parse_Package_Import_Chain (Parent : Node) return Node
   is
      Items : Items_Constr;
   begin
      if Current_Token /= Tok_Import then
         return Null_Node;
      end if;

      --  Skip 'import'
      Scan;

      Init_Constr (Items, Parent);
      Parse_Package_Import_Declaration (Items);
      return Get_Constr_Chain (Items);
   end Parse_Package_Import_Chain;

   procedure Parse_Import_Declaration (Constr : in out Items_Constr) is
   begin
      --  Skip 'import'.
      Scan;

      case Current_Token is
         when Tok_Identifier =>
            Parse_Package_Import_Declaration (Constr);
         when Tok_String_Literal =>
            Append_Node (Constr, Parse_DPI_Import_Declaration);
         when others =>
            Error_Msg_Parse ("package identifier or DPI spec string expected");
            Skip_Until_Semicolon;
      end case;
   end Parse_Import_Declaration;

   --  1800-2017 35.5.4 Import declarations
   --  dpi_import_export ::=
   --    | ...
   --    | EXPORT dpi_spec_string [ c_identifier = ]
   --        FUNCTION function_identifier ;
   --    | EXPORT dpi_spec_string [ c_identifier = ]
   --        TASK function_identifier ;
   function Parse_DPI_Export_Declaration return Node
   is
      Res : Node;
      Spec : DPI_Spec_Type;
      C_Id : Name_Id;
   begin
      --  Skip 'export'.
      Scan;

      if Current_Token /= Tok_String_Literal then
         Error_Msg_Parse ("DPI spec string expected after 'export'");
         Spec := DPI_Unknown;
      else
         Spec := Parse_DPI_Spec_String;
      end if;

      --  C identifier
      if Current_Token = Tok_Identifier then
         C_Id := Current_Identifier;

         --  Skip identifier.
         Scan;

         Scan_Or_Error (Tok_Equal, "'=' expected after c_identifier");
      else
         C_Id := Null_Identifier;
      end if;

      case Current_Token is
         when Tok_Function =>
            Res := Create_Node (N_Export_DPI_Function);
         when Tok_Task =>
            Res := Create_Node (N_Export_DPI_Task);
         when others =>
            Error_Msg_Parse ("'function' or 'task' expected");
            Skip_Until_Semicolon;
            return Null_Node;
      end case;

      Set_C_Identifier (Res, C_Id);
      Set_DPI_Spec (Res, Spec);

      --  Skip 'task'/'function'.
      Scan;

      if Current_Token = Tok_Identifier then
         Set_Identifier (Res, Current_Identifier);
         Set_Token_Location (Res);

         --  Skip identifier.
         Scan;
      else
         Error_Msg_Parse ("function/task identifier required");
      end if;

      --  Skip ';'.
      Scan_Or_Error
        (Tok_Semicolon, "';' expected at end of export declaration");

      return Res;
   end Parse_DPI_Export_Declaration;

   procedure Parse_Parameter_Override (Constr : in out Items_Constr)
   is
      Res : Node;
   begin
      --  Skip 'defparam'.
      Scan;

      loop
         Res := Create_Node (N_Defparam);
         Set_Token_Location (Res);
         Set_Lvalue (Res, Parse_Lvalue);
         if Current_Token /= Tok_Equal then
            Error_Msg_Parse ("'=' required after parameter");
         else
            Scan;
         end if;
         Set_Expression (Res, Parse_Expression);
         Append_Node (Constr, Res);
         exit when Current_Token /= Tok_Comma;
         Scan;
      end loop;

      --  Skip ';'.
      Scan_Statement_Semicolon;
   end Parse_Parameter_Override;

   --  1800-2017 30.5 Assigning delays to module paths
   --  path_delay_value ::=
   --      list_of_path_delay_expressions
   --    | ( list_of_path_delay_expressions )
   --
   --  list_of_path_delay_expressions ::=
   --      t_path_delay_expression
   --    | trise_path_delay_expression , tfall_path_delay_expression
   --    | trise_path_delay_expression , tfall_path_delay_expression ,
   --      tz_path_delay_expression
   --    | t01_path_delay_expression , t10_path_delay_expression ,
   --      t0z_path_delay_expression , tz1_path_delay_expression ,
   --      t1z_path_delay_expression , tz0_path_delay_expression
   --    | t01_path_delay_expression , t10_path_delay_expression ,
   --      t0z_path_delay_expression , tz1_path_delay_expression ,
   --      t1z_path_delay_expression , tz0_path_delay_expression ,
   --      t0x_path_delay_expression , tx1_path_delay_expression ,
   --      t1x_path_delay_expression , tx0_path_delay_expression ,
   --      txz_path_delay_expression , tzx_path_delay_expression
   function Parse_Path_Delay return Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Res : Node;
      T : Node_Array (1 .. 12);
      Num : Nat32;
   begin
      T := (others => Null_Node);

      --  Parse the first delay.
      T (1) := Parse_Expression (Prio_Paren);
      Num := 1;

      --  Parse up to 12 delays.
      for I in Int32 range 2 .. 12 loop
         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;

         T (I) := Parse_Expression (Prio_Paren);
         Num := I;
      end loop;

      if Num <= 3 then
         Res := Create_Node (N_Path_Delay3);
         Set_Location (Res, Loc);
         Set_Delay_Rise (Res, T (1));
         Set_Delay_Fall (Res, T (2));
         Set_Delay_Z (Res, T (3));

         return Res;
      end if;

      if Num /= 6 and Num /= 12 then
         Error_Msg_Parse ("missing delays (1, 2, 3, 6 or 12 required)");
      end if;

      if Num <= 6 then
         Res := Create_Node (N_Path_Delay6);
      else
         Res := Create_Node (N_Path_Delay12);
      end if;

      Set_Location (Res, Loc);
      Set_Delay_01 (Res, T (1));
      Set_Delay_10 (Res, T (2));
      Set_Delay_0z (Res, T (3));
      Set_Delay_z1 (Res, T (4));
      Set_Delay_1z (Res, T (5));
      Set_Delay_z0 (Res, T (6));

      if Num > 6 then
         Set_Delay_0x (Res, T (7));
         Set_Delay_x1 (Res, T (8));
         Set_Delay_1x (Res, T (9));
         Set_Delay_x0 (Res, T (10));
         Set_Delay_xz (Res, T (11));
         Set_Delay_zx (Res, T (12));
      end if;

      return Res;
   end Parse_Path_Delay;

   --  1800-2017 30.4.2 Simple module paths
   --  list_of_path_inputs ::=
   --    specify_input_terminal_descriptor
   --      { , specify_input_terminal_descriptor }
   --
   --  list_of_path_outputs ::=
   --    specify_output_terminal_descriptor
   --      { , specify_output_terminal_descriptor }
   --
   --  specify_input_terminal_descriptor ::=
   --    input_identifier [ '[' constant_range_expression ']' ]
   --
   --  specify_output_terminal_descriptor ::=
   --    output_identifier [ '[' constant_range_expression ']' ]
   function Parse_List_Of_Path return Node
   is
      Res : Node;
      First, Last : Node;
   begin
      Res := Parse_Lvalue;
      if Current_Token /= Tok_Comma then
         return Res;
      end if;
      First := Create_Node (N_Path_Element);
      Set_Lvalue (First, Res);
      Last := First;
      loop
         --  Skip ','.
         Scan;

         Res := Create_Node (N_Path_Element);
         Set_Lvalue (Res, Parse_Lvalue);
         Set_Chain (Last, Res);
         Last := Res;
         exit when Current_Token /= Tok_Comma;
      end loop;
      return First;
   end Parse_List_Of_Path;

   --  1800-2017 30.4.2 Simple module paths
   --  polarity_operator ::= + | -
   function Parse_Polarity return Polarity_Type is
   begin
      case Current_Token is
         when Tok_Plus =>
            Scan;
            return Polarity_Positive;
         when Tok_Minus =>
            Scan;
            return Polarity_Negative;
         when others =>
            return Polarity_Unknown;
      end case;
   end Parse_Polarity;

   function Parse_Polarity_Colon return Polarity_Type
   is
      Res : Polarity_Type;
   begin
      case Current_Token is
         when Tok_Plus =>
            --  Skip '+'.
            Scan;

            Res := Polarity_Positive;

         when Tok_Plus_Colon =>
            --  Skip '+:'.
            Scan;

            return Polarity_Positive;

         when Tok_Minus =>
            --  Skip '-'.
            Scan;

            Res := Polarity_Negative;

         when Tok_Minus_Colon =>
            --  Skip '-:'.
            Scan;

            return Polarity_Negative;

         when others =>
            Res := Polarity_Unknown;
      end case;

      --  Skip ':'.
      Scan_Or_Error (Tok_Colon, "':' expected before data source");

      return Res;
   end Parse_Polarity_Colon;

   --  1800-2017 30.4 Module path declarations
   --  path_declaration ::=
   --      simple_path_declaration ;
   --    | edge_sensitive_path_declaration ;
   --    | state_dependent_path_declaration ;
   --
   --  simple_path_declaration ::=
   --      parallel_path_description = path_delay_value
   --    | full_path_description = path_delay_value
   --
   --  parallel_path_desription ::=
   --    ( specify_input_terminal_desriptor [ polarity_operator ] =>
   --      specify_output_terminal_descriptor )
   --
   --  full_path_description ::=
   --    ( list_of_path_inputs [ polarity_operator ] *> list_of_path_outputs )
   function Parse_Path_Declaration return Node
   is
      Res : Node;
      Loc : Location_Type;
      Inputs : Node;
      Polarity : Polarity_Type;
      Edge : Edge_Type;
      pragma Unreferenced (Edge);
   begin
      Loc := Get_Token_Location;

      --  Skip '('.
      Scan;

      --  Edge identifier
      case Current_Token is
         when Tok_Posedge =>
            Edge := Edge_Posedge;
            Scan;
         when Tok_Negedge =>
            Edge := Edge_Negedge;
            Scan;
         when others =>
            Edge := Edge_None;
            null;
      end case;

      Inputs := Parse_List_Of_Path;

      Polarity := Parse_Polarity;

      case Current_Token is
         when Tok_Full_Conn =>
            --  Skip '*>'.
            Scan;

            if Current_Token = Tok_Left_Paren then
               Res := Create_Node (N_Full_Edge_Path);
            else
               Res := Create_Node (N_Full_Path);
            end if;

         when Tok_Par_Conn =>
            --  Skip '=>'.
            Scan;

            if Current_Token = Tok_Left_Paren then
               Res := Create_Node (N_Par_Edge_Path);
            else
               Res := Create_Node (N_Par_Path);
            end if;
         when others =>
            Error_Msg_Parse ("'=>' or '*>' expected");
            Res := Create_Node (N_Full_Path);
      end case;

      Set_Location (Res, Loc);
      Set_Specify_Input (Res, Inputs);

      if Current_Token = Tok_Left_Paren then
         --  Skip '('.
         Scan;

         Set_Specify_Output (Res, Parse_List_Of_Path);
         Set_Polarity (Res, Parse_Polarity_Colon);

         Set_Data_Source (Res, Parse_Expression);

         --  Skip ')'.
         Scan_Or_Error (Tok_Right_Paren, "')' expected after data source");
      else
         Set_Polarity (Res, Polarity);
         Set_Specify_Output (Res, Parse_List_Of_Path);
      end if;

      --  Skip ')'.
      Scan_Or_Error (Tok_Right_Paren, "')' expected after path description");

      --  Skip '='.
      Scan_Or_Error (Tok_Equal, "'=' expected before path delay value");

      if Current_Token = Tok_Left_Paren then
         --  Skip '('.
         Scan;

         Set_Path_Delay (Res, Parse_Path_Delay);

         --  Skip ')'.
         Scan_Or_Error (Tok_Right_Paren, "missing ')'");
      else
         Set_Path_Delay (Res, Parse_Path_Delay);
      end if;

      --  Skip ';'.
      Scan_Declaration_Semicolon;

      return Res;
   end Parse_Path_Declaration;

   --  1800-2017 31 Timing checks
   function Parse_Timing_Task return Node
   is
      Res : Node;

      Last : Node;
      Arg : Node;
   begin
      Res := Create_Node (N_Timing_Check);
      Set_Token_Location (Res);
      Set_Identifier (Res, Current_Identifier);

      --  Skip identifier.
      Scan;

      --  Skip '('.
      Scan_Or_Error (Tok_Left_Paren, "'(' expected after timing check task");

      Last := Null_Node;
      loop
         Arg := Create_Node (N_Argument);
         Set_Token_Location (Arg);
         if Current_Token /= Tok_Comma and Current_Token /= Tok_Right_Paren
         then
            Set_Expression (Arg, Parse_Event_Expression);
         end if;
         if Last = Null_Node then
            Set_Arguments (Res, Arg);
         else
            Set_Chain (Last, Arg);
         end if;
         Last := Arg;
         exit when Current_Token /= Tok_Comma;
         --  Skip ','.
         Scan;
      end loop;

      --  Skip ')'.
      Scan_Or_Error (Tok_Right_Paren, "')' expected at end of arguments");

      --  Skip ';'.
      Scan_Statement_Semicolon;

      return Res;
   end Parse_Timing_Task;

   --  1364-2005 4.10.3 Specify parameters
   --  specparam_declaration ::=
   --    SPECPARAM [ range ] list_of_specparam_assignments ;
   --  list_of_specparam_assignments ::=
   --    specparam_assignment { , specparam_assignment }
   --  specparam_assignment ::=
   --      specparam_identifier = constant_mintypmax_expression
   --    | pulse_control_specparam
   --  pulse_control_specparam ::=
   --      PATHPULSE$ = ( reject_limit_value [, error_limit_value ] )
   --    | PATHPULSE$input_terminal$output_terminal =
   --        ( reject_limit_value [, error_limit_value ] )
   --  error_limit_value ::= limit_value
   --  reject_limit_value ::= limit_value
   --  limit_value ::= constant_mintypmax_expression
   --
   --  Note:
   --  input_terminal is specify_input_terminal_descriptor
   --  output_terminal is specify_output_terminal_descriptor
   procedure Parse_Specparam_Declaration (Constr : in out Items_Constr)
   is
      Res : Node;
   begin
      --  Skip 'specparam'.
      Scan;

      loop
         if Current_Token /= Tok_Identifier then
            Error_Msg_Parse ("specparam identifier expected");
            Skip_Until_Semicolon;
            return;
         end if;
         if Current_Dollar_In_Id then
            --  TODO: Check PATHPULSE$ prefix
            Res := Create_Node (N_Pulse_Control_Specparam);
            Set_Token_Location (Res);
            Set_Identifier (Res, Current_Identifier);
            Scan;

            if Current_Token /= Tok_Equal then
               Error_Msg_Parse ("'=' expected after specparam identifier");
            else
               Scan;
            end if;

            if Current_Token /= Tok_Left_Paren then
               Error_Msg_Parse ("'(' expected for pulse control value");
            else
               Scan;
            end if;

            Set_Reject_Limit (Res, Parse_Expression (Prio_Paren));

            if Current_Token = Tok_Comma then
               Scan;
               Set_Error_Limit (Res, Parse_Expression (Prio_Paren));
            end if;

            if Current_Token /= Tok_Right_Paren then
               Error_Msg_Parse ("')' expected at end of value");
            else
               Scan;
            end if;
         else
            Res := Create_Node (N_Specparam);
            Set_Token_Location (Res);
            Set_Identifier (Res, Current_Identifier);
            Scan;

            if Current_Token /= Tok_Equal then
               Error_Msg_Parse ("'=' expected after specparam identifier");
            else
               Scan;
               Set_Expression (Res, Parse_Expression (Prio_Paren));
            end if;
         end if;

         Append_Node (Constr, Res);
         exit when Current_Token /= Tok_Comma;
         Scan;
      end loop;

      --  Skip ';'.
      Scan_Declaration_Semicolon;
   end Parse_Specparam_Declaration;

   --  1800-2017 30.4.4 State-dependent paths
   --  state_dependent_path_declaration ::=
   --      IF ( module_path_expression ) simple_path_declaration
   --    | IF ( module_path_expression ) edge_sensitive_path_declaration
   --    | IFNONE simple_path_declaration
   function Parse_State_Dependent_Path_Declaration_If return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_If);
      Set_Token_Location (Res);

      --  Skip 'if'.
      Scan;

      --  Skip '('.
      Scan_Or_Error (Tok_Left_Paren, "'(' expected after 'if'");

      Set_Condition (Res, Parse_Expression);

      --  Skip ')'.
      Scan_Or_Error (Tok_Right_Paren, "')' expected after 'if' condition");

      Set_True_Stmt (Res, Parse_Path_Declaration);
      return Res;
   end Parse_State_Dependent_Path_Declaration_If;

   --  1800-2017 30.4.4 State-dependent paths
   --  state_dependent_path_declaration ::=
   --      [...]
   --    | IFNONE simple_path_declaration
   function Parse_State_Dependent_Path_Declaration_Ifnone return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Ifnone);
      Set_Token_Location (Res);

      --  Skip 'ifnone'.
      Scan;

      Set_True_Stmt (Res, Parse_Path_Declaration);
      return Res;
   end Parse_State_Dependent_Path_Declaration_Ifnone;

   --  1800-2017 30.3 Specify block declaration
   --  specify_block ::= SPECIFY { specify_item } ENDSPECIFY
   --
   --  specify_item ::=
   --      specparam_declaration
   --    | pulsestyle_declaration
   --    | showcanceled_declaration
   --    | path_declaration
   --    | system_timing_check
   function Parse_Specify_Block return Node
   is
      Res : Node;
      Constr : Items_Constr;
   begin
      Res := Create_Node (N_Specify);
      Set_Token_Location (Res);

      --  Skip 'specify'.
      Scan;

      Init_Constr (Constr, Res);
      loop
         case Current_Token is
            when Tok_Endspecify
              | Tok_Eof =>
               exit;
            when Tok_Left_Paren =>
               Append_Node (Constr, Parse_Path_Declaration);
            when Tok_System =>
               Append_Node (Constr, Parse_Timing_Task);
            when Tok_Specparam =>
               Parse_Specparam_Declaration (Constr);
            when Tok_If =>
               Append_Node (Constr, Parse_State_Dependent_Path_Declaration_If);
            when Tok_Ifnone =>
               Append_Node (Constr,
                            Parse_State_Dependent_Path_Declaration_Ifnone);
            when others =>
               Error_Msg_Parse ("unknown specify item");
               Skip_Until_Semicolon;
         end case;
      end loop;
      Set_Specify_Item_Chain (Res, Get_Constr_Chain (Constr));

      --  Skip 'endspecify'.
      Scan_Or_Error (Tok_Endspecify,
                     "missing 'endspecify' at end of specify block");

      return Res;
   end Parse_Specify_Block;

   --  precond:  GENVAR
   --
   --  1364-2005 12.4 Generate constructs
   --  genvar_declaration ::=
   --    GENVAR list_of_genvar_identifier ;
   --
   --  list_of_genvar_identifiers ::=
   --    genvar_identifier { , genvar_identifier }
   procedure Parse_Genvar_Declaration (Constr : in out Items_Constr)
   is
      N : Node;
   begin
      --  Skip 'genvar'.
      Scan;

      loop
         N := Create_Node (N_Genvar);
         Set_Token_Location (N);

         --  Skip identifier.
         Scan_Identifier (N, "identifier expected after genvar");

         Append_Node (Constr, N);

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
      end loop;

      --  Skip ';'.
      Scan_Or_Error (Tok_Semicolon,
                     "';' expected at end of genvar declaration");
   end Parse_Genvar_Declaration;

   --  1800-2017 23.2.2.1 Non-ANSI style port declarations
   --  port_reference ::= port_identifier constant_select
   function Parse_Port_Reference return Node
   is
      Res : Node;
   begin
      --  Skip identifier.
      Res := Scan_Name;

      --  handle constant_range_expression.
      if Current_Token = Tok_Left_Brack then
         declare
            Pfx : constant Node := Res;
            Loc : Location_Type;
            Msb : Node;
         begin
            Loc := Get_Token_Location;

            --  Skip '['.
            Scan;

            Msb := Parse_Expression;

            if Current_Token = Tok_Colon then
               Res := Create_Node (N_Part_Select);

               Set_Msb (Res, Msb);

               --  Skip ':'.
               Scan;

               Set_Lsb (Res, Parse_Expression);
            else
               Res := Create_Node (N_Bit_Select);
               Set_Expression (Res, Msb);
            end if;

            Set_Location (Res, Loc);
            Set_Name (Res, Pfx);

            Scan_Or_Error (Tok_Right_Brack, "']' expected at end of range");
         end;
      end if;

      return Res;
   end Parse_Port_Reference;

   --  1800-2017 23.2.2.1 Non-ANSI style port declarations
   --  port_expression ::=  port_reference
   --                     | { port_reference { , port_reference } }
   function Parse_Port_Expression return Node
   is
      Res : Node;
      El : Node;
      Last_El : Node;
   begin
      if Current_Token = Tok_Left_Curly then
         Res := Create_Node (N_Concatenation);
         Set_Token_Location (Res);

         --  Skip '{'.
         Scan;

         El := Create_Node (N_Element);
         Set_Token_Location (El);
         Set_Expressions (Res, El);
         Set_Expression (El, Parse_Port_Reference);
         Last_El := El;

         loop
            exit when Current_Token /= Tok_Comma;

            El := Create_Node (N_Element);
            Set_Token_Location (El);
            Set_Chain (Last_El, El);
            Last_El := El;

            --  Skip ','.
            Scan;

            Set_Expression (El, Parse_Port_Reference);
         end loop;

         --  Skip '}'.
         Scan_Or_Error
           (Tok_Right_Curly, "'}' expected at end of concatenation");

         return Res;
      else
         return Parse_Port_Reference;
      end if;
   end Parse_Port_Expression;

   --  Pre:  next token.
   --
   --  Parse of list_of_ports or a list_of_port_declarations.
   --  The syntax of them is very similar, hence this special procedure.
   --  Can be ANSI or non-ANSI style.
   --
   --  1800-2017 23.2.2.1 Non-ANSI style port declarations
   --  list_of_ports ::= ( port { , port } )
   --
   --  port ::=   [ port_expression ]
   --           | . port_identifier ( [ port_expression ] )
   --
   --  port_expression ::=  port_reference
   --                     | { port_reference { , port_reference } }
   --
   --  port_reference ::= port_identifier constant_select
   --
   --  1800-2017 23.2.2.2 ANSI style list of port declarations
   --  list_of_port_declarations ::=
   --      ( [ { attribute_instance } ansi_port_declaration
   --        { , { attribute_instance } ansi_port_declaration } ] )
   --
   --  ansi_port_declaration ::=
   --      [ net_port_header | interface_port_header ] port_identifier
   --        { unpacked_dimension } [ = constant_expression ]
   --    | [ variable_port_header ] port_identifier
   --        { variable_dimension } [ = constant_expression ]
   --    | [ port_direction ] . port_identifier ( [ expression ] )
   --
   --  net_port_header ::= [ port_direction ] net_port_type
   --
   --  variable_port_header ::= [ port_direction ] variable_port_type
   --
   --  interface_port_header ::=
   --      interface_identifier [ . modport_identifier ]
   --    | INTERFACE [ . modport_identifier ]
   --
   --  net_port_type ::=
   --      [ net_type ] data_type_or_implicit
   --    | net_type_identifier
   --    | INTERCONNECT implicit_data_type
   --
   --  variable_port_type ::= var_data_type
   --
   --  var_data_type ::= data_type | VAR data_type_or_implicit
   procedure Parse_Ports_List1 (Parent : Node)
   is
      Head, Last : Node;
      Port_Direction : Nkind;
      Port_Kind : Nkind;
      Port_Kind_Set : Boolean;
      Port_Type : Type_Node;
      Decl : Node;
      Port : Node;
      Is_Ansi : Tri_State_Type;
      El : Node;
      Port_Id : Name_Id;
      Port_Loc : Location_Type;
      Expr : Node;
      Name : Node;

      Attrs : Node;

      Last_Kind : Nkind;
      Last_Direction : Nkind;
      Last_Type : Type_Node;

      procedure Check_Mix (Ansi_Port : Tri_State_Type) is
      begin
         if Is_Ansi /= Unknown and Is_Ansi /= Ansi_Port then
            Error_Msg_Parse ("cannot mix ansi port and non-ansi port");
         end if;
         Is_Ansi := Ansi_Port;
      end Check_Mix;
   begin
      Last_Kind := N_Error;

      Init_Chain (Head, Last);
      Is_Ansi := Unknown;
      loop
         Port := Null_Node;
         Decl := Null_Node;

         Attrs := Parse_Attribute_Instances_Rep;

         --  port_direction.
         Port_Direction := Parse_Port_Direction_Opt;
         if Port_Direction /= N_Error then
            --  Port direction is present, so this is ANSI style.
            Check_Mix (Ansi_Port => True);
         end if;

         --  port_kind and/or port_type.
         case Current_Token is
            when Tok_Dot =>
               --  A port identifier (non-ANSI style).
               Check_Mix (Ansi_Port => False);
               Port := Create_Node (N_Port);
               Set_Token_Location (Port);
               Set_Parent (Port, Parent);

               --  Skip '.'.
               Scan;

               --  Skip identifier.
               Scan_Identifier (Port, "port identifier expected after '.'");

               if Current_Token /= Tok_Left_Paren then
                  Error_Msg_Parse ("'(' expected after port identifier");
               else
                  --  Skip '('.
                  Scan;

                  if Current_Token /= Tok_Right_Paren then
                     Set_Expression (Port, Parse_Port_Expression);
                  end if;

                  Scan_Or_Error
                    (Tok_Right_Paren, "missing ')' after port expression");
               end if;

            when Tok_Left_Curly =>
               --  A port expression (non-ANSI style).
               Check_Mix (Ansi_Port => False);
               Port := Create_Node (N_Port);
               Set_Token_Location (Port);
               Set_Parent (Port, Parent);
               Set_Expression (Port, Parse_Port_Expression);

            when Tok_Comma | Tok_Right_Paren =>
               --  A non-present port expression (non-ANSI style).
               Check_Mix (Ansi_Port => False);
               Port := Create_Node (N_Port);
               Set_Token_Location (Port);
               Set_Parent (Port, Parent);

            when Tok_Wire =>
               Check_Mix (Ansi_Port => True);
               Port := Null_Node;
               Port_Kind := N_Wire;
               Port_Kind_Set := True;

               --  Skip 'wire'.
               Scan;

               Port_Type := Parse_Data_Type_Or_Implicit;

            when Tok_Var =>
               Check_Mix (Ansi_Port => True);
               Port := Null_Node;
               Port_Kind := N_Var;
               Port_Kind_Set := True;

               --  Skip 'var'.
               Scan;

               Port_Type := Parse_Data_Type_Or_Implicit;
            when others =>
               --  TODO: other net type keywords.
               Port := Null_Node;
               Port_Kind := N_Error;
               Port_Kind_Set := False;

               Port_Type := Parse_Data_Type_Or_Implicit;
         end case;

         if Port = Null_Node then
            --  Maybe an ANSI style port.
            if Current_Token = Tok_Identifier then
               --  There is a port_identifier.

               Check_Mix (Ansi_Port => True);

               Port_Id := Current_Identifier;
               Port_Loc := Get_Token_Location;

               --  Skip identifier.
               Scan;
            elsif Port_Type.Typ = Null_Node then
               Error_Msg_Parse ("missing port identifier");
               Port_Id := Null_Identifier;
               Port_Loc := Get_Token_Location;
            else
               Port_Loc := Get_Location (Port_Type.Typ);
               Data_Type_To_Identifier (Port_Id, Port_Type);
            end if;

            --  Scan unpacked_dimension / variable_dimension.
            if Current_Token = Tok_Left_Brack then
               Port_Type := Parse_Variable_Dimension_Rep (Port_Type);
            end if;

            --  Scan default expression.
            if Current_Token = Tok_Equal then
               --  Skip '='.
               Scan;

               Expr := Parse_Expression;
            else
               Expr := Null_Node;
            end if;

            --  1800-2017 23.2.2.3 Rules for determining port kind, data type,
            --  and direction
            --
            --  If the direction, port kind, and data type are all omitted for
            --  the first port in the port list, then all ports shall be
            --  assumed to be non-ANSI tyle, and port direction and optional
            --  type declarations shall be declared after the port list.
            --  Otherwise, all ports shall be assumed to be ANSI style.
            --
            --  GHDL: what about expression ?

            if Is_Ansi = False
              or else (Head = Null_Node
                         and then Port_Direction = N_Error
                         and then Port_Kind = N_Error
                         and then Port_Type.Typ = Implicit_Typedef
                         and then Expr = Null_Node)
            then
               --  This is an implicit non-ANSI style port.
               Is_Ansi := False;

               Port := Create_Node (N_Port);
               Set_Location (Port, Port_Loc);
               Set_Parent (Port, Parent);

               Name := Create_Node (N_Name);
               Set_Location (Name, Port_Loc);
               Set_Identifier (Name, Port_Id);
               Set_Expression (Port, Name);
            end if;
         end if;

         if Port /= Null_Node then
            --  Non-ANSI style port
            if Attrs /= Null_Node then
               Error_Msg_Parse
                 (+Port, "attributes not allowed on non-ansi port ");
            end if;
            Append_Chain (Head, Last, Port);
         else
            --  This is an ANSI port.
            Check_Mix (Ansi_Port => True);
            if Head = Null_Node then
               --  1800-2017 23.2.2.3 Rules for determining port kind,
               --    data type, and direction
               --  For the first port in an ANSI style port list:
               --    - if the direction is omitted, it shall default to
               --      INOUT.
               if Port_Direction = N_Error then
                  Last_Direction := N_Inout;
               else
                  Last_Direction := Port_Direction;
               end if;

               --    - if the data type is omitted, it shall default to
               --      logic, except for interconnect ports, which have no
               --      data type.
               if Port_Type.Typ = Null_Node then
                  Last_Type := (Implicit_Typedef, False);
               else
                  Last_Type := Port_Type;
               end if;

               --    - if the port kind is omitted:
               --      [...  See below]
            else
               --  1800-2017 23.2.2.3 Rules for determining port kind,
               --    data type, and direction
               --  For subsequent ports in an ANSI style port list:
               --  - If the direction, port kind and data type are all omitted,
               --    then they shall be inherited from the previous port.  If
               --    the previous port was an interconnect port, this port
               --    shall also be an interconnect port.
               --  Otherwise:
               --  - If the direction is omitted, it shall be inherited from
               --    the previous port.
               --  - If the port kind is omitted, it shall be determined as
               --    previously specified.
               --  - If the data type is omitted, is shall default to logic
               --    except for interconnect ports that have no data type.
               if Port_Direction = N_Error
                 and then Port_Kind = N_Error
                 and then Port_Type.Typ = Null_Node
               then
                  --  Make as if port_kind was known.
                  Port_Kind_Set := True;
               else
                  if Port_Direction = N_Error then
                     --  Inherited.
                     pragma Assert (Last_Direction /= N_Error);
                     null;
                  else
                     Last_Direction := Port_Direction;
                  end if;

                  if Port_Type.Typ = Null_Node then
                     Last_Type := (Implicit_Typedef, False);
                  else
                     Last_Type := Port_Type;
                  end if;
               end if;
            end if;

            --  Common to first and subsequent ports.
            if not Port_Kind_Set then
               --  1800-2017 23.2.2.3 Rules for determining port kind,
               --    data type, and direction
               --  - if the port kind is omitted:
               --    * For input and inout ports, the port shall default to
               --      a net of default net type.  The default net type can
               --      be changed using the `default_nettype compiler
               --      directive.
               --    * For output ports, the default port kind depends on
               --      how the data type is specified:
               --      - if the data type is omitted or declared with the
               --        implicit_data_type syntax, the port kind shall
               --        default to a net a default net type.
               --      - if the data type is declared with the explicit
               --        data_type syntax, the port kind shall default to
               --        variable.
               case Nkinds_Net_Port (Last_Direction) is
                  when N_Input
                    | N_Inout =>
                     Last_Kind := N_Wire;
                  when N_Output =>
                     if Is_Implicit_Type (Last_Type.Typ) then
                        Last_Kind := N_Wire;
                     else
                        Last_Kind := N_Var;
                     end if;
               end case;
            else
               Last_Kind := Port_Kind;
            end if;

            pragma Assert (Last_Kind /= N_Error);
            pragma Assert (Last_Direction /= N_Error);

            --  Create the port declaration.
            El := Create_Node (Last_Direction);
            Set_Location (El, Port_Loc);
            Set_Parent (El, Parent);
            Set_Identifier (El, Port_Id);
            Set_Has_Direction (El, Port_Direction /= N_Error);
            Set_Complete_Flag (El, Port_Kind /= N_Error);
            Set_Type_Node (El, Last_Type);
            if Last_Direction = N_Input then
               Set_Default_Value (El, Expr);
            elsif Expr /= Null_Node then
               --  1800-2017 23.2.2.4 Default port values
               --  A module declaration may specify a default value for each
               --  singular input port.
               Error_Msg_Parse
                 (+Expr, "default port value allowed only for input ports");
            end if;
            Apply_Attributes (Attrs, El);

            --  Create the object.
            Decl := Create_Node (Last_Kind);
            Set_Location (Decl, Port_Loc);
            Set_Parent (Decl, Parent);
            Set_Identifier (Decl, Port_Id);
            if Port_Kind = N_Var then
               pragma Assert (Last_Kind = N_Var);
               Set_Has_Var (Decl, True);
            end if;

            Set_Redeclaration (El, Decl);
            Set_Redeclaration_Flag (Decl, True);

            Append_Chain (Head, Last, El);
         end if;

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
      end loop;

      pragma Assert (Is_Ansi /= Unknown);
      Set_Ansi_Port_Flag (Parent, Is_Ansi = True);

      Set_Ports_Chain (Parent, Head);
   end Parse_Ports_List1;

   --  Module ports list (either ANSI or non-ANSI style).
   procedure Parse_Ports_List (Parent : Node) is
   begin
      --  List_of_port_declarations is optional while list_of_ports must have
      --  at least one port.
      if Current_Token /= Tok_Left_Paren then
         --  No port declarations.
         Set_Ansi_Port_Flag (Parent, True);
         return;
      end if;

      --  Skip '('.
      Scan;

      if Current_Token = Tok_Right_Paren then
         Set_Ansi_Port_Flag (Parent, True);
      else
         Parse_Ports_List1 (Parent);
      end if;

      --  Skip ')'
      Scan_Or_Error (Tok_Right_Paren, "')' expected after ports");
   end Parse_Ports_List;

   --  1800-2017 14.3 Clocking block declaration
   --  clocking_skew ::=
   --      edge_identifier [ delay_control ]
   --    | delay_control
   --
   --  edge_identifier ::=  POSEDGE | NEGEDGE | EDGE
   --
   --  delay_control ::=
   --      # delay_value
   --    | # ( mintypmax_expression )
   function Parse_Clocking_Skew return Node
   is
      Res : Node;
      Edge : Edge_Type;
   begin
      Res := Create_Node (N_Clocking_Skew);
      Set_Token_Location (Res);

      case Current_Token is
         when Tok_Posedge =>
            Edge := Edge_Posedge;

            --  Skip 'posedge'.
            Scan;
         when Tok_Negedge =>
            Edge := Edge_Negedge;

            --  Skip 'negedge'.
            Scan;
         when Tok_Edge =>
            Edge := Edge_Any;

            --  Skip 'edge'.
            Scan;
         when others =>
            Edge := Edge_None;
      end case;
      Set_Edge_Identifier (Res, Edge);

      if Current_Token = Tok_Sharp then
         --  Skip '#'.
         Scan;

         Set_Delay_Control (Res, Parse_Delay_Value);
      elsif Edge = Edge_None then
         Error_Msg_Parse ("delay control expected");
      end if;

      return Res;
   end Parse_Clocking_Skew;

   function Parse_Clocking_Skew_Opt return Node is
   begin
      if Current_Token /= Tok_Identifier then
         return Parse_Clocking_Skew;
      else
         return Null_Node;
      end if;
   end Parse_Clocking_Skew_Opt;

   --  1800-2017 14.3 Clocking block declaration
   --  clocking_item ::=
   --      DEFAULT default_skew ;
   --    | ...
   --
   --  default_skew ::=
   --      INPUT clocking_skew
   --    | OUTPUT clocking_skew
   --    | INPUT clocking_skew OUTPUT clocking_skew
   function Parse_Default_Skew return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Default_Skew);
      Set_Token_Location (Res);

      --  Skip 'default'.
      Scan;

      if Current_Token = Tok_Input then
         --  Skip 'input'.
         Scan;

         Set_Input_Skew (Res, Parse_Clocking_Skew);
      end if;

      if Current_Token = Tok_Output then
         --  Skip 'output'.
         Scan;

         Set_Output_Skew (Res, Parse_Clocking_Skew);
      end if;

      Scan_Or_Error (Tok_Semicolon, "';' expected at end of default skew");

      return Res;
   end Parse_Default_Skew;

   --  1800-2017 14.3 Clocking block declaration
   --  clocking_declaration ::=
   --      ...
   --    | clocking_direction list_of_clocking_decl_assign ;
   --    | ...
   --
   --  clocking_direction ::=
   --      INPUT [ clocking_skew ]
   --    | OUTPUT [ clocking_skew ]
   --    | INPUT [ clocking_skew ] OUTPUT [ clocking_skew ]
   --    | INOUT
   --
   --  list_of_clocking_decl_assign ::=
   --    clocking_edge_assign { , clocking_decl_assign }
   --
   --  clocking_decl_assign ::= signal_identifier [ = expression ]
   procedure Parse_Clocking_Decl (First, Last : in out Node)
   is
      Decl : Node;
      In_Skew : Node;
      Out_Skew : Node;
   begin
      In_Skew := Null_Node;
      Out_Skew := Null_Node;

      case Current_Token is
         when Tok_Input =>
            --  Skip 'input'.
            Scan;

            In_Skew := Parse_Clocking_Skew_Opt;

            if Current_Token = Tok_Output then
               --  Skip 'output'.
               Scan;

               Out_Skew := Parse_Clocking_Skew_Opt;
            end if;
         when Tok_Output =>
            --  Skip 'output'.
            Scan;

            Out_Skew := Parse_Clocking_Skew_Opt;

         when Tok_Inout =>
            --  Skip 'inout'.
            Scan;

         when others =>
            Error_Msg_Parse ("clocking direction expected");
      end case;

      loop
         Decl := Create_Node (N_Clock_Var);
         Set_Token_Location (Decl);

         Scan_Identifier (Decl, "signal identifier expected");

         if Current_Token = Tok_Equal then
            Set_Expression (Decl, Parse_Expression);
         end if;

         --  FIXME: this shares nodes!
         Set_Input_Skew (Decl, In_Skew);
         Set_Output_Skew (Decl, Out_Skew);

         Append_Chain (First, Last, Decl);

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
      end loop;

      Scan_Or_Error (Tok_Semicolon, "';' expected at end of clocking decl");
   end Parse_Clocking_Decl;

   --  1800-2017 14.3 Clocking block declaration
   --  clocking_declaration ::=
   --      [ default ] clocking [ clocking_identifier ] clocking_event ;
   --        { clocking_item }
   --      endclocking [ : clocking_identifier ]
   --    | ...
   function Parse_Default_Clocking return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Default_Clocking);

      --  Skip 'clocking'.
      Scan;

      if Current_Token = Tok_Identifier then
         Set_Identifier (Res, Current_Identifier);

         --  Skip identifier.
         Scan;
      end if;

      Set_Event (Res, Parse_Clocking_Event);

      --  Skip ';'.
      Scan_Or_Error (Tok_Semicolon, "';' expected after clocking event");

      --  TODO: clocking items.

      --  Skip 'endclocking'.
      Scan_Or_Error (Tok_Endclocking, "missing 'endclocking'");

      Parse_End_Name (Res);

      return Res;
   end Parse_Default_Clocking;

   --  precond: CLOCKING (TODO: | DEFAULT | GLOBAL)
   --
   --  1800-2017 14.3 Clocking block declaration
   --  clocking_declaration ::=
   --      [ DEFAULT ] CLOCKING [ clocking_identifier ] clocking_event ;
   --        { clocking_item }
   --     ENDCLOCKING [ : clocking_identifier ]
   --   | GLOBAL CLOCKING [ clocking_identifier ] clocking_event ;
   --     ENDCLOCKING [ : clocking_identifier ]
   --
   --  clocking_event ::=
   --      @ identifier
   --    | @ ( event_expression )
   function Parse_Clocking_Declaration return Node
   is
      Res : Node;
      First, Last : Node;
   begin
      Res := Create_Node (N_Clocking);
      Set_Token_Location (Res);

      --  Skip 'clocking'.
      Scan;

      --  Clocking identifier.
      if Current_Token = Tok_Identifier then
         Set_Identifier (Res, Current_Identifier);

         --  Skip identifier.
         Scan;
      end if;

      --  Clocking event
      if Current_Token = Tok_At then
         Set_Event (Res, Parse_Clocking_Event);
      else
         Error_Msg_Parse ("'@' expected for clocking event");
      end if;

      Scan_Or_Error
        (Tok_Semicolon, "';' expected at end of clocking declaration");

      Init_Chain (First, Last);
      loop
         case Current_Token is
            when Tok_Endclocking =>
               exit;
            when Tok_Default =>
               Append_Chain (First, Last, Parse_Default_Skew);
            when Tok_Input | Tok_Output | Tok_Inout =>
               Parse_Clocking_Decl (First, Last);
            when others =>
               exit;
         end case;
      end loop;
      Set_Clocking_Item_Chain (Res, First);

      Scan_Or_Error
        (Tok_Endclocking,
         "'endclocking' expected at end of clocking declaration");
      Parse_End_Name (Res);

      return Res;
   end Parse_Clocking_Declaration;

   --  precond: GENERATE
   --
   --  1364-2005 12.4 Generate constructs
   --  1800-2017 27.3 Generate construct syntax
   --  generate_region ::=
   --    GENERATE { generate_item } ENDGENERATE
   function Parse_Generate_Region return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Generate_Region);
      Set_Token_Location (Res);

      --  Skip 'generate'
      Scan;

      Set_Generate_Item_Chain (Res, Parse_Module_Items_Rep (Res));

      --  Skip 'endgenerate'.
      Scan_Or_Error (Tok_Endgenerate,
                     "'endgenerate' expected at end of generate region");

      return Res;
   end Parse_Generate_Region;

   --  1364-2005 12.4 Generate constructs
   --  generate_block ::=
   --      ...
   --    | BEGIN [ : generate_block_identifier ]
   --        { module_or_generate_item }
   --      END
   --
   --  1800-2017 27.3 Generate construct syntax
   --  generate_block ::=
   --      generate item
   --    | [ generate_bock_identifier : ] BEGIN [ : generate_block_identifier ]
   --        { generate_item }
   --      END [ : generate_block_identifier ]
   function Parse_Generate_Block return Node
   is
      Res : Node;
   begin
      pragma Assert (Current_Token = Tok_Begin);

      Res := Create_Node (N_Generate_Block);
      Set_Token_Location (Res);

      --  Skip 'begin'.
      Scan;

      if Current_Token = Tok_Colon then
         --  Skip ':'.
         Scan;

         --  Skip identifier.
         Scan_Identifier (Res, "block identifier expected after ':'");
      end if;

      Set_Generate_Item_Chain (Res, Parse_Module_Items_Rep (Res));

      --  Skip 'end'.
      Scan_Or_Error (Tok_End, "'end' expected at end of generate block");

      Parse_End_Name (Res);

      return Res;
   end Parse_Generate_Block;

   --  1364-2005 12.4 Generate constructs
   --  generate_block ::=
   --      module_or_generate_item
   --    | BEGIN [ : generate_block_identifier ]
   --      { module_or_generate_item } END
   function Parse_Generate_Block_Or_Item (Parent : Node) return Node is
   begin
      if Current_Token = Tok_Begin then
         return Parse_Generate_Block;
      else
         return Parse_Module_Item (Parent);
      end if;
   end Parse_Generate_Block_Or_Item;

   --  1364-2005 12.4 Generate constructs
   --  generate_block_or_null ::=
   --    generate_block | ;
   function Parse_Generate_Block_Or_Null (Parent : Node) return Node is
   begin
      if Current_Token = Tok_Semicolon then
         --  Skip ';'.
         Scan;

         return Null_Node;
      else
         return Parse_Generate_Block_Or_Item (Parent);
      end if;
   end Parse_Generate_Block_Or_Null;

   --  1800-2017 27.3 Generate construct syntax
   --  if_generate_construct ::=
   --    IF ( constant_expression ) generate_block [ ELSE generate_block ]
   function Parse_If_Generate return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_If_Generate);
      Set_Token_Location (Res);

      --  Skip 'if'.
      Scan;

      Set_Condition (Res, Parse_Parenthesis_Expression);

      Set_True_Block (Res, Parse_Generate_Block_Or_Item (Res));

      if Current_Token = Tok_Else then
         --  Skip 'else'.
         Scan;

         Set_False_Block (Res, Parse_Generate_Block_Or_Item (Res));
      end if;

      return Res;
   end Parse_If_Generate;

   --  precond: FOR
   --
   --  1364-2005 12.4 Generate constructs
   --  loop_generate_construct ::=
   --    FOR ( genvar_initialization ; genvar_expression ; genvar_iteration )
   --      generate_block
   function Parse_Loop_Generate return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Loop_Generate);
      Set_Token_Location (Res);

      Parse_For (Res);

      Set_Generate_Block (Res, Parse_Generate_Block_Or_Item (Res));

      return Res;
   end Parse_Loop_Generate;

   --  precond: CASE
   --
   --  1364-2005 12.4 Generate constructs
   --  case_generate_construct ::=
   --    CASE ( constant_expression )
   --      case_generate_item { case_generate_item } ENDCASE
   --
   --  case_generate_item ::=
   --       constant_expression { , constant_expression } :
   --         generate_block_or_null
   --    |  DEFAULT [ : ] generate_block_or_null
   function Parse_Case_Generate return Node
   is
      Res : Node;
      First_Item, Last_Item : Node;
   begin
      Res := Create_Node (N_Case_Generate);
      Set_Token_Location (Res);

      --  Skip 'case'.
      Scan;

      Set_Expression (Res, Parse_Parenthesis_Expression);

      Init_Chain (First_Item, Last_Item);
      loop
         exit when Current_Token = Tok_Endcase;

         --  Parse case_generate_item.
         Parse_Case_Item (First_Item, Last_Item);

         Set_Statement (Last_Item, Parse_Generate_Block_Or_Null (Res));
      end loop;

      Set_Case_Items (Res, First_Item);
      if Last_Item = Null_Node then
         Error_Msg_Parse ("at least one case item is required");
      end if;

      if Current_Token /= Tok_Endcase then
         Error_Msg_Parse ("'endcase' expected");
      else
         Scan;
      end if;

      return Res;
   end Parse_Case_Generate;

   --  1800-2017 Interface syntax
   --  modport_simple_ports_declaration ::=
   --    port_direction modport_simple_port { , modport_simple_port }
   --
   --  modport_simple_port ::=
   --      port_identifier
   --    | . port_identifier ( [ expression ] )
   function Parse_Modport_Simple_Port (Kind : Nkind) return Node
   is
      Decl : Node;
   begin
      Decl := Create_Node (Kind);
      Set_Token_Location (Decl);

      if Current_Token = Tok_Dot then
         --  Skip '.'
         Scan;

         Scan_Identifier (Decl, "port identifier expected");

         Scan_Or_Error (Tok_Left_Paren, "'(' expected after port identifier");

         if Current_Token /= Tok_Right_Paren then
            Set_Expression (Decl, Parse_Expression);
         end if;

         Scan_Or_Error (Tok_Right_Paren, "')' expected after expression");
      else
         Scan_Identifier (Decl, "port identifier expected");
      end if;

      return Decl;
   end Parse_Modport_Simple_Port;

   --  1800-2017 25.3 Interface syntax
   --  modport_clocking_declaration ::= CLOCKING clocking_identifier
   function Parse_Modport_Clocking_Declaration return Node
   is
      Decl : Node;
   begin
      Decl := Create_Node (N_Modport_Clocking);
      Set_Token_Location (Decl);

      --  Skip 'clocking'.
      Scan;

      Scan_Identifier (Decl, "clocking identifier expected");

      return Decl;
   end Parse_Modport_Clocking_Declaration;

   --  1800-2017 25.3 Interface syntax
   --  modport_declaration ::= MODPORT modport_item { , modport_item } ;
   --
   --  modport_item ::=
   --    modport_identifier ( modport_ports_declarations
   --                         { , modport_ports_declaration } )
   --
   --  modport_ports_declaration ::=
   --      modport_simple_ports_declaration
   --    | modport_tf_ports_declaration
   --    | modport_clocking_declaration
   procedure Parse_Modport_Declaration (Constr : in out Items_Constr)
   is
      Decl : Node;
      Port_Constr : Items_Constr;
      Last : Nkind;
      Port : Node;
   begin
      loop
         --  Skip 'modport' or ','.
         Scan;

         Decl := Create_Node (N_Modport);
         Set_Token_Location (Decl);

         Scan_Identifier (Decl, "modport identifier expected");

         Scan_Or_Error (Tok_Left_Paren, "'(' expected before modport ports");

         Last := N_Error;
         Init_Constr (Port_Constr, Decl);
         loop
            Port := Null_Node;

            case Current_Token is
               when Tok_Input =>
                  Last := N_Modport_Input;

                  --  Skip 'input'.
                  Scan;

                  Port := Parse_Modport_Simple_Port (Last);

               when Tok_Output =>
                  Last := N_Modport_Output;

                  --  Skip 'output'.
                  Scan;

                  Port := Parse_Modport_Simple_Port (Last);

               when Tok_Inout =>
                  Last := N_Modport_Inout;

                  --  Skip 'inout'.
                  Scan;

                  Port := Parse_Modport_Simple_Port (Last);

               when Tok_Ref =>
                  Last := N_Modport_Ref;

                  --  Skip 'ref'.
                  Scan;

                  Port := Parse_Modport_Simple_Port (Last);

               when Tok_Clocking =>
                  Last := N_Error;
                  Port := Parse_Modport_Clocking_Declaration;

               when Tok_Dot
                 | Tok_Identifier =>
                  if Last = N_Error then
                     Error_Msg_Parse
                       ("port direction (input, output, inout) expected");
                  end if;
                  Port := Parse_Modport_Simple_Port (N_Modport_Input);
               when others =>
                  Error_Msg_Parse ("modport port declaration expected");
                  exit;
            end case;
            Append_Node (Port_Constr, Port);

            if Current_Token = Tok_Comma then
               --  Skip ','.
               Scan;
            else
               Scan_Or_Error
                 (Tok_Right_Paren, "')' expected after modport ports");
               exit;
            end if;
         end loop;
         Set_Modport_Ports_Chain (Decl, Get_Constr_Chain (Port_Constr));

         Append_Node (Constr, Decl);

         exit when Current_Token /= Tok_Comma;
      end loop;

      Scan_Declaration_Semicolon;
   end Parse_Modport_Declaration;

   --  1800-2017 23.2.1 Module header definition
   --  timeunits_declaration ::=
   --      TIMEUNIT time_literal [ / time_literal ] ;
   --    | TIMEPRECISION time_literal ;
   --    | TIMEUNIT time_literal ; TIMEPRECISION time_literal ;
   --    | TIMEPRECISION time_literal ; TIMEUNIT time_literal ;
   function Parse_Timeunit return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Timeunit);
      Set_Token_Location (Res);

      --  Skip 'timeunit'.
      Scan;

      Set_Timeunit (Res, Parse_Time_Literal);

      if Current_Token = Tok_Slash then
         --  Skip '/'.
         Scan;

         Set_Timeprecision (Res, Parse_Time_Literal);
      end if;

      Scan_Or_Error (Tok_Semicolon, "';' expected at end of timeunit");

      return Res;
   end Parse_Timeunit;

   function Parse_Timeprecision return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Timeprecision);
      Set_Token_Location (Res);

      --  Skip 'timeprecision'.
      Scan;

      Set_Timeprecision (Res, Parse_Time_Literal);

      Scan_Or_Error (Tok_Semicolon, "';' expected at end of timeprecision");

      return Res;
   end Parse_Timeprecision;

   --  1800-2017 Disable iff resolution
   --  module_or_generate_item_declaration ::=
   --      ...
   --    | DEFAULT DISABLE IFF expression_or_dist ;
   function Parse_Disable_Iff return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Disable_Iff);

      --  Skip 'disable'.
      Scan;

      --  Skip 'iff'.
      Scan_Or_Error (Tok_Iff, "'iff' expected after 'disable'");

      Set_Expression (Res, Parse_Expression);

      Scan_Or_Error (Tok_Semicolon, "';' expected after expression");
      return Res;
   end Parse_Disable_Iff;

   function Parse_Default return Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Res : Node;
   begin
      --  Skip 'default'.
      Scan;

      case Current_Token is
         when Tok_Clocking =>
            Res := Parse_Default_Clocking;

         when Tok_Disable =>
            Res := Parse_Disable_Iff;

         when others =>
            Error_Msg_Parse ("clocking or disabled expected after default");
            return Null_Node;
      end case;

      Set_Location (Res, Loc);
      return Res;
   end Parse_Default;

   procedure Skip_Unknown_Item is
   begin
      loop
         case Current_Token is
            when Tok_Eof =>
               exit;
            when Tok_End
              | Tok_Endmodule
              | Tok_Endgenerate
              | Tok_Endinterface
              | Tok_Endclass =>
               exit;
            when Tok_Semicolon =>
               Scan;
               exit;
            when others =>
               --  Skip current token.
               Scan;
         end case;
      end loop;
   end Skip_Unknown_Item;

   function Parse_Module_Items_Rep (Parent : Node) return Node
   is
      Constr : Items_Constr;
   begin
      --  Parse module items.
      Init_Constr (Constr, Parent);
      loop
         case Current_Token is
            when Tok_Endmodule
              | Tok_Endgenerate
              | Tok_Endinterface
              | Tok_Endclass
              | Tok_Endprogram
              | Tok_Endpackage
              | Tok_End =>
               exit;
            when Tok_Eof =>
               exit;
            when others =>
               Parse_Module_Item (Constr);
         end case;
      end loop;

      return Get_Constr_Chain (Constr);
   end Parse_Module_Items_Rep;

   --  1800-2017 23.2.1 Module header definition
   --  1800-2017 6.20.1 Parameter declaration syntax
   --  parameter_port_list ::=
   --      # ( list_of_param_assignment { , parameter_port_declaration } )
   --    | # ( parameter_port_declaration { , parameter_port_declaration } )
   --    | # ( )
   --
   --  parameter_port_declaration ::=
   --      parameter_declaration
   --    | local_parameter_declaration
   --    | data_type list_of_param_assignments
   --    | TYPE list_of_type_assignments
   --
   --  parameter_declaration ::=
   --      PARAMETER data_type_or_implicit list_of_param_assignments
   --    | PARAMETER TYPE list_of_type_assignments
   --
   --  local_parameter_declaration ::=
   --      LOCALPARAM data_type_of_implicit list_of_param_assignments
   --    | LOCALPARAM TYPE list_of_type_assignments
   --
   --  list_of_param_assignments ::=
   --    param_assignment { , param_assignment }
   --
   --  param_assignment ::=
   --    parameter_identifier { unpacked_dimension }
   --      [ = constant_param_expression ]
   --
   --  list_of_type_assignments ::=
   --    type_assignment { , type_assignment }
   --
   --  type_assignment ::=
   --    type_identifier [ = data_type ]
   --
   function Parse_Parameter_Port_List (Parent : Node) return Node
   is
      Kind : Nkind;
      Constr : Items_Constr;
      Param : Node;
      Decl_Type : Type_Node;
      Atype : Type_Node;
   begin
      --  Parameter post list is always optional.
      if Current_Token /= Tok_Sharp then
         return Null_Node;
      end if;

      --  Skip '#'.
      Scan;

      --  Skip '('.
      Scan_Or_Error (Tok_Left_Paren, "missing '(' for port parameters");

      Init_Constr (Constr, Parent);
      Kind := N_Parameter;
      loop
         case Current_Token is
            when Tok_Parameter =>
               --  Skip 'parameter'.
               Scan;

               if Current_Token = Tok_Type then
                  --  Skip 'type'.
                  Scan;

                  Kind := N_Type_Parameter;

                  Parse_Type_Assignment (Kind, Param);
                  Set_Has_Type (Param, True);
               else
                  Kind := N_Parameter;
                  Decl_Type := Parse_Data_Type_Or_Implicit;

                  Parse_Param_Assignment (Kind, Decl_Type, Param);
               end if;

            when Tok_Localparam =>
               --  Skip 'localparam'.
               Scan;

               if Current_Token = Tok_Type then
                  --  Skip 'type'.
                  Scan;

                  Kind := N_Type_Localparam;
                  Parse_Type_Assignment (Kind, Param);
                  Set_Has_Type (Param, True);
               else
                  Kind := N_Localparam;
                  Decl_Type := Parse_Data_Type_Or_Implicit;

                  Parse_Param_Assignment (Kind, Decl_Type, Param);
               end if;

            when Tok_Type =>
               --  Skip 'type'.
               Scan;

               Kind := N_Type_Parameter;
               Parse_Type_Assignment (Kind, Param);
               Set_Has_Type (Param, True);

            when others =>
               Atype := Parse_Data_Type_Or_Implicit;

               if Atype.Typ = Null_Node then
                  Error_Msg_Parse ("parameter port declaration expected");
                  Param := Null_Node;
               else
                  case Current_Token is
                     when Tok_Comma | Tok_Right_Paren | Tok_Equal =>
                        --  Type parsed was a type or parameter identifier.
                        --  FIXME: packed array (what about foo[1:0] ?)
                        Param := Create_Node (Kind);
                        Set_Location (Param, Get_Location (Atype.Typ));
                        Data_Type_To_Identifier (Param, Atype);
                        if Current_Token = Tok_Equal then
                           --  Skip '='.
                           Scan;

                           case Kind is
                              when N_Parameter | N_Localparam =>
                                 Set_Expression (Param, Parse_Expression);
                              when N_Type_Parameter | N_Type_Localparam =>
                                 Atype := Parse_Data_Type_Or_Implicit;
                                 Set_Default_Type_Node (Param, Atype);
                              when others =>
                                 raise Program_Error;
                           end case;
                        end if;
                     when Tok_Identifier =>
                        if Kind /= N_Parameter and Kind /= N_Localparam then
                           Kind := N_Parameter;
                        end if;
                        Parse_Param_Assignment (Kind, Atype, Param);
                        Decl_Type := Atype;
                     when others =>
                        Error_Msg_Parse
                          ("parameter port declaration expected");
                        Param := Null_Node;
                  end case;
               end if;
         end case;

         if Param /= Null_Node then
            Append_Node (Constr, Param);
         end if;

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
      end loop;

      --  Skip ')'.
      Scan_Or_Error (Tok_Right_Paren, "missing ')' at end of port parameters");

      return Get_Constr_Chain (Constr);
   end Parse_Parameter_Port_List;

   --  1364-2005 12.1 Modules
   --  module_declaration ::=
   --      { attribute_instance } module_keyword module_identifier
   --        [ module_parameter_port_list ] list_of_ports ; { module_item }
   --        ENDMODULE
   --    | { attribute_instance } module_keyword module_identifier
   --        [ module_parameter_port_list ] [ list_of_port_declarations ] ;
   --        { non_port_module_item } ENDMODULE
   --
   --  1800-2017 23.2.1 Module header definition
   --  module_declaration ::=
   --      module_nonansi_header [ timeunits_declaration ]
   --        { module_item }
   --        ENDMODULE [ : module_identifier ]
   --    | module_ansi_header [ timeunits_declaration ]
   --        { non_port_module_item }
   --        ENDMODULE [ : module_identifier ]
   --    | { attribute_instance } module_keyword [ lifetime ] module_identifier
   --        ( . *) ;
   --        [ timeunits_declaration ] { module_item }
   --        ENDMODULE [ : module_identifier ]
   --    | EXTERN module_nonansi_header
   --    | EXTERN module_ansi_header
   function Parse_Module_Declaration (Attrs : Node) return Node
   is
      Res : Node;
      Old_Scope : Node;
      Attr : Node;
   begin
      Res := Create_Node (N_Module);

      if Attrs /= Null_Node then
         Set_Attributes_Chain (Res, Attrs);
         Attr := Attrs;
         while Attr /= Null_Node loop
            Set_Attribute_Item (Attr, Res);
            Attr := Get_Chain (Attr);
         end loop;
      end if;

      --  Skip 'module'.
      Scan;

      Set_Token_Location (Res);

      Push_Scope (Res, Old_Scope);

      --  Skip identifier.
      Scan_Identifier (Res, "identifier expected after module");

      Set_Package_Import_Chain (Res, Parse_Package_Import_Chain (Res));

      --  Handle module_parameter_port_list.
      Set_Parameter_Port_Chain (Res, Parse_Parameter_Port_List (Res));

      --  List of ports / list of ports declarations.
      Parse_Ports_List (Res);

      --  Skip ';'.
      Scan_Or_Error (Tok_Semicolon, "missing ';' at end of module");

      --  Parse module items.
      Set_Items_Chain (Res, Parse_Module_Items_Rep (Res));

      --  Skip 'endmodule'
      Scan_Or_Error (Tok_Endmodule,
                     "'endmodule' expected at end of module items");
      Parse_End_Name (Res);

      if Current_Token = Tok_Semicolon then
         Error_Msg_Parse ("useless ';' after endmodule");
         Scan;
      end if;

      Pop_Scope (Old_Scope);

      return Res;
   end Parse_Module_Declaration;

   --  1800-2017 24.3 The program construct
   --  program_nonansi_header ::=
   --    { attribute_instance } program [ lifetime ] program_identifier
   --      { package_import_declaration } [ parameter_port_list ]
   --      list_of_ports ;
   --
   --  program_ansi_header ::=
   --    { attribute_instance } program [ lifetime ] program_identifier
   --      { package_import_declaration } [ parameter_port_list ]
   --      [ list_of_port_declarations ] ;
   --
   --  program_declaration ::=
   --      program_nonansi_header [ timeunits_declaration ]
   --        { program_item }
   --        endprogram [ : program_identifier ]
   --    | program_ansi_header [ timeunits_declaration ]
   --        { non_port_program_item }
   --        endprogram [ : program_identifier ]
   --    | { attribute_instance } program program_identifier ( .* ) ;
   --        [ timeunits_declaration ] { program_item }
   --        endprogram [ : program_identifier ]
   --    | extern program_nonansi_header
   --    | extern program_ansi_header
   function Parse_Program_Declaration return Node
   is
      Res : Node;
      Old_Scope : Node;
      Lifetime : Lifetime_State;
   begin
      Res := Create_Node (N_Program_Declaration);
      Set_Token_Location (Res);

      --  Skip 'program'.
      Scan;

      Lifetime := Parse_Lifetime;
      Set_Lifetime (Res, Lifetime);

      Push_Scope (Res, Old_Scope);

      --  Skip identifier.
      Scan_Identifier (Res, "identifier expected after program");

      --  Handle parameter_port_list.
      Set_Parameter_Port_Chain (Res, Parse_Parameter_Port_List (Res));

      --  List of ports / list of ports declarations.
      Parse_Ports_List (Res);

      --  Skip ';'.
      Scan_Or_Error (Tok_Semicolon, "missing ';' at end of program");

      --  Parse program items.
      Set_Items_Chain (Res, Parse_Module_Items_Rep (Res));

      --  Skip 'endprogram'
      Scan_Or_Error (Tok_Endprogram,
                     "'endprogram' expected at end of program items");
      Parse_End_Name (Res);

      if Current_Token = Tok_Semicolon then
         Error_Msg_Parse ("useless ';' after endprogram");
         Scan;
      end if;

      Pop_Scope (Old_Scope);

      return Res;
   end Parse_Program_Declaration;

   --  1800-2017 26.2 Package declarations
   --  package_declaration ::=
   --    PACKAGE [ lifetime ] package_identifier ;
   --      [ timeunits_declaration ] { package_item }
   --    ENDPACKAGE [ : package_identifier ]
   --
   --  package_item ::=
   --      package_or_generate_item_declaration
   --    | anonymous_program
   --    | package_export_declaration
   --    | timeunits_declaration
   function Parse_Package_Declaration return Node
   is
      Res : Node;
      Lifetime : Lifetime_State;
   begin
      Res := Create_Node (N_Package);
      Set_Token_Location (Res);

      --  Skip 'package'.
      Scan;

      Lifetime := Parse_Lifetime;
      Set_Lifetime (Res, Lifetime);

      --  Skip identifier.
      Scan_Identifier (Res, "identifier expected after package");

      --  Skip ';'.
      Scan_Or_Error (Tok_Semicolon, "missing ';' at end of package");

      --  Parse program items.
      Set_Package_Item_Chain (Res, Parse_Module_Items_Rep (Res));

      --  Skip 'endprogram'
      Scan_Or_Error (Tok_Endpackage,
                     "'endpackage' expected at end of package items");
      Parse_End_Name (Res);

      if Current_Token = Tok_Semicolon then
         Error_Msg_Parse ("useless ';' after endpackage");
         Scan;
      end if;

      return Res;
   end Parse_Package_Declaration;

   --  1800-2017 29.3 UDP definition
   --  udp_port_declaration ::=
   --      udp_output_declaration ;
   --    | udp_input_declaration ;
   --    | udp_reg_declaration ;

   --  1800-2017 29.3 UDP definition
   --  udp_output_declaration ::=
   --      OUTPUT port_identifier
   --    | OUTPUT REG port_identifier [ = constant_expression ]
   procedure Parse_Udp_Output_Declaration (Constr : in out Items_Constr)
   is
      Res : Node;
      Reg : Node;
   begin
      pragma Assert (Current_Token = Tok_Output);

      --  Skip 'output'
      Scan;

      Res := Create_Node (N_Output);

      if Current_Token = Tok_Reg then
         --  Skip 'reg'.
         Scan;

         Reg := Create_Node (N_Var);
         Set_Redeclaration (Res, Reg);
      else
         Reg := Null_Node;
      end if;

      Set_Token_Location (Res);

      Scan_Identifier (Res, "output identifier expected");

      if Reg /= Null_Node then
         Set_Identifier (Reg, Get_Identifier (Res));
         Set_Location (Reg, Get_Location (Res));

         if Current_Token = Tok_Equal then
            --  Skip '='.
            Scan;

            Set_Expression (Reg, Parse_Expression);
         end if;
      end if;

      Append_Node (Constr, Res);
   end Parse_Udp_Output_Declaration;

   --  1800-2017 29.3 UDP definition
   --  udp_input_declaration ::=
   --    INPUT list_of_udp_port_identifiers
   procedure Parse_Udp_Input_Declaration (Constr : in out Items_Constr;
                                          End_Comma : out Boolean)
   is
      Res : Node;
   begin
      pragma Assert (Current_Token = Tok_Input);

      --  Skip 'input'.
      Scan;

      Res := Create_Node (N_Input);
      Set_Token_Location (Res);

      Scan_Identifier (Res, "input identifier expected");
      Append_Node (Constr, Res);

      loop
         if Current_Token /= Tok_Comma then
            End_Comma := False;
            exit;
         end if;

         --  Skip ','.
         Scan;

         if Current_Token /= Tok_Identifier then
            End_Comma := True;
            exit;
         end if;

         Set_Has_Identifier_List (Res, True);

         Res := Create_Node (N_Input);
         Set_Token_Location (Res);
         Set_Identifier (Res, Current_Identifier);

         --  Skip identifier.
         Scan;

         Append_Node (Constr, Res);
      end loop;
   end Parse_Udp_Input_Declaration;

   --  1800-2017 29.3 UDP definition
   --  udp_port_list ::=
   --    output_port_identifier , input_port_identifier
   --    { , input_port_identifier }
   procedure Parse_Udp_Port_List (Udp : Node)
   is
      First_Port, Last_Port : Node;
      Port : Node;
   begin
      Init_Chain (First_Port, Last_Port);

      loop
         Port := Create_Node (N_Port);
         Set_Token_Location (Port);
         Scan_Identifier (Port, "missing port identifier");
         Append_Chain (First_Port, Last_Port, Port);

         exit when Current_Token /= Tok_Comma;
         --  Skip ','.
         Scan;
      end loop;
      Set_Ports_Chain (Udp, First_Port);
   end Parse_Udp_Port_List;

   --  1800-2017 29.3 UDP definition
   --  udp_declaration_port_list ::=
   --    udp_output_declaration , udp_input_declaration
   --      { , udp_input_declaration }
   --
   --  udp_input_declaration ::= INPUT list_of_udp_port_identifiers
   --  udp_output_declaration ::=
   --      OUTPUT port_identifier
   --    | OUTPUT REG port_identifier [ = constant_expression ]
   procedure Parse_Udp_Declaration_Port_List (Udp : Node)
   is
      Constr : Items_Constr;
      Comma : Boolean;
   begin
      Init_Constr (Constr, Udp);

      if Current_Token = Tok_Output then
         Parse_Udp_Output_Declaration (Constr);

         Scan_Or_Error (Tok_Comma, "',' expected after udp output port");
      else
         Error_Msg_Parse ("first udp port must be an output");
      end if;

      if Current_Token = Tok_Input then
         loop
            Parse_Udp_Input_Declaration (Constr, Comma);
            exit when not Comma;

            if Current_Token /= Tok_Input then
               Error_Msg_Parse ("',' expected between udp ports");
               exit;
            end if;
         end loop;
      end if;

      Set_Ports_Chain (Udp, Get_Constr_Chain (Constr));
   end Parse_Udp_Declaration_Port_List;

   function Tok_To_Udp_Symbol (Tok : Tok_Udp_Symbols) return Udp_Symbol is
   begin
      case Tok is
         when Tok_Udp_0    => return Udp_0;
         when Tok_Udp_1    => return Udp_1;
         when Tok_Udp_X    => return Udp_X;
         when Tok_Udp_Qm   => return Udp_Qm;
         when Tok_Udp_B    => return Udp_B;
         when Tok_Udp_R    => return Udp_R;
         when Tok_Udp_F    => return Udp_F;
         when Tok_Udp_P    => return Udp_P;
         when Tok_Udp_N    => return Udp_N;
         when Tok_Udp_Star => return Udp_Any;
         when Tok_Udp_Dash => return Udp_No;
      end case;
   end Tok_To_Udp_Symbol;

   --  1800-2017 29.3 UDP definition
   --  udp_declaration ::=
   --      udp_nonansi_declaration
   --        udp_port_declaration { udp_port_declaration }
   --        udp_body
   --      ENDPRIMITIVE [ : udp_identifier ]
   --    | udp_ansi_declaration
   --        udp_body
   --      ENDPRIMITIVE [ : udp_identifier ]
   --    [...]
   --
   --  udp_nonansi_declaration ::=
   --    PRIMITIVE udp_identifier ( udp_port_list ) ;
   --
   --  udp_ansi_declaration ::=
   --    PRIMITIVE udp_identifier ( udp_declaration_port_list ) ;
   function Parse_Udp_Declaration return Node
   is
      Res : Node;
      Ent, Sym : Node;
      Constr, Constr_Sym : Items_Constr;
      Kind : Udp_Kind;
   begin
      Res := Create_Node (N_Primitive);
      Set_Token_Location (Res);

      --  Skip 'primitive'.
      Scan;

      --  Skip identifier.
      Scan_Identifier (Res, "identifier expected after 'primitive'");

      --  Skip '('.
      Scan_Or_Error (Tok_Left_Paren, "missing '(' after udp identifier");

      if Current_Token = Tok_Identifier then
         --  Non-ansi declaration
         Parse_Udp_Port_List (Res);
      else
         --  Ansi declaration
         Set_Ansi_Port_Flag (Res, True);
         Parse_Udp_Declaration_Port_List (Res);
      end if;

      --  Skip ')'.
      Scan_Or_Error (Tok_Right_Paren, "')' expected after udp port list");

      --  Skip ';'.
      Scan_Or_Error (Tok_Semicolon, "';' expected after udp port list");

      Init_Constr (Constr, Res);

      --  Udp port declarations.
      loop
         case Current_Token is
            when Tok_Input =>
               declare
                  Comma : Boolean;
               begin
                  Parse_Udp_Input_Declaration (Constr, Comma);
                  if Comma then
                     Error_Msg_Parse ("';' expected at end of input port");
                  else
                     Scan_Declaration_Semicolon;
                  end if;
               end;
            when Tok_Output =>
               Parse_Udp_Output_Declaration (Constr);
               Scan_Declaration_Semicolon;
            when Tok_Reg =>
               declare
                  Reg : Node;
               begin
                  Set_Udp_Kind (Res, Udp_Level_Sensitive);
                  Reg := Create_Node (N_Var);

                  --  Skip 'reg'.
                  Scan;

                  Set_Token_Location (Reg);
                  Scan_Identifier (Reg, "variable identifier expected");
                  Scan_Declaration_Semicolon;

                  Append_Node (Constr, Reg);
               end;
            when Tok_Table
              | Tok_Initial =>
               exit;
            when others =>
               Error_Msg_Parse ("udp port declaration expected");
               exit;
         end case;
      end loop;
      Set_Udp_Port_Declaration_Chain (Res, Get_Constr_Chain (Constr));

      Kind := Get_Udp_Kind (Res);

      --  Udp initial statement.
      if Current_Token = Tok_Initial then
         if Kind = Udp_Combinational then
            Error_Msg_Parse ("initial not expected without a reg output");
         end if;

         --  Skip 'initial'.
         Scan;

         if Current_Token /= Tok_Identifier then
            Error_Msg_Parse ("udp output port identifier expected");
         else
            --  FIXME: check same identifier.
            Scan;
         end if;

         --  Skip '='.
         Scan_Or_Error (Tok_Equal, "'=' expected after udp output identifier");

         Set_Udp_Initial (Res, Parse_Expression);

         --  Skip ';'.
         Scan_Or_Error (Tok_Semicolon,
                        "';' expected at end of udp initial statement");
      end if;

      if Current_Token /= Tok_Table then
         Error_Msg_Parse ("missing 'table' keyword");
      end if;

      --  Entries.
      Init_Constr (Constr, Res);
      loop
         --  Skip 'table' or ';'
         Scan_Udp;
         exit when Current_Token = Tok_Endtable;

         if Kind = Udp_Combinational then
            Ent := Create_Node (N_Udp_Combinational_Entry);
         else
            Ent := Create_Node (N_Udp_Sequential_Entry);
         end if;
         Set_Token_Location (Ent);
         Append_Node (Constr, Ent);

         Init_Constr (Constr_Sym, Ent);

         loop
            case Current_Token is
               when Tok_Level_Symbols =>
                  Sym := Create_Node (N_Udp_Level_Symbol);
                  Set_Token_Location (Sym);
                  Append_Node (Constr_Sym, Sym);
                  Set_Symbol (Sym, Tok_To_Udp_Symbol (Current_Token));

                  -- Skip symbol
                  Scan_Udp;

               when Tok_Edge_Symbols =>
                  Sym := Create_Node (N_Udp_Level_Symbol);
                  Set_Token_Location (Sym);
                  Append_Node (Constr_Sym, Sym);
                  Set_Symbol (Sym, Tok_To_Udp_Symbol (Current_Token));

                  -- Skip symbol
                  Scan_Udp;

               when Tok_Left_Paren =>
                  Sym := Create_Node (N_Udp_Change_Symbol);
                  Set_Token_Location (Sym);
                  Append_Node (Constr_Sym, Sym);

                  --  Skip '('.
                  Scan_Udp;

                  if Current_Token not in Tok_Level_Symbols then
                     Error_Msg_Parse ("level symbol expected");
                  else
                     Set_From_Symbol (Sym, Tok_To_Udp_Symbol (Current_Token));
                  end if;

                  --  Skip first symbol.
                  Scan_Udp;

                  if Current_Token not in Tok_Level_Symbols then
                     Error_Msg_Parse ("level symbol expected");
                  else
                     Set_From_Symbol (Sym, Tok_To_Udp_Symbol (Current_Token));
                  end if;

                  --  Skip second symbol.
                  Scan_Udp;

                  if Current_Token /= Tok_Right_Paren then
                     Error_Msg_Parse ("')' expected at end of edge indicator");
                  end if;

                  --  Skip ')'.
                  Scan_Udp;

               when Tok_Colon =>
                  exit;

               when others =>
                  Error_Msg_Parse ("udp symbol or ':' expected");
                  exit;
            end case;
         end loop;

         Set_Input_Chain (Ent, Get_Constr_Chain (Constr_Sym));

         if Current_Token = Tok_Colon then
            --  Skip ':'.
            Scan_Udp;

            if Kind = Udp_Combinational then
               if Current_Token in Tok_Level_Symbols then
                  Set_Output_Symbol (Ent, Tok_To_Udp_Symbol (Current_Token));
               else
                  Error_Msg_Parse ("output symbol expected after ':'");
               end if;

               --  Skip symbol.
               Scan_Udp;
            else
               if Current_Token in Tok_Level_Symbols then
                  Set_Current_State (Ent, Tok_To_Udp_Symbol (Current_Token));
               else
                  Error_Msg_Parse ("current state symbol expected after ':'");
               end if;

               --  Skip symbol.
               Scan_Udp;

               if Current_Token = Tok_Colon then
                  --  Skip ':'.
                  Scan_Udp;
               else
                  Error_Msg_Parse ("':' expected after current state");
               end if;

               if Current_Token in Tok_Output_Symbols then
                  Set_Next_State (Ent, Tok_To_Udp_Symbol (Current_Token));
               else
                  Error_Msg_Parse ("next state symbol expected after ':'");
               end if;

               --  Skip symbol.
               Scan_Udp;
            end if;
         end if;

         if Current_Token /= Tok_Semicolon then
            Error_Msg_Parse ("';' expected at end of udp entry");
         end if;
      end loop;

      --  Skip 'endtable'.
      Scan;

      Set_Udp_Entries_Chain (Res, Get_Constr_Chain (Constr));

      Scan_Or_Error (Tok_Endprimitive,
                     "'endprimitive' expected at end of udp");
      Parse_End_Name (Res);

      return Res;
   end Parse_Udp_Declaration;

   --  1800-2017 25.3 Interface syntax
   --  interface_declaration ::=
   --      interface_nonansi_header [ timeunits_declaration ]
   --         { interface_item }
   --         endinterface [ : interface_identifier ]
   --    | interface_ansi_header [ timeunits_declaration ]
   --         { non_port_interface_item }
   --         endinterface [ : interface_identifier ]
   --    | ...
   function Parse_Interface_Declaration return Node
   is
      Res : Node;
      Old_Scope : Node;
      Lifetime : Lifetime_State;
   begin
      Res := Create_Node (N_Interface_Declaration);
      Set_Token_Location (Res);

      --  Skip 'interface'.
      Scan;

      Push_Scope (Res, Old_Scope);

      Lifetime := Parse_Lifetime;
      Set_Lifetime (Res, Lifetime);

      --  Skip identifier.
      Scan_Identifier (Res, "interface identifier expected");

      --  TODO: package import declaration

      --  Handle parameter_port_list.
      Set_Parameter_Port_Chain (Res, Parse_Parameter_Port_List (Res));

      --  List of ports / list of ports declarations.
      Parse_Ports_List (Res);

      Scan_Or_Error (Tok_Semicolon, "missing ';' at end of interface");

      --  Parse interface items.
      Set_Items_Chain (Res, Parse_Module_Items_Rep (Res));

      Scan_Or_Error (Tok_Endinterface,
                     "'endinterface' expected at end of interface items");
      Parse_End_Name (Res);

      if Current_Token = Tok_Semicolon then
         Error_Msg_Parse ("useless ';' after endinterface");
         Scan;
      end if;

      Pop_Scope (Old_Scope);

      return Res;
   end Parse_Interface_Declaration;

   type Property_Qualifiers is record
      Has_Static : Boolean;
      Has_Protected : Boolean;
      Has_Local : Boolean;
      Has_Virtual : Boolean;

      Has_Rand : Boolean;
      Has_Randc : Boolean;
   end record;

   --  1800-2017 8.3 Syntax
   --  property_qualifier ::=
   --      random_qualifier
   --    | class_item_qualifier
   --
   --  class_item_qualifier ::=
   --      STATIC
   --    | PROTECTED
   --    | LOCAL
   --
   --  random_qualifier ::=
   --      RAND
   --    | RANDC
   function Parse_Property_Qualifiers return Property_Qualifiers
   is
      procedure Set_Qualifier (V : in out Boolean) is
      begin
         if V then
            Error_Msg_Parse ("duplicate %t qualifier", +Current_Token);
         else
            V := True;
         end if;
      end Set_Qualifier;

      Qualifiers : Property_Qualifiers;
   begin
      Qualifiers := (others => False);
      loop
         case Current_Token is
            when Tok_Static =>
               Set_Qualifier (Qualifiers.Has_Static);
            when Tok_Protected =>
               if Qualifiers.Has_Local then
                  Error_Msg_Parse ("'protected' incompatible with 'local'");
               end if;
               Set_Qualifier (Qualifiers.Has_Protected);
            when Tok_Local =>
               if Qualifiers.Has_Protected then
                  Error_Msg_Parse ("'local' incompatible with 'protected'");
               end if;
               Set_Qualifier (Qualifiers.Has_Local);
            when Tok_Virtual =>
               Set_Qualifier (Qualifiers.Has_Virtual);
            when Tok_Rand =>
               if Qualifiers.Has_Randc then
                  Error_Msg_Parse ("'rand' incompatible with 'randc'");
               end if;
               Set_Qualifier (Qualifiers.Has_Rand);
            when Tok_Randc =>
               if Qualifiers.Has_Rand then
                  Error_Msg_Parse ("'randc' incompatible with 'rand'");
               end if;
               Set_Qualifier (Qualifiers.Has_Randc);
            when others =>
               exit;
         end case;

         --  Skip qualifier.
         Scan;
      end loop;

      return Qualifiers;
   end Parse_Property_Qualifiers;

   procedure Check_No_Random_Qualifier (Qual : in out Property_Qualifiers) is
   begin
      if Qual.Has_Rand or Qual.Has_Randc then
         Error_Msg_Parse ("rand/randc qualifiers not allowed for methods");
         Qual.Has_Rand := False;
         Qual.Has_Randc := False;
      end if;
   end Check_No_Random_Qualifier;

   procedure Set_Property_Qualifiers (N : Node; Qual : Property_Qualifiers) is
   begin
      if Qual.Has_Static then
         --  'static' was present.
         Set_Static_Flag (N, True);
      end if;

      if Qual.Has_Virtual then
         Set_Virtual_Flag (N, True);
      end if;

      if Qual.Has_Protected then
         Set_Has_Visibility (N, True);
         Set_Visibility (N, Visibility_Protected);
      elsif Qual.Has_Local then
         Set_Has_Visibility (N, True);
         Set_Visibility (N, Visibility_Local);
      else
         Set_Visibility (N, Visibility_Public);
      end if;
   end Set_Property_Qualifiers;

   procedure Parse_Class_Properties (Constr : in out Items_Constr;
                                     Qual : Property_Qualifiers;
                                     Is_Const : Boolean)
   is
      Local_Constr : Items_Constr;
      N : Node;
      Decl_Type : Type_Node;
      Has_Var : Boolean;
      Qual2 : Property_Qualifiers;
   begin
      Parse_Var_Opt (Has_Var);
      Decl_Type := Parse_Data_Type_Or_Implicit;

      if Qual.Has_Virtual then
         --  'virtual' was parsed as a qualifier, but in fact it is part of
         --  the data type (for a virtual interface).
         case Get_Kind (Decl_Type.Typ) is
            when N_Name
              | N_Dotted_Name =>
               pragma Assert (Decl_Type.Own);
               --  TODO: handle parameter values.
               N := Create_Node (N_Virtual_Interface);
               Location_Copy (N, Decl_Type.Typ);
               Set_Interface (N, Decl_Type.Typ);
               Decl_Type.Typ := N;

            when others =>
               Error_Msg_Parse ("properties cannot be virtual");
         end case;
      end if;

      --  No virtual qualifiers for properties.
      Qual2 := Qual;
      Qual2.Has_Virtual := False;

      Init_Constr (Local_Constr, Get_Parent (Constr));
      Parse_List_Of_Variable_Decl_Assignments (Local_Constr, Decl_Type, N_Var);

      N := Get_Constr_Chain (Local_Constr);
      while N /= Null_Node loop
         Set_Is_Const (N, Is_Const);
         Set_Has_Var (N, Has_Var);
         Set_Property_Qualifiers (N, Qual2);

         if Qual.Has_Rand then
            Set_Random_Flag (N, True);
         end if;
         if Qual.Has_Randc then
            Set_Random_Flag (N, True);
            Set_Randc_Flag (N, True);
         end if;

         N := Get_Chain (N);
      end loop;

      Append_Constr (Constr, Local_Constr);

      --  Skip ';'.
      Scan_Declaration_Semicolon;
   end Parse_Class_Properties;

   --  1800-2017 8.3 Syntax
   --  method_prototype ::=
   --      task_prototype
   --    | function_prototype
   function Parse_Method_Prototype (Fkind : Nkind; Tkind : Nkind) return Node
   is
      Res : Node;
      Atype : Type_Node;
   begin
      case Current_Token is
         when Tok_Function =>
            Res := Create_Node (Fkind);

            --  Skip 'function'.
            Scan;

            Atype := Parse_Data_Type_Or_Implicit;
            Set_Type_Node (Res, Atype);

         when Tok_Task =>
            Res := Create_Node (Tkind);

            --  Skip 'task'.
            Scan;

         when others =>
            Error_Msg_Parse ("'function' or 'task' expected");
            Skip_Until_Semicolon;
            return Null_Node;
      end case;

      if Current_Token = Tok_Identifier or Current_Token = Tok_New then
         Set_Identifier (Res, Current_Identifier);
         Set_Token_Location (Res);

         --  Skip identifier.
         Scan;
      else
         Error_Msg_Parse ("function/task identifier required");
      end if;

      if Current_Token = Tok_Left_Paren then
         Set_Ansi_Port_Flag (Res, True);
         Set_Tf_Ports_Chain (Res, Parse_Tf_Port_List (Res));
      end if;

      return Res;
   end Parse_Method_Prototype;

   --  1800-2017 18.5 Constraint blocks
   --  constraint_set ::=
   --      constraint_expression
   --    | '{' { constraint_expression } '}'
   function Parse_Constraint_Set return Node
   is
      First, Last : Node;
      El : Node;
   begin
      if Current_Token = Tok_Left_Curly then
         --  Skip '{'.
         Scan;

         Init_Chain (First, Last);
         loop
            exit when Current_Token = Tok_Right_Curly;

            El := Parse_Constraint_Expression;
            exit when El = Null_Node;

            Append_Chain (First, Last, El);
         end loop;

         Scan_Or_Error (Tok_Right_Curly, "'}' expected after constraint_set");
         return First;
      else
         return Parse_Constraint_Expression;
      end if;
   end Parse_Constraint_Set;

   --  1800-2017 18.5 Constraint blocks
   --  constraint_expression ::=
   --      [ SOFT ] expression_or_dist ;
   --    | expression -> constraint_set
   --    | IF ( expression ) constraint_set else constraint_set
   --    | FOREACH (ps_or_hierarhical_array_identifier '[' loop_variables ']' )
   --        constraint_set
   --    | DISABLE SOFT constraint_primary ;
   function Parse_Constraint_Expression return Node
   is
      Expr : Node;
      Res : Node;
   begin
      case Current_Token is
         when Tok_If =>
            Res := Create_Node (N_Constraint_If);
            Set_Token_Location (Res);

            --  Skip 'if'.
            Scan;

            Set_Condition (Res, Parse_Parenthesis_Expression);

            Set_Cond_True (Res, Parse_Constraint_Set);

            if Current_Token = Tok_Else then
               --  Skip 'else'.
               Scan;

               Set_Cond_False (Res, Parse_Constraint_Set);
            end if;

         when Tok_Foreach =>
            Res := Parse_Foreach (N_Constraint_Foreach);
            Set_Constraint_Set (Res, Parse_Constraint_Set);

         when others =>
            Expr := Parse_Expression;
            if Expr = Null_Node then
               Error_Msg_Parse ("constraint expression expected");
               return Null_Node;
            end if;

            Res := Create_Node (N_Constraint_Expression);
            Set_Location (Res, Get_Location (Expr));
            Set_Expression (Res, Expr);

            Scan_Or_Error (Tok_Semicolon,
                           "';' expected at end of constraint expression");
      end case;

      return Res;
   end Parse_Constraint_Expression;

   --  1800-2017 18.5 Constraint blocks
   --  constraint_block ::= '{' { constraint_block_item } '}'
   --
   --  constraint_block_item ::=
   --      SOLVE solve_before_list BEFORE solve_before_list ;
   --    | constraint_expression
   function Parse_Constraint_Block return Node
   is
      First, Last : Node;
      El : Node;
   begin
      Scan_Or_Error (Tok_Left_Curly, "'{' expected before constraint block");

      Init_Chain (First, Last);
      loop
         exit when Current_Token = Tok_Right_Curly;
         El := Parse_Constraint_Expression;
         exit when El = Null_Node;

         Append_Chain (First, Last, El);
      end loop;

      --  Skip '}'.
      Scan;

      return First;
   end Parse_Constraint_Block;

   --  1800-2017 18.5 Constraint blocks
   --  constraint_declaration ::=
   --    [ STATIC ] CONSTRAINT constraint_identifier constraint_block
   function Parse_Constraint_Declaration return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Constraint);

      --  Skip 'constraint'.
      Scan;

      Set_Token_Location (Res);
      Scan_Identifier (Res, "constraint identifier expected");

      Set_Constraint_Block_Chain (Res, Parse_Constraint_Block);

      return Res;
   end Parse_Constraint_Declaration;

   --  1800-2017 8.3 Syntax
   --  class_item ::=
   --      class_property
   --    | class_method
   --    | class_constraint
   --    | class_declaration
   --    | covergroup_declaration
   --    | local_parameter_declaration ;
   --    | parameter_declaration ;
   --    | ;
   function Parse_Class_Items (Parent : Node) return Node
   is
      Constr : Items_Constr;
      Qual : Property_Qualifiers;
   begin
      --  Parse module items.
      Init_Constr (Constr, Parent);
      loop
         case Current_Token is
            when Tok_Endmodule
              | Tok_Endgenerate
              | Tok_Endinterface
              | Tok_Endclass
              | Tok_End =>
               exit;
            when Tok_Parameter =>
               Parse_Parameter_Declaration (N_Parameter, Constr);
            when Tok_Localparam =>
               Parse_Parameter_Declaration (N_Localparam, Constr);
            when Tok_Constraint =>
               Append_Node (Constr, Parse_Constraint_Declaration);
            when Toks_Verilog_Types | Toks_SV30_Types | Toks_SV31_Types
              | Tok_Event | Tok_Identifier =>
               Parse_Class_Properties (Constr, (others => False), False);
            when Tok_Const =>
               --  Skip 'const'.
               Scan;

               Qual := Parse_Property_Qualifiers;
               Parse_Class_Properties (Constr, Qual, True);
            when Tok_Static
              | Tok_Protected
              | Tok_Local
              | Tok_Virtual
              | Tok_Rand
              | Tok_Randc =>
               declare
                  Qual : Property_Qualifiers;
                  Is_Const : Boolean;
                  Decl : Node;
               begin
                  Qual := Parse_Property_Qualifiers;

                  case Current_Token is
                     when Toks_Verilog_Types
                       | Toks_SV30_Types | Toks_SV31_Types
                       | Tok_Event
                       | Tok_Identifier
                       | Tok_Const =>
                        Parse_Const_Opt (Is_Const);
                        Parse_Class_Properties (Constr, Qual, Is_Const);
                     when Tok_Constraint =>
                        -- Decl := Parse_Constraint_Declaration;
                        -- if Qual.Has_Static then
                        --   Set_Static_Flag (Decl, True);
                        -- end if;
                        --  FIXME: check for invalid constraints.
                        --  Append_Node (Constr, Decl);
                        raise Program_Error;
                     when Tok_Task =>
                        Check_No_Random_Qualifier (Qual);
                        Decl := Parse_Task_Declaration;
                        Set_Property_Qualifiers (Decl, Qual);
                        Append_Node (Constr, Decl);
                     when Tok_Function =>
                        Check_No_Random_Qualifier (Qual);
                        Decl := Parse_Function_Declaration;
                        Set_Property_Qualifiers (Decl, Qual);
                        Append_Node (Constr, Decl);
                     when others =>
                        Error_Msg_Parse ("property or method expected");
                        Skip_Until_Semicolon;
                  end case;
               end;
            when Tok_Extern =>
               declare
                  Decl : Node;
               begin
                  --  Skip 'extern'.
                  Scan;

                  Set_Has_Extern_Flag (Parent, True);

                  Qual := Parse_Property_Qualifiers;
                  Check_No_Random_Qualifier (Qual);
                  Decl := Parse_Method_Prototype
                    (N_Extern_Function, N_Extern_Task);
                  Set_Property_Qualifiers (Decl, Qual);
                  Scan_Declaration_Semicolon;
                  Append_Node (Constr, Decl);
               end;
            when Tok_Task =>
               Append_Node (Constr, Parse_Task_Declaration);
            when Tok_Function =>
               Append_Node (Constr, Parse_Function_Declaration);
            when Tok_Pure =>
               declare
                  Decl : Node;
                  Qual : Property_Qualifiers;
               begin
                  --  Skip 'pure'.
                  Scan;

                  Scan_Or_Error
                    (Tok_Virtual, "'virtual' required after 'pure'");

                  Qual := Parse_Property_Qualifiers;
                  Check_No_Random_Qualifier (Qual);

                  case Current_Token is
                     when Tok_Function =>
                        Decl := Parse_Function_Declaration (True);
                     when Tok_Task =>
                        Decl := Parse_Task_Declaration (True);
                     when others =>
                        Error_Msg_Parse ("'task' or 'function' expected");
                        Decl := Null_Node;
                  end case;

                  if Decl /= Null_Node then
                     Set_Virtual_Flag (Decl, True);
                     Set_Pure_Flag (Decl, True);
                     Set_Property_Qualifiers (Decl, Qual);
                     Append_Node (Constr, Decl);
                  end if;
               end;
            when Tok_Typedef =>
               Append_Node (Constr, Parse_Type_Declaration);
            when Tok_Semicolon =>
               --  FIXME: Warning ?
               Scan;
            when Tok_Eof =>
               exit;
            when others =>
               Error_Msg_Parse ("unknown class item");
               Skip_Unknown_Item;
         end case;
      end loop;
      return Get_Constr_Chain (Constr);
   end Parse_Class_Items;

   --  1800-2017 8.3 Syntax
   --  class_declaration ::=
   --    [ VIRTUAL ] CLASS [ lifetime ] class_identifier
   --      [ parameter_port_list ]
   --      [ EXTENDS class_type [ ( list_of_arguments ) ] ]
   --      [ IMPLEMENTS interface_class_type { , interface_class_type } ] ;
   --      { class_item }
   --    ENDCLASS [ : class_identifier ]
   function Parse_Class_Declaration return Node
   is
      Res : Node;
      Has_Virtual : Boolean;
      Loc : Location_Type;
      Id : Name_Id;
      Old_Scope : Node;
      Lifetime : Lifetime_State;
      Base_Type : Type_Node;
   begin
      if Current_Token = Tok_Virtual then
         Has_Virtual := True;

         --  Skip 'virtual'.
         Scan;

         Scan_Or_Error (Tok_Class, "'class' expected after 'virtual'");
      else
         Has_Virtual := False;

         --  Skip 'class'.
         Scan;
      end if;

      Lifetime := Parse_Lifetime;

      --  Identifier.
      Loc := Get_Token_Location;
      if Current_Token = Tok_Identifier then
         Id := Current_Identifier;

         --  Skip identifier.
         Scan;
      else
         Error_Msg_Parse ("identifier for module expected");
         Id := Null_Identifier;
      end if;

      if Current_Token = Tok_Sharp then
         Res := Create_Node (N_Generic_Class);
      else
         Res := Create_Node (N_Class);
      end if;
      Set_Location (Res, Loc);
      Set_Virtual_Flag (Res, Has_Virtual);
      Set_Lifetime (Res, Lifetime);
      Set_Identifier (Res, Id);

      Push_Scope (Res, Old_Scope);

      Set_Parameter_Port_Chain (Res, Parse_Parameter_Port_List (Res));

      if Current_Token = Tok_Extends then
         --  Skip 'extends'.
         Scan;

         Base_Type := Parse_Data_Type_Or_Implicit;
         --  FIXME: not implicit!
         Set_Base_Class_Type (Res, Base_Type.Typ);
         Set_Type_Owner (Res, Base_Type.Own);
      end if;

      --  TODO: implements

      --  Skip ';'.
      Scan_Or_Error (Tok_Semicolon, "missing ';' at end of class");

      --  Class items.
      Set_Class_Item_Chain (Res, Parse_Class_Items (Res));

      Scan_Or_Error (Tok_Endclass, "missing 'endclass' at end of class");
      Parse_End_Name (Res);

      --  TODO: ': class identifier'.

      Pop_Scope (Old_Scope);

      return Res;
   end Parse_Class_Declaration;

   --  Verilog-AMS 2.4.0 5  Analog Behavior
   --  analog_statement ::=
   --      { attribute_instance } analog_loop_generate_statement
   --    | ...
   --    | { attribute_instance } contribution_statement
   --    ...

   --  Verilog-AMS 2.4.0 5  Analog Behavior
   --  analog_construct ::=
   --      ANALOG analog_statement
   --    | ANALOG INITIAL analog_function_statement
   function Parse_Analog_Construct return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Analog);
      Set_Token_Location (Res);

      --  Skip 'analog'.
      Scan;

      Set_Statement (Res, Parse_Statement_Or_Null (Res));

      return Res;
   end Parse_Analog_Construct;

   --  Verilog-AMS 2.4.0 3.6.2 Disciplines
   --  nature_binding ::= potential_or_flow nature_identifier ;
   --
   --  nature_attribute_override ::= potential_or_flow . nature_attribute
   --
   --  potential_or_flow ::= POTENTIAL | FLOW
   function Parse_Discipline_Nature_Binding (Kind : Nkind) return Node
   is
      Loc : constant Location_Type := Get_Token_Location;
      Res : Node;
   begin
      --  Skip 'potential'/'flow'.
      Scan;

      if Current_Token = Tok_Dot then
         --  nature attribute override.
         raise Internal_Error;
      else
         --  nature binding.
         Res := Create_Node (Kind);
         Set_Location (Res, Loc);

         if Current_Token /= Tok_Identifier then
            Error_Msg_Parse ("nature identifier expected");
         else
            Set_Nature (Res, Scan_Name);
         end if;

         --  Skip ';'.
         if Current_Token = Tok_Semicolon then
            Scan;
         else
            Error_Msg_Parse ("';' expected after nature binding");
         end if;

      end if;
      return Res;
   end Parse_Discipline_Nature_Binding;

   --  Verilog-AMS 2.4.0 3.6.2 Disciplines
   --  discipline_declaration ::=
   --    DISCIPLINE discipline_identifier [ ; ]
   --      { discipline_item }
   --   ENDDISCIPLINE
   function Parse_Discipline return Node
   is
      Res : Node;
      First, Last : Node;
      El : Node;
   begin
      Res := Create_Node (N_Discipline);
      Set_Token_Location (Res);

      --  Skip 'analog'.
      Scan;

      Scan_Identifier (Res, "discipline identifier expected");

      --  Skip optional ';'.
      if Current_Token = Tok_Semicolon then
         Scan;
      end if;

      --  Discipline items
      First := Null_Node;
      Last := Null_Node;
      loop
         case Current_Token is
            when Tok_Enddiscipline =>
               --  Skip 'enddiscipline'.
               Scan;

               exit;
            when Tok_Domain =>
               El := Create_Node (N_Discipline_Domain);
               Set_Token_Location (El);

               --  Skip 'domain'.
               Scan;

               case Current_Token is
                  when Tok_Discrete =>
                     Set_Continuous_Flag (El, False);
                     --  Skip 'discrete'.
                     Scan;
                  when Tok_Continuous =>
                     Set_Continuous_Flag (El, True);
                     --  Skip 'continue'.
                     Scan;
                  when others =>
                     Error_Msg_Parse ("'discrete' or 'continuous' expected");
               end case;
               if Current_Token = Tok_Semicolon then
                  --  Skip ','
                  Scan;
               else
                  Error_Msg_Parse ("';' expected after domain binding");
                  Skip_Until_Semicolon;
               end if;

            when Tok_Potential =>
               El := Parse_Discipline_Nature_Binding
                 (N_Discipline_Potential);
            when Tok_Flow =>
               El := Parse_Discipline_Nature_Binding
                 (N_Discipline_Flow);
            when others =>
               Error_Msg_Parse ("discipline item expected");
               Skip_Until_Semicolon;
         end case;
         Set_Parent (El, Res);
         Append_Chain (First, Last, El);
      end loop;

      Set_Discipline_Items (Res, First);

      return Res;
   end Parse_Discipline;

   --  Verilog-AMS 2.4.0 3.6.1 Natures
   --  nature_declaration ::=
   --    NATURE nature_identifier [ : parent_nature ] [ ; ]
   --      { nature_item }
   --    ENDNATURE
   function Parse_Nature return Node
   is
      Res : Node;
      First, Last : Node;
      Item : Node;
   begin
      Res := Create_Node (N_Nature);
      Set_Token_Location (Res);

      --  Skip 'nature'.
      Scan;

      Scan_Identifier (Res, "nature identifier expected");

      --  Skip optional ';'.
      if Current_Token = Tok_Semicolon then
         Scan;
      end if;

      --  Nature items
      First := Null_Node;
      Last := Null_Node;
      loop
         if Current_Token = Tok_Endnature then
            --  Skip 'endnature'.
            Scan;

            exit;
         end if;

         case Current_Token is
            when Tok_Access =>
               Item := Create_Node (N_Nature_Access);
               Set_Token_Location (Item);

               --  Skip then identifier
               Scan;

               Scan_Or_Error
                 (Tok_Equal, "'=' expected after attribute identifier");

               Scan_Identifier
                 (Item, "identifier for access function expected");

            when Tok_Abstol
              | Tok_Ddt_Nature
              | Tok_Idt_Nature
              | Tok_Units
              | Tok_Identifier =>
               Item := Create_Node (N_Nature_Attribute);
               Set_Token_Location (Item);

               Set_Identifier (Item, Current_Identifier);

               --  Skip then identifier
               Scan;

               Scan_Or_Error
                 (Tok_Equal, "'=' expected after attribute identifier");

               Set_Expression (Item, Parse_Expression);

            when others =>
               Error_Msg_Parse ("nature attribute identifier expected");
         end case;

         Scan_Or_Error (Tok_Semicolon,
                        "';' expected after nature attribute");

         Append_Chain (First, Last, Item);
         Set_Parent (Item, Res);
      end loop;

      Set_Nature_Items (Res, First);
      return Res;
   end Parse_Nature;

   --  Verilog-AMS 2.4.0 3.12 Branches
   --  branch_declaration ::=
   --      BRANCH ( branch_terminal [, branch_terminal ] )
   --        list_of_branch_identifiers ;
   --    | ...
   --
   --  list_of_branch_identifiers ::=
   --    branch_identifier [ range ] { , branch_identifier [ range ] }
   --
   --  TODO: range, port_branch_declaration
   procedure Parse_Branch (Constr : in out Items_Constr)
   is
      Branch : Node;
      Term1, Term2 : Node;
   begin
      --  Skip 'branch'
      Scan;

      --  Skip '('.
      Scan_Or_Error (Tok_Left_Paren, "'(' expected after 'branch'");

      Term1 := Parse_Lvalue;

      if Current_Token = Tok_Comma then
         --  Skip ','.
         Scan;

         Term2 := Parse_Lvalue;
      else
         Term2 := Null_Node;
      end if;

      --  Skip ')'.
      Scan_Or_Error (Tok_Right_Paren, "')' expected");

      loop
         Branch := Create_Node (N_Branch);
         Set_Token_Location (Branch);

         Scan_Identifier (Branch, "branch identifier expected");
         Set_Arg1 (Branch, Term1);
         Set_Arg2 (Branch, Term2);
         Append_Node (Constr, Branch);

         exit when Current_Token /= Tok_Comma;

         --  Skip ','.
         Scan;
      end loop;

      Scan_Declaration_Semicolon;
   end Parse_Branch;

   --  Parse time_unit or time_precision in a timescale directive.
   function Parse_Timescale_Time return Int32
   is
      Val : Int32;
   begin
      case Current_Token is
         when Tok_Dec_Number | Tok_Time_Literal =>
            --  1800-2017 22.7 `timescale
            --  The integers in these arguments specify an order of magnitude
            --  for the size of the value; the valid integers are 1, 10, and
            --  100.
            if Current_Number_Lo.Val = 1 then
               Val := 0;
            elsif Current_Number_Lo.Val = 10 then
               Val := 1;
            elsif Current_Number_Lo.Val = 100 then
               Val := 2;
            else
               Error_Msg_Parse ("time integer can only be 1, 10 or 100");
               Val := 0;
            end if;

            --  Skip integer.
            Scan;

         when Tok_Identifier =>
            Error_Msg_Parse ("missing integer");
            Val := 0;

         when others =>
            Error_Msg_Parse ("time value expected");
            return 0;
      end case;

      if Current_Token = Tok_Identifier then
         Val := Val + Parse_Time_Unit;

         --  Skip identifier
         Scan;
      else
         Error_Msg_Parse ("time unit expected");
      end if;

      return Val;
   end Parse_Timescale_Time;

   procedure Update_Simulation_Time_Unit (Precision : Int32) is
   begin
      Simulation_Time_Unit := Int32'Min (Simulation_Time_Unit, Precision);
   end Update_Simulation_Time_Unit;

   procedure Parse_Timescale_Directive
   is
      Res : Node;
      Timeunit : Int32;
      Timeprecision : Int32;
   begin
      Res := Create_Node (N_Timescale_Directive);
      Set_Token_Location (Res);

      --  Skip '`timescale'
      Scan;

      Timeunit := Parse_Timescale_Time;
      Set_Time_Unit (Res, Timeunit);

      --  Skip '/'.
      Scan_Or_Error
        (Tok_Slash, "'/' expected between time unit and time precision");

      Timeprecision := Parse_Timescale_Time;
      Set_Time_Precision (Res, Timeprecision);

      --  1800-2017 22.7 `timescale
      --  The time_precision argument shall be at least as precise as the
      --  time_unit argument; it cannot specify a longer unit of time than
      --  time_unit.
      if Timeprecision > Timeunit then
         Error_Msg_Parse ("time precision cannot be larger than time unit");
      end if;

      --  1800-2017 3.14.3 Simulation time unit
      --  The global time precision, also called simulation time unit, is the
      --  minimum of all the TIMEPRECISION statements, all the time precision
      --  arguments to TIMEUNIT declarations, and the smallest time precision
      --  arguments of all the `timescale compiler directives in the design.
      Update_Simulation_Time_Unit (Timeprecision);

      Current_Timescale := Res;
   end Parse_Timescale_Directive;

   --  1800-2017 17.2 Checker declaration
   --  checker_or_generate_item ::=
   --      checker_or_generate_item_declaration
   --    | initial_construct
   --    | always_construct
   --    | final_construct
   --    | assertion_item
   --    | continuous_assign
   --    | checker_generate_item
   --
   --  checker_or_generate_item_declaraiton ::=
   --      [ RAND ] data_declaration
   --    | function_declaration
   --    | checker_declaration
   --    | assertion_item_declaration
   --    | covergroup_declaration
   --    | genvar_declaration
   --    | clocking_declaration
   --    | DEFAULT CLOCKING clocking_identifier ;
   --    | DEFAULT DISABLE IFF expression_or_dist ;
   --    | ;
   --
   --  checker_generate_item ::=
   --      loop_generate_construct
   --    | condition_generate_construct
   --    | generate_region
   --    | elaboration_system_task
   --
   --  1800-2017 23.2.4 Module contents
   --  module_common_item ::=
   --      module_or_generate_item_declaration
   --    | interface_instantiation
   --    | program_instantiation
   --    | assertion_item
   --    | bind_direction
   --    | continuous_assign
   --    | net_alias
   --    | initial_construct
   --    | final_construct
   --    | always_construct
   --    | loop_generate_construct
   --    | conditional_generate_construct
   --
   --  module_item ::=
   --      port_declaration ;
   --    | non_port_module_item
   --
   --  module_or_generate_item ::=
   --      { attribute_instante } parameter_override
   --    | { attribute_instante } gate_instantiation
   --    | { attribute_instante } udp_instantiation
   --    | { attribute_instante } module_instantiation
   --    | { attribute_instante } module_common_item
   --
   --  module_or_generate_item_declaration
   --      package_or_generate_item_declaration
   --    | genvar_declaration
   --    | clocking_declaration
   --    | DEFAULT CLOCKING clocking_identifier ;
   --    | DEFAULT DISABLE IFF expression_or_dist ;
   --
   --  non_port_module_item ::=
   --      generate_region
   --    | module_or_generate_item
   --    | specify_block
   --    | { attribute_instante } specparam_declaration
   --    | program_declaration
   --    | module_declaration
   --    | interface_declaration
   --    | timeunits_declaration
   --
   --  1800-2017 24.3 The program construct
   --  program_item ::=
   --      port_declaration ;
   --    | non_port_program_item
   --
   --  non_port_program_item ::=
   --      continuous_assign
   --    | module_or_generate_item_declaration
   --    | initial_construct
   --    | final_construct
   --    | concurrent_assertion_item
   --    | timeunits_declaration
   --    | program_generate_item
   --
   --  program_generate_item ::=
   --      loop_generate_construct
   --    | conditional_generate_construct
   --    | generate_region
   --
   --  1800-2017 25.3 Interface syntax
   --  interface_item ::=
   --      port_declaration ;
   --    | non_port_interface_item
   --
   --  non_port_interface_item ::=
   --      generate_region
   --    | interface_or_generate_item
   --    | program_declaration
   --    | modport_declaration
   --    | interface_declaration
   --    | timeunits_declaration
   --
   --  interface_or_generate_item ::=
   --      { attribute_instante } module_common_item
   --    | { attribute_instante } extern_tf_declaration
   --
   --  1800-2017 26.2 Package declarations
   --  package_item ::=
   --      package_or_generate_item_declaration
   --    | anonymous_program
   --    | package_export_declaration
   --    | timeunits_declaration
   --
   --  package_or_generate_item_declaration ::=
   --      net_declaration
   --    | data_declaration
   --    | task_declaration
   --    | function_declaration
   --    | checker_declaration
   --    | dpi_import_export
   --    | extern_constraint_declaration
   --    | class_declaration
   --    | class_constructor_declaration
   --    | local_parameter_declaration ;
   --    | parameter_declaration ;
   --    | covergroup_declaration
   --    | assertion_item_declaration
   --    | ;
   --
   --  1800-2017 27.3 Generate construct syntax
   --  generate_item ::=
   --      module_or_generate_item
   --    | interface_or_generate_item
   --    | checker_or_generate_item
   --
   --  1800-2017 (FIXME: ref ???)
   --  description ::=
   --      module_declaration
   --    | udp_declaration
   --    | interface_declaration
   --    | program_declaration
   --    | package_declaration
   --    | package_item
   --    | bind_directive
   --    | config_declaration
   procedure Parse_Module_Item (Constr : in out Items_Constr)
   is
      Pkind : constant Nkind := Get_Kind (Get_Parent (Constr));
      Attrs : Node;
   begin
      --  TODO: checker, interface, program
      pragma Assert (Pkind = N_Compilation_Unit
                       or Pkind = N_Interface_Declaration
                       or Pkind = N_Package
                       or Pkind = N_Module
                       or Pkind = N_Generate_Region
                       or Pkind = N_Generate_Block
                       or Pkind = N_If_Generate
                       or Pkind = N_Loop_Generate
                       or Pkind = N_Program_Declaration);
      Attrs := Parse_Attribute_Instances_Rep;

      --  FIXME: save attributes.

      case Current_Token is
         when Tok_Endmodule
           | Tok_Endgenerate
           | Tok_Endinterface
           | Tok_Endclass
           | Tok_Endprogram
           | Tok_Endpackage
           | Tok_End =>
            null;
         when Tok_Eof =>
            null;

         when Tok_Timeunit =>
            Append_Node (Constr, Parse_Timeunit);
         when Tok_Timeprecision =>
            Append_Node (Constr, Parse_Timeprecision);

         when Tok_Parameter =>
            Parse_Parameter_Declaration (N_Parameter, Constr);
         when Tok_Localparam =>
            Parse_Parameter_Declaration (N_Localparam, Constr);
         when Tok_Input =>
            Parse_Port_Declaration_Semicolon (Constr, N_Input);
         when Tok_Output =>
            Parse_Port_Declaration_Semicolon (Constr, N_Output);
         when Tok_Inout =>
            Parse_Port_Declaration_Semicolon (Constr, N_Inout);
         when Toks_Verilog_Types | Toks_SV30_Types | Toks_SV31_Types
           | Tok_Event
           | Tok_Const | Tok_Var | Tok_Static | Tok_Automatic =>
            Parse_Variable_Declarations (Constr);

         when Tok_Identifier =>
            Parse_Module_Instantiation (Constr);
         when Tok_Typedef =>
            Append_Node (Constr, Parse_Type_Declaration);
         when Tok_Assert =>
            Append_Node (Constr, Parse_Assert);
         when Tok_Assume =>
            Append_Node (Constr, Parse_Assume);
         when Tok_Default =>
            --  FIXME: append only if not null.
            Append_Node (Constr, Parse_Default);

         when Tok_Wire =>
            Parse_Net_Declarations (Constr, N_Wire);
         when Tok_Wand =>
            Parse_Net_Declarations (Constr, N_Wand);
         when Tok_Wor =>
            Parse_Net_Declarations (Constr, N_Wor);
         when Tok_Tri =>
            Parse_Net_Declarations (Constr, N_Wire);
         when Tok_Tri0 =>
            Parse_Net_Declarations (Constr, N_Tri0);
         when Tok_Tri1 =>
            Parse_Net_Declarations (Constr, N_Tri1);
         when Tok_Trireg =>
            Parse_Net_Declarations (Constr, N_Trireg);
         when Tok_Triand =>
            Parse_Net_Declarations (Constr, N_Triand);
         when Tok_Trior =>
            Parse_Net_Declarations (Constr, N_Trior);
         when Tok_Supply0 =>
            Parse_Net_Declarations (Constr, N_Supply0);
         when Tok_Supply1 =>
            Parse_Net_Declarations (Constr, N_Supply1);

         when Tok_Assign =>
            Parse_Continuous_Assignment (Constr);
         when Tok_Always =>
            Parse_Initial_Always_Statement (Constr, N_Always);
         when Tok_Always_Comb =>
            Parse_Initial_Always_Statement (Constr, N_Always_Comb);
         when Tok_Always_Latch =>
            Parse_Initial_Always_Statement (Constr, N_Always_Latch);
         when Tok_Always_Ff =>
            Parse_Initial_Always_Statement (Constr, N_Always_Ff);
         when Tok_Initial =>
            Parse_Initial_Always_Statement (Constr, N_Initial);
         when Tok_Final =>
            Parse_Initial_Always_Statement (Constr, N_Final);

         when Tok_And =>
            Parse_Gate_Instantiation (Constr, N_Gate_And);
         when Tok_Nand =>
            Parse_Gate_Instantiation (Constr, N_Gate_Nand);
         when Tok_Or =>
            Parse_Gate_Instantiation (Constr, N_Gate_Or);
         when Tok_Nor =>
            Parse_Gate_Instantiation (Constr, N_Gate_Nor);
         when Tok_Xor =>
            Parse_Gate_Instantiation (Constr, N_Gate_Xor);
         when Tok_Xnor =>
            Parse_Gate_Instantiation (Constr, N_Gate_Xnor);

         when Tok_Buf =>
            Parse_Gate_Instantiation (Constr, N_Gate_Buf);
         when Tok_Not =>
            Parse_Gate_Instantiation (Constr, N_Gate_Not);

         when Tok_Bufif0 =>
            Parse_Gate_Instantiation (Constr, N_Gate_Bufif0);
         when Tok_Notif0 =>
            Parse_Gate_Instantiation (Constr, N_Gate_Notif0);
         when Tok_Bufif1 =>
            Parse_Gate_Instantiation (Constr, N_Gate_Bufif1);
         when Tok_Notif1 =>
            Parse_Gate_Instantiation (Constr, N_Gate_Notif1);

         when Tok_Pullup =>
            Parse_Gate_Instantiation (Constr, N_Gate_Pullup);
         when Tok_Pulldown =>
            Parse_Gate_Instantiation (Constr, N_Gate_Pulldown);

         when Tok_Tran =>
            Parse_Gate_Instantiation (Constr, N_Gate_Tran);
         when Tok_Rtran =>
            Parse_Gate_Instantiation (Constr, N_Gate_Rtran);
         when Tok_Tranif0 =>
            Parse_Gate_Instantiation (Constr, N_Gate_Tranif0);
         when Tok_Rtranif0 =>
            Parse_Gate_Instantiation (Constr, N_Gate_Rtranif0);
         when Tok_Tranif1 =>
            Parse_Gate_Instantiation (Constr, N_Gate_Tranif1);
         when Tok_Rtranif1 =>
            Parse_Gate_Instantiation (Constr, N_Gate_Rtranif1);

         when Tok_Nmos =>
            Parse_Gate_Instantiation (Constr, N_Gate_Nmos);
         when Tok_Rnmos =>
            Parse_Gate_Instantiation (Constr, N_Gate_Rnmos);
         when Tok_Pmos =>
            Parse_Gate_Instantiation (Constr, N_Gate_Pmos);
         when Tok_Rpmos =>
            Parse_Gate_Instantiation (Constr, N_Gate_Rpmos);
         when Tok_Cmos =>
            Parse_Gate_Instantiation (Constr, N_Gate_Cmos);

         when Tok_Task =>
            Append_Node (Constr, Parse_Task_Declaration);
         when Tok_Function =>
            Append_Node (Constr, Parse_Function_Declaration);
         when Tok_Import =>
            Parse_Import_Declaration (Constr);
         when Tok_Export =>
            Append_Node (Constr, Parse_DPI_Export_Declaration);
         when Tok_Defparam =>
            Parse_Parameter_Override (Constr);
         when Tok_Specify =>
            Append_Node (Constr, Parse_Specify_Block);
         when Tok_Class
           | Tok_Virtual =>
            Append_Node (Constr, Parse_Class_Declaration);
         when Tok_Modport =>
            Parse_Modport_Declaration (Constr);

         when Tok_Genvar =>
            Parse_Genvar_Declaration (Constr);
         when Tok_Generate =>
            Append_Node (Constr, Parse_Generate_Region);
         when Tok_If =>
            Append_Node (Constr, Parse_If_Generate);
         when Tok_For =>
            Append_Node (Constr, Parse_Loop_Generate);
         when Tok_Case =>
            Append_Node (Constr, Parse_Case_Generate);
         when Tok_Begin =>
            Append_Node (Constr, Parse_Generate_Block);

         when Tok_Clocking =>
            Append_Node (Constr, Parse_Clocking_Declaration);

         when Tok_Module =>
            Append_Node (Constr, Parse_Module_Declaration (Attrs));
         when Tok_Pp_Timescale =>
            Parse_Timescale_Directive;
         when Tok_Primitive =>
            Append_Node (Constr, Parse_Udp_Declaration);
         when Tok_Interface =>
            Append_Node (Constr, Parse_Interface_Declaration);
         when Tok_Program =>
            Append_Node (Constr, Parse_Program_Declaration);
         when Tok_Package =>
            Append_Node (Constr, Parse_Package_Declaration);

         when Tok_Analog =>
            Append_Node (Constr, Parse_Analog_Construct);
         when Tok_Discipline =>
            Append_Node (Constr, Parse_Discipline);
         when Tok_Nature =>
            Append_Node (Constr, Parse_Nature);
         when Tok_Branch =>
            Parse_Branch (Constr);

         when Tok_Semicolon =>
            Error_Msg_Parse ("extra ';' ignored");
            Scan;
         when others =>
            case Get_Kind (Get_Parent (Constr)) is
               when N_Module =>
                  Error_Msg_Parse ("unknown module item");
               when others =>
                  Error_Msg_Parse ("unknown item");
            end case;

            Skip_Unknown_Item;
      end case;
   end Parse_Module_Item;

   function Parse_Module_Item (Parent : Node) return Node
   is
      Constr : Items_Constr;
   begin
      Init_Constr (Constr, Parent);
      Parse_Module_Item (Constr);

      return Get_Constr_Chain (Constr);
   end Parse_Module_Item;

   function Parse_File (Sfe : Source_File_Entry) return Node
   is
      Source : Node;
      Constr : Items_Constr;
   begin
      if Default_Timescale = Null_Node then
         Default_Timescale := Create_Node (N_Timeunits_Declaration);
         Set_Time_Unit (Default_Timescale, -9);
         Set_Time_Precision (Default_Timescale, -9);
      end if;

      Set_File (Sfe);

      Source := Create_Node (N_Compilation_Unit);
      Set_Token_Location (Source);

      --  Scan first token.
      Scan;

      Init_Constr (Constr, Source);
      loop
         case Current_Token is
            when Tok_Eof =>
               exit;
            when Tok_End
               | Tok_Endmodule =>
               Error_Msg_Parse ("unexpected %t in a compilation unit",
                               +Current_Token);
               Scan;
            when others =>
               Parse_Module_Item (Constr);
         end case;
      end loop;

      Set_Descriptions (Source, Get_Constr_Chain (Constr));

      Close_File;

      return Source;
   end Parse_File;
end Verilog.Parse;
