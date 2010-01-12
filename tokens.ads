--  Scanner token definitions.
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
package Tokens is
   pragma Pure (Tokens);

   type Token_Type is
      (
       Tok_Invalid,     -- current_token is not valid.

       Tok_Left_Paren,          -- (
       Tok_Right_Paren,         -- )
       Tok_Left_Bracket,        -- [
       Tok_Right_Bracket,       -- ]
       Tok_Colon,               -- :
       Tok_Semi_Colon,          -- ;
       Tok_Comma,               -- ,
       Tok_Double_Arrow,        -- =>
       Tok_Tick,                -- '
       Tok_Double_Star,         -- **
       Tok_Assign,              -- :=
       Tok_Bar,                 -- |
       Tok_Box,                 -- <>
       Tok_Dot,                 -- .

       Tok_Eof,                 -- End of file.
       Tok_Newline,
       Tok_Comment,
       Tok_Character,
       Tok_Identifier,
       Tok_Integer,
       Tok_Real,
       Tok_String,
       Tok_Bit_String,

   -- relational_operator
       Tok_Equal,               -- =
       Tok_Not_Equal,           -- /=
       Tok_Less,                -- <
       Tok_Less_Equal,          -- <=
       Tok_Greater,             -- >
       Tok_Greater_Equal,       -- >=

   -- sign token
       Tok_Plus,                -- +
       Tok_Minus,               -- -
   -- and adding_operator
       Tok_Ampersand,           -- &

   --  PSL
       Tok_And_And,             -- &&
       Tok_Bar_Bar,             -- ||
       Tok_Left_Curly,          -- {
       Tok_Right_Curly,         -- }
       Tok_Exclam_Mark,         -- !
       Tok_Brack_Star,          -- [*
       Tok_Brack_Plus_Brack,    -- [+]
       Tok_Brack_Arrow,         -- [->
       Tok_Brack_Equal,         -- [=
       Tok_Bar_Arrow,           -- |->
       Tok_Bar_Double_Arrow,    -- |=>
       Tok_Minus_Greater,       -- ->
       Tok_Arobase,             -- @

   -- multiplying operator
       Tok_Star,                -- *
       Tok_Slash,               -- /
       Tok_Mod,                 -- mod
       Tok_Rem,                 -- rem

   -- relation token:
       Tok_And,
       Tok_Or,
       Tok_Xor,
       Tok_Nand,
       Tok_Nor,

   --  miscellaneous operator
       Tok_Abs,
       Tok_Not,

   -- Key words
       Tok_Access,
       Tok_After,
       Tok_Alias,
       Tok_All,
       Tok_Architecture,
       Tok_Array,
       Tok_Assert,
       Tok_Attribute,

       Tok_Begin,
       Tok_Block,
       Tok_Body,
       Tok_Buffer,
       Tok_Bus,

       Tok_Case,
       Tok_Component,
       Tok_Configuration,
       Tok_Constant,

       Tok_Disconnect,
       Tok_Downto,

       Tok_Else,
       Tok_Elsif,
       Tok_End,
       Tok_Entity,
       Tok_Exit,

       Tok_File,
       Tok_For,
       Tok_Function,

       Tok_Generate,
       Tok_Generic,
       Tok_Guarded,

       Tok_If,
       Tok_In,
       Tok_Inout,
       Tok_Is,

       Tok_Label,
       Tok_Library,
       Tok_Linkage,
       Tok_Loop,

       Tok_Map,

       Tok_New,
       Tok_Next,
       Tok_Null,

       Tok_Of,
       Tok_On,
       Tok_Open,
       Tok_Others,
       Tok_Out,

       Tok_Package,
       Tok_Port,
       Tok_Procedure,
       Tok_Process,

       Tok_Range,
       Tok_Record,
       Tok_Register,
       Tok_Report,
       Tok_Return,

       Tok_Select,
       Tok_Severity,
       Tok_Signal,
       Tok_Subtype,

       Tok_Then,
       Tok_To,
       Tok_Transport,
       Tok_Type,

       Tok_Units,
       Tok_Until,
       Tok_Use,

       Tok_Variable,

       Tok_Wait,
       Tok_When,
       Tok_While,
       Tok_With,

   -- Tokens below this line are key words in vhdl93 but not in vhdl87
       Tok_Xnor,
       Tok_Group,
       Tok_Impure,
       Tok_Inertial,
       Tok_Literal,
       Tok_Postponed,
       Tok_Pure,
       Tok_Reject,
       Tok_Shared,
       Tok_Unaffected,

   -- shift_operator
       Tok_Sll,
       Tok_Sla,
       Tok_Sra,
       Tok_Srl,
       Tok_Rol,
       Tok_Ror,

   -- Added by Vhdl 2000:
       Tok_Protected,

   -- PSL words
       Tok_Psl_Default,
       Tok_Psl_Clock,
       Tok_Psl_Property,
       Tok_Psl_Sequence,
       Tok_Psl_Endpoint,
       Tok_Psl_Assert,

       Tok_Psl_Const,
       Tok_Psl_Boolean,
       Tok_Inf,

       Tok_Within,
       Tok_Abort,
       Tok_Before,
       Tok_Always,
       Tok_Never,
       Tok_Eventually,
       Tok_Next_A,
       Tok_Next_E,
       Tok_Next_Event,
       Tok_Next_Event_A,
       Tok_Next_Event_E
      );

   -- subtype Token_Relation_Type is Token_Type range Tok_And .. Tok_Xnor;
   subtype Token_Relational_Operator_Type is Token_Type range
     Tok_Equal .. Tok_Greater_Equal;
   subtype Token_Shift_Operator_Type is Token_Type range
     Tok_Sll .. Tok_Ror;
   subtype Token_Sign_Type is Token_Type range
     Tok_Plus .. Tok_Minus;
   subtype Token_Adding_Operator_Type is Token_Type range
     Tok_Plus .. Tok_Ampersand;
   subtype Token_Multiplying_Operator_Type is Token_Type range
     Tok_Star .. Tok_Rem;

   Tok_First_Keyword :  constant Tokens.Token_Type := Tokens.Tok_Mod;

   -- Return the name of the token.
   function Image (Token: Token_Type) return String;
end Tokens;
