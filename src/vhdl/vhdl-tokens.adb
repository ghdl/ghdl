--  Scanner token definitions.
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

package body Vhdl.Tokens is
   -- Return the name of the token.
   function Image (Token: Token_Type) return String is
   begin
      case Token is
         when Tok_Invalid =>
            return "<invalid>";
         when Tok_Left_Paren =>
            return "(";
         when Tok_Right_Paren =>
            return ")";
         when Tok_Left_Bracket =>
            return "[";
         when Tok_Right_Bracket =>
            return "]";
         when Tok_Colon =>
            return ":";
         when Tok_Semi_Colon =>
            return ";";
         when Tok_Comma =>
            return ",";
         when Tok_Tick =>
            return "'";
         when Tok_Double_Star =>
            return "**";
         when Tok_Double_Arrow =>
            return "=>";
         when Tok_Assign =>
            return ":=";
         when Tok_Bar =>
            return "|";
         when Tok_Box =>
            return "<>";
         when Tok_Dot =>
            return ".";

         when Tok_Block_Comment_Start =>
            return "/*";
         when Tok_Block_Comment_End =>
            return "*/";

         when Tok_Eof =>
            return "<EOF>";
         when Tok_Newline =>
            return "<newline>";
         when Tok_Line_Comment =>
            return "<line-comment>";
         when Tok_Block_Comment_Text =>
            return "<block-comment>";
         when Tok_Character =>
            return "<character>";
         when Tok_Identifier =>
            return "<identifier>";
         when Tok_Integer
           | Tok_Integer_Letter =>
            return "<integer>";
         when Tok_Real =>
            return "<real>";
         when Tok_String =>
            return "<string>";
         when Tok_Bit_String =>
            return "<bit string>";

         when Tok_Equal_Equal =>
            return "==";

         -- relational_operator:
         when Tok_Equal =>
            return "=";
         when Tok_Not_Equal =>
            return "/=";
         when Tok_Less =>
            return "<";
         when Tok_Less_Equal =>
            return "<=";
         when Tok_Greater =>
            return ">";
         when Tok_Greater_Equal =>
            return ">=";

         when Tok_Match_Equal =>
            return "?=";
         when Tok_Match_Not_Equal =>
            return "?/=";
         when Tok_Match_Less =>
            return "?<";
         when Tok_Match_Less_Equal =>
            return "?<=";
         when Tok_Match_Greater =>
            return "?>";
         when Tok_Match_Greater_Equal =>
            return "?>=";

         -- sign token
         when Tok_Plus =>
            return "+";
         when Tok_Minus =>
            return "-";
         -- and adding_operator
         when Tok_Ampersand =>
            return "&";

         when Tok_Question_Mark =>
            return "?";

         when Tok_Condition =>
            return "??";

         when Tok_Double_Less =>
            return "<<";
         when Tok_Double_Greater =>
            return ">>";
         when Tok_Caret =>
            return "^";

         -- multiplying operator
         when Tok_Star =>
            return "*";
         when Tok_Slash =>
            return "/";
         when Tok_Mod =>
            return "mod";
         when Tok_Rem =>
            return "rem";

         -- relation token:
         when Tok_And =>
            return "and";
         when Tok_Or =>
            return "or";
         when Tok_Xor =>
            return "xor";
         when Tok_Nand =>
            return "nand";
         when Tok_Nor =>
            return "nor";
         when Tok_Xnor =>
            return "xnor";

         -- Reserved words.
         when Tok_Abs =>
            return "abs";
         when Tok_Access =>
            return "access";
         when Tok_After =>
            return "after";
         when Tok_Alias =>
            return "alias";
         when Tok_All =>
            return "all";
         when Tok_Architecture =>
            return "architecture";
         when Tok_Array =>
            return "array";
         when Tok_Assert =>
            return "assert";
         when Tok_Attribute =>
            return "attribute";

         when Tok_Begin =>
            return "begin";
         when Tok_Block =>
            return "block";
         when Tok_Body =>
            return "body";
         when Tok_Buffer =>
            return "buffer";
         when Tok_Bus =>
            return "bus";

         when Tok_Case =>
            return "case";
         when Tok_Component =>
            return "component";
         when Tok_Configuration =>
            return "configuration";
         when Tok_Constant =>
            return "constant";

         when Tok_Disconnect =>
            return "disconnect";
         when Tok_Downto =>
            return "downto";

         when Tok_Else =>
            return "else";
         when Tok_Elsif =>
            return "elsif";
         when Tok_End =>
            return "end";
         when Tok_Entity =>
            return "entity";
         when Tok_Exit =>
            return "exit";

         when Tok_File =>
            return "file";
         when Tok_For =>
            return "for";
         when Tok_Function =>
            return "function";

         when Tok_Generate =>
            return "generate";
         when Tok_Generic =>
            return "generic";
         when Tok_Group =>
            return "group";
         when Tok_Guarded =>
            return "guarded";

         when Tok_If =>
            return "if";
         when Tok_Impure =>
            return "impure";
         when Tok_In =>
            return "in";
         when Tok_Inertial =>
            return "inertial";
         when Tok_Inout =>
            return "inout";
         when Tok_Is =>
            return "is";

         when Tok_Label =>
            return "label";
         when Tok_Library =>
            return "library";
         when Tok_Linkage =>
            return "linkage";
         when Tok_Literal =>
            return "literal";
         when Tok_Loop =>
            return "loop";

         when Tok_Map =>
            return "map";

         when Tok_New =>
            return "new";
         when Tok_Next =>
            return "next";
         when Tok_Not =>
            return "not";
         when Tok_Null =>
            return "null";

         when Tok_Of =>
            return "of";
         when Tok_On =>
            return "on";
         when Tok_Open =>
            return "open";
         when Tok_Out =>
            return "out";
         when Tok_Others =>
            return "others";

         when Tok_Package =>
            return "package";
         when Tok_Port =>
            return "port";
         when Tok_Postponed =>
            return "postponed";
         when Tok_Procedure =>
            return "procedure";
         when Tok_Process =>
            return "process";
         when Tok_Pure =>
            return "pure";

         when Tok_Range =>
            return "range";
         when Tok_Record =>
            return "record";
         when Tok_Register =>
            return "register";
         when Tok_Reject =>
            return "reject";
         when Tok_Report =>
            return "report";
         when Tok_Return =>
            return "return";

         when Tok_Select =>
            return "select";
         when Tok_Severity =>
            return "severity";
         when Tok_Shared =>
            return "shared";
         when Tok_Signal =>
            return "signal";
         when Tok_Subtype =>
            return "subtype";

         when Tok_Then =>
            return "then";
         when Tok_To =>
            return "to";
         when Tok_Transport =>
            return "transport";
         when Tok_Type =>
            return "type";

         when Tok_Unaffected =>
            return "unaffected";
         when Tok_Units =>
            return "units";
         when Tok_Until =>
            return "until";
         when Tok_Use =>
            return "use";

         when Tok_Variable =>
            return "variable";

         when Tok_Wait =>
            return "wait";
         when Tok_When =>
            return "when";
         when Tok_While =>
            return "while";
         when Tok_With =>
            return "with";

         -- shift_operator
         when Tok_Sll =>
            return "sll";
         when Tok_Sla =>
            return "sla";
         when Tok_Sra =>
            return "sra";
         when Tok_Srl =>
            return "srl";
         when Tok_Rol =>
            return "rol";
         when Tok_Ror =>
            return "ror";

         --  VHDL 00
         when Tok_Protected =>
            return "protected";

         --  VHDL 08
         when Tok_Assume =>
            return "assume";
         when Tok_Context =>
            return "context";
         when Tok_Cover =>
            return "cover";
         when Tok_Default =>
            return "default";
         when Tok_Force =>
            return "force";
         when Tok_Parameter =>
            return "parameter";
         when Tok_Property =>
            return "property";
         when Tok_Release =>
            return "release";
         when Tok_Restrict =>
            return "restrict";
         when Tok_Restrict_Guarantee =>
            return "restrict_guarantee";
         when Tok_Sequence =>
            return "sequence";
         when Tok_Inherit =>
            return "inherit";
         when Tok_Vmode =>
            return "vmode";
         when Tok_Vprop =>
            return "vprop";
         when Tok_Vunit =>
            return "vunit";

         --  AMS-VHDL
         when Tok_Across =>
            return "across";
         when Tok_Break =>
            return "break";
         when Tok_Limit =>
            return "limit";
         when Tok_Nature =>
            return "nature";
         when Tok_Noise =>
            return "noise";
         when Tok_Procedural =>
            return "procedural";
         when Tok_Quantity =>
            return "quantity";
         when Tok_Reference =>
            return "reference";
         when Tok_Spectrum =>
            return "spectrum";
         when Tok_Subnature =>
            return "subnature";
         when Tok_Terminal =>
            return "terminal";
         when Tok_Through =>
            return "through";
         when Tok_Tolerance =>
            return "tolerance";

         --  PSL operators
         when Tok_And_And =>
            return "&&";
         when Tok_Bar_Bar =>
            return "||";
         when Tok_Left_Curly =>
            return "{";
         when Tok_Right_Curly =>
            return "}";
         when Tok_Exclam_Mark =>
            return "!";
         when Tok_Brack_Star =>
            return "[*";
         when Tok_Brack_Plus_Brack =>
            return "[+]";
         when Tok_Brack_Arrow =>
            return "[->";
         when Tok_Brack_Equal =>
            return "[=";
         when Tok_Bar_Arrow =>
            return "|->";
         when Tok_Bar_Double_Arrow =>
            return "|=>";
         when Tok_Minus_Greater =>
            return "->";
         when Tok_Equiv_Arrow =>
            return "<->";
         when Tok_Arobase =>
            return "@";

         --  PSL keywords
         when Tok_Psl_Clock =>
            return "clock";
         when Tok_Onehot0 =>
            return "onehot0";
         when Tok_Onehot =>
            return "onehot";
         when Tok_Fell =>
            return "fell";
         when Tok_Rose =>
            return "rose";
         when Tok_Stable =>
            return "stable";
         when Tok_Prev =>
            return "prev";
         when Tok_Psl_Endpoint =>
            return "endpoint";
         when Tok_Psl_Const =>
            return "const";
         when Tok_Psl_Boolean =>
            return "boolean";
         when Tok_Inf =>
            return "inf";
         when Tok_Within =>
            return "within";
         when Tok_Abort =>
            return "abort";
         when Tok_Async_Abort =>
            return "async_abort";
         when Tok_Sync_Abort =>
            return "sync_abort";
         when Tok_Before =>
            return "before";
         when Tok_Before_Em =>
            return "before!";
         when Tok_Before_Un =>
            return "before_";
         when Tok_Before_Em_Un =>
            return "before!_";
         when Tok_Until_Em =>
            return "until!";
         when Tok_Until_Un =>
            return "until_";
         when Tok_Until_Em_Un =>
            return "until!_";
         when Tok_Always =>
            return "always";
         when Tok_Never =>
            return "never";
         when Tok_Eventually_Em =>
            return "eventually!";
         when Tok_Next_Em =>
            return "next!";
         when Tok_Next_A =>
            return "next_a";
         when Tok_Next_A_Em =>
            return "next_a!";
         when Tok_Next_E =>
            return "next_e";
         when Tok_Next_E_Em =>
            return "next_e!";
         when Tok_Next_Event =>
            return "next_event";
         when Tok_Next_Event_Em =>
            return "next_event!";
         when Tok_Next_Event_A =>
            return "next_event_a";
         when Tok_Next_Event_A_Em =>
            return "next_event_a!";
         when Tok_Next_Event_E =>
            return "next_event_e";
         when Tok_Next_Event_E_Em =>
            return "next_event_e!";
      end case;
   end Image;

end Vhdl.Tokens;
