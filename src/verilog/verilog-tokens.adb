--  Verilog tokens
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

package body Verilog.Tokens is
   function Image (Tok : Token_Type) return String is
   begin
      case Tok is
         when Tok_None =>
            return "NONE";

         when Tok_Left_Paren =>
            return "(";
         when Tok_Right_Paren =>
            return ")";

         when Tok_Left_Brack =>
            return "[";
         when Tok_Right_Brack =>
            return "]";

         when Tok_Left_Curly =>
            return "{";
         when Tok_Right_Curly =>
            return "}";

         when Tok_Paren_Star =>
            return "(*";
         when Tok_Star_Paren =>
            return "*)";

         when Tok_Logic_Neg =>
            return "!";
         when Tok_Logic_Ne =>
            return "!=";
         when Tok_Case_Ne =>
            return "!==";
         when Tok_Sharp =>
            return "#";
         when Tok_Sharp_Sharp =>
            return "##";
         when Tok_Bit_And =>
            return "&";
         when Tok_Logic_And =>
            return "&&";
         when Tok_Star =>
            return "*";
         when Tok_Slash =>
            return "/";
         when Tok_Modulo =>
            return "%";
         when Tok_Plus =>
            return "+";
         when Tok_Comma =>
            return ",";
         when Tok_Dot =>
            return ".";
         when Tok_Minus =>
            return "-";
         when Tok_Colon =>
            return ":";
         when Tok_Semicolon =>
            return ";";
         when Tok_Less =>
            return "<";
         when Tok_Less_Equal =>
            return "<=";
         when Tok_Left_Lshift =>
            return "<<";
         when Tok_Left_Ashift =>
            return "<<<";
         when Tok_Less_Plus =>
            return "<+";
         when Tok_Equal =>
            return "=";
         when Tok_Logic_Eq =>
            return "==";
         when Tok_Case_Eq =>
            return "===";
         when Tok_Greater =>
            return ">";
         when Tok_Greater_Equal =>
            return ">=";
         when Tok_Right_Lshift =>
            return ">>";
         when Tok_Right_Ashift =>
            return ">>>";
         when Tok_Question =>
            return "?";
         when Tok_At =>
            return "@";
         when Tok_Bit_Neg =>
            return "~";
         when Tok_Bit_Nxor =>
            return "~^";
         when Tok_Bit_Xnor =>
            return "^~";
         when Tok_Red_Nand =>
            return "~&";
         when Tok_Red_Nor =>
            return "~|";
         when Tok_Bit_Xor =>
            return "^";
         when Tok_Bit_Or =>
            return "|";
         when Tok_Logic_Or =>
            return "||";
         when Tok_Trigger =>
            return "->";
         when Tok_Full_Conn =>
            return "*>";
         when Tok_Par_Conn =>
            return "=>";
         when Tok_Plus_Colon =>
            return "+:";
         when Tok_Minus_Colon =>
            return "-:";
         when Tok_Tick_Curly =>
            return "'{";
         when Tok_Dollar =>
            return "$";
         when Tok_Plus_Plus =>
            return "++";
         when Tok_Minus_Minus =>
            return "--";
         when Tok_Dot_Star =>
            return ".*";

         when Tok_Plus_Asgn =>
            return "+=";
         when Tok_Minus_Asgn =>
            return "-=";
         when Tok_Mul_Asgn =>
            return "*=";
         when Tok_Div_Asgn =>
            return "/=";
         when Tok_Mod_Asgn =>
            return "%=";
         when Tok_And_Asgn =>
            return "&=";
         when Tok_Or_Asgn =>
            return "|=";
         when Tok_Xor_Asgn =>
            return "^=";
         when Tok_Shr_Asgn =>
            return "<<=";
         when Tok_Shl_Asgn =>
            return ">>=";
         when Tok_Asr_Asgn =>
            return "<<<=";
         when Tok_Asl_Asgn =>
            return ">>>=";

         when Tok_Brack_Star_Brack =>
            return "[*]";
         when Tok_Brack_Star =>
            return "[*";
         when Tok_Brack_Plus_Brack =>
            return "[+]";

         when Tok_Sharp_Star_Concat =>
            return "##[*]";
         when Tok_Sharp_Plus_Concat =>
            return "##[+]";
         when Tok_Sharp_Bracket =>
            return "##[";

         when Tok_Bar_Arrow =>
            return "|->";
         when Tok_Bar_Double_Arrow =>
            return "|=>";
         when Tok_Sharp_Minus_Sharp =>
            return "#-#";
         when Tok_Sharp_Equal_Sharp =>
            return "#=#";

         when Tok_Base_Bin =>
            return "'b";
         when Tok_Base_Oct =>
            return "'o";
         when Tok_Base_Hex =>
            return "'h";
         when Tok_Base_Dec =>
            return "'d";
         when Tok_Base_Signed_Bin =>
            return "'sb";
         when Tok_Base_Signed_Oct =>
            return "'so";
         when Tok_Base_Signed_Hex =>
            return "'sh";
         when Tok_Base_Signed_Dec =>
            return "'sd";

         when Tok_Always =>
            return "always";
         when Tok_Assign =>
            return "assign";
         when Tok_Begin =>
            return "begin";
         when Tok_Case =>
            return "case";
         when Tok_Casex =>
            return "casex";
         when Tok_Casez =>
            return "casez";
         when Tok_Deassign =>
            return "deassign";
         when Tok_Default =>
            return "default";
         when Tok_Defparam =>
            return "defparam";
         when Tok_Disable =>
            return "disable";
         when Tok_Edge =>
            return "edge";
         when Tok_Else =>
            return "else";
         when Tok_End =>
            return "end";
         when Tok_Endcase =>
            return "endcase";
         when Tok_Endfunction =>
            return "endfunction";
         when Tok_Endmodule =>
            return "endmodule";
         when Tok_Endprimitive =>
            return "endprimitive";
         when Tok_Endspecify =>
            return "endspecify";
         when Tok_Endtable =>
            return "endtable";
         when Tok_Endtask =>
            return "endtask";
         when Tok_Event =>
            return "event";
         when Tok_For =>
            return "for";
         when Tok_Force =>
            return "force";
         when Tok_Forever =>
            return "forever";
         when Tok_Fork =>
            return "fork";
         when Tok_Function =>
            return "function";
         when Tok_If =>
            return "if";
         when Tok_Ifnone =>
            return "ifnone";
         when Tok_Initial =>
            return "initial";
         when Tok_Join =>
            return "join";
         when Tok_Macromodule =>
            return "macromodule";
         when Tok_Module =>
            return "module";
         when Tok_Negedge =>
            return "negedge";
         when Tok_Parameter =>
            return "parameter";
         when Tok_Posedge =>
            return "posedge";
         when Tok_Primitive =>
            return "primitive";
         when Tok_Real =>
            return "real";
         when Tok_Realtime =>
            return "realtime";
         when Tok_Reg =>
            return "reg";
         when Tok_Release =>
            return "release";
         when Tok_Repeat =>
            return "repeat";
         when Tok_Scalared =>
            return "scalared";
         when Tok_Specify =>
            return "specify";
         when Tok_Specparam =>
            return "specparam";
         when Tok_Table =>
            return "table";
         when Tok_Task =>
            return "task";
         when Tok_Time =>
            return "time";
         when Tok_Vectored =>
            return "vectored";
         when Tok_Wait =>
            return "wait";
         when Tok_While =>
            return "while";

         when Tok_Integer =>
            return "integer";

         when Tok_Input =>
            return "input";
         when Tok_Output =>
            return "output";
         when Tok_Inout =>
            return "inout";

         when Tok_Cmos =>
            return "cmos";
         when Tok_Rcmos =>
            return "rcmos";
         when Tok_Bufif0 =>
            return "bufif0";
         when Tok_Bufif1 =>
            return "Bufif1";
         when Tok_Notif0 =>
            return "notif0";
         when Tok_Notif1 =>
            return "notif1";
         when Tok_Nmos =>
            return "nmos";
         when Tok_Pmos =>
            return "pmos";
         when Tok_Rnmos =>
            return "rnmos";
         when Tok_Rpmos =>
            return "rpmos";
         when Tok_And =>
            return "and";
         when Tok_Nand =>
            return "nand";
         when Tok_Or =>
            return "or";
         when Tok_Nor =>
            return "nor";
         when Tok_Xor =>
            return "xor";
         when Tok_Xnor =>
            return "xnor";
         when Tok_Buf =>
            return "buf";
         when Tok_Not =>
            return "not";
         when Tok_Tranif0 =>
            return "tranif0";
         when Tok_Tranif1 =>
            return "tranif1";
         when Tok_Rtranif0 =>
            return "rtranif0";
         when Tok_Rtranif1 =>
            return "rtranif1";
         when Tok_Rtran =>
            return "rtran";
         when Tok_Tran =>
            return "tran";
         when Tok_Pulldown =>
            return "pulldown";
         when Tok_Pullup =>
            return "pullup";

         when Tok_Supply0 =>
            return "supply0";
         when Tok_Strong0 =>
            return "strong0";
         when Tok_Pull0 =>
            return "pull0";
         when Tok_Weak0 =>
            return "weak0";
         when Tok_Highz0 =>
            return "highz0";
         when Tok_Supply1 =>
            return "supply1";
         when Tok_Strong1 =>
            return "strong1";
         when Tok_Pull1 =>
            return "pull1";
         when Tok_Weak1 =>
            return "weak1";
         when Tok_Highz1 =>
            return "highz1";

         when Tok_Tri =>
            return "tri";
         when Tok_Triand =>
            return "triand";
         when Tok_Trior =>
            return "trior";
         when Tok_Trireg =>
            return "trireg";
         when Tok_Tri0 =>
            return "tri0";
         when Tok_Tri1 =>
            return "tri1";
         when Tok_Wire =>
            return "wire";
         when Tok_Wand =>
            return "wand";
         when Tok_Wor =>
            return "wor";

         when Tok_Large =>
            return "large";
         when Tok_Medium =>
            return "medium";
         when Tok_Small =>
            return "small";

         when Tok_Udp_0 =>
            return "0";
         when Tok_Udp_1 =>
            return "1";
         when Tok_Udp_X =>
            return "x";
         when Tok_Udp_Qm =>
            return "?";
         when Tok_Udp_B =>
            return "b";
         when Tok_Udp_R =>
            return "r";
         when Tok_Udp_F =>
            return "f";
         when Tok_Udp_P =>
            return "p";
         when Tok_Udp_N =>
            return "n";
         when Tok_Udp_Star =>
            return "*";
         when Tok_Udp_Dash =>
            return "-";

         when Tok_Automatic =>
            return "automatic";
         when Tok_Endgenerate =>
            return "endgenerate";
         when Tok_Generate =>
            return "generate";
         when Tok_Genvar =>
            return "Genvar";
         when Tok_Localparam =>
            return "localparam";
         when Tok_Noshowcancelled =>
            return "noshowcancelled";
         when Tok_Pulsestyle_Onevent =>
            return "Pulsestyle_onevent";
         when Tok_Pulsestyle_Ondetect =>
            return "pulsestyle_ondetect";
         when Tok_Showcancelled =>
            return "showcancelled";
         when Tok_Signed =>
            return "signed";
         when Tok_Unsigned =>
            return "unsigned";

         when Tok_Cell =>
            return "cell";
         when Tok_Config =>
            return "config";
         when Tok_Design =>
            return "design";
         when Tok_Endconfig =>
            return "endconfig";
         when Tok_Incdir =>
            return "incdir";
         when Tok_Include =>
            return "include";
         when Tok_Instance =>
            return "instance";
         when Tok_Liblist =>
            return "liblist";
         when Tok_Library =>
            return "library";
         when Tok_Use =>
            return "use";

         when Tok_Uwire =>
            return "uwire";

         when Tok_Always_Comb =>
            return "always_comb";
         when Tok_Always_Ff =>
            return "always_ff";
         when Tok_Always_Latch =>
            return "always_latch";
         when Tok_Assert =>
            return "assert";
         when Tok_Bit =>
            return "bit";
         when Tok_Break =>
            return "break";
         when Tok_Byte =>
            return "byte";
         when Tok_Const =>
            return "const";
         when Tok_Continue =>
            return "continue";
         when Tok_Do =>
            return "do";
         when Tok_Endinterface =>
            return "endinterface";
         when Tok_Enum =>
            return "enum";
         when Tok_Export =>
            return "export";
         when Tok_Extern =>
            return "extern";
         when Tok_Forkjoin =>
            return "forkjoin";
         when Tok_Iff =>
            return "iff";
         when Tok_Import =>
            return "import";
         when Tok_Int =>
            return "int";
         when Tok_Interface =>
            return "interface";
         when Tok_Logic =>
            return "logic";
         when Tok_Longint =>
            return "longint";
         when Tok_Modport =>
            return "modport";
         when Tok_Packed =>
            return "packed";
         when Tok_Priority =>
            return "priority";
         when Tok_Return =>
            return "return";
         when Tok_Shortint =>
            return "shortint";
         when Tok_Shortreal =>
            return "shortreal";
         when Tok_Static =>
            return "static";
         when Tok_Struct =>
            return "struct";
         when Tok_Timeprecision =>
            return "timeprecision";
         when Tok_Timeunit =>
            return "timeunit";
         when Tok_Type =>
            return "type";
         when Tok_Typedef =>
            return "typedef";
         when Tok_Union =>
            return "union";
         when Tok_Unique =>
            return "unique";
         when Tok_Void =>
            return "void";

         when Tok_Alias =>
            return "alias";
         when Tok_Before =>
            return "before";
         when Tok_Bind =>
            return "bind";
         when Tok_Chandle =>
            return "chandle";
         when Tok_Class =>
            return "class";
         when Tok_Clocking =>
            return "clocking";
         when Tok_Constraint =>
            return "constraint";
         when Tok_Context =>
            return "context";
         when Tok_Cover =>
            return "cover";
         when Tok_Dist =>
            return "dist";
         when Tok_Endclass =>
            return "endclass";
         when Tok_Endclocking =>
            return "endclocking";
         when Tok_Endprogram =>
            return "endprogram";
         when Tok_Endproperty =>
            return "endproperty";
         when Tok_Endsequence =>
            return "endsequence";
         when Tok_Extends =>
            return "extends";
         when Tok_Final =>
            return "final";
         when Tok_First_Match =>
            return "first_match";
         when Tok_Inside =>
            return "inside";
         when Tok_Intersect =>
            return "intersect";
         when Tok_Join_Any =>
            return "join_any";
         when Tok_Join_None =>
            return "join_none";
         when Tok_Local =>
            return "local";
         when Tok_New =>
            return "new";
         when Tok_Null =>
            return "null";
         when Tok_Program =>
            return "program";
         when Tok_Property =>
            return "property";
         when Tok_Protected =>
            return "protected";
         when Tok_Pure =>
            return "pure";
         when Tok_Rand =>
            return "rand";
         when Tok_Randc =>
            return "randc";
         when Tok_Ref =>
            return "ref";
         when Tok_Sequence =>
            return "sequence";
         when Tok_Solve =>
            return "solve";
         when Tok_String =>
            return "string";
         when Tok_Super =>
            return "super";
         when Tok_This =>
            return "this";
         when Tok_Throughout =>
            return "throughout";
         when Tok_Var =>
            return "var";
         when Tok_Virtual =>
            return "virtual";
         when Tok_Wait_Order =>
            return "wait_order";
         when Tok_With =>
            return "with";
         when Tok_Within =>
            return "within";

         when Tok_Assume =>
            return "assume";
         when Tok_Bins =>
            return "bins";
         when Tok_Binsof =>
            return "binsof";
         when Tok_Covergroup =>
            return "covergroup";
         when Tok_Coverpoint =>
            return "coverpoint";
         when Tok_Cross =>
            return "cross";
         when Tok_Endgroup =>
            return "endgroup";
         when Tok_Endpackage =>
            return "endpackage";
         when Tok_Expect =>
            return "expect";
         when Tok_Foreach =>
            return "foreach";
         when Tok_Ignore_Bins =>
            return "ignore_bins";
         when Tok_Illegal_Bins =>
            return "illegal_bins";
         when Tok_Matches =>
            return "matches";
         when Tok_Package =>
            return "package";
         when Tok_Randcase =>
            return "randcase";
         when Tok_Randsequence =>
            return "randsequence";
         when Tok_Tagged =>
            return "tagged";
         when Tok_Wildcard =>
            return "wildcard";

         when Tok_Accept_On =>
            return "accept_on";
         when Tok_Checker =>
            return "checker";
         when Tok_Endchecker =>
            return "endchecker";
         when Tok_Eventually =>
            return "eventually";
         when Tok_Global =>
            return "global";
         when Tok_Implies =>
            return "implies";
         when Tok_Let =>
            return "let";
         when Tok_Nexttime =>
            return "nexttime";
         when Tok_Reject_On =>
            return "reject_on";
         when Tok_Restrict =>
            return "restrict";
         when Tok_S_Always =>
            return "s_always";
         when Tok_S_Eventually =>
            return "s_eventually";
         when Tok_S_Nexttime =>
            return "s_nexttime";
         when Tok_S_Until =>
            return "s_until";
         when Tok_S_Until_With =>
            return "s_until_with";
         when Tok_Strong =>
            return "strong";
         when Tok_Sync_Accept_On =>
            return "sync_accept_on";
         when Tok_Sync_Reject_On =>
            return "sync_reject_on";
         when Tok_Unique0 =>
            return "unique0";
         when Tok_Until =>
            return "until";
         when Tok_Until_With =>
            return "until_with";
         when Tok_Untyped =>
            return "untyped";
         when Tok_Weak =>
            return "weak";

         when Tok_Implements =>
            return "implements";
         when Tok_Interconnect =>
            return "interconnect";
         when Tok_Nettype =>
            return "nettype";
         when Tok_Soft =>
            return "soft";

         when Tok_Analog =>
            return "analog";
         when Tok_Discipline =>
            return "discipline";
         when Tok_Enddiscipline =>
            return "enddiscipline";
         when Tok_Nature =>
            return "nature";
         when Tok_Endnature =>
            return "endnature";
         when Tok_Potential =>
            return "potential";
         when Tok_Flow =>
            return "flow";
         when Tok_Domain =>
            return "domain";
         when Tok_Discrete =>
            return "discrete";
         when Tok_Continuous =>
            return "continuous";
         when Tok_Abstol =>
            return "abstol";
         when Tok_Access =>
            return "access";
         when Tok_Ddt_Nature =>
            return "ddt_nature";
         when Tok_Idt_Nature =>
            return "idt_nature";
         when Tok_Branch =>
            return "branch";
         when Tok_From =>
            return "from";
         when Tok_Exclude =>
            return "exclude";
         when Tok_Ddt =>
            return "ddt";
         when Tok_Idt =>
            return "idt";
         when Tok_White_Noise =>
            return "white_noise";
         when Tok_Units =>
            return "units";

         when Tok_Number_32 =>
            return "NUMBER_32";
         when Tok_Number_64 =>
            return "NUMBER_64";
         when Tok_Dec_Number =>
            return "DEC_NUMBER";
         when Tok_Dec_Bignum =>
            return "DEC_BIGNUM";
         when Tok_Bignum =>
            return "BIG_NUMBER";
         when Tok_Real_Number =>
            return "REAL_NUMBER";
         when Tok_Scale_Number =>
            return "SCALE_NUMBER";
         when Tok_Time_Literal =>
            return "TIME_LITERAL";
         when Tok_Fixed_Time_Literal =>
            return "FIXED_TIME_LITERAL";
         when Tok_Identifier =>
            return "IDENTIFIER";
         when Tok_System =>
            return "SYSTEM";
         when Tok_String_Literal =>
            return "STRING_LITERAL";

         when Tok_Colon_Colon =>
            return "::";

         when Tok_Lidentifier =>
            return "identifier";
         when Tok_Uidentifier =>
            return "Identifier";
         when Tok_Numeric =>
            return "numeric";
         when Tok_Method =>
            return "method";
         when Tok_Endmethod =>
            return "endmethod";
         when Tok_Rule =>
            return "rule";
         when Tok_Endrule =>
            return "endrule";
         when Tok_Action =>
            return "action";
         when Tok_Endaction =>
            return "endaction";
         when Tok_Endinstance =>
            return "endinstance";
         when Tok_Provisos =>
            return "provisos";
         when Tok_Deriving =>
            return "deriving";
         when Tok_Seq =>
            return "seq";
         when Tok_Endseq =>
            return "endseq";
         when Tok_Par =>
            return "par";
         when Tok_Endpar =>
            return "endpar";
         when Tok_Valueof =>
            return "valueof";
         when Tok_Rules =>
            return "Rules";
         when Tok_Endrules =>
            return "endrules";
         when Tok_Typeclass =>
            return "typeclass";
         when Tok_Endtypeclass =>
            return "endtypeclass";
         when Tok_Dependencies =>
            return "dependencies";
         when Tok_Determines =>
            return "determines";

         when Tok_Default_Clock =>
            return "default_clock";
         when Tok_Default_Reset =>
            return "default_reset";
         when Tok_Input_Clock =>
            return "input_clock";
         when Tok_Input_Reset =>
            return "input_reset";
         when Tok_Output_Clock =>
            return "output_clock";
         when Tok_Output_Reset =>
            return "output_reset";
         when Tok_Enable =>
            return "enable";
         when Tok_Ready =>
            return "ready";
         when Tok_Clocked_By =>
            return "clocked_by";
         when Tok_Reset_By =>
            return "reset_by";
         when Tok_Ancestor =>
            return "ancestor";
         when Tok_Same_Family =>
            return "same_family";
         when Tok_Schedule =>
            return "schedule";
         when Tok_CF =>
            return "CF";
         when Tok_SB =>
            return "SB";
         when Tok_SBR =>
            return "SBR";
         when Tok_C =>
            return "C";
         when Tok_Path =>
            return "path";
         when Tok_Ifc_Inout =>
            return "ifc_inout";
         when Tok_Port =>
            return "port";

         when Tok_Less_Minus =>
            return "<-";
         when Tok_Unbased_0 =>
            return "'0";
         when Tok_Unbased_1 =>
            return "'1";
         when Tok_Unbased_X =>
            return "'x";
         when Tok_Unbased_Z =>
            return "'z";
         when Tok_Star_Star =>
            return "**";
         when Tok_Tick =>
            return "'";

         when Tok_Pp_Ifdef =>
            return "`ifdef";
         when Tok_Pp_Ifndef =>
            return "`ifndef";
         when Tok_Pp_Else =>
            return "`else";
         when Tok_Pp_Endif =>
            return "`endif";
         when Tok_Pp_Include =>
            return "`include";
         when Tok_Pp_Endinclude =>
            return "`endinclude";
         when Tok_Pp_Define =>
            return "`define";
         when Tok_Pp_Undef =>
            return "`undef";
         when Tok_Pp_Arg =>
            return "`arg";
         when Tok_Pp_Timescale =>
            return "`timescale";
         when Tok_Pp_File =>
            return "`__FILE__";
         when Tok_Pp_Line =>
            return "`__LINE__";
         when Tok_Pp_Macro =>
            return "`macro";
         when Tok_Pp_String_Start =>
            return "`""";
         when Tok_Pp_String_End =>
            return "STR`""";
         when Tok_Pp_String_Arg =>
            return "STR ARG";
         when Tok_Pp_Concat =>
            return "``";

         when Tok_Translate_Off =>
            return "// synthesis translate_off";
         when Tok_Translate_On =>
            return "// synthesis translate_on";
         when Tok_Pragma_Comment =>
            return "// synthesis";
         when Tok_Pragma_End_Comment =>
            return "*END PRAGMA*";

         when Tok_Line_Comment =>
            return "//COMMENT";
         when Tok_Block_Comment =>
            return "/*COMMENT*/";
         when Tok_Backslash_Eol =>
            return "\EOL";
         when Tok_Eol =>
            return "EOL";
         when Tok_Eof =>
            return "EOF";
         --  when others =>
         --     return "???" & Token_Type'Image (Tok);
      end case;
   end Image;
end Verilog.Tokens;
