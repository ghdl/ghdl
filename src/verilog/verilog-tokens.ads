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

package Verilog.Tokens is
   pragma Pure (Tokens);

   type Token_Type is
     (
      Tok_None,

      Tok_Left_Paren,     --  (
      Tok_Right_Paren,    --  )

      Tok_Left_Brack,     --  [
      Tok_Right_Brack,    --  ]

      Tok_Left_Curly,     --  {
      Tok_Right_Curly,    --  }

      Tok_Paren_Star,     --  (*
      Tok_Star_Paren,     --  *)  --  Not used.

      Tok_Logic_Neg,      --  !
      Tok_Logic_Ne,       --  !=
      Tok_Case_Ne,        --  !==
      Tok_Sharp,          --  #
      Tok_Sharp_Sharp,    --  ##
      Tok_Bit_And,        --  &
      Tok_Logic_And,      --  &&
      Tok_Star,           --  *
      Tok_Slash,          --  /
      Tok_Modulo,         --  %
      Tok_Plus,           --  +
      Tok_Comma,          --  ,
      Tok_Dot,            --  .
      Tok_Minus,          --  -
      Tok_Colon,          --  :
      Tok_Semicolon,      --  ;
      Tok_Less,           --  <
      Tok_Less_Equal,     --  <=
      Tok_Left_Lshift,    --  <<
      Tok_Left_Ashift,    --  <<<
      Tok_Less_Plus,      --  <+ (verilog-a)
      Tok_Equal,          --  =
      Tok_Logic_Eq,       --  ==
      Tok_Case_Eq,        --  ===
      Tok_Greater,        --  >
      Tok_Greater_Equal,  --  >=
      Tok_Right_Lshift,   --  >>
      Tok_Right_Ashift,   --  >>>
      Tok_Question,       --  ?
      Tok_At,             --  @
      Tok_Bit_Neg,        --  ~
      Tok_Bit_Xnor,       --  ^~
      Tok_Bit_Nxor,       --  ~^
      Tok_Red_Nand,       --  ~&
      Tok_Red_Nor,        --  ~|
      Tok_Bit_Xor,        --  ^
      Tok_Bit_Or,         --  |
      Tok_Logic_Or,       --  ||
      Tok_Trigger,        --  ->
      Tok_Full_Conn,      --  *>
      Tok_Par_Conn,       --  =>
      Tok_Plus_Colon,     --  +:
      Tok_Minus_Colon,    --  -:
      Tok_Tick_Curly,     --  '{
      Tok_Dollar,         --  $
      Tok_Plus_Plus,      --  ++
      Tok_Minus_Minus,    --  --
      Tok_Colon_Colon,    --  ::
      Tok_Dot_Star,       --  .*

      --  Toks_Op_Asgn
      Tok_Plus_Asgn,    --  +=
      Tok_Minus_Asgn,   --  -=
      Tok_Mul_Asgn,     --  *=
      Tok_Div_Asgn,     --  /=
      Tok_Mod_Asgn,     --  %=
      Tok_And_Asgn,     --  &=
      Tok_Or_Asgn,      --  |=
      Tok_Xor_Asgn,     --  ^=
      Tok_Shr_Asgn,     --  <<=
      Tok_Shl_Asgn,     --  >>=
      Tok_Asr_Asgn,     --  <<<=
      Tok_Asl_Asgn,     --  >>>=

      --  Sequence and property
      Tok_Brack_Star_Brack,    --  [*]
      Tok_Brack_Star,          --  [*
      Tok_Brack_Plus_Brack,    --  [+]
      Tok_Sharp_Star_Concat,   --  ##[*]
      Tok_Sharp_Plus_Concat,   --  ##[+]
      Tok_Sharp_Bracket,       --  ##[
      Tok_Bar_Arrow,           --  |->
      Tok_Bar_Double_Arrow,    --  |=>
      Tok_Sharp_Minus_Sharp,   --  #-#
      Tok_Sharp_Equal_Sharp,   --  #=#

      Tok_Base_Bin,            --  'b
      Tok_Base_Oct,            --  'o
      Tok_Base_Hex,            --  'h
      Tok_Base_Dec,            --  'd
      Tok_Base_Signed_Bin,     --  'sb
      Tok_Base_Signed_Oct,     --  'so
      Tok_Base_Signed_Hex,     --  'sh
      Tok_Base_Signed_Dec,     --  'sd

      --  Literals.
      --  These tokens are not self-contained.
      Tok_Number_32,
      Tok_Number_64,
      Tok_Dec_Number,   --  [0-9][0-9_]*
      Tok_Dec_Bignum,
      Tok_Bignum,
      Tok_Real_Number,
      Tok_Scale_Number,
      Tok_Time_Literal,
      Tok_Fixed_Time_Literal,
      Tok_Identifier,
      Tok_System,
      Tok_String_Literal,

      --  Patterns in UDP tables.
      Tok_Udp_Dash,  --  Output
      Tok_Udp_0,     --  Output, level
      Tok_Udp_1,     --  Output, level
      Tok_Udp_X,     --  Output, level
      Tok_Udp_Qm,    --  Level
      Tok_Udp_B,     --  Level
      Tok_Udp_R,     --  Edge
      Tok_Udp_F,     --  Edge
      Tok_Udp_P,     --  Edge
      Tok_Udp_N,     --  Edge
      Tok_Udp_Star,  --  Edge

      --  First verilog keyword.

      --  Keywords in alpha order (except for some keywords below).
      Tok_Always,
      Tok_Assign,
      Tok_Begin,
      Tok_Case,
      Tok_Casex,
      Tok_Casez,
      Tok_Deassign,
      Tok_Default,
      Tok_Defparam,
      Tok_Disable,
      Tok_Edge,
      Tok_Else,
      Tok_End,
      Tok_Endcase,
      Tok_Endfunction,
      Tok_Endmodule,
      Tok_Endprimitive,
      Tok_Endspecify,
      Tok_Endtable,
      Tok_Endtask,
      Tok_Event,
      Tok_For,
      Tok_Force,
      Tok_Forever,
      Tok_Fork,
      Tok_Function,
      Tok_If,
      Tok_Ifnone,
      Tok_Initial,
      Tok_Join,
      Tok_Macromodule,
      Tok_Module,
      Tok_Negedge,
      Tok_Parameter,
      Tok_Posedge,
      Tok_Primitive,
      Tok_Release,
      Tok_Repeat,
      Tok_Scalared,
      Tok_Specify,
      Tok_Specparam,
      Tok_Table,
      Tok_Task,
      Tok_Vectored,
      Tok_Wait,
      Tok_While,

      --  Toks_Verilog_Types
      Tok_Integer,
      Tok_Real,
      Tok_Realtime,
      Tok_Reg,
      Tok_Time,

      --  Toks_Port
      Tok_Input,
      Tok_Inout,
      Tok_Output,

      --  Gate keywords.
      Tok_Cmos,
      Tok_Rcmos,
      Tok_Bufif0,
      Tok_Bufif1,
      Tok_Notif0,
      Tok_Notif1,
      Tok_Nmos,
      Tok_Pmos,
      Tok_Rnmos,
      Tok_Rpmos,
      Tok_And,
      Tok_Nand,
      Tok_Or,
      Tok_Nor,
      Tok_Xor,
      Tok_Xnor,
      Tok_Buf,
      Tok_Not,
      Tok_Tranif0,
      Tok_Tranif1,
      Tok_Rtranif0,
      Tok_Rtranif1,
      Tok_Rtran,
      Tok_Tran,
      Tok_Pulldown,
      Tok_Pullup,

      --  Drive strength keywords.
      Tok_Supply0,
      Tok_Strong0,
      Tok_Pull0,
      Tok_Weak0,
      Tok_Highz0,
      Tok_Supply1,
      Tok_Strong1,
      Tok_Pull1,
      Tok_Weak1,
      Tok_Highz1,

      --  Net type
      Tok_Tri,
      Tok_Triand,
      Tok_Trior,
      Tok_Trireg,
      Tok_Tri0,
      Tok_Tri1,
      Tok_Wire,
      Tok_Wand,
      Tok_Wor,

      --  Charge strength keywords.
      Tok_Large,
      Tok_Medium,
      Tok_Small,

      --  Verilog 2001
      Tok_Automatic,
      Tok_Endgenerate,
      Tok_Generate,
      Tok_Genvar,
      Tok_Localparam,
      Tok_Noshowcancelled,
      Tok_Pulsestyle_Onevent,
      Tok_Pulsestyle_Ondetect,
      Tok_Showcancelled,
      Tok_Signed,
      Tok_Unsigned,

      --  Verilog 2001 config
      Tok_Cell,
      Tok_Config,
      Tok_Design,
      Tok_Endconfig,
      Tok_Incdir,
      Tok_Include,
      Tok_Instance,
      Tok_Liblist,
      Tok_Library,
      Tok_Use,

      --  Verilog 2005
      Tok_Uwire,

      --  SV 3.0
      Tok_Always_Comb,
      Tok_Always_Ff,
      Tok_Always_Latch,
      Tok_Assert,
      --  Tok_Assert_Strobe,  --  Not in SV2005
      Tok_Break,
      --  Tok_Char,     --  Not in SV2005
      --  Tok_Changed,  --  Not in SV2005
      Tok_Const,
      Tok_Continue,
      Tok_Do,
      Tok_Endinterface,
      --  Tok_Endtransition,  --  Not in SV2005
      Tok_Export,
      Tok_Extern,
      Tok_Forkjoin,
      Tok_Iff,
      Tok_Import,
      Tok_Interface,
      --  Tok_Longreal,  -- Not in SV2005
      Tok_Modport,
      Tok_Packed,
      --  Tok_Process,  -- Not in SV2005
      Tok_Priority,
      Tok_Return,
      Tok_Static,
      Tok_Timeprecision,
      Tok_Timeunit,
      --  Tok_Transition,  -- Not in SV2005
      Tok_Type,
      Tok_Typedef,
      Tok_Unique,
      Tok_Void,

      --  Toks_SV30_Types
      Tok_Bit,
      Tok_Byte,
      Tok_Enum,
      Tok_Int,
      Tok_Logic,
      Tok_Longint,
      Tok_Shortint,
      Tok_Shortreal,
      Tok_Struct,
      Tok_Union,

      --  SV 3.1
      Tok_Alias,
      Tok_Before,
      Tok_Bind,
      Tok_Class,
      Tok_Clocking,
      Tok_Constraint,
      Tok_Context,
      Tok_Cover,
      Tok_Dist,
      Tok_Endclass,
      Tok_Endclocking,
      Tok_Endprogram,
      Tok_Endproperty,
      Tok_Endsequence,
      Tok_Extends,
      Tok_Final,
      Tok_First_Match,
      Tok_Inside,
      Tok_Intersect,
      Tok_Join_Any,
      Tok_Join_None,
      Tok_Local,
      Tok_New,
      Tok_Null,
      Tok_Program,
      Tok_Property,
      Tok_Protected,
      Tok_Pure,
      Tok_Rand,
      Tok_Randc,
      Tok_Ref,
      Tok_Sequence,
      Tok_Solve,
      Tok_Super,
      Tok_This,
      Tok_Throughout,
      Tok_Var,
      Tok_Virtual,
      Tok_Wait_Order,
      Tok_With,
      Tok_Within,

      --  Toks_SV31_Types
      Tok_Chandle,
      Tok_String,

      --  SV 3.1a
      Tok_Assume,
      Tok_Bins,
      Tok_Binsof,
      Tok_Covergroup,
      Tok_Coverpoint,
      Tok_Cross,
      Tok_Endgroup,
      Tok_Endpackage,
      Tok_Expect,
      Tok_Foreach,
      Tok_Ignore_Bins,
      Tok_Illegal_Bins,
      Tok_Matches,
      Tok_Package,
      Tok_Randcase,
      Tok_Randsequence,
      Tok_Tagged,
      Tok_Wildcard,

      --  SV 2009
      Tok_Accept_On,
      Tok_Checker,
      Tok_Endchecker,
      Tok_Eventually,
      Tok_Global,
      Tok_Implies,
      Tok_Let,
      Tok_Nexttime,
      Tok_Reject_On,
      Tok_Restrict,
      Tok_S_Always,
      Tok_S_Eventually,
      Tok_S_Nexttime,
      Tok_S_Until,
      Tok_S_Until_With,
      Tok_Strong,
      Tok_Sync_Accept_On,
      Tok_Sync_Reject_On,
      Tok_Unique0,
      Tok_Until,
      Tok_Until_With,
      Tok_Untyped,
      Tok_Weak,

      --  SV 2012
      Tok_Implements,
      Tok_Interconnect,
      Tok_Nettype,
      Tok_Soft,

      --  Last SV keyword.

      --  Verilog-A

      Tok_Analog,
      Tok_Discipline,
      Tok_Enddiscipline,
      Tok_Nature,
      Tok_Endnature,
      Tok_Potential,
      Tok_Flow,
      Tok_Domain,
      Tok_Discrete,
      Tok_Continuous,
      Tok_Abstol,
      Tok_Access,
      Tok_Ddt_Nature,
      Tok_Idt_Nature,
      Tok_Branch,
      Tok_From,
      Tok_Exclude,
      Tok_Ddt,
      Tok_Idt,
      Tok_White_Noise,
      Tok_Units,

      --  BSV tokens
      Tok_Uidentifier, --  Upper case identifier
      Tok_Lidentifier, --  Lower case identifier (or '_')
      Tok_Numeric,
      Tok_Method,
      Tok_Endmethod,
      Tok_Rule,
      Tok_Endrule,
      Tok_Action,
      Tok_Endaction,
      Tok_Endinstance,
      Tok_Provisos,
      Tok_Deriving,
      Tok_Seq,
      Tok_Endseq,
      Tok_Par,
      Tok_Endpar,
      Tok_Valueof,
      Tok_Rules,
      Tok_Endrules,
      Tok_Typeclass,
      Tok_Endtypeclass,
      Tok_Dependencies,
      Tok_Determines,

      Tok_Default_Clock,
      Tok_Default_Reset,
      Tok_Input_Clock,
      Tok_Input_Reset,
      Tok_Output_Clock,
      Tok_Output_Reset,
      Tok_Enable,
      Tok_Ready,
      Tok_Clocked_By,
      Tok_Reset_By,
      Tok_Ancestor,
      Tok_Same_Family,
      Tok_Schedule,
      Tok_CF,
      Tok_SB,
      Tok_SBR,
      Tok_C,
      Tok_Path,
      Tok_Ifc_Inout,
      Tok_Port,

      Tok_Less_Minus,   --  <-
      Tok_Unbased_0,    --  '0
      Tok_Unbased_1,    --  '1
      Tok_Unbased_Z,    --  'z
      Tok_Unbased_X,    --  'x
      Tok_Star_Star,    --  **
      Tok_Tick,         --  '

      Tok_Pp_Ifdef,
      Tok_Pp_Ifndef,
      Tok_Pp_Else,
      Tok_Pp_Endif,
      Tok_Pp_Include,
      Tok_Pp_Endinclude,  --  Pseudo token at end of included file.
      Tok_Pp_Define,
      Tok_Pp_Undef,
      Tok_Pp_Arg,
      Tok_Pp_Timescale,
      Tok_Pp_File,        --  __FILE__
      Tok_Pp_Line,        --  __LINE__
      Tok_Pp_Macro,
      Tok_Pp_String_Start,  --  Initial `"
      Tok_Pp_String_Arg,    --  String + argument within `"
      Tok_Pp_String_End,    --  String + ending `"
      Tok_Pp_Concat,

      --  Pragma

      Tok_Translate_Off,
      Tok_Translate_On,
      Tok_Pragma_Comment,
      Tok_Pragma_End_Comment,

      --  Misc

      Tok_Line_Comment,
      Tok_Block_Comment,
      Tok_Backslash_Eol,  --  Line continuation in a macro.
      Tok_Eol,
      Tok_Eof
      );

   subtype Tok_Output_Symbols is Token_Type range Tok_Udp_Dash .. Tok_Udp_X;
   subtype Tok_Level_Symbols  is Token_Type range Tok_Udp_0 .. Tok_Udp_B;
   subtype Tok_Edge_Symbols   is Token_Type range Tok_Udp_R .. Tok_Udp_Star;
   subtype Tok_Udp_Symbols    is Token_Type range Tok_Udp_Dash .. Tok_Udp_Star;

   subtype Toks_Strength is Token_Type range
     Tok_Supply0 ..
   --Tok_Strong0
   --Tok_Pull0
   --Tok_Weak0
   --Tok_Highz0
   --Tok_Supply1
   --Tok_Strong1
   --Tok_Pull1
   --Tok_Weak1
     Tok_Highz1;

   subtype Toks_Verilog_Types is Token_Type range
     Tok_Integer ..
   --Tok_Real
   --Tok_Realtime
   --Tok_Reg
     Tok_Time;

   subtype Toks_SV30_Types is Token_Type range
     Tok_Bit ..
   --Tok_Byte
   --Tok_Enum
   --Tok_Int
   --Tok_Logic
   --Tok_Longint
   --Tok_Shortint
   --Tok_Shortreal
   --Tok_Struct
     Tok_Union;

   subtype Toks_SV31_Types is Token_Type range
     Tok_Chandle ..
     Tok_String;

   subtype Toks_Port is Token_Type range
     Tok_Input ..
   --Tok_Inout
     Tok_Output;

   subtype Toks_Base is Token_Type range
     Tok_Base_Bin ..
   --Tok_Base_Oct
   --Tok_Base_Hex
   --Tok_Base_Dec
   --Tok_Base_Signed_Bin
   --Tok_Base_Signed_Oct
   --Tok_Base_Signed_Hex
     Tok_Base_Signed_Dec;

   subtype Toks_BSV_Identifier is Token_Type range
     Tok_Uidentifier .. Tok_Lidentifier;

   subtype Toks_Literals is Token_Type range
     Tok_Number_32 ..
   --Tok_Number_64
   --Tok_Dec_Number
   --Tok_Big_Number
   --Tok_Real_Number
   --Tok_Scale_Number
   --Tok_Time_Literal
   --Tok_Fixed_Time_Literal
   --Tok_Identifier
   --Tok_Extended_Identifier
   --Tok_System
     Tok_String_Literal;

   subtype Toks_Op_Asgn is Token_Type range
     Tok_Plus_Asgn ..
   --Tok_Minus_Asgn
   --Tok_Mul_Asgn
   --Tok_Div_Asgn
   --Tok_Mod_Asgn
   --Tok_And_Asgn
   --Tok_Or_Asgn
   --Tok_Xor_Asgn
   --Tok_Shr_Asgn
   --Tok_Shl_Asgn
   --Tok_Asr_Asgn
     Tok_Asl_Asgn;

   subtype Toks_Keyword is Token_Type range
     Tok_Always .. Tok_Soft;

   function Image (Tok : Token_Type) return String;
end Verilog.Tokens;
