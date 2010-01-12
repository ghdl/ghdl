--  Well known name table entries.
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
with Types; use Types;

-- Note: since all identifiers declared in this package begins with either
-- std_names or name, this package is expected to be use'd.

package Std_Names is
   -- Predefined names.
   Name_First_Character : constant Name_Id := 1;
   Name_Last_Character : constant Name_Id :=
     Name_First_Character + Character'Pos (Character'Last)
     - Character'Pos (Character'First);
   subtype Name_Characters is Name_Id
     range Name_First_Character .. Name_Last_Character;

   Name_First_Keyword : constant Name_Id := Name_Last_Character + 1;

   --  Word operators.
   Name_Mod :            constant Name_Id := Name_First_Keyword + 000;
   Name_Rem :            constant Name_Id := Name_First_Keyword + 001;

   Name_And :            constant Name_Id := Name_First_Keyword + 002;
   Name_Or :             constant Name_Id := Name_First_Keyword + 003;
   Name_Xor :            constant Name_Id := Name_First_Keyword + 004;
   Name_Nand :           constant Name_Id := Name_First_Keyword + 005;
   Name_Nor :            constant Name_Id := Name_First_Keyword + 006;

   Name_Abs :            constant Name_Id := Name_First_Keyword + 007;
   Name_Not :            constant Name_Id := Name_First_Keyword + 008;

   subtype Name_Logical_Operators is Name_Id range Name_And .. Name_Nor;
   subtype Name_Word_Operators is Name_Id range Name_Mod .. Name_Not;

   Name_Access :         constant Name_Id := Name_First_Keyword + 009;
   Name_After :          constant Name_Id := Name_First_Keyword + 010;
   Name_Alias :          constant Name_Id := Name_First_Keyword + 011;
   Name_All :            constant Name_Id := Name_First_Keyword + 012;
   Name_Architecture :   constant Name_Id := Name_First_Keyword + 013;
   Name_Array :          constant Name_Id := Name_First_Keyword + 014;
   Name_Assert :         constant Name_Id := Name_First_Keyword + 015;
   Name_Attribute :      constant Name_Id := Name_First_Keyword + 016;

   Name_Begin :          constant Name_Id := Name_First_Keyword + 017;
   Name_Block :          constant Name_Id := Name_First_Keyword + 018;
   Name_Body :           constant Name_Id := Name_First_Keyword + 019;
   Name_Buffer :         constant Name_Id := Name_First_Keyword + 020;
   Name_Bus :            constant Name_Id := Name_First_Keyword + 021;

   Name_Case :           constant Name_Id := Name_First_Keyword + 022;
   Name_Component :      constant Name_Id := Name_First_Keyword + 023;
   Name_Configuration :  constant Name_Id := Name_First_Keyword + 024;
   Name_Constant :       constant Name_Id := Name_First_Keyword + 025;

   Name_Disconnect :     constant Name_Id := Name_First_Keyword + 026;
   Name_Downto :         constant Name_Id := Name_First_Keyword + 027;

   Name_Else :           constant Name_Id := Name_First_Keyword + 028;
   Name_Elsif :          constant Name_Id := Name_First_Keyword + 029;
   Name_End :            constant Name_Id := Name_First_Keyword + 030;
   Name_Entity :         constant Name_Id := Name_First_Keyword + 031;
   Name_Exit :           constant Name_Id := Name_First_Keyword + 032;

   Name_File :           constant Name_Id := Name_First_Keyword + 033;
   Name_For :            constant Name_Id := Name_First_Keyword + 034;
   Name_Function :       constant Name_Id := Name_First_Keyword + 035;

   Name_Generate :       constant Name_Id := Name_First_Keyword + 036;
   Name_Generic :        constant Name_Id := Name_First_Keyword + 037;
   Name_Guarded :        constant Name_Id := Name_First_Keyword + 038;

   Name_If :             constant Name_Id := Name_First_Keyword + 039;
   Name_In :             constant Name_Id := Name_First_Keyword + 040;
   Name_Inout :          constant Name_Id := Name_First_Keyword + 041;
   Name_Is :             constant Name_Id := Name_First_Keyword + 042;

   Name_Label :          constant Name_Id := Name_First_Keyword + 043;
   Name_Library :        constant Name_Id := Name_First_Keyword + 044;
   Name_Linkage :        constant Name_Id := Name_First_Keyword + 045;
   Name_Loop :           constant Name_Id := Name_First_Keyword + 046;

   Name_Map :            constant Name_Id := Name_First_Keyword + 047;

   Name_New :            constant Name_Id := Name_First_Keyword + 048;
   Name_Next :           constant Name_Id := Name_First_Keyword + 049;
   Name_Null :           constant Name_Id := Name_First_Keyword + 050;

   Name_Of :             constant Name_Id := Name_First_Keyword + 051;
   Name_On :             constant Name_Id := Name_First_Keyword + 052;
   Name_Open :           constant Name_Id := Name_First_Keyword + 053;
   Name_Others :         constant Name_Id := Name_First_Keyword + 054;
   Name_Out :            constant Name_Id := Name_First_Keyword + 055;

   Name_Package :        constant Name_Id := Name_First_Keyword + 056;
   Name_Port :           constant Name_Id := Name_First_Keyword + 057;
   Name_Procedure :      constant Name_Id := Name_First_Keyword + 058;
   Name_Process :        constant Name_Id := Name_First_Keyword + 059;

   Name_Range :          constant Name_Id := Name_First_Keyword + 060;
   Name_Record :         constant Name_Id := Name_First_Keyword + 061;
   Name_Register :       constant Name_Id := Name_First_Keyword + 062;
   Name_Report :         constant Name_Id := Name_First_Keyword + 063;
   Name_Return :         constant Name_Id := Name_First_Keyword + 064;

   Name_Select :         constant Name_Id := Name_First_Keyword + 065;
   Name_Severity :       constant Name_Id := Name_First_Keyword + 066;
   Name_Signal :         constant Name_Id := Name_First_Keyword + 067;
   Name_Subtype :        constant Name_Id := Name_First_Keyword + 068;

   Name_Then :           constant Name_Id := Name_First_Keyword + 069;
   Name_To :             constant Name_Id := Name_First_Keyword + 070;
   Name_Transport :      constant Name_Id := Name_First_Keyword + 071;
   Name_Type :           constant Name_Id := Name_First_Keyword + 072;

   Name_Units :          constant Name_Id := Name_First_Keyword + 073;
   Name_Until :          constant Name_Id := Name_First_Keyword + 074;
   Name_Use :            constant Name_Id := Name_First_Keyword + 075;

   Name_Variable :       constant Name_Id := Name_First_Keyword + 076;

   Name_Wait :           constant Name_Id := Name_First_Keyword + 077;
   Name_When :           constant Name_Id := Name_First_Keyword + 078;
   Name_While :          constant Name_Id := Name_First_Keyword + 079;
   Name_With :           constant Name_Id := Name_First_Keyword + 080;

   Name_Last_Vhdl87 :    constant Name_Id := Name_With;

   -- VHDL93 keywords.
   Name_Xnor :           constant Name_Id := Name_First_Keyword + 081;
   Name_Group :          constant Name_Id := Name_First_Keyword + 082;
   Name_Impure :         constant Name_Id := Name_First_Keyword + 083;
   Name_Inertial :       constant Name_Id := Name_First_Keyword + 084;
   Name_Literal :        constant Name_Id := Name_First_Keyword + 085;
   Name_Postponed :      constant Name_Id := Name_First_Keyword + 086;
   Name_Pure :           constant Name_Id := Name_First_Keyword + 087;
   Name_Reject :         constant Name_Id := Name_First_Keyword + 088;
   Name_Shared :         constant Name_Id := Name_First_Keyword + 089;
   Name_Unaffected :     constant Name_Id := Name_First_Keyword + 090;

   Name_Sll :            constant Name_Id := Name_First_Keyword + 091;
   Name_Sla :            constant Name_Id := Name_First_Keyword + 092;
   Name_Sra :            constant Name_Id := Name_First_Keyword + 093;
   Name_Srl :            constant Name_Id := Name_First_Keyword + 094;
   Name_Rol :            constant Name_Id := Name_First_Keyword + 095;
   Name_Ror :            constant Name_Id := Name_First_Keyword + 096;
   subtype Name_Shift_Operators is Name_Id range Name_Sll .. Name_Ror;

   Name_Last_Vhdl93 :    constant Name_Id := Name_Ror;

   Name_Protected :      constant Name_Id := Name_First_Keyword + 097;

   Name_Last_Keyword :   constant Name_Id := Name_Protected;

   subtype Name_Id_Keywords is
     Name_Id range Name_First_Keyword .. Name_Last_Keyword;

   Name_First_Operator : constant Name_Id := Name_Last_Keyword + 1;
   Name_Op_Equality :    constant Name_Id := Name_First_Operator + 000;
   Name_Op_Inequality :  constant Name_Id := Name_First_Operator + 001;
   Name_Op_Less :        constant Name_Id := Name_First_Operator + 002;
   Name_Op_Less_Equal :  constant Name_Id := Name_First_Operator + 003;
   Name_Op_Greater :     constant Name_Id := Name_First_Operator + 004;
   Name_Op_Greater_Equal : constant Name_Id := Name_First_Operator + 5;
   Name_Op_Plus :        constant Name_Id := Name_First_Operator + 006;
   Name_Op_Minus :       constant Name_Id := Name_First_Operator + 007;
   Name_Op_Mul :         constant Name_Id := Name_First_Operator + 008;
   Name_Op_Div :         constant Name_Id := Name_First_Operator + 009;
   Name_Op_Exp :         constant Name_Id := Name_First_Operator + 010;
   Name_Op_Concatenation : constant Name_Id := Name_First_Operator + 011;
   Name_Op_Condition :   constant Name_Id := Name_First_Operator + 012;
   Name_Last_Operator :  constant Name_Id := Name_Op_Condition;

   subtype Name_Relational_Operators is Name_Id
     range Name_Op_Equality .. Name_Op_Greater_Equal;

   --  List of symbolic operators (available as string).
   subtype Name_Id_Operators is Name_Id
     range Name_First_Operator .. Name_Last_Operator;

   Name_First_Attribute : constant Name_Id := Name_Last_Operator + 1;
   Name_Base :           constant Name_Id := Name_First_Attribute + 000;
   Name_Left :           constant Name_Id := Name_First_Attribute + 001;
   Name_Right :          constant Name_Id := Name_First_Attribute + 002;
   Name_High :           constant Name_Id := Name_First_Attribute + 003;
   Name_Low :            constant Name_Id := Name_First_Attribute + 004;
   Name_Pos :            constant Name_Id := Name_First_Attribute + 005;
   Name_Val :            constant Name_Id := Name_First_Attribute + 006;
   Name_Succ :           constant Name_Id := Name_First_Attribute + 007;
   Name_Pred :           constant Name_Id := Name_First_Attribute + 008;
   Name_Leftof :         constant Name_Id := Name_First_Attribute + 009;
   Name_Rightof :        constant Name_Id := Name_First_Attribute + 010;
   Name_Reverse_Range :  constant Name_Id := Name_First_Attribute + 011;
   Name_Length :         constant Name_Id := Name_First_Attribute + 012;
   Name_Delayed :        constant Name_Id := Name_First_Attribute + 013;
   Name_Stable :         constant Name_Id := Name_First_Attribute + 014;
   Name_Quiet :          constant Name_Id := Name_First_Attribute + 015;
   Name_Transaction :    constant Name_Id := Name_First_Attribute + 016;
   Name_Event :          constant Name_Id := Name_First_Attribute + 017;
   Name_Active :         constant Name_Id := Name_First_Attribute + 018;
   Name_Last_Event :     constant Name_Id := Name_First_Attribute + 019;
   Name_Last_Active :    constant Name_Id := Name_First_Attribute + 020;
   Name_Last_Value :     constant Name_Id := Name_First_Attribute + 021;
   Name_Last_Attribute : constant Name_Id := Name_Last_Value;

   subtype Name_Id_Attributes is Name_Id
     range Name_First_Attribute ..Name_Last_Attribute;

   Name_First_Vhdl87_Attribute : constant Name_Id := Name_Last_Value + 1;
   Name_Behavior :       constant Name_Id := Name_First_Attribute + 022;
   Name_Structure :      constant Name_Id := Name_First_Attribute + 023;
   Name_Last_Vhdl87_Attribute : constant Name_Id := Name_Structure;

   subtype Name_Id_Vhdl87_Attributes is Name_Id
     range Name_First_Vhdl87_Attribute ..Name_Last_Vhdl87_Attribute;

   Name_First_Vhdl93_Attribute : constant Name_Id := Name_Structure + 1;
   Name_Ascending :      constant Name_Id := Name_First_Attribute + 024;
   Name_Image :          constant Name_Id := Name_First_Attribute + 025;
   Name_Value :          constant Name_Id := Name_First_Attribute + 026;
   Name_Driving :        constant Name_Id := Name_First_Attribute + 027;
   Name_Driving_Value :  constant Name_Id := Name_First_Attribute + 028;
   Name_Simple_Name :    constant Name_Id := Name_First_Attribute + 029;
   Name_Instance_Name :  constant Name_Id := Name_First_Attribute + 030;
   Name_Path_Name :      constant Name_Id := Name_First_Attribute + 031;
   Name_Last_Vhdl93_Attribute : constant Name_Id := Name_Path_Name;

   subtype Name_Id_Vhdl93_Attributes is Name_Id
     range Name_First_Vhdl93_Attribute ..Name_Last_Vhdl93_Attribute;
   subtype Name_Id_Name_Attributes is Name_Id
     range Name_Simple_Name .. Name_Path_Name;

   --  Names used in std.standard package.
   Name_First_Standard : constant Name_Id := Name_Last_Vhdl93_Attribute + 1;
   Name_Std :            constant Name_Id := Name_First_Standard + 000;
   Name_Standard :       constant Name_Id := Name_First_Standard + 001;
   Name_Boolean :        constant Name_Id := Name_First_Standard + 002;
   Name_False :          constant Name_Id := Name_First_Standard + 003;
   Name_True :           constant Name_Id := Name_First_Standard + 004;
   Name_Bit :            constant Name_Id := Name_First_Standard + 005;
   Name_Character :      constant Name_Id := Name_First_Standard + 006;
   Name_Severity_Level : constant Name_Id := Name_First_Standard + 007;
   Name_Note :           constant Name_Id := Name_First_Standard + 008;
   Name_Warning :        constant Name_Id := Name_First_Standard + 009;
   Name_Error :          constant Name_Id := Name_First_Standard + 010;
   Name_Failure :        constant Name_Id := Name_First_Standard + 011;
   Name_Universal_Integer : constant Name_Id := Name_First_Standard + 012;
   Name_Universal_Real : constant Name_Id := Name_First_Standard + 013;
   Name_Convertible_Integer : constant Name_Id := Name_First_Standard + 014;
   Name_Convertible_Real : constant Name_Id := Name_First_Standard + 015;
   Name_Integer :        constant Name_Id := Name_First_Standard + 016;
   Name_Real :           constant Name_Id := Name_First_Standard + 017;
   Name_Time :           constant Name_Id := Name_First_Standard + 018;
   Name_Fs :             constant Name_Id := Name_First_Standard + 019;
   Name_Ps :             constant Name_Id := Name_First_Standard + 020;
   Name_Ns :             constant Name_Id := Name_First_Standard + 021;
   Name_Us :             constant Name_Id := Name_First_Standard + 022;
   Name_Ms :             constant Name_Id := Name_First_Standard + 023;
   Name_Sec :            constant Name_Id := Name_First_Standard + 024;
   Name_Min :            constant Name_Id := Name_First_Standard + 025;
   Name_Hr :             constant Name_Id := Name_First_Standard + 026;
   Name_Delay_Length :   constant Name_Id := Name_First_Standard + 027;
   Name_Now :            constant Name_Id := Name_First_Standard + 028;
   Name_Natural :        constant Name_Id := Name_First_Standard + 029;
   Name_Positive :       constant Name_Id := Name_First_Standard + 030;
   Name_String :         constant Name_Id := Name_First_Standard + 031;
   Name_Bit_Vector :     constant Name_Id := Name_First_Standard + 032;
   Name_File_Open_Kind : constant Name_Id := Name_First_Standard + 033;
   Name_Read_Mode :      constant Name_Id := Name_First_Standard + 034;
   Name_Write_Mode :     constant Name_Id := Name_First_Standard + 035;
   Name_Append_Mode :    constant Name_Id := Name_First_Standard + 036;
   Name_File_Open_Status : constant Name_Id := Name_First_Standard + 037;
   Name_Open_Ok :        constant Name_Id := Name_First_Standard + 038;
   Name_Status_Error :   constant Name_Id := Name_First_Standard + 039;
   Name_Name_Error :     constant Name_Id := Name_First_Standard + 040;
   Name_Mode_Error :     constant Name_Id := Name_First_Standard + 041;
   Name_Foreign :        constant Name_Id := Name_First_Standard + 042;
   Name_Last_Standard :  constant Name_Id := Name_Foreign;

   Name_First_Charname : constant Name_Id := Name_Last_Standard + 1;
   Name_Nul :            constant Name_Id := Name_First_Charname + 00;
   Name_Soh :            constant Name_Id := Name_First_Charname + 01;
   Name_Stx :            constant Name_Id := Name_First_Charname + 02;
   Name_Etx :            constant Name_Id := Name_First_Charname + 03;
   Name_Eot :            constant Name_Id := Name_First_Charname + 04;
   Name_Enq :            constant Name_Id := Name_First_Charname + 05;
   Name_Ack :            constant Name_Id := Name_First_Charname + 06;
   Name_Bel :            constant Name_Id := Name_First_Charname + 07;
   Name_Bs :             constant Name_Id := Name_First_Charname + 08;
   Name_Ht :             constant Name_Id := Name_First_Charname + 09;
   Name_Lf :             constant Name_Id := Name_First_Charname + 10;
   Name_Vt :             constant Name_Id := Name_First_Charname + 11;
   Name_Ff :             constant Name_Id := Name_First_Charname + 12;
   Name_Cr :             constant Name_Id := Name_First_Charname + 13;
   Name_So :             constant Name_Id := Name_First_Charname + 14;
   Name_Si :             constant Name_Id := Name_First_Charname + 15;
   Name_Dle :            constant Name_Id := Name_First_Charname + 16;
   Name_Dc1 :            constant Name_Id := Name_First_Charname + 17;
   Name_Dc2 :            constant Name_Id := Name_First_Charname + 18;
   Name_Dc3 :            constant Name_Id := Name_First_Charname + 19;
   Name_Dc4 :            constant Name_Id := Name_First_Charname + 20;
   Name_Nak :            constant Name_Id := Name_First_Charname + 21;
   Name_Syn :            constant Name_Id := Name_First_Charname + 22;
   Name_Etb :            constant Name_Id := Name_First_Charname + 23;
   Name_Can :            constant Name_Id := Name_First_Charname + 24;
   Name_Em :             constant Name_Id := Name_First_Charname + 25;
   Name_Sub :            constant Name_Id := Name_First_Charname + 26;
   Name_Esc :            constant Name_Id := Name_First_Charname + 27;
   Name_Fsp :            constant Name_Id := Name_First_Charname + 28;
   Name_Gsp :            constant Name_Id := Name_First_Charname + 29;
   Name_Rsp :            constant Name_Id := Name_First_Charname + 30;
   Name_Usp :            constant Name_Id := Name_First_Charname + 31;

   Name_Del :            constant Name_Id := Name_First_Charname + 32;

   Name_C128 :           constant Name_Id := Name_First_Charname + 33;
   Name_C129 :           constant Name_Id := Name_First_Charname + 34;
   Name_C130 :           constant Name_Id := Name_First_Charname + 35;
   Name_C131 :           constant Name_Id := Name_First_Charname + 36;
   Name_C132 :           constant Name_Id := Name_First_Charname + 37;
   Name_C133 :           constant Name_Id := Name_First_Charname + 38;
   Name_C134 :           constant Name_Id := Name_First_Charname + 39;
   Name_C135 :           constant Name_Id := Name_First_Charname + 40;
   Name_C136 :           constant Name_Id := Name_First_Charname + 41;
   Name_C137 :           constant Name_Id := Name_First_Charname + 42;
   Name_C138 :           constant Name_Id := Name_First_Charname + 43;
   Name_C139 :           constant Name_Id := Name_First_Charname + 44;
   Name_C140 :           constant Name_Id := Name_First_Charname + 45;
   Name_C141 :           constant Name_Id := Name_First_Charname + 46;
   Name_C142 :           constant Name_Id := Name_First_Charname + 47;
   Name_C143 :           constant Name_Id := Name_First_Charname + 48;
   Name_C144 :           constant Name_Id := Name_First_Charname + 49;
   Name_C145 :           constant Name_Id := Name_First_Charname + 50;
   Name_C146 :           constant Name_Id := Name_First_Charname + 51;
   Name_C147 :           constant Name_Id := Name_First_Charname + 52;
   Name_C148 :           constant Name_Id := Name_First_Charname + 53;
   Name_C149 :           constant Name_Id := Name_First_Charname + 54;
   Name_C150 :           constant Name_Id := Name_First_Charname + 55;
   Name_C151 :           constant Name_Id := Name_First_Charname + 56;
   Name_C152 :           constant Name_Id := Name_First_Charname + 57;
   Name_C153 :           constant Name_Id := Name_First_Charname + 58;
   Name_C154 :           constant Name_Id := Name_First_Charname + 59;
   Name_C155 :           constant Name_Id := Name_First_Charname + 60;
   Name_C156 :           constant Name_Id := Name_First_Charname + 61;
   Name_C157 :           constant Name_Id := Name_First_Charname + 62;
   Name_C158 :           constant Name_Id := Name_First_Charname + 63;
   Name_C159 :           constant Name_Id := Name_First_Charname + 64;
   Name_Last_Charname :  constant Name_Id := Name_C159;

   Name_First_Misc : constant Name_Id := Name_Last_Charname + 1;
   Name_Guard :          constant Name_Id := Name_First_Misc + 000;
   Name_Deallocate :     constant Name_Id := Name_First_Misc + 001;
   Name_File_Open :      constant Name_Id := Name_First_Misc + 002;
   Name_File_Close :     constant Name_Id := Name_First_Misc + 003;
   Name_Read :           constant Name_Id := Name_First_Misc + 004;
   Name_Write :          constant Name_Id := Name_First_Misc + 005;
   Name_Flush :          constant Name_Id := Name_First_Misc + 006;
   Name_Endfile :        constant Name_Id := Name_First_Misc + 007;
   Name_P :              constant Name_Id := Name_First_Misc + 008;
   Name_F :              constant Name_Id := Name_First_Misc + 009;
   Name_External_Name :  constant Name_Id := Name_First_Misc + 010;
   Name_Open_Kind :      constant Name_Id := Name_First_Misc + 011;
   Name_Status :         constant Name_Id := Name_First_Misc + 012;
   Name_First :          constant Name_Id := Name_First_Misc + 013;
   Name_Last :           constant Name_Id := Name_First_Misc + 014;
   Name_Textio :         constant Name_Id := Name_First_Misc + 015;
   Name_Work :           constant Name_Id := Name_First_Misc + 016;
   Name_Text :           constant Name_Id := Name_First_Misc + 017;
   Name_To_String :      constant Name_Id := Name_First_Misc + 018;
   Name_Untruncated_Text_Read : constant Name_Id := Name_First_Misc + 019;
   Name_Last_Misc :      constant Name_Id := Name_Untruncated_Text_Read;

   Name_First_Ieee :     constant Name_Id := Name_Last_Misc + 1;
   Name_Ieee :           constant Name_Id := Name_First_Ieee + 000;
   Name_Std_Logic_1164 : constant Name_Id := Name_First_Ieee + 001;
   Name_Std_Ulogic :     constant Name_Id := Name_First_Ieee + 002;
   Name_Std_Ulogic_Vector : constant Name_Id := Name_First_Ieee + 003;
   Name_Std_Logic :      constant Name_Id := Name_First_Ieee + 004;
   Name_Std_Logic_Vector : constant Name_Id := Name_First_Ieee + 005;
   Name_Rising_Edge :    constant Name_Id := Name_First_Ieee + 006;
   Name_Falling_Edge :   constant Name_Id := Name_First_Ieee + 007;
   Name_VITAL_Timing :   constant Name_Id := Name_First_Ieee + 008;
   Name_VITAL_Level0 :   constant Name_Id := Name_First_Ieee + 009;
   Name_VITAL_Level1 :   constant Name_Id := Name_First_Ieee + 010;
   Name_Last_Ieee :      constant Name_Id := Name_VITAL_Level1;

   --  Verilog keywords.
   Name_First_Verilog :  constant Name_Id := Name_Last_Ieee + 1;
   Name_Always :         constant Name_Id := Name_First_Verilog + 00;
   Name_Assign :         constant Name_Id := Name_First_Verilog + 01;
   Name_Buf :            constant Name_Id := Name_First_Verilog + 02;
   Name_Bufif0 :         constant Name_Id := Name_First_Verilog + 03;
   Name_Bufif1 :         constant Name_Id := Name_First_Verilog + 04;
   Name_Casex :          constant Name_Id := Name_First_Verilog + 05;
   Name_Casez :          constant Name_Id := Name_First_Verilog + 06;
   Name_Cmos :           constant Name_Id := Name_First_Verilog + 07;
   Name_Deassign :       constant Name_Id := Name_First_Verilog + 08;
   Name_Default :        constant Name_Id := Name_First_Verilog + 09;
   Name_Defparam :       constant Name_Id := Name_First_Verilog + 10;
   Name_Disable :        constant Name_Id := Name_First_Verilog + 11;
   Name_Endcase :        constant Name_Id := Name_First_Verilog + 12;
   Name_Endfunction :    constant Name_Id := Name_First_Verilog + 13;
   Name_Endmodule :      constant Name_Id := Name_First_Verilog + 14;
   Name_Endprimitive :   constant Name_Id := Name_First_Verilog + 15;
   Name_Endspecify :     constant Name_Id := Name_First_Verilog + 16;
   Name_Endtable :       constant Name_Id := Name_First_Verilog + 17;
   Name_Endtask :        constant Name_Id := Name_First_Verilog + 18;
   Name_Forever :        constant Name_Id := Name_First_Verilog + 19;
   Name_Fork :           constant Name_Id := Name_First_Verilog + 20;
   Name_Highz0 :         constant Name_Id := Name_First_Verilog + 21;
   Name_Highz1 :         constant Name_Id := Name_First_Verilog + 22;
   Name_Initial :        constant Name_Id := Name_First_Verilog + 23;
   Name_Input :          constant Name_Id := Name_First_Verilog + 24;
   Name_Join :           constant Name_Id := Name_First_Verilog + 25;
   Name_Large :          constant Name_Id := Name_First_Verilog + 26;
   Name_Medium :         constant Name_Id := Name_First_Verilog + 27;
   Name_Module :         constant Name_Id := Name_First_Verilog + 28;
   Name_Negedge :        constant Name_Id := Name_First_Verilog + 29;
   Name_Nmos :           constant Name_Id := Name_First_Verilog + 30;
   Name_Notif0 :         constant Name_Id := Name_First_Verilog + 31;
   Name_Notif1 :         constant Name_Id := Name_First_Verilog + 32;
   Name_Output :         constant Name_Id := Name_First_Verilog + 33;
   Name_Parameter :      constant Name_Id := Name_First_Verilog + 34;
   Name_Pmos :           constant Name_Id := Name_First_Verilog + 35;
   Name_Posedge :        constant Name_Id := Name_First_Verilog + 36;
   Name_Primitive :      constant Name_Id := Name_First_Verilog + 37;
   Name_Pull0 :          constant Name_Id := Name_First_Verilog + 38;
   Name_Pull1 :          constant Name_Id := Name_First_Verilog + 39;
   Name_Pulldown :       constant Name_Id := Name_First_Verilog + 40;
   Name_Pullup :         constant Name_Id := Name_First_Verilog + 41;
   Name_Reg :            constant Name_Id := Name_First_Verilog + 42;
   Name_Repeat :         constant Name_Id := Name_First_Verilog + 43;
   Name_Rcmos :          constant Name_Id := Name_First_Verilog + 44;
   Name_Rnmos :          constant Name_Id := Name_First_Verilog + 45;
   Name_Rpmos :          constant Name_Id := Name_First_Verilog + 46;
   Name_Rtran :          constant Name_Id := Name_First_Verilog + 47;
   Name_Rtranif0 :       constant Name_Id := Name_First_Verilog + 48;
   Name_Rtranif1 :       constant Name_Id := Name_First_Verilog + 49;
   Name_Small :          constant Name_Id := Name_First_Verilog + 50;
   Name_Specify :        constant Name_Id := Name_First_Verilog + 51;
   Name_Specparam :      constant Name_Id := Name_First_Verilog + 52;
   Name_Strong0 :        constant Name_Id := Name_First_Verilog + 53;
   Name_Strong1 :        constant Name_Id := Name_First_Verilog + 54;
   Name_Supply0 :        constant Name_Id := Name_First_Verilog + 55;
   Name_Supply1 :        constant Name_Id := Name_First_Verilog + 56;
   Name_Tablex :         constant Name_Id := Name_First_Verilog + 57;
   Name_Task :           constant Name_Id := Name_First_Verilog + 58;
   Name_Tran :           constant Name_Id := Name_First_Verilog + 59;
   Name_Tranif0 :        constant Name_Id := Name_First_Verilog + 60;
   Name_Tranif1 :        constant Name_Id := Name_First_Verilog + 61;
   Name_Tri :            constant Name_Id := Name_First_Verilog + 62;
   Name_Tri0 :           constant Name_Id := Name_First_Verilog + 63;
   Name_Tri1 :           constant Name_Id := Name_First_Verilog + 64;
   Name_Trireg :         constant Name_Id := Name_First_Verilog + 65;
   Name_Wand :           constant Name_Id := Name_First_Verilog + 66;
   Name_Weak0 :          constant Name_Id := Name_First_Verilog + 67;
   Name_Weak1 :          constant Name_Id := Name_First_Verilog + 68;
   Name_Wire :           constant Name_Id := Name_First_Verilog + 69;
   Name_Wor :            constant Name_Id := Name_First_Verilog + 70;
   Name_Last_Verilog :   constant Name_Id := Name_Wor;

   --  Verilog Directives.
   Name_First_Directive : constant Name_Id := Name_Last_Verilog + 1;
   Name_Define :         constant Name_Id := Name_First_Directive + 00;
   Name_Endif :          constant Name_Id := Name_First_Directive + 01;
   Name_Ifdef :          constant Name_Id := Name_First_Directive + 02;
   Name_Include :        constant Name_Id := Name_First_Directive + 03;
   Name_Timescale :      constant Name_Id := Name_First_Directive + 04;
   Name_Undef :          constant Name_Id := Name_First_Directive + 05;
   Name_Last_Directive : constant Name_Id := Name_Undef;

   --  Verilog system tasks.
   Name_First_Systask :  constant Name_Id := Name_Last_Directive + 1;
   Name_Display :        constant Name_Id := Name_First_Systask + 00;
   Name_Finish :         constant Name_Id := Name_First_Systask + 01;
   Name_Last_Systask :   constant Name_Id := Name_Finish;

   Name_First_Comment :  constant Name_Id := Name_Last_Systask + 1;
   Name_Psl :            constant Name_Id := Name_First_Comment + 0;
   Name_Pragma :         constant Name_Id := Name_First_Comment + 1;
   Name_Last_Comment :   constant Name_Id := Name_First_Comment + 1;

   --  PSL words.
   Name_First_PSL :          constant Name_Id := Name_Last_Comment + 1;
   Name_A :                  constant Name_Id := Name_First_PSL + 00;
   Name_Af :                 constant Name_Id := Name_First_PSL + 01;
   Name_Ag :                 constant Name_Id := Name_First_PSL + 02;
   Name_Ax :                 constant Name_Id := Name_First_PSL + 03;
   Name_Abort :              constant Name_Id := Name_First_PSL + 04;
   --  Name_Always
   --  Name_And
   Name_Assume :             constant Name_Id := Name_First_PSL + 05;
   Name_Assume_Guarantee :   constant Name_Id := Name_First_PSL + 06;
   Name_Before :             constant Name_Id := Name_First_PSL + 07;
   --  Name_Boolean
   Name_Clock :              constant Name_Id := Name_First_PSL + 08;
   Name_Const :              constant Name_Id := Name_First_PSL + 09;
   Name_Cover :              constant Name_Id := Name_First_PSL + 10;
   --  Name_Default
   Name_E :                  constant Name_Id := Name_First_PSL + 11;
   Name_Ef :                 constant Name_Id := Name_First_PSL + 12;
   Name_Eg :                 constant Name_Id := Name_First_PSL + 13;
   Name_Ex :                 constant Name_Id := Name_First_PSL + 14;
   Name_Endpoint  :          constant Name_Id := Name_First_PSL + 15;
   Name_Eventually :         constant Name_Id := Name_First_PSL + 16;
   Name_Fairness :           constant Name_Id := Name_First_PSL + 17;
   Name_Fell  :              constant Name_Id := Name_First_PSL + 18;
   Name_forall :             constant Name_Id := Name_First_PSL + 19;
   Name_G :                  constant Name_Id := Name_First_PSL + 20;
   --  Name_In
   Name_Inf :                constant Name_Id := Name_First_PSL + 21;
   Name_Inherit :            constant Name_Id := Name_First_PSL + 22;
   --  Name_Is
   Name_Never :              constant Name_Id := Name_First_PSL + 23;
   --  Name_Next
   Name_Next_A :             constant Name_Id := Name_First_PSL + 24;
   Name_Next_E :             constant Name_Id := Name_First_PSL + 25;
   Name_Next_Event :         constant Name_Id := Name_First_PSL + 26;
   Name_Next_Event_A :       constant Name_Id := Name_First_PSL + 27;
   Name_Next_Event_E :       constant Name_Id := Name_First_PSL + 28;
   --  Name_Not
   --  Name_Or
   Name_Property :           constant Name_Id := Name_First_PSL + 29;
   Name_Prev :               constant Name_Id := Name_First_PSL + 30;
   Name_Restrict :           constant Name_Id := Name_First_PSL + 31;
   Name_Restrict_Guarantee : constant Name_Id := Name_First_PSL + 32;
   Name_Rose :               constant Name_Id := Name_First_PSL + 33;
   Name_Sequence :           constant Name_Id := Name_First_PSL + 34;
   Name_Strong :             constant Name_Id := Name_First_PSL + 35;
   Name_Union :              constant Name_Id := Name_First_PSL + 36;
   --  Name_Until
   Name_Vmode :              constant Name_Id := Name_First_PSL + 37;
   Name_Vprop :              constant Name_Id := Name_First_PSL + 38;
   Name_Vunit :              constant Name_Id := Name_First_PSL + 39;
   Name_W :                  constant Name_Id := Name_First_PSL + 40;
   Name_Whilenot :           constant Name_Id := Name_First_PSL + 41;
   Name_Within :             constant Name_Id := Name_First_PSL + 42;
   Name_X :                  constant Name_Id := Name_First_PSL + 43;
   Name_Last_PSL :           constant Name_Id := Name_X;

   subtype Name_Id_PSL_Keywords is
     Name_Id range Name_First_PSL .. Name_Last_PSL;

   -- Initialize the name table with the values defined here.
   procedure Std_Names_Initialize;
end Std_Names;
