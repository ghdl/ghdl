--  Well known name table entries.
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
with Types; use Types;

-- Note: since all identifiers declared in this package begins with either
-- std_names or name, this package is expected to be use'd.

package Std_Names is
   -- Predefined names.
   Name_First_Character : constant Name_Id := 1;
   Name_Last_Character : constant Name_Id := Name_First_Character + 255;

   subtype Name_Characters is Name_Id
     range Name_First_Character .. Name_Last_Character;

   Name_First_Keyword : constant Name_Id := Name_Last_Character + 1;

   --  Word operators.
   Name_Mod :            constant Name_Id := Name_First_Keyword + 000;
   Name_Rem :            constant Name_Id := Name_First_Keyword + 001;

   Name_Abs :            constant Name_Id := Name_First_Keyword + 002;
   Name_Not :            constant Name_Id := Name_First_Keyword + 003;

   subtype Name_Word_Operators is Name_Id range Name_Mod .. Name_Not;

   Name_Access :         constant Name_Id := Name_First_Keyword + 004;
   Name_After :          constant Name_Id := Name_First_Keyword + 005;
   Name_Alias :          constant Name_Id := Name_First_Keyword + 006;
   Name_All :            constant Name_Id := Name_First_Keyword + 007;
   Name_Architecture :   constant Name_Id := Name_First_Keyword + 008;
   Name_Array :          constant Name_Id := Name_First_Keyword + 009;
   Name_Assert :         constant Name_Id := Name_First_Keyword + 010;
   Name_Attribute :      constant Name_Id := Name_First_Keyword + 011;

   Name_Begin :          constant Name_Id := Name_First_Keyword + 012;
   Name_Block :          constant Name_Id := Name_First_Keyword + 013;
   Name_Body :           constant Name_Id := Name_First_Keyword + 014;
   Name_Buffer :         constant Name_Id := Name_First_Keyword + 015;
   Name_Bus :            constant Name_Id := Name_First_Keyword + 016;

   Name_Case :           constant Name_Id := Name_First_Keyword + 017;
   Name_Component :      constant Name_Id := Name_First_Keyword + 018;
   Name_Configuration :  constant Name_Id := Name_First_Keyword + 019;
   Name_Constant :       constant Name_Id := Name_First_Keyword + 020;

   Name_Disconnect :     constant Name_Id := Name_First_Keyword + 021;
   Name_Downto :         constant Name_Id := Name_First_Keyword + 022;

   Name_Else :           constant Name_Id := Name_First_Keyword + 023;
   Name_Elsif :          constant Name_Id := Name_First_Keyword + 024;
   Name_End :            constant Name_Id := Name_First_Keyword + 025;
   Name_Entity :         constant Name_Id := Name_First_Keyword + 026;
   Name_Exit :           constant Name_Id := Name_First_Keyword + 027;

   Name_File :           constant Name_Id := Name_First_Keyword + 028;
   Name_For :            constant Name_Id := Name_First_Keyword + 029;
   Name_Function :       constant Name_Id := Name_First_Keyword + 030;

   Name_Generate :       constant Name_Id := Name_First_Keyword + 031;
   Name_Generic :        constant Name_Id := Name_First_Keyword + 032;
   Name_Guarded :        constant Name_Id := Name_First_Keyword + 033;

   Name_If :             constant Name_Id := Name_First_Keyword + 034;
   Name_In :             constant Name_Id := Name_First_Keyword + 035;
   Name_Inout :          constant Name_Id := Name_First_Keyword + 036;
   Name_Is :             constant Name_Id := Name_First_Keyword + 037;

   Name_Label :          constant Name_Id := Name_First_Keyword + 038;
   Name_Library :        constant Name_Id := Name_First_Keyword + 039;
   Name_Linkage :        constant Name_Id := Name_First_Keyword + 040;
   Name_Loop :           constant Name_Id := Name_First_Keyword + 041;

   Name_Map :            constant Name_Id := Name_First_Keyword + 042;

   Name_New :            constant Name_Id := Name_First_Keyword + 043;
   Name_Next :           constant Name_Id := Name_First_Keyword + 044;
   Name_Null :           constant Name_Id := Name_First_Keyword + 045;

   Name_Of :             constant Name_Id := Name_First_Keyword + 046;
   Name_On :             constant Name_Id := Name_First_Keyword + 047;
   Name_Open :           constant Name_Id := Name_First_Keyword + 048;
   Name_Others :         constant Name_Id := Name_First_Keyword + 049;
   Name_Out :            constant Name_Id := Name_First_Keyword + 050;

   Name_Package :        constant Name_Id := Name_First_Keyword + 051;
   Name_Port :           constant Name_Id := Name_First_Keyword + 052;
   Name_Procedure :      constant Name_Id := Name_First_Keyword + 053;
   Name_Process :        constant Name_Id := Name_First_Keyword + 054;

   Name_Range :          constant Name_Id := Name_First_Keyword + 055;
   Name_Record :         constant Name_Id := Name_First_Keyword + 056;
   Name_Register :       constant Name_Id := Name_First_Keyword + 057;
   Name_Report :         constant Name_Id := Name_First_Keyword + 058;
   Name_Return :         constant Name_Id := Name_First_Keyword + 059;

   Name_Select :         constant Name_Id := Name_First_Keyword + 060;
   Name_Severity :       constant Name_Id := Name_First_Keyword + 061;
   Name_Signal :         constant Name_Id := Name_First_Keyword + 062;
   Name_Subtype :        constant Name_Id := Name_First_Keyword + 063;

   Name_Then :           constant Name_Id := Name_First_Keyword + 064;
   Name_To :             constant Name_Id := Name_First_Keyword + 065;
   Name_Transport :      constant Name_Id := Name_First_Keyword + 066;
   Name_Type :           constant Name_Id := Name_First_Keyword + 067;

   Name_Units :          constant Name_Id := Name_First_Keyword + 068;
   Name_Until :          constant Name_Id := Name_First_Keyword + 069;
   Name_Use :            constant Name_Id := Name_First_Keyword + 070;

   Name_Variable :       constant Name_Id := Name_First_Keyword + 071;

   Name_Wait :           constant Name_Id := Name_First_Keyword + 072;
   Name_When :           constant Name_Id := Name_First_Keyword + 073;
   Name_While :          constant Name_Id := Name_First_Keyword + 074;
   Name_With :           constant Name_Id := Name_First_Keyword + 075;

   Name_And :            constant Name_Id := Name_First_Keyword + 076;
   Name_Or :             constant Name_Id := Name_First_Keyword + 077;
   Name_Xor :            constant Name_Id := Name_First_Keyword + 078;
   Name_Nand :           constant Name_Id := Name_First_Keyword + 079;
   Name_Nor :            constant Name_Id := Name_First_Keyword + 080;

   subtype Name_Logical_Operators is Name_Id range Name_And .. Name_Nor;

   Name_Last_Vhdl87 :    constant Name_Id := Name_Nor;
   subtype Name_Id_Vhdl87_Reserved_Words is
     Name_Id range Name_First_Keyword .. Name_Nor;

   -- VHDL93 reserved words.
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
   subtype Name_Id_Vhdl93_Reserved_Words is
     Name_Id range Name_Xnor .. Name_Ror;

   Name_Protected :      constant Name_Id := Name_First_Keyword + 097;

   Name_Last_Vhdl00 :    constant Name_Id := Name_Protected;
   subtype Name_Id_Vhdl00_Reserved_Words is
     Name_Id range Name_Protected .. Name_Last_Vhdl00;

   Name_Assume :             constant Name_Id := Name_First_Keyword + 098;
   Name_Context :            constant Name_Id := Name_First_Keyword + 099;
   Name_Cover :              constant Name_Id := Name_First_Keyword + 100;
   Name_Default :            constant Name_Id := Name_First_Keyword + 101;
   Name_Force :              constant Name_Id := Name_First_Keyword + 102;
   Name_Parameter :          constant Name_Id := Name_First_Keyword + 103;
   Name_Property :           constant Name_Id := Name_First_Keyword + 104;
   Name_Release :            constant Name_Id := Name_First_Keyword + 105;
   Name_Restrict :           constant Name_Id := Name_First_Keyword + 106;
   Name_Restrict_Guarantee : constant Name_Id := Name_First_Keyword + 107;
   Name_Sequence :           constant Name_Id := Name_First_Keyword + 108;
   Name_Inherit :            constant Name_Id := Name_First_Keyword + 109;
   Name_Vmode :              constant Name_Id := Name_First_Keyword + 110;
   Name_Vprop :              constant Name_Id := Name_First_Keyword + 111;
   Name_Vunit :              constant Name_Id := Name_First_Keyword + 112;
   Name_Last_Vhdl08 :        constant Name_Id := Name_Vunit;

   subtype Name_Id_Vhdl08_Reserved_Words is
     Name_Id range Name_Assume .. Name_Last_Vhdl08;

   Name_First_Ams_Keyword :  constant Name_Id := Name_Last_Vhdl08 + 1;
   Name_Across :         constant Name_Id := Name_First_Ams_Keyword + 000;
   Name_Break :          constant Name_Id := Name_First_Ams_Keyword + 001;
   Name_Limit :          constant Name_Id := Name_First_Ams_Keyword + 002;
   Name_Nature :         constant Name_Id := Name_First_Ams_Keyword + 003;
   Name_Noise :          constant Name_Id := Name_First_Ams_Keyword + 004;
   Name_Procedural :     constant Name_Id := Name_First_Ams_Keyword + 005;
   Name_Quantity :       constant Name_Id := Name_First_Ams_Keyword + 006;
   Name_Reference :      constant Name_Id := Name_First_Ams_Keyword + 007;
   Name_Spectrum :       constant Name_Id := Name_First_Ams_Keyword + 008;
   Name_Subnature :      constant Name_Id := Name_First_Ams_Keyword + 009;
   Name_Terminal :       constant Name_Id := Name_First_Ams_Keyword + 010;
   Name_Through :        constant Name_Id := Name_First_Ams_Keyword + 011;
   Name_Tolerance :      constant Name_Id := Name_First_Ams_Keyword + 012;
   Name_Last_AMS_Vhdl :  constant Name_Id := Name_Tolerance;

   subtype Name_Id_AMS_Reserved_Words is
     Name_Id range Name_Across .. Name_Tolerance;

   Name_Last_Keyword :   constant Name_Id := Name_Tolerance;

   subtype Name_Id_Keywords is
     Name_Id range Name_First_Keyword .. Name_Last_Keyword;

   --  Verilog keywords.
   Name_First_Verilog :  constant Name_Id := Name_Last_Keyword + 1;
   Name_Always :         constant Name_Id := Name_First_Verilog + 00;
   Name_Assign :         constant Name_Id := Name_First_Verilog + 01;
   Name_Buf :            constant Name_Id := Name_First_Verilog + 02;
   Name_Bufif0 :         constant Name_Id := Name_First_Verilog + 03;
   Name_Bufif1 :         constant Name_Id := Name_First_Verilog + 04;
   Name_Casex :          constant Name_Id := Name_First_Verilog + 05;
   Name_Casez :          constant Name_Id := Name_First_Verilog + 06;
   Name_Cmos :           constant Name_Id := Name_First_Verilog + 07;
   Name_Deassign :       constant Name_Id := Name_First_Verilog + 08;
   Name_Defparam :       constant Name_Id := Name_First_Verilog + 09;
   Name_Disable :        constant Name_Id := Name_First_Verilog + 10;
   Name_Edge :           constant Name_Id := Name_First_Verilog + 11;
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
   Name_Ifnone :         constant Name_Id := Name_First_Verilog + 23;
   Name_Initial :        constant Name_Id := Name_First_Verilog + 24;
   Name_Input :          constant Name_Id := Name_First_Verilog + 25;
   Name_Join :           constant Name_Id := Name_First_Verilog + 26;
   Name_Large :          constant Name_Id := Name_First_Verilog + 27;
   Name_Macromodule :    constant Name_Id := Name_First_Verilog + 28;
   Name_Medium :         constant Name_Id := Name_First_Verilog + 29;
   Name_Module :         constant Name_Id := Name_First_Verilog + 30;
   Name_Negedge :        constant Name_Id := Name_First_Verilog + 31;
   Name_Nmos :           constant Name_Id := Name_First_Verilog + 32;
   Name_Notif0 :         constant Name_Id := Name_First_Verilog + 33;
   Name_Notif1 :         constant Name_Id := Name_First_Verilog + 34;
   Name_Output :         constant Name_Id := Name_First_Verilog + 35;
   Name_Pmos :           constant Name_Id := Name_First_Verilog + 36;
   Name_Posedge :        constant Name_Id := Name_First_Verilog + 37;
   Name_Primitive :      constant Name_Id := Name_First_Verilog + 38;
   Name_Pull0 :          constant Name_Id := Name_First_Verilog + 39;
   Name_Pull1 :          constant Name_Id := Name_First_Verilog + 40;
   Name_Pulldown :       constant Name_Id := Name_First_Verilog + 41;
   Name_Pullup :         constant Name_Id := Name_First_Verilog + 42;
   Name_Realtime :       constant Name_Id := Name_First_Verilog + 43;
   Name_Reg :            constant Name_Id := Name_First_Verilog + 44;
   Name_Repeat :         constant Name_Id := Name_First_Verilog + 45;
   Name_Rcmos :          constant Name_Id := Name_First_Verilog + 46;
   Name_Rnmos :          constant Name_Id := Name_First_Verilog + 47;
   Name_Rpmos :          constant Name_Id := Name_First_Verilog + 48;
   Name_Rtran :          constant Name_Id := Name_First_Verilog + 49;
   Name_Rtranif0 :       constant Name_Id := Name_First_Verilog + 50;
   Name_Rtranif1 :       constant Name_Id := Name_First_Verilog + 51;
   Name_Scalared :       constant Name_Id := Name_First_Verilog + 52;
   Name_Small :          constant Name_Id := Name_First_Verilog + 53;
   Name_Specify :        constant Name_Id := Name_First_Verilog + 54;
   Name_Specparam :      constant Name_Id := Name_First_Verilog + 55;
   Name_Strong0 :        constant Name_Id := Name_First_Verilog + 56;
   Name_Strong1 :        constant Name_Id := Name_First_Verilog + 57;
   Name_Supply0 :        constant Name_Id := Name_First_Verilog + 58;
   Name_Supply1 :        constant Name_Id := Name_First_Verilog + 59;
   Name_Tablex :         constant Name_Id := Name_First_Verilog + 60;
   Name_Task :           constant Name_Id := Name_First_Verilog + 61;
   Name_Tran :           constant Name_Id := Name_First_Verilog + 62;
   Name_Tranif0 :        constant Name_Id := Name_First_Verilog + 63;
   Name_Tranif1 :        constant Name_Id := Name_First_Verilog + 64;
   Name_Tri :            constant Name_Id := Name_First_Verilog + 65;
   Name_Tri0 :           constant Name_Id := Name_First_Verilog + 66;
   Name_Tri1 :           constant Name_Id := Name_First_Verilog + 67;
   Name_Triand :         constant Name_Id := Name_First_Verilog + 68;
   Name_Trior :          constant Name_Id := Name_First_Verilog + 69;
   Name_Trireg :         constant Name_Id := Name_First_Verilog + 70;
   Name_Vectored :       constant Name_Id := Name_First_Verilog + 71;
   Name_Wand :           constant Name_Id := Name_First_Verilog + 72;
   Name_Weak0 :          constant Name_Id := Name_First_Verilog + 73;
   Name_Weak1 :          constant Name_Id := Name_First_Verilog + 74;
   Name_Wire :           constant Name_Id := Name_First_Verilog + 75;
   Name_Wor :            constant Name_Id := Name_First_Verilog + 76;
   Name_Last_Verilog :   constant Name_Id := Name_Wor;

   --  Verilog 2001
   Name_First_V2001 :    constant Name_Id := Name_Last_Verilog + 1;
   Name_Automatic :      constant Name_Id := Name_First_V2001 + 0;
   Name_Endgenerate :    constant Name_Id := Name_First_V2001 + 1;
   Name_Genvar :         constant Name_Id := Name_First_V2001 + 2;
   Name_Localparam :     constant Name_Id := Name_First_V2001 + 3;
   Name_Unsigned :       constant Name_Id := Name_First_V2001 + 4;
   Name_Signed :         constant Name_Id := Name_First_V2001 + 5;
   Name_Last_V2001 :     constant Name_Id := Name_First_V2001 + 5;

   --  Verilog 2005
   Name_Uwire :          constant Name_Id := Name_Last_V2001 + 1;

   --  SystemVerilog
   Name_First_SV3_0 :    constant Name_Id := Name_Last_V2001 + 2;
   Name_Always_Comb :    constant Name_Id := Name_First_SV3_0 + 0;
   Name_Always_Ff :      constant Name_Id := Name_First_SV3_0 + 1;
   Name_Always_Latch :   constant Name_Id := Name_First_SV3_0 + 2;
   Name_Bit :            constant Name_Id := Name_First_SV3_0 + 3;
   Name_Byte :           constant Name_Id := Name_First_SV3_0 + 4;
   Name_Changed :        constant Name_Id := Name_First_SV3_0 + 5;
   Name_Char :           constant Name_Id := Name_First_SV3_0 + 6;
   Name_Const :          constant Name_Id := Name_First_SV3_0 + 7;
   Name_Continue :       constant Name_Id := Name_First_SV3_0 + 8;
   Name_Do :             constant Name_Id := Name_First_SV3_0 + 9;
   Name_Endinterface :   constant Name_Id := Name_First_SV3_0 + 10;
   Name_Endtransition :  constant Name_Id := Name_First_SV3_0 + 11;
   Name_Enum :           constant Name_Id := Name_First_SV3_0 + 12;
   Name_Export :         constant Name_Id := Name_First_SV3_0 + 13;
   Name_Extern :         constant Name_Id := Name_First_SV3_0 + 14;
   Name_Forkjoin :       constant Name_Id := Name_First_SV3_0 + 15;
   Name_Iff :            constant Name_Id := Name_First_SV3_0 + 16;
   Name_Import :         constant Name_Id := Name_First_SV3_0 + 17;
   Name_Int :            constant Name_Id := Name_First_SV3_0 + 18;
   Name_Interface :      constant Name_Id := Name_First_SV3_0 + 19;
   Name_Logic :          constant Name_Id := Name_First_SV3_0 + 20;
   Name_Longint :        constant Name_Id := Name_First_SV3_0 + 21;
   Name_Longreal :       constant Name_Id := Name_First_SV3_0 + 22;
   Name_Modport :        constant Name_Id := Name_First_SV3_0 + 23;
   Name_Packed :         constant Name_Id := Name_First_SV3_0 + 24;
   Name_Priority :       constant Name_Id := Name_First_SV3_0 + 25;
   Name_Shortint :       constant Name_Id := Name_First_SV3_0 + 26;
   Name_Shortreal :      constant Name_Id := Name_First_SV3_0 + 27;
   Name_Static :         constant Name_Id := Name_First_SV3_0 + 28;
   Name_Struct :         constant Name_Id := Name_First_SV3_0 + 29;
   Name_Timeprecision :  constant Name_Id := Name_First_SV3_0 + 30;
   Name_Timeunit :       constant Name_Id := Name_First_SV3_0 + 31;
   Name_Transition :     constant Name_Id := Name_First_SV3_0 + 32;
   Name_Typedef :        constant Name_Id := Name_First_SV3_0 + 33;
   Name_Union :          constant Name_Id := Name_First_SV3_0 + 34;
   Name_Unique :         constant Name_Id := Name_First_SV3_0 + 35;
   Name_Unique0 :        constant Name_Id := Name_First_SV3_0 + 36;
   Name_Void :           constant Name_Id := Name_First_SV3_0 + 37;
   Name_Last_SV3_0 :     constant Name_Id := Name_First_SV3_0 + 37;

   Name_First_SV3_1 :    constant Name_Id := Name_Last_SV3_0 + 1;
   Name_Chandle :        constant Name_Id := Name_First_SV3_1 + 0;
   Name_Class :          constant Name_Id := Name_First_SV3_1 + 1;
   Name_Clocking :       constant Name_Id := Name_First_SV3_1 + 2;
   Name_Constraint :     constant Name_Id := Name_First_SV3_1 + 3;
   Name_Dist :           constant Name_Id := Name_First_SV3_1 + 4;
   Name_Endclass :       constant Name_Id := Name_First_SV3_1 + 5;
   Name_Endclocking :    constant Name_Id := Name_First_SV3_1 + 6;
   Name_Endprogram :     constant Name_Id := Name_First_SV3_1 + 7;
   Name_Endproperty :    constant Name_Id := Name_First_SV3_1 + 8;
   Name_Endsequence :    constant Name_Id := Name_First_SV3_1 + 9;
   Name_Extends :        constant Name_Id := Name_First_SV3_1 + 10;
   Name_Final :          constant Name_Id := Name_First_SV3_1 + 11;
   Name_First_Match :    constant Name_Id := Name_First_SV3_1 + 12;
   Name_Inside :         constant Name_Id := Name_First_SV3_1 + 13;
   Name_Intersect :      constant Name_Id := Name_First_SV3_1 + 14;
   Name_Join_Any :       constant Name_Id := Name_First_SV3_1 + 15;
   Name_Join_None :      constant Name_Id := Name_First_SV3_1 + 16;
   Name_Local :          constant Name_Id := Name_First_SV3_1 + 17;
   Name_Program :        constant Name_Id := Name_First_SV3_1 + 18;
   Name_Rand :           constant Name_Id := Name_First_SV3_1 + 19;
   Name_Randc :          constant Name_Id := Name_First_SV3_1 + 20;
   Name_Ref :            constant Name_Id := Name_First_SV3_1 + 21;
   Name_Solve :          constant Name_Id := Name_First_SV3_1 + 22;
   Name_String :         constant Name_Id := Name_First_SV3_1 + 23;
   Name_Super :          constant Name_Id := Name_First_SV3_1 + 24;
   Name_This :           constant Name_Id := Name_First_SV3_1 + 25;
   Name_Throughout :     constant Name_Id := Name_First_SV3_1 + 26;
   Name_Var :            constant Name_Id := Name_First_SV3_1 + 27;
   Name_Virtual :        constant Name_Id := Name_First_SV3_1 + 28;
   Name_Wait_Order :     constant Name_Id := Name_First_SV3_1 + 29;
   Name_Last_SV3_1 :     constant Name_Id := Name_Wait_Order;

   Name_First_SV3_1a :   constant Name_Id := Name_Last_SV3_1 + 1;
   Name_Covergroup :     constant Name_Id := Name_First_SV3_1a + 0;
   Name_Coverpoint :     constant Name_Id := Name_First_SV3_1a + 1;
   Name_Endgroup :       constant Name_Id := Name_First_SV3_1a + 2;
   Name_Endpackage :     constant Name_Id := Name_First_SV3_1a + 3;
   Name_Expect :         constant Name_Id := Name_First_SV3_1a + 4;
   Name_Foreach :        constant Name_Id := Name_First_SV3_1a + 5;
   Name_Ignore_Bins :    constant Name_Id := Name_First_SV3_1a + 6;
   Name_Illegal_Bins :   constant Name_Id := Name_First_SV3_1a + 7;
   Name_Matches :        constant Name_Id := Name_First_SV3_1a + 8;
   Name_Randcase :       constant Name_Id := Name_First_SV3_1a + 9;
   Name_Randsequence :   constant Name_Id := Name_First_SV3_1a + 10;
   Name_Tagged :         constant Name_Id := Name_First_SV3_1a + 11;
   Name_Wildcard :       constant Name_Id := Name_First_SV3_1a + 12;
   Name_Last_SV3_1a :    constant Name_Id := Name_Wildcard;

   Name_First_SV2009 :   constant Name_Id := Name_Last_SV3_1a + 1;
   Name_Implies :        constant Name_Id := Name_First_SV2009 + 0;
   Name_S_Until :        constant Name_Id := Name_First_SV2009 + 1;
   Name_S_Until_With :   constant Name_Id := Name_First_SV2009 + 2;
   Name_Until_With :     constant Name_Id := Name_First_SV2009 + 3;
   Name_Last_SV2009 :    constant Name_Id := Name_First_SV2009 + 3;

   --  VHDL operators.  Used as identifiers for declaration of overloaded
   --  operators.
   Name_First_Operator :         constant Name_Id := Name_Last_SV2009 + 1;
   Name_Op_Equality :            constant Name_Id := Name_First_Operator + 000;
   Name_Op_Inequality :          constant Name_Id := Name_First_Operator + 001;
   Name_Op_Less :                constant Name_Id := Name_First_Operator + 002;
   Name_Op_Less_Equal :          constant Name_Id := Name_First_Operator + 003;
   Name_Op_Greater :             constant Name_Id := Name_First_Operator + 004;
   Name_Op_Greater_Equal :       constant Name_Id := Name_First_Operator + 005;
   Name_Op_Plus :                constant Name_Id := Name_First_Operator + 006;
   Name_Op_Minus :               constant Name_Id := Name_First_Operator + 007;
   Name_Op_Mul :                 constant Name_Id := Name_First_Operator + 008;
   Name_Op_Div :                 constant Name_Id := Name_First_Operator + 009;
   Name_Op_Exp :                 constant Name_Id := Name_First_Operator + 010;
   Name_Op_Concatenation :       constant Name_Id := Name_First_Operator + 011;
   Name_Op_Condition :           constant Name_Id := Name_First_Operator + 012;
   Name_Op_Match_Equality :      constant Name_Id := Name_First_Operator + 013;
   Name_Op_Match_Inequality :    constant Name_Id := Name_First_Operator + 014;
   Name_Op_Match_Less :          constant Name_Id := Name_First_Operator + 015;
   Name_Op_Match_Less_Equal :    constant Name_Id := Name_First_Operator + 016;
   Name_Op_Match_Greater :       constant Name_Id := Name_First_Operator + 017;
   Name_Op_Match_Greater_Equal : constant Name_Id := Name_First_Operator + 018;
   Name_Last_Operator :  constant Name_Id := Name_Op_Match_Greater_Equal;

   subtype Name_Relational_Operators is Name_Id
     range Name_Op_Equality .. Name_Op_Greater_Equal;

   --  List of symbolic operators (available as string).
   subtype Name_Id_Operators is Name_Id
     range Name_First_Operator .. Name_Last_Operator;

   Name_First_Attribute : constant Name_Id := Name_Last_Operator + 1;
   Name_Base :            constant Name_Id := Name_First_Attribute + 000;
   Name_Left :            constant Name_Id := Name_First_Attribute + 001;
   Name_Right :           constant Name_Id := Name_First_Attribute + 002;
   Name_High :            constant Name_Id := Name_First_Attribute + 003;
   Name_Low :             constant Name_Id := Name_First_Attribute + 004;
   Name_Pos :             constant Name_Id := Name_First_Attribute + 005;
   Name_Val :             constant Name_Id := Name_First_Attribute + 006;
   Name_Succ :            constant Name_Id := Name_First_Attribute + 007;
   Name_Pred :            constant Name_Id := Name_First_Attribute + 008;
   Name_Leftof :          constant Name_Id := Name_First_Attribute + 009;
   Name_Rightof :         constant Name_Id := Name_First_Attribute + 010;
   Name_Reverse_Range :   constant Name_Id := Name_First_Attribute + 011;
   Name_Length :          constant Name_Id := Name_First_Attribute + 012;
   Name_Delayed :         constant Name_Id := Name_First_Attribute + 013;
   Name_Stable :          constant Name_Id := Name_First_Attribute + 014;
   Name_Quiet :           constant Name_Id := Name_First_Attribute + 015;
   Name_Transaction :     constant Name_Id := Name_First_Attribute + 016;
   Name_Event :           constant Name_Id := Name_First_Attribute + 017;
   Name_Active :          constant Name_Id := Name_First_Attribute + 018;
   Name_Last_Event :      constant Name_Id := Name_First_Attribute + 019;
   Name_Last_Active :     constant Name_Id := Name_First_Attribute + 020;
   Name_Last_Value :      constant Name_Id := Name_First_Attribute + 021;
   Name_Last_Attribute :  constant Name_Id := Name_Last_Value;

   subtype Name_Id_Attributes is Name_Id
     range Name_First_Attribute .. Name_Last_Attribute;

   Name_First_Vhdl87_Attribute : constant Name_Id := Name_Last_Value + 1;
   Name_Behavior :              constant Name_Id := Name_First_Attribute + 022;
   Name_Structure :             constant Name_Id := Name_First_Attribute + 023;
   Name_Last_Vhdl87_Attribute : constant Name_Id := Name_Structure;

   subtype Name_Id_Vhdl87_Attributes is Name_Id
     range Name_First_Vhdl87_Attribute .. Name_Last_Vhdl87_Attribute;

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
     range Name_First_Vhdl93_Attribute .. Name_Last_Vhdl93_Attribute;

   Name_First_Vhdl08_Attribute : constant Name_Id :=
                                             Name_Last_Vhdl93_Attribute + 01;
   Name_Element :        constant Name_Id := Name_First_Vhdl08_Attribute + 00;
   Name_Last_Vhdl08_Attribute :  constant Name_Id :=
                                             Name_First_Vhdl08_Attribute + 00;

   Name_First_AMS_Attribute : constant Name_Id :=
     Name_Last_Vhdl08_Attribute + 1;
   Name_Contribution :   constant Name_Id := Name_First_AMS_Attribute + 000;
   Name_Dot :            constant Name_Id := Name_First_AMS_Attribute + 001;
   Name_Integ :          constant Name_Id := Name_First_AMS_Attribute + 002;
   Name_Above :          constant Name_Id := Name_First_AMS_Attribute + 003;
   Name_Zoh :            constant Name_Id := Name_First_AMS_Attribute + 004;
   Name_Ltf :            constant Name_Id := Name_First_AMS_Attribute + 005;
   Name_Ztf :            constant Name_Id := Name_First_AMS_Attribute + 006;
   Name_Ramp :           constant Name_Id := Name_First_AMS_Attribute + 007;
   Name_Slew :           constant Name_Id := Name_First_AMS_Attribute + 008;
   Name_Last_AMS_Attribute : constant Name_Id := Name_Slew;

   subtype Name_Id_Name_Attributes is Name_Id
     range Name_Simple_Name .. Name_Path_Name;

   --  Names used in std.standard package.
   Name_First_Standard :      constant Name_Id := Name_Last_AMS_Attribute + 1;
   Name_Std :                 constant Name_Id := Name_First_Standard + 000;
   Name_Standard :            constant Name_Id := Name_First_Standard + 001;
   Name_Boolean :             constant Name_Id := Name_First_Standard + 002;
   Name_False :               constant Name_Id := Name_First_Standard + 003;
   Name_True :                constant Name_Id := Name_First_Standard + 004;
   Name_Character :           constant Name_Id := Name_First_Standard + 005;
   Name_Severity_Level :      constant Name_Id := Name_First_Standard + 006;
   Name_Note :                constant Name_Id := Name_First_Standard + 007;
   Name_Warning :             constant Name_Id := Name_First_Standard + 008;
   Name_Error :               constant Name_Id := Name_First_Standard + 009;
   Name_Failure :             constant Name_Id := Name_First_Standard + 010;
   Name_Universal_Integer :   constant Name_Id := Name_First_Standard + 011;
   Name_Universal_Real :      constant Name_Id := Name_First_Standard + 012;
   Name_Convertible_Integer : constant Name_Id := Name_First_Standard + 013;
   Name_Convertible_Real :    constant Name_Id := Name_First_Standard + 014;
   Name_Integer :             constant Name_Id := Name_First_Standard + 015;
   Name_Real :                constant Name_Id := Name_First_Standard + 016;
   Name_Time :                constant Name_Id := Name_First_Standard + 017;
   Name_Fs :                  constant Name_Id := Name_First_Standard + 018;
   Name_Ps :                  constant Name_Id := Name_First_Standard + 019;
   Name_Ns :                  constant Name_Id := Name_First_Standard + 020;
   Name_Us :                  constant Name_Id := Name_First_Standard + 021;
   Name_Ms :                  constant Name_Id := Name_First_Standard + 022;
   Name_Sec :                 constant Name_Id := Name_First_Standard + 023;
   Name_Min :                 constant Name_Id := Name_First_Standard + 024;
   Name_Hr :                  constant Name_Id := Name_First_Standard + 025;
   Name_Max :                 constant Name_Id := Name_First_Standard + 026;
   Name_Delay_Length :        constant Name_Id := Name_First_Standard + 027;
   Name_Now :                 constant Name_Id := Name_First_Standard + 028;
   Name_Natural :             constant Name_Id := Name_First_Standard + 029;
   Name_Positive :            constant Name_Id := Name_First_Standard + 030;
   Name_Bit_Vector :          constant Name_Id := Name_First_Standard + 031;
   Name_File_Open_Kind :      constant Name_Id := Name_First_Standard + 032;
   Name_Read_Mode :           constant Name_Id := Name_First_Standard + 033;
   Name_Write_Mode :          constant Name_Id := Name_First_Standard + 034;
   Name_Append_Mode :         constant Name_Id := Name_First_Standard + 035;
   Name_File_Open_Status :    constant Name_Id := Name_First_Standard + 036;
   Name_Open_Ok :             constant Name_Id := Name_First_Standard + 037;
   Name_Status_Error :        constant Name_Id := Name_First_Standard + 038;
   Name_Name_Error :          constant Name_Id := Name_First_Standard + 039;
   Name_Mode_Error :          constant Name_Id := Name_First_Standard + 040;
   Name_Foreign :             constant Name_Id := Name_First_Standard + 041;

   --  Added by VHDL 08
   Name_Boolean_Vector :   constant Name_Id := Name_First_Standard + 042;
   Name_To_Bstring :       constant Name_Id := Name_First_Standard + 043;
   Name_To_Binary_String : constant Name_Id := Name_First_Standard + 044;
   Name_To_Ostring :       constant Name_Id := Name_First_Standard + 045;
   Name_To_Octal_String :  constant Name_Id := Name_First_Standard + 046;
   Name_To_Hstring :       constant Name_Id := Name_First_Standard + 047;
   Name_To_Hex_String :    constant Name_Id := Name_First_Standard + 048;
   Name_Integer_Vector :   constant Name_Id := Name_First_Standard + 049;
   Name_Real_Vector :      constant Name_Id := Name_First_Standard + 050;
   Name_Time_Vector :      constant Name_Id := Name_First_Standard + 051;
   Name_Digits      :      constant Name_Id := Name_First_Standard + 052;
   Name_Format      :      constant Name_Id := Name_First_Standard + 053;
   Name_Unit        :      constant Name_Id := Name_First_Standard + 054;

   --  Added by AMS vhdl.
   Name_Domain_Type :      constant Name_Id := Name_First_Standard + 055;
   Name_Quiescent_Domain : constant Name_Id := Name_First_Standard + 056;
   Name_Time_Domain :      constant Name_Id := Name_First_Standard + 057;
   Name_Frequency_Domain : constant Name_Id := Name_First_Standard + 058;
   Name_Domain :           constant Name_Id := Name_First_Standard + 059;
   Name_Frequency :        constant Name_Id := Name_First_Standard + 060;

   --  For Std.Env
   Name_First_Env :        constant Name_Id := Name_Frequency + 1;
   Name_Env :              constant Name_Id := Name_First_Env + 0;
   Name_Stop :             constant Name_Id := Name_First_Env + 1;
   Name_Finish :           constant Name_Id := Name_First_Env + 2;
   Name_Resolution_Limit : constant Name_Id := Name_First_Env + 3;

   Name_First_Charname : constant Name_Id := Name_Resolution_Limit + 1;
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

   Name_First_Misc :            constant Name_Id := Name_Last_Charname + 1;
   Name_Guard :                 constant Name_Id := Name_First_Misc + 000;
   Name_Deallocate :            constant Name_Id := Name_First_Misc + 001;
   Name_File_Open :             constant Name_Id := Name_First_Misc + 002;
   Name_File_Close :            constant Name_Id := Name_First_Misc + 003;
   Name_Read :                  constant Name_Id := Name_First_Misc + 004;
   Name_Write :                 constant Name_Id := Name_First_Misc + 005;
   Name_Flush :                 constant Name_Id := Name_First_Misc + 006;
   Name_Endfile :               constant Name_Id := Name_First_Misc + 007;
   Name_I :                     constant Name_Id := Name_First_Misc + 008;
   Name_J :                     constant Name_Id := Name_First_Misc + 009;
   Name_F :                     constant Name_Id := Name_First_Misc + 010;
   Name_L :                     constant Name_Id := Name_First_Misc + 011;
   Name_P :                     constant Name_Id := Name_First_Misc + 012;
   Name_R :                     constant Name_Id := Name_First_Misc + 013;
   Name_S :                     constant Name_Id := Name_First_Misc + 014;
   Name_V :                     constant Name_Id := Name_First_Misc + 015;
   Name_External_Name :         constant Name_Id := Name_First_Misc + 016;
   Name_Open_Kind :             constant Name_Id := Name_First_Misc + 017;
   Name_First :                 constant Name_Id := Name_First_Misc + 018;
   Name_Last :                  constant Name_Id := Name_First_Misc + 019;
   Name_Textio :                constant Name_Id := Name_First_Misc + 020;
   Name_Work :                  constant Name_Id := Name_First_Misc + 021;
   Name_Text :                  constant Name_Id := Name_First_Misc + 022;
   Name_To_String :             constant Name_Id := Name_First_Misc + 023;
   Name_Minimum :               constant Name_Id := Name_First_Misc + 024;
   Name_Maximum :               constant Name_Id := Name_First_Misc + 025;
   Name_Untruncated_Text_Read : constant Name_Id := Name_First_Misc + 026;
   Name_Textio_Read_Real :      constant Name_Id := Name_First_Misc + 027;
   Name_Textio_Write_Real :     constant Name_Id := Name_First_Misc + 028;
   Name_Get_Resolution_Limit :  constant Name_Id := Name_First_Misc + 029;
   Name_Control_Simulation :    constant Name_Id := Name_First_Misc + 030;
   Name_Step :                  constant Name_Id := Name_First_Misc + 031;
   Name_Index :                 constant Name_Id := Name_First_Misc + 032;
   Name_Item :                  constant Name_Id := Name_First_Misc + 033;
   Name_Uu_File_Uu :            constant Name_Id := Name_First_Misc + 034;
   Name_Uu_Line_Uu :            constant Name_Id := Name_First_Misc + 035;
   Name_Label_Applies_To :      constant Name_Id := Name_First_Misc + 036;
   Name_Return_Port_Name :      constant Name_Id := Name_First_Misc + 037;
   Name_Map_To_Operator :       constant Name_Id := Name_First_Misc + 038;
   Name_Type_Function :         constant Name_Id := Name_First_Misc + 039;
   Name_Built_In :              constant Name_Id := Name_First_Misc + 040;
   Name_None :                  constant Name_Id := Name_First_Misc + 041;
   Name_Last_Misc :             constant Name_Id := Name_None;

   Name_First_Ieee_Pkg       : constant Name_Id := Name_Last_Misc + 1;
   Name_Ieee                 : constant Name_Id := Name_First_Ieee_Pkg + 000;
   Name_Std_Logic_1164       : constant Name_Id := Name_First_Ieee_Pkg + 001;
   Name_VITAL_Timing         : constant Name_Id := Name_First_Ieee_Pkg + 002;
   Name_VITAL_Primitives     : constant Name_Id := Name_First_Ieee_Pkg + 003;
   Name_Numeric_Std          : constant Name_Id := Name_First_Ieee_Pkg + 004;
   Name_Numeric_Bit          : constant Name_Id := Name_First_Ieee_Pkg + 005;
   Name_Numeric_Std_Unsigned : constant Name_Id := Name_First_Ieee_Pkg + 006;
   Name_Std_Logic_Arith      : constant Name_Id := Name_First_Ieee_Pkg + 007;
   Name_Std_Logic_Signed     : constant Name_Id := Name_First_Ieee_Pkg + 008;
   Name_Std_Logic_Unsigned   : constant Name_Id := Name_First_Ieee_Pkg + 009;
   Name_Std_Logic_Textio     : constant Name_Id := Name_First_Ieee_Pkg + 010;
   Name_Std_Logic_Misc       : constant Name_Id := Name_First_Ieee_Pkg + 011;
   Name_Math_Real            : constant Name_Id := Name_First_Ieee_Pkg + 012;
   Name_Last_Ieee_Pkg        : constant Name_Id := Name_Math_Real;

   Name_First_Ieee_Name    : constant Name_Id := Name_Last_Ieee_Pkg + 1;
   Name_Std_Ulogic         : constant Name_Id := Name_First_Ieee_Name + 000;
   Name_Std_Ulogic_Vector  : constant Name_Id := Name_First_Ieee_Name + 001;
   Name_Std_Logic          : constant Name_Id := Name_First_Ieee_Name + 002;
   Name_Std_Logic_Vector   : constant Name_Id := Name_First_Ieee_Name + 003;
   Name_Rising_Edge        : constant Name_Id := Name_First_Ieee_Name + 004;
   Name_Falling_Edge       : constant Name_Id := Name_First_Ieee_Name + 005;
   Name_VITAL_Level0       : constant Name_Id := Name_First_Ieee_Name + 006;
   Name_VITAL_Level1       : constant Name_Id := Name_First_Ieee_Name + 007;
   Name_Unresolved_Unsigned : constant Name_Id := Name_First_Ieee_Name + 008;
   Name_Unresolved_Signed  : constant Name_Id := Name_First_Ieee_Name + 009;
   Name_To_Integer         : constant Name_Id := Name_First_Ieee_Name + 010;
   Name_To_Unsigned        : constant Name_Id := Name_First_Ieee_Name + 011;
   Name_To_Signed          : constant Name_Id := Name_First_Ieee_Name + 012;
   Name_Resize             : constant Name_Id := Name_First_Ieee_Name + 013;
   Name_Std_Match          : constant Name_Id := Name_First_Ieee_Name + 014;
   Name_Shift_Left         : constant Name_Id := Name_First_Ieee_Name + 015;
   Name_Shift_Right        : constant Name_Id := Name_First_Ieee_Name + 016;
   Name_Rotate_Left        : constant Name_Id := Name_First_Ieee_Name + 017;
   Name_Rotate_Right       : constant Name_Id := Name_First_Ieee_Name + 018;
   Name_To_Bit             : constant Name_Id := Name_First_Ieee_Name + 019;
   Name_To_Bitvector       : constant Name_Id := Name_First_Ieee_Name + 020;
   Name_To_Stdulogic       : constant Name_Id := Name_First_Ieee_Name + 021;
   Name_To_Stdlogicvector  : constant Name_Id := Name_First_Ieee_Name + 022;
   Name_To_Stdulogicvector : constant Name_Id := Name_First_Ieee_Name + 023;
   Name_Is_X               : constant Name_Id := Name_First_Ieee_Name + 024;
   Name_To_01              : constant Name_Id := Name_First_Ieee_Name + 025;
   Name_To_X01             : constant Name_Id := Name_First_Ieee_Name + 026;
   Name_To_X01Z            : constant Name_Id := Name_First_Ieee_Name + 027;
   Name_To_UX01            : constant Name_Id := Name_First_Ieee_Name + 028;
   Name_Conv_Signed        : constant Name_Id := Name_First_Ieee_Name + 029;
   Name_Conv_Unsigned      : constant Name_Id := Name_First_Ieee_Name + 030;
   Name_Conv_Integer       : constant Name_Id := Name_First_Ieee_Name + 031;
   Name_Conv_Std_Logic_Vector : constant Name_Id := Name_First_Ieee_Name + 032;
   Name_And_Reduce         : constant Name_Id := Name_First_Ieee_Name + 033;
   Name_Nand_Reduce        : constant Name_Id := Name_First_Ieee_Name + 034;
   Name_Or_Reduce          : constant Name_Id := Name_First_Ieee_Name + 035;
   Name_Nor_Reduce         : constant Name_Id := Name_First_Ieee_Name + 036;
   Name_Xor_Reduce         : constant Name_Id := Name_First_Ieee_Name + 037;
   Name_Xnor_Reduce        : constant Name_Id := Name_First_Ieee_Name + 038;
   Name_Ceil               : constant Name_Id := Name_First_Ieee_Name + 039;
   Name_Floor              : constant Name_Id := Name_First_Ieee_Name + 040;
   Name_Round              : constant Name_Id := Name_First_Ieee_Name + 041;
   Name_Log2               : constant Name_Id := Name_First_Ieee_Name + 042;
   Name_Log10              : constant Name_Id := Name_First_Ieee_Name + 043;
   Name_Sin                : constant Name_Id := Name_First_Ieee_Name + 044;
   Name_Cos                : constant Name_Id := Name_First_Ieee_Name + 045;
   Name_Arctan             : constant Name_Id := Name_First_Ieee_Name + 046;
   Name_Sign               : constant Name_Id := Name_First_Ieee_Name + 047;
   Name_Sqrt               : constant Name_Id := Name_First_Ieee_Name + 048;
   Name_Shl                : constant Name_Id := Name_First_Ieee_Name + 049;
   Name_Shr                : constant Name_Id := Name_First_Ieee_Name + 050;
   Name_Ext                : constant Name_Id := Name_First_Ieee_Name + 051;
   Name_Sxt                : constant Name_Id := Name_First_Ieee_Name + 052;
   Name_Find_Leftmost      : constant Name_Id := Name_First_Ieee_Name + 053;
   Name_Find_Rightmost     : constant Name_Id := Name_First_Ieee_Name + 054;
   Name_Last_Ieee_Name     : constant Name_Id := Name_Find_Rightmost;

   Name_First_Synthesis    : constant Name_Id := Name_Last_Ieee_Name + 1;
   Name_Allconst           : constant Name_Id := Name_First_Synthesis + 000;
   Name_Allseq             : constant Name_Id := Name_First_Synthesis + 001;
   Name_Anyconst           : constant Name_Id := Name_First_Synthesis + 002;
   Name_Anyseq             : constant Name_Id := Name_First_Synthesis + 003;
   Name_Gclk               : constant Name_Id := Name_First_Synthesis + 004;
   Name_Loc                : constant Name_Id := Name_First_Synthesis + 005;
   Name_Keep               : constant Name_Id := Name_First_Synthesis + 006;
   Name_Syn_Black_Box      : constant Name_Id := Name_First_Synthesis + 007;
   Name_Last_Synthesis     : constant Name_Id := Name_Syn_Black_Box;

   --  Verilog Directives.
   Name_First_Directive : constant Name_Id := Name_Last_Synthesis + 1;
   Name_Define :          constant Name_Id := Name_First_Directive + 00;
   Name_Endif :           constant Name_Id := Name_First_Directive + 01;
   Name_Ifdef :           constant Name_Id := Name_First_Directive + 02;
   Name_Ifndef :          constant Name_Id := Name_First_Directive + 03;
   Name_Include :         constant Name_Id := Name_First_Directive + 04;
   Name_Timescale :       constant Name_Id := Name_First_Directive + 05;
   Name_Undef :           constant Name_Id := Name_First_Directive + 06;
   Name_Protect :         constant Name_Id := Name_First_Directive + 07;
   Name_Begin_Protected : constant Name_Id := Name_First_Directive + 08;
   Name_End_Protected :   constant Name_Id := Name_First_Directive + 09;
   Name_Key_Block :       constant Name_Id := Name_First_Directive + 10;
   Name_Data_Block :      constant Name_Id := Name_First_Directive + 11;
   Name_Line :            constant Name_Id := Name_First_Directive + 12;
   Name_Celldefine :      constant Name_Id := Name_First_Directive + 13;
   Name_Endcelldefine :   constant Name_Id := Name_First_Directive + 14;
   Name_Default_Nettype : constant Name_Id := Name_First_Directive + 15;
   Name_Resetall :        constant Name_Id := Name_First_Directive + 16;
   Name_Last_Directive :  constant Name_Id := Name_Resetall;

   --  Verilog system tasks/functions.
   Name_First_Systask :  constant Name_Id := Name_Last_Directive + 1;
   Name_Bits :           constant Name_Id := Name_First_Systask + 00;
   Name_D_Root :         constant Name_Id := Name_First_Systask + 01;
   Name_D_Unit :         constant Name_Id := Name_First_Systask + 02;
   Name_Last_Systask :   constant Name_Id := Name_D_Unit;

   --  Verilog methods
   Name_First_SV_Method :   constant Name_Id := Name_Last_Systask + 1;
   Name_Size :              constant Name_Id := Name_First_SV_Method + 0;
   Name_Insert :            constant Name_Id := Name_First_SV_Method + 1;
   Name_Delete :            constant Name_Id := Name_First_SV_Method + 2;
   Name_Pop_Front :         constant Name_Id := Name_First_SV_Method + 3;
   Name_Pop_Back :          constant Name_Id := Name_First_SV_Method + 4;
   Name_Push_Front :        constant Name_Id := Name_First_SV_Method + 5;
   Name_Push_Back :         constant Name_Id := Name_First_SV_Method + 6;
   Name_Name :              constant Name_Id := Name_First_SV_Method + 7;
   Name_Len :               constant Name_Id := Name_First_SV_Method + 8;
   Name_Substr :            constant Name_Id := Name_First_SV_Method + 9;
   Name_Exists :            constant Name_Id := Name_First_SV_Method + 10;
   Name_Atoi :              constant Name_Id := Name_First_SV_Method + 11;
   Name_Itoa :              constant Name_Id := Name_First_SV_Method + 12;
   Name_Find :              constant Name_Id := Name_First_SV_Method + 13;
   Name_Find_Index :        constant Name_Id := Name_First_SV_Method + 14;
   Name_Find_First :        constant Name_Id := Name_First_SV_Method + 15;
   Name_Find_First_Index :  constant Name_Id := Name_First_SV_Method + 16;
   Name_Find_Last :         constant Name_Id := Name_First_SV_Method + 17;
   Name_Find_Last_Index :   constant Name_Id := Name_First_SV_Method + 18;
   Name_Num :               constant Name_Id := Name_First_SV_Method + 19;
   Name_Randomize :         constant Name_Id := Name_First_SV_Method + 20;
   Name_Pre_Randomize :     constant Name_Id := Name_First_SV_Method + 21;
   Name_Post_Randomize :    constant Name_Id := Name_First_SV_Method + 22;
   Name_Srandom :           constant Name_Id := Name_First_SV_Method + 23;
   Name_Get_Randstate :     constant Name_Id := Name_First_SV_Method + 24;
   Name_Set_Randstate :     constant Name_Id := Name_First_SV_Method + 25;
   Name_Seed :              constant Name_Id := Name_First_SV_Method + 26;
   Name_State :             constant Name_Id := Name_First_SV_Method + 27;
   Name_Last_SV_Method :    constant Name_Id := Name_State;

   --  BSV names
   Name_First_BSV :         constant Name_Id := Name_Last_SV_Method + 1;
   Name_uAction :           constant Name_Id := Name_First_BSV + 0;
   Name_uActionValue :      constant Name_Id := Name_First_BSV + 1;
   Name_BVI :               constant Name_Id := Name_First_BSV + 2;
   Name_uC :                constant Name_Id := Name_First_BSV + 3;
   Name_uCF :               constant Name_Id := Name_First_BSV + 4;
   Name_uE :                constant Name_Id := Name_First_BSV + 5;
   Name_uSB :               constant Name_Id := Name_First_BSV + 6;
   Name_uSBR :              constant Name_Id := Name_First_BSV + 7;
   Name_Action :            constant Name_Id := Name_First_BSV + 8;
   Name_Endaction :         constant Name_Id := Name_First_BSV + 9;
   Name_Actionvalue :       constant Name_Id := Name_First_BSV + 10;
   Name_Endactionvalue :    constant Name_Id := Name_First_BSV + 11;
   Name_Ancestor :          constant Name_Id := Name_First_BSV + 12;
   --   begin
   --   bit
   --   case
   --   endcase
   Name_Clocked_By :        constant Name_Id := Name_First_BSV + 13;
   --   continue
   --   default
   Name_Default_Clock :     constant Name_Id := Name_First_BSV + 14;
   Name_Default_Reset :     constant Name_Id := Name_First_BSV + 15;
   Name_Dependencies  :     constant Name_Id := Name_First_BSV + 16;
   Name_Deriving :          constant Name_Id := Name_First_BSV + 17;
   Name_Determines :        constant Name_Id := Name_First_BSV + 18;
   --   e
   --   else
   Name_Enable :            constant Name_Id := Name_First_BSV + 19;
   --   end
   --   enum
   --   export
   --   for
   --   function
   --   endfunction
   --   if
   Name_Ifc_Inout :         constant Name_Id := Name_First_BSV + 20;
   --   import
   --   inout
   Name_Input_Clock :       constant Name_Id := Name_First_BSV + 21;
   Name_Input_Reset :       constant Name_Id := Name_First_BSV + 22;
   Name_Instance :          constant Name_Id := Name_First_BSV + 23;
   Name_Endinstance :       constant Name_Id := Name_First_BSV + 24;
   --   interface
   --   endinterface
   Name_Let :               constant Name_Id := Name_First_BSV + 25;
   Name_Match :             constant Name_Id := Name_First_BSV + 26;
   --   matches
   Name_Method :            constant Name_Id := Name_First_BSV + 27;
   Name_Endmethod :         constant Name_Id := Name_First_BSV + 28;
   --   module
   --   endmodule
   Name_Numeric :           constant Name_Id := Name_First_BSV + 29;
   Name_Output_Clock :      constant Name_Id := Name_First_BSV + 30;
   Name_Output_Reset :      constant Name_Id := Name_First_BSV + 31;
   --   package
   --   endpackage
   --   parameter
   Name_Par :               constant Name_Id := Name_First_BSV + 32;
   Name_Endpar :            constant Name_Id := Name_First_BSV + 33;
   Name_Path :              constant Name_Id := Name_First_BSV + 34;
   --   port
   Name_Provisos :          constant Name_Id := Name_First_BSV + 35;
   Name_Ready :             constant Name_Id := Name_First_BSV + 36;
   Name_Reset_By :          constant Name_Id := Name_First_BSV + 37;
   --   return
   Name_Rule :              constant Name_Id := Name_First_BSV + 38;
   Name_Endrule :           constant Name_Id := Name_First_BSV + 39;
   Name_Rules :             constant Name_Id := Name_First_BSV + 40;
   Name_Endrules :          constant Name_Id := Name_First_BSV + 41;
   Name_Same_Family :       constant Name_Id := Name_First_BSV + 42;
   Name_Schedule :          constant Name_Id := Name_First_BSV + 43;
   Name_Seq :               constant Name_Id := Name_First_BSV + 44;
   Name_Endseq :            constant Name_Id := Name_First_BSV + 45;
   --   struct
   --   tagged
   --   type
   Name_Typeclass :         constant Name_Id := Name_First_BSV + 46;
   Name_Endtypeclass :      constant Name_Id := Name_First_BSV + 47;
   --   typedef
   --   union
   Name_Valueof :           constant Name_Id := Name_First_BSV + 48;
   Name_uValueof :          constant Name_Id := Name_First_BSV + 49;
   --   void
   --   while
   Name_Last_BSV :          constant Name_Id := Name_First_BSV + 49;

   --  Special comments
   Name_First_Comment :  constant Name_Id := Name_Last_BSV + 1;
   Name_Psl :            constant Name_Id := Name_First_Comment + 0;
   Name_Pragma :         constant Name_Id := Name_First_Comment + 1;
   Name_Synthesis :      constant Name_Id := Name_First_Comment + 2;
   Name_Synopsys :       constant Name_Id := Name_First_Comment + 3;
   Name_Translate_Off :  constant Name_Id := Name_First_Comment + 4;
   Name_Translate_On :   constant Name_Id := Name_First_Comment + 5;
   Name_Translate :      constant Name_Id := Name_First_Comment + 6;
   Name_Synthesis_Off :  constant Name_Id := Name_First_Comment + 7;
   Name_Synthesis_On :   constant Name_Id := Name_First_Comment + 8;
   Name_Off :            constant Name_Id := Name_First_Comment + 9;
   Name_Full_Case :      constant Name_Id := Name_First_Comment + 10;
   Name_Parallel_Case :  constant Name_Id := Name_First_Comment + 11;
   Name_Last_Comment :   constant Name_Id := Name_Parallel_Case;

   --  PSL words.
   Name_First_PSL :          constant Name_Id := Name_Last_Comment + 1;
   Name_A :                  constant Name_Id := Name_First_PSL + 00;
   Name_Af :                 constant Name_Id := Name_First_PSL + 01;
   Name_Ag :                 constant Name_Id := Name_First_PSL + 02;
   Name_Ax :                 constant Name_Id := Name_First_PSL + 03;
   Name_Abort :              constant Name_Id := Name_First_PSL + 04;
   --  Name_Always
   --  Name_And
   --  Name_Assume
   Name_Assume_Guarantee :   constant Name_Id := Name_First_PSL + 05;
   Name_Async_Abort :        constant Name_Id := Name_First_PSL + 06;
   Name_Before :             constant Name_Id := Name_First_PSL + 07;
   --  Name_Boolean
   Name_Clock :              constant Name_Id := Name_First_PSL + 08;
   --  Name_Const
   --  Name_Cover
   --  Name_Default
   Name_E :                  constant Name_Id := Name_First_PSL + 09;
   Name_Ef :                 constant Name_Id := Name_First_PSL + 10;
   Name_Eg :                 constant Name_Id := Name_First_PSL + 11;
   Name_Ex :                 constant Name_Id := Name_First_PSL + 12;
   Name_Endpoint :           constant Name_Id := Name_First_PSL + 13;
   Name_Eventually :         constant Name_Id := Name_First_PSL + 14;
   Name_Fairness :           constant Name_Id := Name_First_PSL + 15;
   Name_Fell :               constant Name_Id := Name_First_PSL + 16;
   Name_Forall :             constant Name_Id := Name_First_PSL + 17;
   Name_G :                  constant Name_Id := Name_First_PSL + 18;
   --  Name_In
   Name_Inf :                constant Name_Id := Name_First_PSL + 19;
   --  Name_Inherit
   --  Name_Is
   Name_Never :              constant Name_Id := Name_First_PSL + 20;
   --  Name_Next
   Name_Next_A :             constant Name_Id := Name_First_PSL + 21;
   Name_Next_E :             constant Name_Id := Name_First_PSL + 22;
   Name_Next_Event :         constant Name_Id := Name_First_PSL + 23;
   Name_Next_Event_A :       constant Name_Id := Name_First_PSL + 24;
   Name_Next_Event_E :       constant Name_Id := Name_First_PSL + 25;
   --  Name_Not
   --  Name_Or
   --  Name_Property
   Name_Onehot :             constant Name_Id := Name_First_PSL + 26;
   Name_Onehot0 :            constant Name_Id := Name_First_PSL + 27;
   Name_Prev :               constant Name_Id := Name_First_PSL + 28;
   --  Name_Restrict
   --  Name_Restrict_Guarantee
   Name_Rose :               constant Name_Id := Name_First_PSL + 29;
   --   sequence
   Name_Strong :             constant Name_Id := Name_First_PSL + 30;
   Name_Sync_Abort :         constant Name_Id := Name_First_PSL + 31;
   --   union
   --   until
   Name_W :                  constant Name_Id := Name_First_PSL + 32;
   Name_Whilenot :           constant Name_Id := Name_First_PSL + 33;
   Name_Within :             constant Name_Id := Name_First_PSL + 34;
   Name_X :                  constant Name_Id := Name_First_PSL + 35;
   Name_Last_PSL :           constant Name_Id := Name_X;

   subtype Name_Id_PSL_Keywords is
     Name_Id range Name_First_PSL .. Name_Last_PSL;

   Name_First_Edif :         constant Name_Id := Name_Last_PSL + 1;
   Name_Edif :               constant Name_Id := Name_First_Edif +  0;
   Name_Edifversion :        constant Name_Id := Name_First_Edif +  1;
   Name_Ediflevel :          constant Name_Id := Name_First_Edif +  2;
   Name_Keywordmap :         constant Name_Id := Name_First_Edif +  3;
   Name_Status :             constant Name_Id := Name_First_Edif +  4;
   Name_Written :            constant Name_Id := Name_First_Edif +  5;
   Name_External :           constant Name_Id := Name_First_Edif +  6;
   Name_Comment :            constant Name_Id := Name_First_Edif +  7;
   Name_Technology :         constant Name_Id := Name_First_Edif +  8;
   Name_Cell :               constant Name_Id := Name_First_Edif +  9;
   Name_Celltype :           constant Name_Id := Name_First_Edif + 10;
   Name_View :               constant Name_Id := Name_First_Edif + 11;
   Name_Viewtype :           constant Name_Id := Name_First_Edif + 12;
   Name_Direction :          constant Name_Id := Name_First_Edif + 13;
   Name_Contents :           constant Name_Id := Name_First_Edif + 14;
   Name_Net :                constant Name_Id := Name_First_Edif + 15;
   Name_Viewref :            constant Name_Id := Name_First_Edif + 16;
   Name_Cellref :            constant Name_Id := Name_First_Edif + 17;
   Name_Libraryref :         constant Name_Id := Name_First_Edif + 18;
   Name_Portinstance :       constant Name_Id := Name_First_Edif + 19;
   Name_Joined :             constant Name_Id := Name_First_Edif + 20;
   Name_Portref :            constant Name_Id := Name_First_Edif + 21;
   Name_Instanceref :        constant Name_Id := Name_First_Edif + 22;
   Name_Design :             constant Name_Id := Name_First_Edif + 23;
   Name_Designator :         constant Name_Id := Name_First_Edif + 24;
   Name_Owner :              constant Name_Id := Name_First_Edif + 25;
   Name_Member :             constant Name_Id := Name_First_Edif + 26;
   Name_Number :             constant Name_Id := Name_First_Edif + 27;
   Name_Rename :             constant Name_Id := Name_First_Edif + 28;
   Name_Userdata :           constant Name_Id := Name_First_Edif + 29;
   Name_Last_Edif :          constant Name_Id := Name_Userdata;

   -- Initialize the name table with the values defined here.
   procedure Std_Names_Initialize;
end Std_Names;
