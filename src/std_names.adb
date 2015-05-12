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
with Name_Table;
with Ada.Exceptions;

package body Std_Names is
   procedure Std_Names_Initialize is
      procedure Def (S : String; Id : Name_Id) is
      begin
         if Name_Table.Get_Identifier (S) /= Id then
            Ada.Exceptions.Raise_Exception
              (Program_Error'Identity, "wrong name_id for " & S);
         end if;
      end Def;
   begin
      Name_Table.Initialize;

      Def ("mod", Name_Mod);
      Def ("rem", Name_Rem);

      Def ("and", Name_And);
      Def ("or", Name_Or);
      Def ("xor", Name_Xor);
      Def ("nand", Name_Nand);
      Def ("nor", Name_Nor);

      Def ("abs", Name_Abs);
      Def ("not", Name_Not);

      Def ("access", Name_Access);
      Def ("after", Name_After);
      Def ("alias", Name_Alias);
      Def ("all", Name_All);
      Def ("architecture", Name_Architecture);
      Def ("array", Name_Array);
      Def ("assert", Name_Assert);
      Def ("attribute", Name_Attribute);

      Def ("begin", Name_Begin);
      Def ("block", Name_Block);
      Def ("body", Name_Body);
      Def ("buffer", Name_Buffer);
      Def ("bus", Name_Bus);

      Def ("case", Name_Case);
      Def ("component", Name_Component);
      Def ("configuration", Name_Configuration);
      Def ("constant", Name_Constant);

      Def ("disconnect", Name_Disconnect);
      Def ("downto", Name_Downto);

      Def ("else", Name_Else);
      Def ("elsif", Name_Elsif);
      Def ("end", Name_End);
      Def ("entity", Name_Entity);
      Def ("exit", Name_Exit);

      Def ("file", Name_File);
      Def ("for", Name_For);
      Def ("function", Name_Function);

      Def ("generate", Name_Generate);
      Def ("generic", Name_Generic);
      Def ("guarded", Name_Guarded);

      Def ("if", Name_If);
      Def ("in", Name_In);
      Def ("inout", Name_Inout);
      Def ("is", Name_Is);

      Def ("label", Name_Label);
      Def ("library", Name_Library);
      Def ("linkage", Name_Linkage);
      Def ("loop", Name_Loop);

      Def ("map", Name_Map);

      Def ("new", Name_New);
      Def ("next", Name_Next);
      Def ("null", Name_Null);

      Def ("of", Name_Of);
      Def ("on", Name_On);
      Def ("open", Name_Open);
      Def ("others", Name_Others);
      Def ("out", Name_Out);

      Def ("package", Name_Package);
      Def ("port", Name_Port);
      Def ("procedure", Name_Procedure);
      Def ("process", Name_Process);

      Def ("range", Name_Range);
      Def ("record", Name_Record);
      Def ("register", Name_Register);
      Def ("report", Name_Report);
      Def ("return", Name_Return);

      Def ("select", Name_Select);
      Def ("severity", Name_Severity);
      Def ("signal", Name_Signal);
      Def ("subtype", Name_Subtype);

      Def ("then", Name_Then);
      Def ("to", Name_To);
      Def ("transport", Name_Transport);
      Def ("type", Name_Type);

      Def ("units", Name_Units);
      Def ("until", Name_Until);
      Def ("use", Name_Use);

      Def ("variable", Name_Variable);

      Def ("wait", Name_Wait);
      Def ("when", Name_When);
      Def ("while", Name_While);
      Def ("with", Name_With);

   -- VHDL93 reserved words.
      Def ("xnor", Name_Xnor);
      Def ("group", Name_Group);
      Def ("impure", Name_Impure);
      Def ("inertial", Name_Inertial);
      Def ("literal", Name_Literal);
      Def ("postponed", Name_Postponed);
      Def ("pure", Name_Pure);
      Def ("reject", Name_Reject);
      Def ("shared", Name_Shared);
      Def ("unaffected", Name_Unaffected);

      Def ("sll", Name_Sll);
      Def ("sla", Name_Sla);
      Def ("sra", Name_Sra);
      Def ("srl", Name_Srl);
      Def ("rol", Name_Rol);
      Def ("ror", Name_Ror);

      Def ("protected", Name_Protected);

      Def ("context", Name_Context);

      Def ("across", Name_Across);
      Def ("break", Name_Break);
      Def ("limit", Name_Limit);
      Def ("nature", Name_Nature);
      Def ("noise", Name_Noise);
      Def ("procedural", Name_Procedural);
      Def ("quantity", Name_Quantity);
      Def ("reference", Name_Reference);
      Def ("spectrum", Name_Spectrum);
      Def ("subnature", Name_Subnature);
      Def ("terminal", Name_Terminal);
      Def ("through", Name_Through);
      Def ("tolerance", Name_Tolerance);

      -- Create operators.
      Def ("=",  Name_Op_Equality);
      Def ("/=", Name_Op_Inequality);
      Def ("<",  Name_Op_Less);
      Def ("<=", Name_Op_Less_Equal);
      Def (">",  Name_Op_Greater);
      Def (">=", Name_Op_Greater_Equal);
      Def ("+",  Name_Op_Plus);
      Def ("-",  Name_Op_Minus);
      Def ("*",  Name_Op_Mul);
      Def ("/",  Name_Op_Div);
      Def ("**", Name_Op_Exp);
      Def ("&",  Name_Op_Concatenation);
      Def ("??", Name_Op_Condition);
      Def ("?=", Name_Op_Match_Equality);
      Def ("?/=", Name_Op_Match_Inequality);
      Def ("?<",  Name_Op_Match_Less);
      Def ("?<=", Name_Op_Match_Less_Equal);
      Def ("?>",  Name_Op_Match_Greater);
      Def ("?>=", Name_Op_Match_Greater_Equal);

      -- Create Attributes.
      Def ("base",          Name_Base);
      Def ("left",          Name_Left);
      Def ("right",         Name_Right);
      Def ("high",          Name_High);
      Def ("low",           Name_Low);
      Def ("pos",           Name_Pos);
      Def ("val",           Name_Val);
      Def ("succ",          Name_Succ);
      Def ("pred",          Name_Pred);
      Def ("leftof",        Name_Leftof);
      Def ("rightof",       Name_Rightof);
      Def ("reverse_range", Name_Reverse_Range);
      Def ("length",        Name_Length);
      Def ("delayed",       Name_Delayed);
      Def ("stable",        Name_Stable);
      Def ("quiet",         Name_Quiet);
      Def ("transaction",   Name_Transaction);
      Def ("event",         Name_Event);
      Def ("active",        Name_Active);
      Def ("last_event",    Name_Last_Event);
      Def ("last_active",   Name_Last_Active);
      Def ("last_value",    Name_Last_Value);

      Def ("behavior",      Name_Behavior);
      Def ("structure",     Name_Structure);

      Def ("ascending",     Name_Ascending);
      Def ("image",         Name_Image);
      Def ("value",         Name_Value);
      Def ("driving",       Name_Driving);
      Def ("driving_value", Name_Driving_Value);
      Def ("simple_name",   Name_Simple_Name);
      Def ("instance_name", Name_Instance_Name);
      Def ("path_name",     Name_Path_Name);

      Def ("contribution",  Name_Contribution);
      Def ("dot",           Name_Dot);
      Def ("integ",         Name_Integ);
      Def ("above",         Name_Above);
      Def ("zoh",           Name_ZOH);
      Def ("ltf",           Name_LTF);
      Def ("ztf",           Name_ZTF);
      Def ("ramp",          Name_Ramp);
      Def ("slew",          Name_Slew);

      --  Create standard.
      Def ("std",                 Name_Std);
      Def ("standard",            Name_Standard);
      Def ("boolean",             Name_Boolean);
      Def ("false",               Name_False);
      Def ("true",                Name_True);
      Def ("bit",                 Name_Bit);
      Def ("character",           Name_Character);
      Def ("severity_level",      Name_Severity_Level);
      Def ("note",                Name_Note);
      Def ("warning",             Name_Warning);
      Def ("error",               Name_Error);
      Def ("failure",             Name_Failure);
      Def ("UNIVERSAL_INTEGER",   Name_Universal_Integer);
      Def ("UNIVERSAL_REAL",      Name_Universal_Real);
      Def ("CONVERTIBLE_INTEGER", Name_Convertible_Integer);
      Def ("CONVERTIBLE_REAL",    Name_Convertible_Real);
      Def ("integer",             Name_Integer);
      Def ("real",                Name_Real);
      Def ("time",                Name_Time);
      Def ("fs",                  Name_Fs);
      Def ("ps",                  Name_Ps);
      Def ("ns",                  Name_Ns);
      Def ("us",                  Name_Us);
      Def ("ms",                  Name_Ms);
      Def ("sec",                 Name_Sec);
      Def ("min",                 Name_Min);
      Def ("hr",                  Name_Hr);
      Def ("delay_length",        Name_Delay_Length);
      Def ("now",                 Name_Now);
      Def ("natural",             Name_Natural);
      Def ("positive",            Name_Positive);
      Def ("string",              Name_String);
      Def ("bit_vector",          Name_Bit_Vector);
      Def ("file_open_kind",      Name_File_Open_Kind);
      Def ("read_mode",           Name_Read_Mode);
      Def ("write_mode",          Name_Write_Mode);
      Def ("append_mode",         Name_Append_Mode);
      Def ("file_open_status",    Name_File_Open_Status);
      Def ("open_ok",             Name_Open_Ok);
      Def ("status_error",        Name_Status_Error);
      Def ("name_error",          Name_Name_Error);
      Def ("mode_error",          Name_Mode_Error);
      Def ("foreign",             Name_Foreign);

      Def ("boolean_vector",      Name_Boolean_Vector);
      Def ("to_bstring",          Name_To_Bstring);
      Def ("to_binary_string",    Name_To_Binary_String);
      Def ("to_ostring",          Name_To_Ostring);
      Def ("to_octal_string",     Name_To_Octal_String);
      Def ("to_hstring",          Name_To_Hstring);
      Def ("to_hex_string",       Name_To_Hex_String);
      Def ("integer_vector",      Name_Integer_Vector);
      Def ("real_vector",         Name_Real_Vector);
      Def ("time_vector",         Name_Time_Vector);
      Def ("digits",              Name_Digits);
      Def ("format",              Name_Format);
      Def ("unit",                Name_Unit);

      Def ("domain_type",         Name_Domain_Type);
      Def ("quiescent_domain",    Name_Quiescent_Domain);
      Def ("time_domain",         Name_Time_Domain);
      Def ("frequency_domain",    Name_Frequency_Domain);
      Def ("domain",              Name_Domain);
      Def ("frequency",           Name_Frequency);
      Def ("real_vector",         Name_Real_Vector);

      Def ("nul", Name_Nul);
      Def ("soh", Name_Soh);
      Def ("stx", Name_Stx);
      Def ("etx", Name_Etx);
      Def ("eot", Name_Eot);
      Def ("enq", Name_Enq);
      Def ("ack", Name_Ack);
      Def ("bel", Name_Bel);
      Def ("bs",  Name_Bs);
      Def ("ht",  Name_Ht);
      Def ("lf",  Name_Lf);
      Def ("vt",  Name_Vt);
      Def ("ff",  Name_Ff);
      Def ("cr",  Name_Cr);
      Def ("so",  Name_So);
      Def ("si",  Name_Si);
      Def ("dle", Name_Dle);
      Def ("dc1", Name_Dc1);
      Def ("dc2", Name_Dc2);
      Def ("dc3", Name_Dc3);
      Def ("dc4", Name_Dc4);
      Def ("nak", Name_Nak);
      Def ("syn", Name_Syn);
      Def ("etb", Name_Etb);
      Def ("can", Name_Can);
      Def ("em",  Name_Em);
      Def ("sub", Name_Sub);
      Def ("esc", Name_Esc);
      Def ("fsp", Name_Fsp);
      Def ("gsp", Name_Gsp);
      Def ("rsp", Name_Rsp);
      Def ("usp", Name_Usp);
      Def ("del", Name_Del);

      Def ("c128", Name_C128);
      Def ("c129", Name_C129);
      Def ("c130", Name_C130);
      Def ("c131", Name_C131);
      Def ("c132", Name_C132);
      Def ("c133", Name_C133);
      Def ("c134", Name_C134);
      Def ("c135", Name_C135);
      Def ("c136", Name_C136);
      Def ("c137", Name_C137);
      Def ("c138", Name_C138);
      Def ("c139", Name_C139);
      Def ("c140", Name_C140);
      Def ("c141", Name_C141);
      Def ("c142", Name_C142);
      Def ("c143", Name_C143);
      Def ("c144", Name_C144);
      Def ("c145", Name_C145);
      Def ("c146", Name_C146);
      Def ("c147", Name_C147);
      Def ("c148", Name_C148);
      Def ("c149", Name_C149);
      Def ("c150", Name_C150);
      Def ("c151", Name_C151);
      Def ("c152", Name_C152);
      Def ("c153", Name_C153);
      Def ("c154", Name_C154);
      Def ("c155", Name_C155);
      Def ("c156", Name_C156);
      Def ("c157", Name_C157);
      Def ("c158", Name_C158);
      Def ("c159", Name_C159);

      -- Create misc.
      Def ("guard",                 Name_Guard);
      Def ("deallocate",            Name_Deallocate);
      Def ("file_open",             Name_File_Open);
      Def ("file_close",            Name_File_Close);
      Def ("read",                  Name_Read);
      Def ("write",                 Name_Write);
      Def ("flush",                 Name_Flush);
      Def ("endfile",               Name_Endfile);
      Def ("p",                     Name_P);
      Def ("f",                     Name_F);
      Def ("l",                     Name_L);
      Def ("r",                     Name_R);
      Def ("s",                     Name_S);
      Def ("external_name",         Name_External_Name);
      Def ("open_kind",             Name_Open_Kind);
      Def ("status",                Name_Status);
      Def ("first",                 Name_First);
      Def ("last",                  Name_Last);
      Def ("textio",                Name_Textio);
      Def ("work",                  Name_Work);
      Def ("text",                  Name_Text);
      Def ("to_string",             Name_To_String);
      Def ("minimum",               Name_Minimum);
      Def ("maximum",               Name_Maximum);
      Def ("untruncated_text_read", Name_Untruncated_Text_Read);
      Def ("get_resolution_limit",  Name_Get_Resolution_Limit);
      Def ("control_simulation",    Name_Control_Simulation);

      Def ("ieee",              Name_Ieee);
      Def ("std_logic_1164",    Name_Std_Logic_1164);
      Def ("std_ulogic",        Name_Std_Ulogic);
      Def ("std_ulogic_vector", Name_Std_Ulogic_Vector);
      Def ("std_logic",         Name_Std_Logic);
      Def ("std_logic_vector",  Name_Std_Logic_Vector);
      Def ("rising_edge",       Name_Rising_Edge);
      Def ("falling_edge",      Name_Falling_Edge);
      Def ("vital_timing",      Name_VITAL_Timing);
      Def ("vital_level0",      Name_VITAL_Level0);
      Def ("vital_level1",      Name_VITAL_Level1);

      --  Verilog keywords
      Def ("always",       Name_Always);
      Def ("assign",       Name_Assign);
      Def ("buf",          Name_Buf);
      Def ("bufif0",       Name_Bufif0);
      Def ("bufif1",       Name_Bufif1);
      Def ("casex",        Name_Casex);
      Def ("casez",        Name_Casez);
      Def ("cmos",         Name_Cmos);
      Def ("deassign",     Name_Deassign);
      Def ("default",      Name_Default);
      Def ("defparam",     Name_Defparam);
      Def ("disable",      Name_Disable);
      Def ("endcase",      Name_Endcase);
      Def ("endfunction",  Name_Endfunction);
      Def ("endmodule",    Name_Endmodule);
      Def ("endprimitive", Name_Endprimitive);
      Def ("endspecify",   Name_Endspecify);
      Def ("endtable",     Name_Endtable);
      Def ("endtask",      Name_Endtask);
      Def ("forever",      Name_Forever);
      Def ("fork",         Name_Fork);
      Def ("highz0",       Name_Highz0);
      Def ("highz1",       Name_Highz1);
      Def ("initial",      Name_Initial);
      Def ("input",        Name_Input);
      Def ("join",         Name_Join);
      Def ("large",        Name_Large);
      Def ("medium",       Name_Medium);
      Def ("module",       Name_Module);
      Def ("negedge",      Name_Negedge);
      Def ("nmos",         Name_Nmos);
      Def ("notif0",       Name_Notif0);
      Def ("notif1",       Name_Notif1);
      Def ("output",       Name_Output);
      Def ("parameter",    Name_Parameter);
      Def ("pmos",         Name_Pmos);
      Def ("posedge",      Name_Posedge);
      Def ("primitive",    Name_Primitive);
      Def ("pull0",        Name_Pull0);
      Def ("pull1",        Name_Pull1);
      Def ("pulldown",     Name_Pulldown);
      Def ("pullup",       Name_Pullup);
      Def ("reg",          Name_Reg);
      Def ("repeat",       Name_Repeat);
      Def ("rcmos",        Name_Rcmos);
      Def ("rnmos",        Name_Rnmos);
      Def ("rpmos",        Name_Rpmos);
      Def ("rtran",        Name_Rtran);
      Def ("rtranif0",     Name_Rtranif0);
      Def ("rtranif1",     Name_Rtranif1);
      Def ("small",        Name_Small);
      Def ("specify",      Name_Specify);
      Def ("specparam",    Name_Specparam);
      Def ("strong0",      Name_Strong0);
      Def ("strong1",      Name_Strong1);
      Def ("supply0",      Name_Supply0);
      Def ("supply1",      Name_Supply1);
      Def ("table",        Name_Tablex);
      Def ("task",         Name_Task);
      Def ("tran",         Name_Tran);
      Def ("tranif0",      Name_Tranif0);
      Def ("tranif1",      Name_Tranif1);
      Def ("tri",          Name_Tri);
      Def ("tri0",         Name_Tri0);
      Def ("tri1",         Name_Tri1);
      Def ("trireg",       Name_Trireg);
      Def ("wand",         Name_Wand);
      Def ("weak0",        Name_Weak0);
      Def ("weak1",        Name_Weak1);
      Def ("wire",         Name_Wire);
      Def ("wor",          Name_Wor);

      Def ("define",       Name_Define);
      Def ("endif",        Name_Endif);
      Def ("ifdef",        Name_Ifdef);
      Def ("include",      Name_Include);
      Def ("timescale",    Name_Timescale);
      Def ("undef",        Name_Undef);

      --  Verilog system tasks
      Def ("display", Name_Display);
      Def ("finish",  Name_Finish);

      --  BSV keywords
      Def ("Action",         Name_uAction);
      Def ("ActionValue",    Name_uActionValue);
      Def ("BVI",            Name_BVI);
      Def ("C",              Name_uC);
      Def ("CF",             Name_uCF);
      Def ("E",              Name_uE);
      Def ("SB",             Name_uSB);
      Def ("SBR",            Name_uSBR);
      Def ("action",         Name_Action);
      Def ("endaction",      Name_Endaction);
      Def ("actionvalue",    Name_Actionvalue);
      Def ("endactionvalue", Name_Endactionvalue);
      Def ("ancestor",       Name_Ancestor);
      Def ("clocked_by",     Name_Clocked_By);
      Def ("continue",       Name_Continue);
      Def ("default_clock",  Name_Default_Clock);
      Def ("default_reset",  Name_Default_Reset);
      Def ("dependencies",   Name_Dependencies);
      Def ("deriving",       Name_Deriving);
      Def ("determines",     Name_Determines);
      Def ("enable",         Name_Enable);
      Def ("enum",           Name_Enum);
      Def ("export",         Name_Export);
      Def ("ifc_inout",      Name_Ifc_Inout);
      Def ("import",         Name_Import);
      Def ("input_clock",    Name_Input_Clock);
      Def ("input_reset",    Name_Input_Reset);
      Def ("instance",       Name_Instance);
      Def ("endinstance",    Name_Endinstance);
      Def ("interface",      Name_Interface);
      Def ("endinterface",   Name_Endinterface);
      Def ("let",            Name_Let);
      Def ("match",          Name_Match);
      Def ("matches",        Name_Matches);
      Def ("method",         Name_Method);
      Def ("endmethod",      Name_Endmethod);
      Def ("numeric",        Name_Numeric);
      Def ("output_clock",   Name_Output_Clock);
      Def ("output_reset",   Name_Output_Reset);
      Def ("endpackage",     Name_Endpackage);
      Def ("par",            Name_Par);
      Def ("endpar",         Name_Endpar);
      Def ("path",           Name_Path);
      Def ("provisos",       Name_Provisos);
      Def ("ready",          Name_Ready);
      Def ("reset_by",       Name_Reset_By);
      Def ("rule",           Name_Rule);
      Def ("endrule",        Name_Endrule);
      Def ("rules",          Name_Rules);
      Def ("endrules",       Name_Endrules);
      Def ("same_family",    Name_Same_Family);
      Def ("schedule",       Name_Schedule);
      Def ("seq",            Name_Seq);
      Def ("endseq",         Name_Endseq);
      Def ("struct",         Name_Struct);
      Def ("tagged",         Name_Tagged);
      Def ("typeclass",      Name_Typeclass);
      Def ("endtypeclass",   Name_Endtypeclass);
      Def ("typedef",        Name_Typedef);
      Def ("union",          Name_Union);
      Def ("valueof",        Name_Valueof);
      Def ("valueOf",        Name_uValueof);
      Def ("void",           Name_Void);

      --  VHDL special comments
      Def ("psl",    Name_Psl);
      Def ("pragma", Name_Pragma);

      --  PSL keywords
      Def ("a",                  Name_A);
      Def ("af",                 Name_Af);
      Def ("ag",                 Name_Ag);
      Def ("ax",                 Name_Ax);
      Def ("abort",              Name_Abort);
      Def ("assume",             Name_Assume);
      Def ("assume_guarantee",   Name_Assume_Guarantee);
      Def ("before",             Name_Before);
      Def ("clock",              Name_Clock);
      Def ("const",              Name_Const);
      Def ("cover",              Name_Cover);
      Def ("e",                  Name_E);
      Def ("ef",                 Name_Ef);
      Def ("eg",                 Name_Eg);
      Def ("ex",                 Name_Ex);
      Def ("endpoint",           Name_Endpoint);
      Def ("eventually",         Name_Eventually);
      Def ("fairness",           Name_Fairness);
      Def ("fell ",              Name_Fell);
      Def ("forall",             Name_Forall);
      Def ("g",                  Name_G);
      Def ("inf",                Name_Inf);
      Def ("inherit",            Name_Inherit);
      Def ("never",              Name_Never);
      Def ("next_a",             Name_Next_A);
      Def ("next_e",             Name_Next_E);
      Def ("next_event",         Name_Next_Event);
      Def ("next_event_a",       Name_Next_Event_A);
      Def ("next_event_e",       Name_Next_Event_E);
      Def ("property",           Name_Property);
      Def ("prev",               Name_Prev);
      Def ("restrict",           Name_Restrict);
      Def ("restrict_guarantee", Name_Restrict_Guarantee);
      Def ("rose",               Name_Rose);
      Def ("sequence",           Name_Sequence);
      Def ("strong",             Name_Strong);
      Def ("union",              Name_Union);
      Def ("vmode",              Name_Vmode);
      Def ("vprop",              Name_Vprop);
      Def ("vunit",              Name_Vunit);
      Def ("w",                  Name_W);
      Def ("whilenot",           Name_Whilenot);
      Def ("within",             Name_Within);
      Def ("x",                  Name_X);
   end Std_Names_Initialize;
end Std_Names;
