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
with Tokens; use Tokens;

package body Std_Names is
   procedure Std_Names_Initialize is
      function GI (S : String) return Name_Id
         renames Name_Table.Get_Identifier;

--       function GI (S : String) return Name_Id is
--       begin
--          Ada.Text_IO.Put_Line ("add " & S);
--          return Name_Table.Get_Identifier (S);
--       end GI;

   begin
      Name_Table.Initialize;

      -- Create keywords.
      for I in Tok_Mod .. Tok_Protected loop
         if GI (Image (I)) /=
           Name_First_Keyword +
           Token_Type'Pos (I) - Token_Type'Pos (Tok_First_Keyword)
         then
            raise Program_Error;
         end if;
      end loop;

      -- Create operators.
      if GI ("=") /= Name_Op_Equality
        or GI ("/=") /= Name_Op_Inequality
        or GI ("<") /= Name_Op_Less
        or GI ("<=") /= Name_Op_Less_Equal
        or GI (">") /= Name_Op_Greater
        or GI (">=") /= Name_Op_Greater_Equal
        or GI ("+") /= Name_Op_Plus
        or GI ("-") /= Name_Op_Minus
        or GI ("*") /= Name_Op_Mul
        or GI ("/") /= Name_Op_Div
        or GI ("**") /= Name_Op_Exp
        or GI ("&") /= Name_Op_Concatenation
        or GI ("??") /= Name_Op_Condition
      then
         raise Program_Error;
      end if;

      -- Create Attributes.
      if   GI ("base") /= Name_Base
        or GI ("left") /= Name_Left
        or GI ("right") /= Name_Right
        or GI ("high") /= Name_High
        or GI ("low") /= Name_Low
        or GI ("pos") /= Name_Pos
        or GI ("val") /= Name_Val
        or GI ("succ") /= Name_Succ
        or GI ("pred") /= Name_Pred
        or GI ("leftof") /= Name_Leftof
        or GI ("rightof") /= Name_Rightof
        or GI ("reverse_range") /= Name_Reverse_Range
        or GI ("length") /= Name_Length
        or GI ("delayed") /= Name_Delayed
        or GI ("stable") /= Name_Stable
        or GI ("quiet") /= Name_Quiet
        or GI ("transaction") /= Name_Transaction
        or GI ("event") /= Name_Event
        or GI ("active") /= Name_Active
        or GI ("last_event") /= Name_Last_Event
        or GI ("last_active") /= Name_Last_Active
        or GI ("last_value") /= Name_Last_Value

        or GI ("behavior") /= Name_Behavior
        or GI ("structure") /= Name_Structure

        or GI ("ascending") /= Name_Ascending
        or GI ("image") /= Name_Image
        or GI ("value") /= Name_Value
        or GI ("driving") /= Name_Driving
        or GI ("driving_value") /= Name_Driving_Value
        or GI ("simple_name") /= Name_Simple_Name
        or GI ("instance_name") /= Name_Instance_Name
        or GI ("path_name") /= Name_Path_Name
      then
         raise Program_Error;
      end if;

      --  Create standard.
      if GI ("std") /= Name_Std
        or GI ("standard") /= Name_Standard
        or GI ("boolean") /= Name_Boolean
        or GI ("false") /= Name_False
        or GI ("true") /= Name_True
        or GI ("bit") /= Name_Bit
        or GI ("character") /= Name_Character
        or GI ("severity_level") /= Name_Severity_Level
        or GI ("note") /= Name_Note
        or GI ("warning") /= Name_Warning
        or GI ("error") /= Name_Error
        or GI ("failure") /= Name_Failure
        or GI ("UNIVERSAL_INTEGER") /= Name_Universal_Integer
        or GI ("UNIVERSAL_REAL") /= Name_Universal_Real
        or GI ("CONVERTIBLE_INTEGER") /= Name_Convertible_Integer
        or GI ("CONVERTIBLE_REAL") /= Name_Convertible_Real
        or GI ("integer") /= Name_Integer
        or GI ("real") /= Name_Real
        or GI ("time") /= Name_Time
        or GI ("fs") /= Name_Fs
        or GI ("ps") /= Name_Ps
        or GI ("ns") /= Name_Ns
        or GI ("us") /= Name_Us
        or GI ("ms") /= Name_Ms
        or GI ("sec") /= Name_Sec
        or GI ("min") /= Name_Min
        or GI ("hr") /= Name_Hr
        or GI ("delay_length") /= Name_Delay_Length
        or GI ("now") /= Name_Now
        or GI ("natural") /= Name_Natural
        or GI ("positive") /= Name_Positive
        or GI ("string") /= Name_String
        or GI ("bit_vector") /= Name_Bit_Vector
        or GI ("file_open_kind") /= Name_File_Open_Kind
        or GI ("read_mode") /= Name_Read_Mode
        or GI ("write_mode") /= Name_Write_Mode
        or GI ("append_mode") /= Name_Append_Mode
        or GI ("file_open_status") /= Name_File_Open_Status
        or GI ("open_ok") /= Name_Open_Ok
        or GI ("status_error") /= Name_Status_Error
        or GI ("name_error") /= Name_Name_Error
        or GI ("mode_error") /= Name_Mode_Error
        or GI ("foreign") /= Name_Foreign
      then
         raise Program_Error;
      end if;

      if GI ("nul") /= Name_Nul
        or GI ("soh") /= Name_Soh
        or GI ("stx") /= Name_Stx
        or GI ("etx") /= Name_Etx
        or GI ("eot") /= Name_Eot
        or GI ("enq") /= Name_Enq
        or GI ("ack") /= Name_Ack
        or GI ("bel") /= Name_Bel
        or GI ("bs") /= Name_Bs
        or GI ("ht") /= Name_Ht
        or GI ("lf") /= Name_Lf
        or GI ("vt") /= Name_Vt
        or GI ("ff") /= Name_Ff
        or GI ("cr") /= Name_Cr
        or GI ("so") /= Name_So
        or GI ("si") /= Name_Si
        or GI ("dle") /= Name_Dle
        or GI ("dc1") /= Name_Dc1
        or GI ("dc2") /= Name_Dc2
        or GI ("dc3") /= Name_Dc3
        or GI ("dc4") /= Name_Dc4
        or GI ("nak") /= Name_Nak
        or GI ("syn") /= Name_Syn
        or GI ("etb") /= Name_Etb
        or GI ("can") /= Name_Can
        or GI ("em") /= Name_Em
        or GI ("sub") /= Name_Sub
        or GI ("esc") /= Name_Esc
        or GI ("fsp") /= Name_Fsp
        or GI ("gsp") /= Name_Gsp
        or GI ("rsp") /= Name_Rsp
        or GI ("usp") /= Name_Usp
        or GI ("del") /= Name_Del
      then
         raise Program_Error;
      end if;

      if GI ("c128") /= Name_C128
        or GI ("c129") /= Name_C129
        or GI ("c130") /= Name_C130
        or GI ("c131") /= Name_C131
        or GI ("c132") /= Name_C132
        or GI ("c133") /= Name_C133
        or GI ("c134") /= Name_C134
        or GI ("c135") /= Name_C135
        or GI ("c136") /= Name_C136
        or GI ("c137") /= Name_C137
        or GI ("c138") /= Name_C138
        or GI ("c139") /= Name_C139
        or GI ("c140") /= Name_C140
        or GI ("c141") /= Name_C141
        or GI ("c142") /= Name_C142
        or GI ("c143") /= Name_C143
        or GI ("c144") /= Name_C144
        or GI ("c145") /= Name_C145
        or GI ("c146") /= Name_C146
        or GI ("c147") /= Name_C147
        or GI ("c148") /= Name_C148
        or GI ("c149") /= Name_C149
        or GI ("c150") /= Name_C150
        or GI ("c151") /= Name_C151
        or GI ("c152") /= Name_C152
        or GI ("c153") /= Name_C153
        or GI ("c154") /= Name_C154
        or GI ("c155") /= Name_C155
        or GI ("c156") /= Name_C156
        or GI ("c157") /= Name_C157
        or GI ("c158") /= Name_C158
        or GI ("c159") /= Name_C159
      then
         raise Program_Error;
      end if;

      -- Create misc.
      if GI ("guard") /= Name_Guard
        or GI ("deallocate") /= Name_Deallocate
        or GI ("file_open") /= Name_File_Open
        or GI ("file_close") /= Name_File_Close
        or GI ("read") /= Name_Read
        or GI ("write") /= Name_Write
        or GI ("flush") /= Name_Flush
        or GI ("endfile") /= Name_Endfile
        or GI ("p") /= Name_P
        or GI ("f") /= Name_F
        or GI ("external_name") /= Name_External_Name
        or GI ("open_kind") /= Name_Open_Kind
        or GI ("status") /= Name_Status
        or GI ("first") /= Name_First
        or GI ("last") /= Name_Last
        or GI ("textio") /= Name_Textio
        or GI ("work") /= Name_Work
        or GI ("text") /= Name_Text
        or GI ("to_string") /= Name_To_String
        or GI ("untruncated_text_read") /= Name_Untruncated_Text_Read
      then
         raise Program_Error;
      end if;

      if GI ("ieee") /= Name_Ieee
        or GI ("std_logic_1164") /= Name_Std_Logic_1164
        or GI ("std_ulogic") /= Name_Std_Ulogic
        or GI ("std_ulogic_vector") /= Name_Std_Ulogic_Vector
        or GI ("std_logic") /= Name_Std_Logic
        or GI ("std_logic_vector") /= Name_Std_Logic_Vector
        or GI ("rising_edge") /= Name_Rising_Edge
        or GI ("falling_edge") /= Name_Falling_Edge
        or GI ("vital_timing") /= Name_VITAL_Timing
        or GI ("vital_level0") /= Name_VITAL_Level0
        or GI ("vital_level1") /= Name_VITAL_Level1
      then
         raise Program_Error;
      end if;

      --  Verilog keywords
      if GI ("always") /= Name_Always
        or GI ("assign") /= Name_Assign
        or GI ("buf") /= Name_Buf
        or GI ("bufif0") /= Name_Bufif0
        or GI ("bufif1") /= Name_Bufif1
        or GI ("casex") /= Name_Casex
        or GI ("casez") /= Name_Casez
        or GI ("cmos") /= Name_Cmos
        or GI ("deassign") /= Name_Deassign
        or GI ("default") /= Name_Default
        or GI ("defparam") /= Name_Defparam
        or GI ("disable") /= Name_Disable
        or GI ("endcase") /= Name_Endcase
        or GI ("endfunction") /= Name_Endfunction
        or GI ("endmodule") /= Name_Endmodule
        or GI ("endprimitive") /= Name_Endprimitive
        or GI ("endspecify") /= Name_Endspecify
        or GI ("endtable") /= Name_Endtable
        or GI ("endtask") /= Name_Endtask
        or GI ("forever") /= Name_Forever
        or GI ("fork") /= Name_Fork
        or GI ("highz0") /= Name_Highz0
        or GI ("highz1") /= Name_Highz1
        or GI ("initial") /= Name_Initial
        or GI ("input") /= Name_Input
        or GI ("join") /= Name_Join
        or GI ("large") /= Name_Large
        or GI ("medium") /= Name_Medium
        or GI ("module") /= Name_Module
        or GI ("negedge") /= Name_Negedge
        or GI ("nmos") /= Name_Nmos
        or GI ("notif0") /= Name_Notif0
        or GI ("notif1") /= Name_Notif1
        or GI ("output") /= Name_Output
        or GI ("parameter") /= Name_Parameter
        or GI ("pmos") /= Name_Pmos
        or GI ("posedge") /= Name_Posedge
        or GI ("primitive") /= Name_Primitive
        or GI ("pull0") /= Name_Pull0
        or GI ("pull1") /= Name_Pull1
        or GI ("pulldown") /= Name_Pulldown
        or GI ("pullup") /= Name_Pullup
        or GI ("reg") /= Name_Reg
        or GI ("repeat") /= Name_Repeat
        or GI ("rcmos") /= Name_Rcmos
        or GI ("rnmos") /= Name_Rnmos
        or GI ("rpmos") /= Name_Rpmos
        or GI ("rtran") /= Name_Rtran
        or GI ("rtranif0") /= Name_Rtranif0
        or GI ("rtranif1") /= Name_Rtranif1
        or GI ("small") /= Name_Small
        or GI ("specify") /= Name_Specify
        or GI ("specparam") /= Name_Specparam
        or GI ("strong0") /= Name_Strong0
        or GI ("strong1") /= Name_Strong1
        or GI ("supply0") /= Name_Supply0
        or GI ("supply1") /= Name_Supply1
        or GI ("table") /= Name_Tablex
        or GI ("task") /= Name_Task
        or GI ("tran") /= Name_Tran
        or GI ("tranif0") /= Name_Tranif0
        or GI ("tranif1") /= Name_Tranif1
        or GI ("tri") /= Name_Tri
        or GI ("tri0") /= Name_Tri0
        or GI ("tri1") /= Name_Tri1
        or GI ("trireg") /= Name_Trireg
        or GI ("wand") /= Name_Wand
        or GI ("weak0") /= Name_Weak0
        or GI ("weak1") /= Name_Weak1
        or GI ("wire") /= Name_Wire
        or GI ("wor") /= Name_Wor
      then
         raise Program_Error;
      end if;

      if GI ("define") /= Name_Define
        or GI ("endif") /= Name_Endif
        or GI ("ifdef") /= Name_Ifdef
        or GI ("include") /= Name_Include
        or GI ("timescale") /= Name_Timescale
        or GI ("undef") /= Name_Undef
      then
         raise Program_Error;
      end if;

      if GI ("display") /= Name_Display
        or GI ("finish") /= Name_Finish
      then
         raise Program_Error;
      end if;

      if GI ("psl") /= Name_Psl
        or GI ("pragma") /= Name_Pragma
      then
         raise Program_Error;
      end if;

      --  PSL keywords
      if GI ("a") /= Name_A
        or GI ("af") /= Name_Af
        or GI ("ag") /= Name_Ag
        or GI ("ax") /= Name_Ax
        or GI ("abort") /= Name_Abort
        or GI ("assume") /= Name_Assume
        or GI ("assume_guarantee") /= Name_Assume_Guarantee
        or GI ("before") /= Name_Before
        or GI ("clock") /= Name_Clock
        or GI ("const") /= Name_Const
        or GI ("cover") /= Name_Cover
        or GI ("e") /= Name_E
        or GI ("ef") /= Name_Ef
        or GI ("eg") /= Name_Eg
        or GI ("ex") /= Name_Ex
        or GI ("endpoint") /= Name_Endpoint
        or GI ("eventually") /= Name_Eventually
        or GI ("fairness") /= Name_Fairness
        or GI ("fell ") /= Name_Fell
        or GI ("forall") /= Name_forall
        or GI ("g") /= Name_G
        or GI ("inf") /= Name_Inf
        or GI ("inherit") /= Name_Inherit
        or GI ("never") /= Name_Never
        or GI ("next_a") /= Name_Next_A
        or GI ("next_e") /= Name_Next_E
        or GI ("next_event") /= Name_Next_Event
        or GI ("next_event_a") /= Name_Next_Event_A
        or GI ("next_event_e") /= Name_Next_Event_E
        or GI ("property") /= Name_Property
        or GI ("prev") /= Name_Prev
        or GI ("restrict") /= Name_Restrict
        or GI ("restrict_guarantee") /= Name_Restrict_Guarantee
        or GI ("rose") /= Name_Rose
        or GI ("sequence") /= Name_Sequence
        or GI ("strong") /= Name_Strong
        or GI ("union") /= Name_Union
        or GI ("vmode") /= Name_Vmode
        or GI ("vprop") /= Name_Vprop
        or GI ("vunit") /= Name_Vunit
        or GI ("w") /= Name_W
        or GI ("whilenot") /= Name_Whilenot
        or GI ("within") /= Name_Within
        or GI ("x") /= Name_X
      then
         raise Program_Error;
      end if;
   end Std_Names_Initialize;
end Std_Names;
