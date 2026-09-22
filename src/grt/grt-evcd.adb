--  GHDL Run Time (GRT) - Extended VCD (EVCD) generator.
--  Copyright (C) 2026 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

-------------------------------------------------------------------------------
--  Writes IEEE 1364-2005 (Clause 18) "extended VCD" (EVCD) files: a
--  ports-only dump using $dumpports semantics, $var port declarations and
--  'p' value records carrying per-bit state and drive-strength information.
--
--  Only ports are dumped (internal signals are not), mirroring the $dumpports
--  task.  Unlike the strict spec example, ports are dumped throughout the
--  whole elaborated hierarchy (each in its own $scope), which is the useful
--  analog for a VHDL testbench whose DUT ports live below the top instance.
--
--  std_logic has no Verilog drive strength, so driven values use 'strong'
--  (6) and high-impedance uses 'highz' (0); this matches the spec's own
--  example bytes (e.g. "pU 0 6", "pD 6 0", "pX 6 6").  Value extraction
--  reuses the helpers exported by Grt.Vcd.
-------------------------------------------------------------------------------

with System; use System;
with Interfaces;
with Grt.Stdio; use Grt.Stdio;
with Grt.Errors; use Grt.Errors;
with Grt.Signals; use Grt.Signals;
with Grt.Table;
with Grt.Astdio; use Grt.Astdio;
with Grt.C; use Grt.C;
with Grt.Hooks; use Grt.Hooks;
with Grt.Avhpi; use Grt.Avhpi;
with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Vcd; use Grt.Vcd;
with Grt.To_Strings;
with Grt.Rtis_Types; use Grt.Rtis_Types;
with Grt.Options;
pragma Elaborate_All (Grt.Table);

package body Grt.Evcd is
   --  ASCII line feed.
   Nl : constant Character := Character'Val (10);

   --  If TRUE, write $date in the EVCD file.  Disable for reproducible output.
   Flag_Evcd_Date : Boolean := True;

   --  Non-standard, opt-in annotations selected by --evcd=FILE:<letters>.
   --  Each rides in a '$comment ghdl:<letter> ...$end' block, so a standard
   --  EVCD reader ignores them and the file stays loadable.
   --    d - port direction (i/o/b/f/l = in/out/inout/buffer/linkage)
   --    t - the resolved VHDL type of the port
   --    9 - the full 9-state std_logic value (U X 0 1 Z W L H -) per change,
   --        which the standard 'p' record collapses to N/X
   Ann_Dir : Boolean := False;
   Ann_Type : Boolean := False;
   Ann_9state : Boolean := False;

   --  TRUE if any annotation is enabled.
   function Any_Annotation return Boolean is
   begin
      return Ann_Dir or Ann_Type or Ann_9state;
   end Any_Annotation;

   Stream : FILEs;

   --  Abstract IO, set up once an output file is selected.
   type Evcd_Put_Acc is access procedure (Str : String);
   type Evcd_Putc_Acc is access procedure (C : Character);
   type Evcd_Close_Acc is access procedure;

   Evcd_Put : Evcd_Put_Acc;
   Evcd_Putc : Evcd_Putc_Acc;
   Evcd_Close : Evcd_Close_Acc;

   --  Last simulation time written to the file.  Used for the final
   --  timestamp and the $vcdclose marker.
   Last_Time_Written : Std_Time := 0;

   procedure My_Evcd_Put (Str : String)
   is
      R : size_t;
      pragma Unreferenced (R);
   begin
      R := fwrite (Str'Address, Str'Length, 1, Stream);
   end My_Evcd_Put;

   procedure My_Evcd_Putc (C : Character)
   is
      R : int;
      pragma Unreferenced (R);
   begin
      R := fputc (Character'Pos (C), Stream);
   end My_Evcd_Putc;

   procedure My_Evcd_Close is
   begin
      fclose (Stream);
      Stream := NULL_Stream;
   end My_Evcd_Close;

   procedure Evcd_Newline is
   begin
      Evcd_Putc (Nl);
   end Evcd_Newline;

   procedure Evcd_Putline (Str : String) is
   begin
      Evcd_Put (Str);
      Evcd_Newline;
   end Evcd_Putline;

   procedure Evcd_Put_I32 (V : Ghdl_I32)
   is
      Str : String (1 .. 11);
      First : Natural;
   begin
      To_Strings.To_String (Str, First, V);
      Evcd_Put (Str (First .. Str'Last));
   end Evcd_Put_I32;

   procedure Evcd_Put_I64 (V : Ghdl_I64)
   is
      Str : String (1 .. 21);
      First : Natural;
   begin
      To_Strings.To_String (Str, First, V);
      Evcd_Put (Str (First .. Str'Last));
   end Evcd_Put_I64;

   --  EVCD identifier codes are '<' followed by a decimal integer that
   --  starts at zero and ascends with port declaration order.
   procedure Evcd_Put_Idcode (N : Ghdl_I32) is
   begin
      Evcd_Putc ('<');
      Evcd_Put_I32 (N);
   end Evcd_Put_Idcode;

   procedure Evcd_Put_Name (Obj : VhpiHandleT)
   is
      Name : String (1 .. 128);
      Name_Len : Integer;
   begin
      Vhpi_Get_Str (VhpiNameP, Obj, Name, Name_Len);
      if Name_Len <= Name'Last then
         Evcd_Put (Name (1 .. Name_Len));
      else
         --  Truncate.
         Evcd_Put (Name);
      end if;
   end Evcd_Put_Name;

   --  TRUE if C may appear in an annotation-letter suffix.
   function Is_Ann_Char (C : Character) return Boolean is
   begin
      return C = 'd' or C = 't' or C = '9';
   end Is_Ann_Char;

   --  Enable the annotations named by S (a sequence of letters, or "all").
   procedure Set_Annotations (S : String) is
   begin
      if S = "all" then
         Ann_Dir := True;
         Ann_Type := True;
         Ann_9state := True;
         return;
      end if;
      for I in S'Range loop
         case S (I) is
            when 'd' => Ann_Dir := True;
            when 't' => Ann_Type := True;
            when '9' => Ann_9state := True;
            when others => null;
         end case;
      end loop;
   end Set_Annotations;

   --  Return TRUE if S is a valid annotation suffix ("all" or letters only).
   function Is_Ann_Suffix (S : String) return Boolean is
   begin
      if S'Length = 0 then
         return False;
      end if;
      if S = "all" then
         return True;
      end if;
      for I in S'Range loop
         if not Is_Ann_Char (S (I)) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Ann_Suffix;

   --  Option parsing: --evcd=FILE[:LETTERS] and --evcd-nodate.
   function Evcd_Option (Opt : String) return Boolean
   is
      F : constant Natural := Opt'First;
      Mode : constant String := "wt" & NUL;
      Filename : String_Access;
      Path_Last : Natural;
      Name_Len : Natural;
   begin
      if Opt'Length < 6 or else Opt (F .. F + 5) /= "--evcd" then
         return False;
      end if;
      if Opt'Length = 13 and then Opt (F + 6 .. F + 12) = "-nodate" then
         Flag_Evcd_Date := False;
         return True;
      end if;
      if Opt'Length > 7 and then Opt (F + 6) = '=' then
         if Evcd_Close /= null then
            Error ("--evcd: file already set");
            return True;
         end if;

         --  Split off a trailing ":LETTERS" annotation selector, but only
         --  when LETTERS is a valid suffix.  This keeps Windows drive paths
         --  (e.g. C:\dir\out.evcd) intact, since the text after their colon
         --  is not a valid annotation suffix.
         Path_Last := Opt'Last;
         for I in reverse F + 7 .. Opt'Last loop
            if Opt (I) = ':' then
               if I < Opt'Last and then Is_Ann_Suffix (Opt (I + 1 .. Opt'Last))
               then
                  Set_Annotations (Opt (I + 1 .. Opt'Last));
                  Path_Last := I - 1;
               end if;
               exit;
            end if;
         end loop;

         if Path_Last < F + 7 then
            Error ("--evcd: missing filename");
            return True;
         end if;

         --  Add an extra NUL character.
         Name_Len := Path_Last - (F + 7) + 1;
         Filename := new String (1 .. Name_Len + 1);
         Filename (1 .. Name_Len) := Opt (F + 7 .. Path_Last);
         Filename (Filename'Last) := NUL;

         if Filename.all = "-" & NUL then
            Stream := stdout;
         else
            Stream := fopen (Filename.all'Address, Mode'Address);
            if Stream = NULL_Stream then
               Error_S ("cannot open ");
               Error_E (Filename (Filename'First .. Filename'Last - 1));
            end if;
         end if;
         Evcd_Putc := My_Evcd_Putc'Access;
         Evcd_Put := My_Evcd_Put'Access;
         Evcd_Close := My_Evcd_Close'Access;
         return True;
      else
         return False;
      end if;
   end Evcd_Option;

   procedure Evcd_Help is
   begin
      Put_Line (" --evcd=FILENAME    dump port values into an extended VCD"
                  & " file");
      Put_Line (" --evcd=FILE:CODES  add non-standard annotations as comments"
                  & " (d/t/9 or all)");
      Put_Line (" --evcd-nodate      do not write date in extended VCD file");
   end Evcd_Help;

   procedure Evcd_Put_End is
   begin
      Evcd_Putline ("$end");
   end Evcd_Put_End;

   --  Called before elaboration: write the file header.
   procedure Evcd_Init is
   begin
      if Evcd_Close = null then
         return;
      end if;
      if Flag_Evcd_Date then
         Evcd_Putline ("$date");
         Evcd_Put ("  ");
         declare
            type time_t is new Interfaces.Integer_64;
            Cur_Time : time_t;

            function time (Addr : Address) return time_t;
            pragma Import (C, time);

            function ctime (Timep : Address) return Ghdl_C_String;
            pragma Import (C, ctime);

            Ct : Ghdl_C_String;
         begin
            Cur_Time := time (Null_Address);
            Ct := ctime (Cur_Time'Address);
            for I in Positive loop
               exit when Ct (I) = NUL;
               Evcd_Putc (Ct (I));
            end loop;
            --  Note: ctime already appends a LF.
         end;
         Evcd_Put_End;
      end if;
      Evcd_Putline ("$version");
      Evcd_Putline ("  GHDL v0");
      Evcd_Put_End;
      Evcd_Putline ("$timescale");
      case Options.Time_Resolution_Scale is
         when 5 =>
            Evcd_Putline ("  1 fs");
         when 4 =>
            Evcd_Putline ("  1 ps");
         when 3 =>
            Evcd_Putline ("  1 ns");
         when 2 =>
            Evcd_Putline ("  1 us");
         when 1 =>
            Evcd_Putline ("  1 ms");
         when 0 =>
            Evcd_Putline ("  1 sec");
      end case;
      Evcd_Put_End;

      --  Record which non-standard annotations this file carries, so a
      --  cooperating reader can self-configure.
      if Any_Annotation then
         Evcd_Put ("$comment ghdl_evcd_annotate ");
         if Ann_Dir then
            Evcd_Putc ('d');
         end if;
         if Ann_Type then
            Evcd_Putc ('t');
         end if;
         if Ann_9state then
            Evcd_Putc ('9');
         end if;
         Evcd_Putc (' ');
         Evcd_Put_End;
      end if;
   end Evcd_Init;

   --  One dumped port: its wire info (for value extraction) and its mode
   --  (which selects the EVCD state-character set).
   type Evcd_Entry is record
      Wire : Verilog_Wire_Info;
      Mode : VhpiModeT;
   end record;

   type Evcd_Index_Type is new Integer;

   package Evcd_Table is new Grt.Table
     (Table_Component_Type => Evcd_Entry,
      Table_Index_Type => Evcd_Index_Type,
      Table_Low_Bound => 0,
      Table_Initial => 32);

   procedure Avhpi_Error (Err : AvhpiErrorT)
   is
      pragma Unreferenced (Err);
   begin
      Put_Line ("Evcd.Avhpi_Error!");
   end Avhpi_Error;

   --  Direction class used to pick the state-character set.
   type Port_Dir is (Pd_In, Pd_Out, Pd_Unknown);

   function Dir_Of (M : VhpiModeT) return Port_Dir is
   begin
      case M is
         when VhpiInMode =>
            return Pd_In;
         when VhpiOutMode
           | VhpiBufferMode =>
            return Pd_Out;
         when others =>
            --  inout, linkage: genuinely bidirectional -> unknown-direction.
            return Pd_Unknown;
      end case;
   end Dir_Of;

   --  Logic level of a std_logic position (0..8 = U X 0 1 Z W L H -).
   type Logic_Level is (Lv_Low, Lv_High, Lv_Hiz, Lv_Unknown);

   function Level_Of (E : Ghdl_E8) return Logic_Level is
   begin
      case E is
         when 2 | 6 =>     --  '0', 'L'
            return Lv_Low;
         when 3 | 7 =>     --  '1', 'H'
            return Lv_High;
         when 4 =>         --  'Z'
            return Lv_Hiz;
         when others =>    --  'U', 'X', 'W', '-'
            return Lv_Unknown;
      end case;
   end Level_Of;

   function State_Char (E : Ghdl_E8; D : Port_Dir) return Character is
   begin
      case Level_Of (E) is
         when Lv_Low =>
            case D is
               when Pd_In => return 'D';
               when Pd_Out => return 'L';
               when Pd_Unknown => return '0';
            end case;
         when Lv_High =>
            case D is
               when Pd_In => return 'U';
               when Pd_Out => return 'H';
               when Pd_Unknown => return '1';
            end case;
         when Lv_Hiz =>
            case D is
               when Pd_In => return 'Z';
               when Pd_Out => return 'T';
               when Pd_Unknown => return 'F';
            end case;
         when Lv_Unknown =>
            case D is
               when Pd_In => return 'N';
               when Pd_Out => return 'X';
               when Pd_Unknown => return '?';
            end case;
      end case;
   end State_Char;

   --  strength0 / strength1 components (Verilog strengths: 0=highz, 6=strong).
   function Str0_Char (E : Ghdl_E8) return Character is
   begin
      case Level_Of (E) is
         when Lv_Low | Lv_Unknown => return '6';
         when Lv_High | Lv_Hiz => return '0';
      end case;
   end Str0_Char;

   function Str1_Char (E : Ghdl_E8) return Character is
   begin
      case Level_Of (E) is
         when Lv_High | Lv_Unknown => return '6';
         when Lv_Low | Lv_Hiz => return '0';
      end case;
   end Str1_Char;

   --  std_logic position for bit J of a port, normalising bit/boolean to
   --  the '0'/'1' positions.
   function Bit_E8 (V : Verilog_Wire_Info; J : Ghdl_Index_Type)
                   return Ghdl_E8 is
   begin
      case V.Vtype is
         when Vcd_Stdlogic =>
            return Verilog_Wire_Val (V).E8;
         when Vcd_Stdlogic_Vector =>
            return Verilog_Wire_Val (V, J).E8;
         when Vcd_Bit
           | Vcd_Bool =>
            if Verilog_Wire_Val (V).B1 then
               return 3;
            else
               return 2;
            end if;
         when Vcd_Bitvector =>
            if Verilog_Wire_Val (V, J).B1 then
               return 3;
            else
               return 2;
            end if;
         when others =>
            return 1;  --  'X'
      end case;
   end Bit_E8;

   --  Emit one 'p' value record for port I.  For an N-bit port the state and
   --  each strength component carry N characters (the scalar form when N=1).
   procedure Evcd_Put_Var (I : Evcd_Index_Type)
   is
      E : Evcd_Entry renames Evcd_Table.Table (I);
      V : Verilog_Wire_Info renames E.Wire;
      D : constant Port_Dir := Dir_Of (E.Mode);
      Len : constant Ghdl_Index_Type := Get_Wire_Length (V);
   begin
      Evcd_Putc ('p');
      for J in 0 .. Len - 1 loop
         Evcd_Putc (State_Char (Bit_E8 (V, J), D));
      end loop;
      Evcd_Putc (' ');
      for J in 0 .. Len - 1 loop
         Evcd_Putc (Str0_Char (Bit_E8 (V, J)));
      end loop;
      Evcd_Putc (' ');
      for J in 0 .. Len - 1 loop
         Evcd_Putc (Str1_Char (Bit_E8 (V, J)));
      end loop;
      Evcd_Putc (' ');
      Evcd_Put_Idcode (Ghdl_I32 (I));
      Evcd_Newline;
   end Evcd_Put_Var;

   --  Annotation 'd': single-letter port direction.
   function Dir_Letter (M : VhpiModeT) return Character is
   begin
      case M is
         when VhpiInMode => return 'i';
         when VhpiOutMode => return 'o';
         when VhpiInoutMode => return 'b';
         when VhpiBufferMode => return 'f';
         when VhpiLinkageMode => return 'l';
         when others => return '?';
      end case;
   end Dir_Letter;

   --  Annotation 't': the resolved VHDL type of a port.
   procedure Evcd_Put_Type (V : Verilog_Wire_Info) is
   begin
      case V.Vtype is
         when Vcd_Bit => Evcd_Put ("bit");
         when Vcd_Bool => Evcd_Put ("boolean");
         when Vcd_Stdlogic => Evcd_Put ("std_logic");
         when Vcd_Bitvector => Evcd_Put ("bit_vector");
         when Vcd_Stdlogic_Vector => Evcd_Put ("std_logic_vector");
         when others => Evcd_Putc ('?');
      end case;
      if V.Vtype in Vcd_Var_Vectors then
         Evcd_Putc ('(');
         Evcd_Put_I32 (V.Vec_Range.I32.Left);
         if V.Vec_Range.I32.Left >= V.Vec_Range.I32.Right then
            Evcd_Put (" downto ");
         else
            Evcd_Put (" to ");
         end if;
         Evcd_Put_I32 (V.Vec_Range.I32.Right);
         Evcd_Putc (')');
      end if;
   end Evcd_Put_Type;

   --  std_logic position (0..8) to its character (annotation '9').
   function Std_Logic_Char (E : Ghdl_E8) return Character is
      Map : constant String := "UX01ZWLH-";
   begin
      if E in 0 .. 8 then
         return Map (Integer (E) + 1);
      else
         return '?';
      end if;
   end Std_Logic_Char;

   --  Annotation '9': the full 9-state std_logic value of a port, which the
   --  standard 'p' record collapses to N/X.  Only meaningful for std_logic
   --  (bit/boolean are already lossless in the 'p' record).
   procedure Evcd_Put_9 (I : Evcd_Index_Type)
   is
      V : Verilog_Wire_Info renames Evcd_Table.Table (I).Wire;
      Len : constant Ghdl_Index_Type := Get_Wire_Length (V);
   begin
      if V.Vtype /= Vcd_Stdlogic and then V.Vtype /= Vcd_Stdlogic_Vector then
         return;
      end if;
      Evcd_Put ("$comment ghdl:9 ");
      Evcd_Put_Idcode (Ghdl_I32 (I));
      Evcd_Putc (' ');
      for J in 0 .. Len - 1 loop
         Evcd_Putc (Std_Logic_Char (Bit_E8 (V, J)));
      end loop;
      Evcd_Putc (' ');
      Evcd_Put_End;
   end Evcd_Put_9;

   procedure Add_Port (Sig : VhpiHandleT)
   is
      N : Evcd_Index_Type;
      Wire : Verilog_Wire_Info;
      Handled : Boolean;
   begin
      Get_Verilog_Wire (Sig, Wire);

      case Wire.Vtype is
         when Vcd_Bool
           | Vcd_Bit
           | Vcd_Stdlogic
           | Vcd_Bitvector
           | Vcd_Stdlogic_Vector =>
            Handled := True;
         when others =>
            Handled := False;
      end case;

      if not Handled then
         Evcd_Put ("$comment ");
         Evcd_Put_Name (Sig);
         Evcd_Put (" is not handled by extended VCD ");
         Evcd_Put_End;
         return;
      end if;

      Evcd_Table.Increment_Last;
      N := Evcd_Table.Last;
      Evcd_Table.Table (N) := (Wire => Wire, Mode => Vhpi_Get_Mode (Sig));

      Evcd_Put ("$var port ");
      if Wire.Vtype in Vcd_Var_Vectors then
         Evcd_Putc ('[');
         Evcd_Put_I32 (Wire.Vec_Range.I32.Left);
         Evcd_Putc (':');
         Evcd_Put_I32 (Wire.Vec_Range.I32.Right);
         Evcd_Putc (']');
      else
         Evcd_Put ("1");
      end if;
      Evcd_Putc (' ');
      Evcd_Put_Idcode (Ghdl_I32 (N));
      Evcd_Putc (' ');
      Evcd_Put_Name (Sig);
      Evcd_Putc (' ');
      Evcd_Put_End;

      --  Optional non-standard annotations (comment-safe).
      if Ann_Dir then
         Evcd_Put ("$comment ghdl:d ");
         Evcd_Put_Idcode (Ghdl_I32 (N));
         Evcd_Putc (' ');
         Evcd_Putc (Dir_Letter (Vhpi_Get_Mode (Sig)));
         Evcd_Putc (' ');
         Evcd_Put_End;
      end if;
      if Ann_Type then
         Evcd_Put ("$comment ghdl:t ");
         Evcd_Put_Idcode (Ghdl_I32 (N));
         Evcd_Putc (' ');
         Evcd_Put_Type (Wire);
         Evcd_Putc (' ');
         Evcd_Put_End;
      end if;
   end Add_Port;

   procedure Evcd_Put_Hierarchy (Inst : VhpiHandleT)
   is
      Decl_It : VhpiHandleT;
      Decl : VhpiHandleT;
      Error : AvhpiErrorT;
   begin
      Vhpi_Iterator (VhpiDecls, Inst, Decl_It, Error);
      if Error /= AvhpiErrorOk then
         Avhpi_Error (Error);
         return;
      end if;

      Evcd_Put ("$scope module ");
      Evcd_Put_Name (Inst);
      Evcd_Putc (' ');
      Evcd_Put_End;

      --  Ports only (no internal signals): this is the $dumpports semantics.
      loop
         Vhpi_Scan (Decl_It, Decl, Error);
         exit when Error = AvhpiErrorIteratorEnd;
         if Error /= AvhpiErrorOk then
            Avhpi_Error (Error);
            return;
         end if;

         case Vhpi_Get_Kind (Decl) is
            when VhpiPortDeclK =>
               Add_Port (Decl);
            when others =>
               null;
         end case;
      end loop;

      --  Recurse into sub-scopes.
      Vhpi_Iterator (VhpiInternalRegions, Inst, Decl_It, Error);
      if Error /= AvhpiErrorOk then
         Avhpi_Error (Error);
         return;
      end if;

      loop
         Vhpi_Scan (Decl_It, Decl, Error);
         exit when Error = AvhpiErrorIteratorEnd;
         if Error /= AvhpiErrorOk then
            Avhpi_Error (Error);
            return;
         end if;
         case Vhpi_Get_Kind (Decl) is
            when VhpiIfGenerateK
              | VhpiForGenerateK
              | VhpiBlockStmtK
              | VhpiCompInstStmtK =>
               Evcd_Put_Hierarchy (Decl);
            when others =>
               null;
         end case;
      end loop;

      Evcd_Put ("$upscope ");
      Evcd_Put_End;
   end Evcd_Put_Hierarchy;

   procedure Evcd_Put_Time
   is
      Str : String (1 .. 21);
      First : Natural;
   begin
      Evcd_Putc ('#');
      To_Strings.To_String (Str, First, Ghdl_I64 (Current_Time));
      Evcd_Put (Str (First .. Str'Last));
      Evcd_Newline;
   end Evcd_Put_Time;

   procedure Evcd_Cycle;

   --  Called after elaboration: emit definitions and arm the cycle hook.
   procedure Evcd_Start
   is
      Root : VhpiHandleT;
   begin
      if Evcd_Close = null then
         return;
      end if;

      --  Be sure the RTI of std_ulogic is set (Get_Verilog_Wire needs it).
      Search_Types_RTI;

      Get_Root_Inst (Root);
      Evcd_Put_Hierarchy (Root);

      Evcd_Put ("$enddefinitions ");
      Evcd_Put_End;

      Register_Cycle_Hook (Evcd_Cycle'Access);
   end Evcd_Start;

   --  Called before each non-delta cycle.
   procedure Evcd_Cycle
   is
      Time_Displayed : Boolean := False;
   begin
      if Current_Time = 0 then
         --  Initial values, wrapped in a $dumpports block.
         Evcd_Put_Time;
         Last_Time_Written := Current_Time;
         Evcd_Putline ("$dumpports");
         for I in Evcd_Table.First .. Evcd_Table.Last loop
            Evcd_Put_Var (I);
         end loop;
         Evcd_Put_End;
         --  9-state annotations go after the block, never inside it.
         if Ann_9state then
            for I in Evcd_Table.First .. Evcd_Table.Last loop
               Evcd_Put_9 (I);
            end loop;
         end if;
      else
         --  Only changed ports; emit the timestamp lazily.
         for I in Evcd_Table.First .. Evcd_Table.Last loop
            if Verilog_Wire_Changed (Evcd_Table.Table (I).Wire, Current_Time)
            then
               if not Time_Displayed then
                  Evcd_Put_Time;
                  Last_Time_Written := Current_Time;
                  Time_Displayed := True;
               end if;
               Evcd_Put_Var (I);
               if Ann_9state then
                  Evcd_Put_9 (I);
               end if;
            end if;
         end loop;
      end if;
   end Evcd_Cycle;

   --  Called at the end of the simulation.
   procedure Evcd_End is
   begin
      if Evcd_Close = null then
         return;
      end if;
      --  Final timestamp, if it advanced and is meaningful.
      if Current_Time /= Last_Time_Written
        and then Current_Time /= Std_Time'Last
      then
         Evcd_Put_Time;
         Last_Time_Written := Current_Time;
      end if;
      --  Record the closing simulation time for parsers.
      Evcd_Put ("$vcdclose #");
      Evcd_Put_I64 (Ghdl_I64 (Last_Time_Written));
      Evcd_Put (" ");
      Evcd_Put_End;
      Evcd_Close.all;
   end Evcd_End;

   Evcd_Hooks : aliased constant Hooks_Type :=
     (Desc => new String'("evcd: save port waveforms in extended VCD format"),
      Option => Evcd_Option'Access,
      Help => Evcd_Help'Access,
      Init => Evcd_Init'Access,
      Start => Evcd_Start'Access,
      Finish => Evcd_End'Access);

   procedure Register is
   begin
      Register_Hooks (Evcd_Hooks'Access);
   end Register;
end Grt.Evcd;
