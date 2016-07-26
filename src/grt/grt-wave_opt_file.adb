--  GHDL Run Time (GRT) - Wave option file top package.
--  Copyright (C) 2016 Jonas Baggett
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

-- Description: See package specifications

with Grt.Types; use Grt.Types;
with Grt.Vstrings; use Grt.Vstrings;
with Grt.Errors; use Grt.Errors;

package body Grt.Wave_Opt_File is

   procedure Print_Context
     (Line_Pos, Column_Pos : Positive; Severity : Severity_Type)
   is
      Value_Str : String (1 .. Value_String_Size);
      First : Natural;
   begin
      case Severity is
         when Error =>
            Error_C ("");
         when Warning =>
            Report_C ("warning: ");
      end case;
      Report_C (File_Path.all);
      Report_C (":");
      To_String (Value_Str, First, Ghdl_I32 (Line_Pos));
      Report_C (Value_Str (First .. Value_Str'Last));
      Report_C (":");
      To_String (Value_Str, First, Ghdl_I32 (Column_Pos));
      Report_C (Value_Str (First .. Value_Str'Last));
      Report_C (": ");
   end Print_Context;

   procedure Print_Context (Element : Elem_Acc; Severity : Severity_Type) is
   begin
      Print_Context
        (Element.Path_Context.Line_Pos, Element.Column_Pos, Severity);
   end Print_Context;

   procedure Error_Context (Msg : String;
                            Line_Pos, Column_Pos : Positive;
                            Severity : Severity_Type := Error) is
   begin
      Print_Context (Line_Pos, Column_Pos, Severity);
      case Severity is
         when Error =>
            Error_E (Msg);
         when Warning =>
            Report_E (Msg);
      end case;
   end Error_Context;

   procedure Error_Context
     (Msg : String; Element : Elem_Acc; Severity : Severity_Type := Error) is
   begin
      Error_Context
        (Msg, Element.Path_Context.Line_Pos, Element.Column_Pos, Severity);
   end Error_Context;

end Grt.Wave_Opt_File;
