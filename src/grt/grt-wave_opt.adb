--  GHDL Run Time (GRT) - Wave option file top package.
--  Copyright (C) 2016 Jonas Baggett
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

-- Description: See package specifications

with Grt.Errors; use Grt.Errors;

package body Grt.Wave_Opt is

   procedure Diag_C_Context (Lineno, Column : Positive) is
   begin
      Diag_C (File_Path.all);
      Diag_C (':');
      Diag_C (Lineno);
      Diag_C (':');
      Diag_C (Column);
      Diag_C (": ");
   end Diag_C_Context;

   procedure Diag_C_Context (Element : Elem_Acc) is
   begin
      Diag_C_Context (Element.Lineno, Element.Column);
   end Diag_C_Context;

   procedure Error_Context (Msg : String;
                            Lineno, Column : Positive) is
   begin
      Error_S;
      Diag_C_Context (Lineno, Column);
      Diag_C (Msg);
      Error_E;
   end Error_Context;

   procedure Error_Context (Msg : String; Element : Elem_Acc) is
   begin
      Error_Context (Msg, Element.Lineno, Element.Column);
   end Error_Context;

end Grt.Wave_Opt;
