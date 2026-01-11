--  GHDL Run Time (GRT) - Constants definition
--  Copyright (C) 2020 Tristan Gingold
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

with Grt.Types; use Grt.Types;

package Grt.Severity is
   pragma Preelaborate (Grt.Severity);

   Note_Severity    : constant Ghdl_E8 := 0;
   Warning_Severity : constant Ghdl_E8 := 1;
   Error_Severity   : constant Ghdl_E8 := 2;
   Failure_Severity : constant Ghdl_E8 := 3;

   subtype Severity_Level is Ghdl_E8 range Note_Severity .. Failure_Severity;

   --  Value returned by Parse_Severity for 'none'.
   None_Severity    : constant Ghdl_E8 := 4;
end Grt.Severity;
