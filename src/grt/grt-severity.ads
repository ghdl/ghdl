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

package Grt.Severity is
   pragma Pure (Grt.Severity);

   Note_Severity    : constant Integer := 0;
   Warning_Severity : constant Integer := 1;
   Error_Severity   : constant Integer := 2;
   Failure_Severity : constant Integer := 3;

   --  Value returned by Parse_Severity for 'none'.
   None_Severity    : constant Integer := 4;
end Grt.Severity;
