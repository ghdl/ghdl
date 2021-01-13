--  Very simple logging package.
--  Copyright (C) 2018 Tristan Gingold
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

with Simple_IO;

package body Logging is
   procedure Log (S : String) is
   begin
      Simple_IO.Put_Err (S);
   end Log;

   procedure Log_Line (S : String := "") is
   begin
      Simple_IO.Put_Line_Err (S);
   end Log_Line;
end Logging;
