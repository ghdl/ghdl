--  Output errors on the console.
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

package Errorout.Console is
   --  Set the program name, used in error messages for options.  Not displayed
   --  if not initialized.
   procedure Set_Program_Name (Name : String);

   --  Report handle for the console.
   procedure Console_Error_Start (E : Error_Record);
   procedure Console_Message (Str : String);
   procedure Console_Message_End;

   --  Install the handlers for reporting errors.
   procedure Install_Handler;
end Errorout.Console;
