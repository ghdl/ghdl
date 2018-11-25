--  Output errors on the console.
--  Copyright (C) 2018 Tristan Gingold
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
