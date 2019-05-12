--  Store error messages
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

with System;

package Errorout.Memory is
   type Error_Index is new Uns32;

   --  Get number of messages available.
   function Get_Nbr_Messages return Error_Index;

   --  Get messages.
   --  Idx is from 1 to Nbr_Messages.
   function Get_Error_Record (Idx : Error_Index) return Error_Record;
   function Get_Error_Message (Idx : Error_Index) return String;
   function Get_Error_Message_Addr (Idx : Error_Index) return System.Address;

   --  Remove all error messages.
   procedure Clear_Errors;

   --  Install the handlers for reporting errors.
   procedure Install_Handler;
end Errorout.Memory;
