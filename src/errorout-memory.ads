--  Store error messages
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

with System;

package Errorout.Memory is
   type Error_Index is new Uns32;

   type Group_Type is (Msg_Single,
                       Msg_Main, Msg_Related, Msg_Last);

   type Error_Message is record
      --  Message error/warning id
      Id : Msgid_Type;

      --  Whether this is an single message or a related one.
      Group : Group_Type;

      --  Error soure file.
      File : Source_File_Entry;

      --  The first line is line 1, 0 can be used when line number is not
      --  relevant.
      Line : Natural;

      --  Offset in the line.  The first character is at offset 0.
      Offset : Natural;

      --  Length of the location (for a range).  It is assumed to be on the
      --  same line; use 0 when unknown.
      Length : Natural;
   end record;

   --  Get number of messages available.
   function Get_Nbr_Messages return Error_Index;

   --  Get messages.
   --  Idx is from 1 to Nbr_Messages.
   function Get_Error_Record (Idx : Error_Index) return Error_Message;
   function Get_Error_Message (Idx : Error_Index) return String;
   function Get_Error_Message_Addr (Idx : Error_Index) return System.Address;

   --  Remove all error messages.
   procedure Clear_Errors;

   --  Install the handlers for reporting errors.
   procedure Install_Handler;
end Errorout.Memory;
