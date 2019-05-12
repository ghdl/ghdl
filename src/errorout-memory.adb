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

with Tables;

package body Errorout.Memory is

   type Char_Index is new Uns32;

   package Messages is new Tables
     (Table_Component_Type => Character,
      Table_Index_Type => Char_Index,
      Table_Low_Bound => 1,
      Table_Initial => 128);

   type Error_Element is record
      Header : Error_Record;
      Str : Char_Index;
   end record;

   package Errors is new Tables
     (Table_Component_Type => Error_Element,
      Table_Index_Type => Error_Index,
      Table_Low_Bound => 1,
      Table_Initial => 32);

   function Get_Nbr_Messages return Error_Index is
   begin
      return Errors.Last;
   end Get_Nbr_Messages;

   function Get_Error_Record (Idx : Error_Index) return Error_Record is
   begin
      return Errors.Table (Idx).Header;
   end Get_Error_Record;

   function Get_Error_Message (Idx : Error_Index) return String
   is
      First : constant Char_Index := Errors.Table (Idx).Str;
      Last : Char_Index;
   begin
      if Idx = Errors.Last then
         Last := Messages.Last;
      else
         Last := Errors.Table (Idx + 1).Str - 1;
      end if;
      return String (Messages.Table (First .. Last - 1));
   end Get_Error_Message;

   function Get_Error_Message_Addr (Idx : Error_Index) return System.Address
   is
      First : constant Char_Index := Errors.Table (Idx).Str;
   begin
      return Messages.Table (First)'Address;
   end Get_Error_Message_Addr;

   procedure Clear_Errors is
   begin
      Errors.Init;
      Messages.Init;
      Nbr_Errors := 0;
   end Clear_Errors;

   procedure Memory_Error_Start (E : Error_Record) is
   begin
      Errors.Append ((E, Messages.Last + 1));
   end Memory_Error_Start;

   procedure Memory_Message (Str : String) is
   begin
      for I in Str'Range loop
         Messages.Append (Str (I));
      end loop;
   end Memory_Message;

   procedure Memory_Message_End is
   begin
      Messages.Append (ASCII.NUL);
   end Memory_Message_End;

   procedure Install_Handler is
   begin
      Set_Report_Handler ((Memory_Error_Start'Access,
                           Memory_Message'Access,
                           Memory_Message_End'Access));
   end Install_Handler;

end Errorout.Memory;
