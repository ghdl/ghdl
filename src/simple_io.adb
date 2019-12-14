--  Very simple IO package
--  Copyright (C) 2019 Tristan Gingold
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

with Ada.Text_IO;

package body Simple_IO is
   procedure Put (S : String) is
   begin
      Ada.Text_IO.Put (S);
   end Put;

   procedure Put (C : Character) is
   begin
      Ada.Text_IO.Put (C);
   end Put;

   procedure Put_Line (S : String) is
   begin
      Ada.Text_IO.Put_Line (S);
   end Put_Line;

   procedure New_Line is
   begin
      Ada.Text_IO.New_Line;
   end New_Line;

   procedure Put_Err (S : String) is
   begin
      Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, S);
   end Put_Err;

   procedure Put_Err (C : Character) is
   begin
      Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, C);
   end Put_Err;

   procedure Put_Line_Err (S : String) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, S);
   end Put_Line_Err;

   procedure New_Line_Err is
   begin
      Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
   end New_Line_Err;
end Simple_IO;
