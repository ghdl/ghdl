--  Generic package to handle chains.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
package body Iir_Chain_Handling is
   procedure Build_Init (Last : out Iir) is
   begin
      Last := Null_Iir;
   end Build_Init;

   procedure Build_Init (Last : out Iir; Parent : Iir)
   is
      El : Iir;
   begin
      El := Get_Chain_Start (Parent);
      if El /= Null_Iir then
         loop
            Last := El;
            El := Get_Chain (El);
            exit when El = Null_Iir;
         end loop;
      else
         Last := Null_Iir;
      end if;
   end Build_Init;

   procedure Append (Last : in out Iir; Parent : Iir; El : Iir) is
   begin
      if Last = Null_Iir then
         Set_Chain_Start (Parent, El);
      else
         Set_Chain (Last, El);
      end if;
      Last := El;
   end Append;

   procedure Append_Subchain (Last : in out Iir; Parent : Iir; Els : Iir)
   is
      El : Iir;
   begin
      if Last = Null_Iir then
         Set_Chain_Start (Parent, Els);
      else
         Set_Chain (Last, Els);
      end if;
      El := Els;
      loop
         Set_Parent (El, Parent);
         Last := El;
         El := Get_Chain (El);
         exit when El = Null_Iir;
      end loop;
   end Append_Subchain;
end Iir_Chain_Handling;

