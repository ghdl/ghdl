--  Global flags.
--  Copyright (C) 2002, 2003, 2004, 2005, 2008 Tristan Gingold
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

package body Flags is
   procedure Create_Flag_String is
   begin
      case Vhdl_Std is
         when Vhdl_87 =>
            Flag_String (1 .. 2) := "87";
         when Vhdl_93c
           | Vhdl_93
           | Vhdl_00
           | Vhdl_02 =>
            Flag_String (1 .. 2) := "93";
         when Vhdl_08 =>
            Flag_String (1 .. 2) := "08";
      end case;
      if Flag_Integer_64 then
         Flag_String (3) := 'I';
      else
         Flag_String (3) := 'i';
      end if;
      if Flag_Time_64 then
         Flag_String (4) := 'T';
      else
         Flag_String (4) := 't';
      end if;

      --  Time_Resolution is always fs, maybe overwritten later.
      Flag_String (5) := '-';
   end Create_Flag_String;
end Flags;
