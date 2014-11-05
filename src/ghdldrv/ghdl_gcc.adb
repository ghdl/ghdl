--  GHDL driver for gcc.
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Ghdlmain;
with Ghdllocal;
with Ghdldrv;
with Ghdlprint;

procedure Ghdl_Gcc is
begin
   --  Manual elaboration so that the order is known (because it is the order
   --  used to display help).
   Ghdlmain.Version_String := new String'("GCC back-end code generator");
   Ghdldrv.Compile_Kind := Ghdldrv.Compile_Gcc;
   Ghdldrv.Register_Commands;
   Ghdllocal.Register_Commands;
   Ghdlprint.Register_Commands;
   Ghdlmain.Register_Commands;
   Ghdlmain.Main;
end Ghdl_Gcc;
