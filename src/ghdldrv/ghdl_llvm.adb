--  GHDL driver for mcode/jit.
--  Copyright (C) 2002-2016 Tristan Gingold
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
with Ghdlmain;
with Ghdllocal;
with Ghdlprint;
with Ghdldrv;
with Ghdlvpi;
with Ghdlxml;
with Ghdllib;
with Ghdlsynth_Maybe;

procedure Ghdl_Llvm is
begin
   --  Manual elaboration so that the order is known (because it is the order
   --  used to display help).
   Ghdlmain.Version_String := new String'("llvm code generator");
   Ghdldrv.Backend := Ghdldrv.Backend_Llvm;
   Ghdldrv.Register_Commands;
   Ghdlsynth_Maybe.Register_Commands;
   Ghdllocal.Register_Commands;
   Ghdlprint.Register_Commands;
   Ghdlvpi.Register_Commands;
   Ghdlxml.Register_Commands;
   Ghdllib.Register_Commands;
   Ghdlmain.Register_Commands;
   Ghdlmain.Main;
end Ghdl_Llvm;
