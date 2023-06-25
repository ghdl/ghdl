--  Verilog driver for synthesis
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.

package Ghdlverilog is
   --  Load a verilog file.
   procedure Load_Verilog_File (Filename : String);

   --  Export all loaded verilog units to vhdl.
   procedure Export_Verilog_Units;

   --  Export all vhdl entities in work library to verilog.
   procedure Export_Vhdl_Units;

   procedure Register_Commands;
end Ghdlverilog;
