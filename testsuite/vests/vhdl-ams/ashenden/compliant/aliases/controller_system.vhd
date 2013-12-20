
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

-- not in book

package alu_types is

  constant data_width : positive := 32;

end package alu_types;


package io_types is

  constant data_width : positive := 32;

end package io_types;


entity controller_system is
end entity controller_system;

-- end not in book



library ieee;  use ieee.std_logic_1164.all;
use work.alu_types.all, work.io_types.all;

architecture structural of controller_system is

  alias alu_data_width is work.alu_types.data_width;
  alias io_data_width is work.io_types.data_width;

  signal alu_in1, alu_in2,
         alu_result : std_logic_vector(0 to alu_data_width - 1);
  signal  io_data : std_logic_vector(0 to io_data_width - 1);
  -- . . .

  -- not in book
  -- following should not analyze: data_width not directly visible
  -- constant test : positive := data_width;
  -- end not in book

begin

  -- . . .

end architecture structural;
