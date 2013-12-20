
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

entity mux is
end entity mux;

architecture test of mux is

  constant prop_delay : time := 5 ns;

  signal a, b, sel, z : bit;

begin

  -- code from book

  mux : process (a, b, sel) is
  begin
    case sel is
      when '0' =>
	z <= a after prop_delay;
      when '1' =>
	z <= b after prop_delay;
    end case;
  end process mux;

  -- end code from book


  stimulus : process is
    subtype stim_vector_type is bit_vector(0 to 3);
    type stim_vector_array is array ( natural range <> ) of stim_vector_type;
    constant stim_vector : stim_vector_array
      := ( "0000",
	   "0010",
           "0100",
	   "0111",
           "1001",
	   "1010",
           "1101",
	   "1111" );
  begin
    for i in stim_vector'range loop
      (a, b, sel) <= stim_vector(i)(0 to 2);
      wait for 10 ns;
      assert z = stim_vector(i)(3);
    end loop;
    wait;
  end process stimulus;


end architecture test;
