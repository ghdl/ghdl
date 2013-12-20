
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

entity full_adder is
  port ( a, b, c_in : bit;  s, c_out : out bit );
end entity full_adder;


architecture truth_table of full_adder is
begin

  with bit_vector'(a, b, c_in) select
    (c_out, s) <= bit_vector'("00") when "000",
                  bit_vector'("01") when "001",
                  bit_vector'("01") when "010",
                  bit_vector'("10") when "011",
                  bit_vector'("01") when "100",
                  bit_vector'("10") when "101",
                  bit_vector'("10") when "110",
                  bit_vector'("11") when "111";

end architecture truth_table;
