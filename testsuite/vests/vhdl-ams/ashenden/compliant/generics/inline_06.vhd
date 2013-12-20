
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

-- code from book

entity reg is
  port ( d : in bit_vector;  q : out bit_vector;  -- . . . );
  -- not in book
         other_port : in bit := '0' );
  -- end not in book
end entity reg;

-- end code from book


architecture test of reg is
begin
  q <= d;
end architecture test;



entity inline_06 is

end entity inline_06;


----------------------------------------------------------------


architecture test of inline_06 is

  -- code from book

  signal small_data : bit_vector(0 to 7);
  signal large_data : bit_vector(0 to 15);
  -- . . .

  -- end code from book


begin

  -- code from book

  problem_reg : entity work.reg
    port map ( d => small_data,  q => large_data, -- . . . );
    -- not in book
               other_port => open );
    -- end not in book

  -- end code from book

end architecture test;
