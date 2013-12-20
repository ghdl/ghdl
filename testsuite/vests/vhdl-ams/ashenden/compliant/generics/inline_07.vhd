
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
  generic ( width : positive );
  port ( d : in bit_vector(0 to width - 1);
         q : out bit_vector(0 to width - 1);
         -- . . . );
  -- not in book
         other_port : in bit := '0' );
  -- end not in book
end entity reg;

-- end code from book


architecture test of reg is
begin
  q <= d;
end architecture test;



entity inline_07 is

end entity inline_07;


----------------------------------------------------------------


architecture test of inline_07 is

  constant bus_size : positive := 16;

  -- code from book

  signal in_data, out_data : bit_vector(0 to bus_size - 1);
  -- . . .

  -- end code from book


begin

  -- code from book

  ok_reg : entity work.reg
    generic map ( width => bus_size )
    port map ( d => in_data,  q => out_data, -- . . . );
    -- not in book
               other_port => open );
    -- end not in book

  -- end code from book

end architecture test;
