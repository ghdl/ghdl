
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

entity inline_01 is
end entity inline_01;


architecture test of inline_01 is

  component computer_system is
    port ( other_port : in bit := '0' );
  end component computer_system;

begin

  system_under_test : component computer_system
    port map ( other_port => open );

end architecture test;



configuration inline_01_test of inline_01 is

  for test

    -- code from book (in text)

    for system_under_test : computer_system
      use entity work.computer_system(block_level)
      generic map ( instrumented => true )
      -- . . .
      -- not in book
      ;
      -- end not in book
    end for;

    -- end code from book

  end for;

end configuration inline_01_test;
