
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

entity inline_09 is

end entity inline_09;


----------------------------------------------------------------


architecture test of inline_09 is
begin


  process_2_d : process is

    -- code from book:

    variable N : integer := 1;

    --

    constant C : integer := 1;

    -- end of code from book

    constant expression : integer := 7;

  begin

    -- code from book:

    -- error: Case choice must be a locally static expression

    -- case expression is         -- example of an illegal case statement
    --   when N | N+1 => -- . . .
    --   when N+2 to N+5 => -- . . .
    --   when others => -- . . .
    -- end case;

    --

    case expression is
      when C | C+1 => -- . . .
      when C+2 to C+5 => -- . . .
      when others => -- . . .
    end case;

    -- end of code from book

    wait;
  end process process_2_d;


end architecture test;
