
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

entity flipflop is
  generic ( Tsetup : delay_length );
  port ( clk, d : in bit;  q : out bit );
end entity flipflop;


-- code from book

architecture behavior of flipflop is
begin

  timing_check : process (clk) is
  begin
    if clk = '1' then
      assert d'last_event >= Tsetup
        report "set up violation detected in " & timing_check'path_name
        severity error;
    end if;
  end process timing_check;

  -- . . .    -- functionality

end architecture behavior;

-- end code from book
