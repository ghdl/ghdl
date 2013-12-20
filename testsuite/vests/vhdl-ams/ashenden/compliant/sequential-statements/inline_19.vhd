
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

entity inline_19 is

end entity inline_19;


----------------------------------------------------------------


architecture test of inline_19 is

  subtype data_type is integer;

  signal transmit_data : data_type := 0;

begin


  -- code from book:
  
  transmit_element : process (transmit_data) is
    -- . . .      -- variable declarations
  begin
    report "transmit_element: data = "
        & data_type'image(transmit_data);
    -- . . .
  end process transmit_element;

  -- end of code from book


  stimulus : process is
  begin
    transmit_data <= 10 after 10 ns, 20 after 20 ns;
    wait;
  end process stimulus;


end architecture test;
