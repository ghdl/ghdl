
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

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

-- ---------------------------------------------------------------------
--
-- $Id: ch_03_ch_03_02.vhd,v 1.2 2001-10-24 23:30:59 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_03_02 is
end entity ch_03_02;

architecture test of ch_03_02 is

  signal sel : integer range 0 to 1 := 0;
  signal input_0 : integer := 0;
  signal input_1 : integer := 10;
  signal result : integer;

begin

  process_3_1_b : process (sel, input_0, input_1) is
  begin

    -- code from book:

    if sel = 0 then
      result <= input_0;  -- executed if sel = 0
    else
      result <= input_1;  -- executed if sel /= 0
    end if;

    -- end of code from book

  end process process_3_1_b;

  stimulus : process is
  begin
    sel <= 1 after 40 ns;
    input_0 <= 1 after 10 ns, 2 after 30 ns, 3 after 50 ns;
    input_1 <= 11 after 15 ns, 12 after 35 ns, 13 after 55 ns;
    wait;
  end process stimulus;

end architecture test;
