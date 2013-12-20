
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
-- $Id: ch_05_ch_05_10.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_05_10 is

end entity ch_05_10;


----------------------------------------------------------------


architecture test of ch_05_10 is

  signal data : bit_vector(7 downto 0) := X"FF";
  signal s : bit := '0';

begin


  process_05_3_l : process is
  begin
    wait for 10 ns;

    -- code from book:

    data <= X"00";

    -- end of code from book

    wait for 10 ns;

    -- code from book:

    s <= '1';
    -- . . .
    if s = '1' then -- . . .
      -- not in book
      report "s is '1'";
    else
      report "s is '0'";
    end if;
    -- end not in boook

    -- end of code from book

    wait;
  end process process_05_3_l;


end architecture test;
