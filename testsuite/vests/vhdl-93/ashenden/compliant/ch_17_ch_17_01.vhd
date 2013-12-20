
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
-- $Id: ch_17_ch_17_01.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_17_01 is

end entity ch_17_01;


----------------------------------------------------------------


architecture test of ch_17_01 is
begin


  process is

            -- code from book:

            type natural_ptr is access natural;

          variable count : natural_ptr;

          -- end of code from book

  begin

    -- code from book:

    count := new natural;

    count.all := 10;

    if count.all = 0 then
      -- . . .
      -- not in book
      report "count.all = 0";
      -- end not in book
    end if;

    -- end of code from book

    if count.all /= 0 then
      report "count.all /= 0";
    end if;

    -- code from book:

    count := new natural'(10);

    -- end of code from book

    wait;
  end process;


end architecture test;
