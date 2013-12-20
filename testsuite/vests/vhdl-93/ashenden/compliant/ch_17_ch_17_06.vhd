
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
-- $Id: ch_17_ch_17_06.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_17_06 is

end entity ch_17_06;


----------------------------------------------------------------


architecture test of ch_17_06 is
begin


  process is

            -- code from book:

            type value_cell is record
                                 value : bit_vector(0 to 3);
                                 next_cell : value_ptr;
                               end record value_cell;

          type value_ptr is access value_cell;

          -- end of code from book

  begin

    wait;
  end process;


end architecture test;
