
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
-- $Id: ch_21_ch_21_02.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_21_02 is

end entity ch_21_02;


----------------------------------------------------------------


architecture test of ch_21_02 is

  signal s : bit;

begin

  -- code from book:

  p : postponed process is
                          -- . . .
                begin
                  -- . . .
                  wait until s = '1';
                  -- . . .       -- s may not be '1'!!
                  -- not in book
                  report bit'image(s);
                  wait;
                  -- end not in book
                end postponed process p;

                                      -- end of code from book

                                      stimulus : process is
                                      begin
                                        wait for 10 ns;
                                        s <= '1';
                                        wait for 0 ns;
                                        s <= '0';
                                        wait;
                                      end process stimulus;

                              end architecture test;
