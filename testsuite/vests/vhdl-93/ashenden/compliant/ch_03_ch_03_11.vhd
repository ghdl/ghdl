
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
-- $Id: ch_03_ch_03_11.vhd,v 1.2 2001-10-24 23:30:59 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_03_11 is
end entity ch_03_11;

architecture test of ch_03_11 is

  signal sensitivity_list : bit := '0';

begin

  -- code from book:

  -- make "sensitivity_list" roman italic
  control_section : process ( sensitivity_list ) is
  begin
    null;
  end process control_section;

  -- end of code from book

  stimulus : process is
  begin
    sensitivity_list <= '1' after 10 ns, '0' after 20 ns;
    wait;
  end process stimulus;

end architecture test;

