
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
-- $Id: ch_04_ch_04_08.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_04_08 is

end entity ch_04_08;


----------------------------------------------------------------


architecture test of ch_04_08 is
begin


  process_04_3_b : process is

                             -- code from book:

                             type array1 is array (1 to 100) of integer;
                           type array2 is array (100 downto 1) of integer;

                           variable a1 : array1;
                           variable a2 : array2;

                           -- end of code from book

  begin

    a1(11 to 20) := a1(11 to 20);
    a2(50 downto 41) := a2(50 downto 41);

    a1(10 to 1) := a1(10 to 1);
    a2(1 downto 10) := a2(1 downto 10);

    a1(10 downto 1) := a1(10 downto 1);  -- illegal
    a2(1 to 10) := a2(1 to 10);  -- illegal;

    wait;
  end process process_04_3_b;


end architecture test;
