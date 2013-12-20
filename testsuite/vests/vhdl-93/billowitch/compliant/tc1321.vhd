
-- Copyright (C) 2001 Bill Billowitch.

-- Some of the work to develop this test suite was done with Air Force
-- support.  The Air Force and Bill Billowitch assume no
-- responsibilities for this software.

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
-- $Id: tc1321.vhd,v 1.2 2001-10-26 16:29:40 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s04b00x00p09n03i01321ent IS
END c08s04b00x00p09n03i01321ent;

ARCHITECTURE c08s04b00x00p09n03i01321arch OF c08s04b00x00p09n03i01321ent IS
  signal S1 : BIT := '1';
  signal S2 : BIT := '1';
  signal S  : BIT := '1';
BEGIN
  S1 <= transport '0' after 5  ns,
        '1' after 10 ns;
  S2 <= transport S1  after 15 ns;
  TEST : PROCESS(S2)
    variable k : integer := 0;
  BEGIN
    if ((S2 = '0') and (NOW = 20 ns)) then   
      k := 1;
    end if;
    if ((S2 = '1') and (NOW = 25 ns) and (k = 1)) then   
      S <= '0' after 10 ns;
    end if;
  END PROCESS TEST;

  TESTING: PROCESS(S)
  BEGIN
    if (NOW > 1 ns) then
      assert NOT(S = '0') 
        report "***PASSED TEST: c08s04b00x00p09n03i01321"
        severity NOTE;
      assert (S = '0')
        report "***FAILED TEST: c08s04b00x00p09n03i01321 - Any pulse is transmitted, not matter how short its durtion"
        severity ERROR;
    end if;
  END PROCESS TESTING;

END c08s04b00x00p09n03i01321arch;
