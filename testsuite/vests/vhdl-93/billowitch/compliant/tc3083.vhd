
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
-- $Id: tc3083.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s06b03x00p02n01i03083ent IS
END c12s06b03x00p02n01i03083ent;

ARCHITECTURE c12s06b03x00p02n01i03083arch OF c12s06b03x00p02n01i03083ent IS
  signal S1 : BIT;
  signal X1 : BIT;
  signal S  : integer := 1;
BEGIN

  S1 <= transport '1' after 5 ns;

  A : block(X1 = '1')
  begin
    process(GUARD)
    begin
      if GUARD then
        assert false
          report "Failure on test. Guard value shouldn't have been changed" ;
        S <= 0;
      end if;
    end process;
  end block A;

  TESTING: PROCESS
  BEGIN
    wait for 10 ns;
    assert NOT(S = 1)
      report "***PASSED TEST: c12s06b03x00p02n01i03083" 
      severity NOTE;
    assert (S = 1)
      report "***FAILED TEST: c12s06b03x00p02n01i03083 - GUARD signal is not modified in the test." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s06b03x00p02n01i03083arch;
