
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
-- $Id: tc3082.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s06b02x00p02n01i03082ent IS
END c12s06b02x00p02n01i03082ent;

ARCHITECTURE c12s06b02x00p02n01i03082arch OF c12s06b02x00p02n01i03082ent IS
  -- This signal will have its 'ACTIVE flag monitored.
  signal MONITOR : BIT := '0';

  -- This signal will be used to check MONITOR'ACTIVE whenever we want to verify that 
  -- its value is OK.
  signal CHECK : BIT := '0';
BEGIN

  TESTING: PROCESS
    variable   testOK   : integer := 0;
  BEGIN
    -- Perform a signal value change on both signals.
    MONITOR <= not MONITOR after 10 ns;
    CHECK   <= not CHECK   after 10 ns;
    wait on CHECK;

    -- Verify that the flags say what we want.
    assert( not( MONITOR'STABLE ) );
    if (MONITOR'STABLE) then
      testOK := 1;
    end if;
    assert( MONITOR'EVENT );
    if (not(MONITOR'EVENT)) then
      testOK := 1;
    end if;
    assert( MONITOR'ACTIVE );
    if (not(MONITOR'ACTIVE)) then
      testOK := 1;
    end if;
    assert( not( MONITOR'QUIET ) );
    if (MONITOR'QUIET) then
      testOK := 1;
    end if;

    -- Perform no signal value change on MONITOR.
    MONITOR <=     MONITOR after 10 ns;
    CHECK   <= not CHECK   after 10 ns;
    wait on CHECK;
    
    -- Verify that the flags say what we want.
    assert( MONITOR'STABLE );
    if (not(MONITOR'STABLE)) then
      testOK := 1;
    end if;
    assert( not( MONITOR'EVENT ) );
    if (MONITOR'EVENT) then
      testOK := 1;
    end if;
    assert( MONITOR'ACTIVE );
    if (not(MONITOR'ACTIVE)) then
      testOK := 1;
    end if;
    assert( not( MONITOR'QUIET ) );
    if (MONITOR'QUIET) then
      testOK := 1;
    end if;
    
    -- Perform no activity at all on MONITOR.
    CHECK   <= not CHECK   after 10 ns;
    wait on CHECK;
    
    -- Verify that the flags say what we want.
    assert( MONITOR'STABLE );
    if (not(MONITOR'STABLE)) then
      testOK := 1;
    end if;
    assert( not( MONITOR'EVENT ) );
    if (MONITOR'EVENT) then
      testOK := 1;
    end if;
    assert( not( MONITOR'ACTIVE ) );
    if (MONITOR'ACTIVE) then
      testOK := 1;
    end if;
    assert( MONITOR'QUIET  );
    if (not(MONITOR'QUIET)) then
      testOK := 1;
    end if;
    
    assert NOT( testOK = 0 )
      report "***PASSED TEST: c12s06b02x00p02n01i03082"
      severity NOTE;
    assert ( testOK = 0 )
      report "***FAILED TEST: c12s06b02x00p02n01i03082 - A signal should be active if one of its sources is active."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s06b02x00p02n01i03082arch;
