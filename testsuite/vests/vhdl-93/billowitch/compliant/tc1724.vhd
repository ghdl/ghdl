
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
-- $Id: tc1724.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s06b01x00p03n02i01724ent IS
END c12s06b01x00p03n02i01724ent;

ARCHITECTURE c12s06b01x00p03n02i01724arch OF c12s06b01x00p03n02i01724ent IS
  type    SWITCH_LEVEL is ( 'X', '0', '1' );
  subtype    LOGIC_SWITCH is SWITCH_LEVEL range '0' to '1';

  -- Global signals.
  SIGNAL B   : LOGIC_SWITCH := '1';
  SIGNAL B2  : LOGIC_SWITCH;
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT( (B='1') and (B2='0') )
      report "***PASSED TEST: c12s06b01x00p03n02i01724"
      severity NOTE;
    assert ( (B='1') and (B2='0') )
      report "***FAILED TEST: c12s06b01x00p03n02i01724 - The initial contents of a driver associated with a given signal is defined by the default value associated with the signal."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s06b01x00p03n02i01724arch;
