
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
-- $Id: tc3062.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s03b02x01p07n01i03062ent IS
  function setatt return integer is
  begin
    return 5;
  end;
END c12s03b02x01p07n01i03062ent;

ARCHITECTURE c12s03b02x01p07n01i03062arch OF c12s03b02x01p07n01i03062ent IS
  signal    sa    :bit;
  attribute    ai   :integer;
  attribute    ai of sa:signal is setatt;
BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert (sa'ai = setatt)
      report "Attribute AI of signal SA was not set to the correct non-static value"
      severity failure;
    assert NOT( sa'ai = setatt )
      report "***PASSED TEST: c12s03b02x01p07n01i03062"
      severity NOTE;
    assert ( sa'ai = setatt )
      report "***FAILED TEST: c12s03b02x01p07n01i03062 - Non-static expression as an attribute specification test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s03b02x01p07n01i03062arch;
