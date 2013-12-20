
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
-- $Id: tc535.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s03b00x00p05n01i00535ent IS
END c03s03b00x00p05n01i00535ent;

ARCHITECTURE c03s03b00x00p05n01i00535arch OF c03s03b00x00p05n01i00535ent IS
  type b is access BIT_VECTOR(0 to 10);   -- Success_here
BEGIN
  TESTING: PROCESS
    variable k : b; 
  BEGIN
    assert NOT(k=null)
      report "***PASSED TEST: c03s03b00x00p05n01i00535"
      severity NOTE;
    assert ( k=null )
      report "***FAILED TEST: c03s03b00x00p05n01i00535 - The only form of constraint that is allowed after the name of an access type in a subtype indication is an index constraint."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p05n01i00535arch;
