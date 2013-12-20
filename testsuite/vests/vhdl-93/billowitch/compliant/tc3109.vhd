
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
-- $Id: tc3109.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c05s01b00x00p17n01i03109pkg is
  attribute p   : POSITIVE;
  attribute p    of c05s01b00x00p17n01i03109pkg : package is 10; --- No_Failure_here
end c05s01b00x00p17n01i03109pkg;


use work.c05s01b00x00p17n01i03109pkg.all;
ENTITY c05s01b00x00p17n01i03109ent IS
  attribute p    of c05s01b00x00p17n01i03109ent : entity is 20;   -- No_Failure_here
END c05s01b00x00p17n01i03109ent;

ARCHITECTURE c05s01b00x00p17n01i03109arch OF c05s01b00x00p17n01i03109ent IS
  attribute p    of c05s01b00x00p17n01i03109arch : architecture is 30;  -- No_Failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(   c05s01b00x00p17n01i03109ent'p   = 20   and
                  c05s01b00x00p17n01i03109arch'p = 30   )
      report "***PASSED TEST: c05s01b00x00p17n01i03109" 
      severity NOTE;
    assert (   c05s01b00x00p17n01i03109ent'p   = 20   and
               c05s01b00x00p17n01i03109arch'p = 30   )
      report "***FAILED TEST: c05s01b00x00p17n01i03109 - Attribute specification for an attribute of a design unit test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c05s01b00x00p17n01i03109arch;
