
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
-- $Id: tc3101.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c05s01b00x00p11n01i03101ent IS
  ATTRIBUTE attr1 : INTEGER;
END c05s01b00x00p11n01i03101ent;

ARCHITECTURE c05s01b00x00p11n01i03101arch OF c05s01b00x00p11n01i03101ent IS

  FUNCTION   one  ( par1      : INTEGER ) RETURN INTEGER IS BEGIN END;
  FUNCTION   two  ( par1      : INTEGER ) RETURN INTEGER IS BEGIN END;
  FUNCTION   tww  ( par1,par2 : STRING  ) RETURN INTEGER IS BEGIN END;

  ATTRIBUTE attr1 OF all: FUNCTION IS 99;

BEGIN
  TESTING: PROCESS
  BEGIN
    ASSERT  one'attr1 = 99 REPORT "ERROR: Wrong value for  one 'attr1" SEVERITY FAILURE;
    ASSERT  two'attr1 = 99 REPORT "ERROR: Wrong value for  two 'attr1" SEVERITY FAILURE;
    ASSERT  tww'attr1 = 99 REPORT "ERROR: Wrong value for  tww 'attr1" SEVERITY FAILURE;

    assert NOT(    one'attr1    = 99   and   
                   two'attr1    = 99   and
                   tww'attr1    = 99   )
      report "***PASSED TEST: c05s01b00x00p11n01i03101"
      severity NOTE;
    assert (    one'attr1    = 99   and   
                two'attr1    = 99   and
                tww'attr1    = 99   )
      report "***FAILED TEST: c05s01b00x00p11n01i03101 - Reserved word all as attribute specification test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c05s01b00x00p11n01i03101arch;
