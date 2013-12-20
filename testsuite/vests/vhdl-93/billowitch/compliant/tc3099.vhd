
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
-- $Id: tc3099.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c05s01b00x00p09n01i03099ent IS
  ATTRIBUTE attr1 : INTEGER;
END c05s01b00x00p09n01i03099ent;

ARCHITECTURE c05s01b00x00p09n01i03099arch OF c05s01b00x00p09n01i03099ent IS
  SIGNAL s1,s2,s3 : BIT;
  SIGNAL s4,s5    : INTEGER;
  SIGNAL s6,s7    : STRING(1 TO 3);

  ATTRIBUTE attr1 OF s1,s2,s3,s4,s5,s6,s7 : SIGNAL IS 101;
BEGIN
  TESTING: PROCESS
  BEGIN
    ASSERT s1'attr1 = 101 REPORT "Bad value for s1'attr1" SEVERITY FAILURE;
    ASSERT s2'attr1 = 101 REPORT "Bad value for s2'attr1" SEVERITY FAILURE;
    ASSERT s3'attr1 = 101 REPORT "Bad value for s3'attr1" SEVERITY FAILURE;
    ASSERT s4'attr1 = 101 REPORT "Bad value for s4'attr1" SEVERITY FAILURE;
    ASSERT s5'attr1 = 101 REPORT "Bad value for s5'attr1" SEVERITY FAILURE;
    ASSERT s6'attr1 = 101 REPORT "Bad value for s6'attr1" SEVERITY FAILURE;
    ASSERT s7'attr1 = 101 REPORT "Bad value for s7'attr1" SEVERITY FAILURE;
    assert NOT(    s1'attr1 = 101   and
                   s2'attr1 = 101   and
                   s3'attr1 = 101   and
                   s4'attr1 = 101   and
                   s5'attr1 = 101   and
                   s6'attr1 = 101   and
                   s7'attr1 = 101   )
      report "***PASSED TEST: c05s01b00x00p09n01i03099"
      severity NOTE;
    assert (    s1'attr1 = 101   and
                s2'attr1 = 101   and
                s3'attr1 = 101   and
                s4'attr1 = 101   and
                s5'attr1 = 101   and
                s6'attr1 = 101   and
                s7'attr1 = 101   )
      report "***FAILED TEST: c05s01b00x00p09n01i03099 - Attribute specification applies to the entity designators list test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c05s01b00x00p09n01i03099arch;
