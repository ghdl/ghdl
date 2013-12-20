
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
-- $Id: tc3063.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s03b02x02p01n01i03063ent IS
  ATTRIBUTE attr1 : INTEGER;
END c12s03b02x02p01n01i03063ent;

ARCHITECTURE c12s03b02x02p01n01i03063arch OF c12s03b02x02p01n01i03063ent IS
  
  FUNCTION f1 ( i : INTEGER ) RETURN INTEGER IS
  BEGIN  
    RETURN i+1;  
  END;
  FUNCTION f2 ( i : INTEGER ) RETURN INTEGER IS
  BEGIN  
    RETURN i+2;  
  END;
  
  ATTRIBUTE attr1 OF f1,f2 : FUNCTION IS f1(f2(1));
  
BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT( f1'attr1 = 4 and f2'attr1 = 4 )
      report "***PASSED TEST: c12s03b02x02p01n01i03063"
      severity NOTE;
    assert ( f1'attr1 = 4 and f2'attr1 = 4 )
      report "***FAILED TEST: c12s03b02x02p01n01i03063 - Elaboration of an attribute test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s03b02x02p01n01i03063arch;
