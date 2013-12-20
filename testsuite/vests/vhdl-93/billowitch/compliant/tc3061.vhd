
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
-- $Id: tc3061.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s03b02x01p06n02i03061ent IS
  SUBTYPE    s10 IS STRING (10 DOWNTO 1);
  ATTRIBUTE    attr1 : s10;
END c12s03b02x01p06n02i03061ent;

ARCHITECTURE c12s03b02x01p06n02i03061arch OF c12s03b02x01p06n02i03061ent IS
  
  FUNCTION f1 ( i : INTEGER ) RETURN INTEGER IS
  BEGIN  RETURN i+1;  END;
  FUNCTION f2 ( i : INTEGER ) RETURN INTEGER IS
  BEGIN  RETURN i+2;  END;
  
  ATTRIBUTE attr1 OF f1,f2 : FUNCTION IS "ABCDEFGHIJ";
  
BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT(    f1'attr1 = "ABCDEFGHIJ"   and
                   f2'attr1 = "ABCDEFGHIJ"   and
                   f1'attr1( 1) = 'J'   and
                   f2'attr1(10) = 'A'   )
      report "***PASSED TEST: c12s03b02x01p06n02i03061"
      severity NOTE;
    assert (    f1'attr1 = "ABCDEFGHIJ"   and
                f2'attr1 = "ABCDEFGHIJ"   and
                f1'attr1( 1) = 'J'   and
                f2'attr1(10) = 'A'   )
      report "***FAILED TEST: c12s03b02x01p06n02i03061 - An attribute of a constrained array type, an implicit sutype conversion is first applied as for an assignment statement test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s03b02x01p06n02i03061arch;
