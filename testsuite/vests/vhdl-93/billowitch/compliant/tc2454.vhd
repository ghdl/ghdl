
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
-- $Id: tc2454.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c07s03b02x02p03n02i02454pkg is
  type    UN_ARR    is array (integer range <>) of character;
  subtype CON_ARR is UN_ARR( 1 to 5) ;
end c07s03b02x02p03n02i02454pkg;

use work.c07s03b02x02p03n02i02454pkg.all;

ENTITY c07s03b02x02p03n02i02454ent IS
  port     (P : in CON_ARR := (others => 'A'));  --- No_failure_here
END c07s03b02x02p03n02i02454ent;

ARCHITECTURE c07s03b02x02p03n02i02454arch OF c07s03b02x02p03n02i02454ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(P(1)='A' and P(2)='A' and P(3)='A' and P(4)='A' and P(5)='A')
      report "***PASSED TEST: c07s03b02x02p03n02i02454"
      severity NOTE;
    assert (P(1)='A' and P(2)='A' and P(3)='A' and P(4)='A' and P(5)='A')
      report "***FAILED TEST: c07s03b02x02p03n02i02454 - As the default expression defining the default initial value of a port declared to be of a constrained array subtype."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p03n02i02454arch;
