
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
-- $Id: tc2458.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p03n02i02458ent IS
END c07s03b02x02p03n02i02458ent;

ARCHITECTURE c07s03b02x02p03n02i02458arch OF c07s03b02x02p03n02i02458ent IS

BEGIN
  TESTING: PROCESS
    type    UNCONSTRAINED_ARRAY is array ( integer range <> ) of character;
    subtype CONSTRAINED_ARRAY is UNCONSTRAINED_ARRAY ( 1 to 3 );
    variable V : CONSTRAINED_ARRAY ;
    -- check in declaration of constrained array variable.
  BEGIN

    V := ( 'd','x',others => '$' );
    -- check in variable assignment to constrained array object.
    wait for 5 ns;
    assert NOT( V(1)='d' and V(2)='x' and V(3)='$' )
      report "***PASSED TEST: c07s03b02x02p03n02i02458"
      severity NOTE;
    assert ( V(1)='d' and V(2)='x' and V(3)='$' )
      report "***FAILED TEST: c07s03b02x02p03n02i02458 - An array aggregate with an others choice may appear as a value expression in an assignment statement."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p03n02i02458arch;
