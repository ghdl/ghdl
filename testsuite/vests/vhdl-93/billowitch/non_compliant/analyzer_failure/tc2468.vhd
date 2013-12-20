
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
-- $Id: tc2468.vhd,v 1.2 2001-10-26 16:30:19 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p03n02i02468ent IS
END c07s03b02x02p03n02i02468ent;

ARCHITECTURE c07s03b02x02p03n02i02468arch OF c07s03b02x02p03n02i02468ent IS

BEGIN
  TESTING: PROCESS
    type    A_ARRAY is array ( integer range <> ) of integer;
    SUBTYPE A_CON   IS A_ARRAY ( 1 to 4 );

    function F_A ( PAR : A_CON ) return A_CON is
    begin
      return (1,2,3,4);
    end F_A;

    variable V_A : A_CON ;
  BEGIN
    V_A := F_A( A_ARRAY'(1,2,others=>3) ); -- Failure_here
    -- SEMANTIC ERROR:  "others" used in aggregate in qualified expression
    --      whose type mark denotes an unconstrained array type.
    assert FALSE 
      report "***FAILED TEST: c07s03b02x02p03n02i02468 - Others cannot be used with an unconstrained array type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p03n02i02468arch;
