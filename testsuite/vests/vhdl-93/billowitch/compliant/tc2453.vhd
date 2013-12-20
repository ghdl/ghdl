
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
-- $Id: tc2453.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p03n02i02453ent IS
END c07s03b02x02p03n02i02453ent;

ARCHITECTURE c07s03b02x02p03n02i02453arch OF c07s03b02x02p03n02i02453ent IS

BEGIN
  TESTING: PROCESS
    type    UNCONSTRAINED_ARRAY is array ( integer range <> ) of character;
    subtype CONSTRAINED_ARRAY   is UNCONSTRAINED_ARRAY ( 1 to 5 );

    function F (A:CONSTRAINED_ARRAY) return CONSTRAINED_ARRAY is
    begin
      return A;
    end F;
    function F2 return CONSTRAINED_ARRAY is
    begin
      return F( ( 'a','b',others => 'c' ) );
      -- sole "others" choice is legal.
    end F2;
    variable k : CONSTRAINED_ARRAY;
  BEGIN
    k := F2;   
    assert NOT(k="abccc") 
      report "***PASSED TEST: c07s03b02x02p03n02i02453" 
      severity NOTE;
    assert (k="abccc") 
      report "***FAILED TEST: c07s03b02x02p03n02i02453 - Others is used in an aggregate which corresponds to an unconstrained formal parameter."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p03n02i02453arch;
