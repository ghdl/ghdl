
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
-- $Id: tc1985.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b02x00p02n01i01985ent IS
END c07s02b02x00p02n01i01985ent;

ARCHITECTURE c07s02b02x00p02n01i01985arch OF c07s02b02x00p02n01i01985ent IS

BEGIN
  TESTING: PROCESS
    constant meg       : integer := 1000000;
    variable bigpos    : integer := 2000 * meg;
    variable bigneg    : integer := -2000 * meg;
    variable smallpos    : integer := 2000;
    variable smallneg    : integer := -2000;
    variable zero       : integer := 0;
  BEGIN

    assert NOT(    ( bigneg < smallneg)   and
                   ( bigneg < zero)   and
                   ( bigneg < smallpos)   and
                   ( bigneg < bigpos)   and
                   ( smallneg < zero)   and
                   ( smallneg < smallpos)   and
                   ( smallneg < bigpos)   and
                   ( zero < smallpos)   and
                   ( zero < bigpos)   and
                   ( smallpos < bigpos)   and
                   ( not(bigneg >= smallneg))   and
                   ( not(bigneg >= zero))      and
                   ( not(bigneg >= smallpos))   and
                   ( not(bigneg >= bigpos))   and
                   ( not(smallneg >= zero))   and
                   ( not(smallneg >= smallpos))   and
                   ( not(smallneg >= bigpos))   and
                   ( not(zero >= smallpos))   and
                   ( not(zero >= bigpos))      and
                   ( not(smallpos >= bigpos))   and
                   ( bigneg <= smallneg)   and
                   ( bigneg <= zero)   and
                   ( bigneg <= smallpos)   and
                   ( bigneg <= bigpos)   and
                   ( smallneg <= zero)   and
                   ( smallneg <= smallpos)   and
                   ( smallneg <= bigpos)   and
                   ( zero <= smallpos)   and
                   ( zero <= bigpos)   and
                   ( smallpos <= bigpos)   and
                   ( bigneg <= bigneg)   and
                   ( smallneg <= smallneg)   and
                   ( zero <=  zero)   and
                   ( smallpos <= smallpos)   and
                   ( bigpos <= bigpos)   and
                   ( not(bigneg > smallneg))   and
                   ( not(bigneg > zero))      and
                   ( not(bigneg > smallpos))   and
                   ( not(bigneg > bigpos))      and
                   ( not(smallneg > zero))      and
                   ( not(smallneg > smallpos))   and
                   ( not(smallneg > bigpos))   and
                   ( not(zero > smallpos))      and
                   ( not(zero > bigpos))      and
                   ( not(smallpos > bigpos))   and
                   ( not(bigneg > bigneg))      and
                   ( not(smallneg > smallneg))   and
                   ( not(zero >  zero))      and
                   ( not(smallpos > smallpos))   and
                   ( not(bigpos > bigpos))      ) 
      report "***PASSED TEST: c07s02b02x00p02n01i01985"
      severity NOTE;
    assert (    ( bigneg < smallneg)   and
                ( bigneg < zero)   and
                ( bigneg < smallpos)   and
                ( bigneg < bigpos)   and
                ( smallneg < zero)   and
                ( smallneg < smallpos)   and
                ( smallneg < bigpos)   and
                ( zero < smallpos)   and
                ( zero < bigpos)   and
                ( smallpos < bigpos)   and
                ( not(bigneg >= smallneg))   and
                ( not(bigneg >= zero))      and
                ( not(bigneg >= smallpos))   and
                ( not(bigneg >= bigpos))   and
                ( not(smallneg >= zero))   and
                ( not(smallneg >= smallpos))   and
                ( not(smallneg >= bigpos))   and
                ( not(zero >= smallpos))   and
                ( not(zero >= bigpos))      and
                ( not(smallpos >= bigpos))   and
                ( bigneg <= smallneg)   and
                ( bigneg <= zero)   and
                ( bigneg <= smallpos)   and
                ( bigneg <= bigpos)   and
                ( smallneg <= zero)   and
                ( smallneg <= smallpos)   and
                ( smallneg <= bigpos)   and
                ( zero <= smallpos)   and
                ( zero <= bigpos)   and
                ( smallpos <= bigpos)   and
                ( bigneg <= bigneg)   and
                ( smallneg <= smallneg)   and
                ( zero <=  zero)   and
                ( smallpos <= smallpos)   and
                ( bigpos <= bigpos)   and
                ( not(bigneg > smallneg))   and
                ( not(bigneg > zero))      and
                ( not(bigneg > smallpos))   and
                ( not(bigneg > bigpos))      and
                ( not(smallneg > zero))      and
                ( not(smallneg > smallpos))   and
                ( not(smallneg > bigpos))   and
                ( not(zero > smallpos))      and
                ( not(zero > bigpos))      and
                ( not(smallpos > bigpos))   and
                ( not(bigneg > bigneg))      and
                ( not(smallneg > smallneg))   and
                ( not(zero >  zero))      and
                ( not(smallpos > smallpos))   and
                ( not(bigpos > bigpos))      ) 
      report "***FAILED TEST: c07s02b02x00p02n01i01985 - Relational operators truth table test for data type of Integer failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b02x00p02n01i01985arch;
