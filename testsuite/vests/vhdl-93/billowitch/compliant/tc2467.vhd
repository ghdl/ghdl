
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
-- $Id: tc2467.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p03n02i02467ent IS
END c07s03b02x02p03n02i02467ent;

ARCHITECTURE c07s03b02x02p03n02i02467arch OF c07s03b02x02p03n02i02467ent IS
  type      UN_ARR  is array (integer range <>) of character;
  subtype   CON_ARR is UN_ARR( 1 to 5 );
  attribute LOCN : CON_ARR ;
  signal    S    : Integer ;
  attribute LOCN of S : signal is ('A', others => 'Z'); -- No_failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT( S'LOCN(1)='A' and S'LOCN(2)='Z' and S'LOCN(3)='Z' and S'LOCN(4)='Z' and S'LOCN(5)='Z' )
      report "***PASSED TEST: c07s03b02x02p03n02i02467"
      severity NOTE;
    assert ( S'LOCN(1)='A' and S'LOCN(2)='Z' and S'LOCN(3)='Z' and S'LOCN(4)='Z' and S'LOCN(5)='Z' )
      report "***FAILED TEST: c07s03b02x02p03n02i02467 - An array aggregate with an others choice may appear as the expression defining the value of an attribute in an attribute specification."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p03n02i02467arch;
