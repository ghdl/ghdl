
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
-- $Id: tc2359.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b07x00p06n01i02359ent IS
END c07s02b07x00p06n01i02359ent;

ARCHITECTURE c07s02b07x00p06n01i02359arch OF c07s02b07x00p06n01i02359ent IS

BEGIN
  TESTING: PROCESS
    variable r1,r2,r3,r4,r5,r6   : real;
    variable r7,r8,r9,r10      : real;
    variable i1         : integer;
  BEGIN

    r2 := 2.0;
    r3 := 10.0;
    i1 := 10;
    r1 := 2.0 ** 10;
    r4 := r2 ** i1;
    r5 := (-2.0)**10;
    r6 := 0.0 ** i1;
    r7 := 0.0 ** 5;
    r8 := 2.0;
    r9 := r8 ** 0;
    r10:= r8 ** (-0);
    wait for 5 ns;
    assert NOT(    ( r1 = r4 )   and
                   ( r1 = 1024.0)   and
                   ( r1 = r5 )   and
                   ( r6 = 0.0)   and
                   ( r6 = r7 )   and
                   ( r9 = 1.0 )   and
                   ( r10= r9 )   )
      report "***PASSED TEST: c07s02b07x00p06n01i02359"
      severity NOTE;
    assert (    ( r1 = r4 )   and
                ( r1 = 1024.0)   and
                ( r1 = r5 )   and
                ( r6 = 0.0)   and
                ( r6 = r7 )   and
                ( r9 = 1.0 )   and
                ( r10= r9 )   )
      report "***FAILED TEST: c07s02b07x00p06n01i02359 - Unary operator exponentiation test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b07x00p06n01i02359arch;
