
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
-- $Id: tc2525.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b05x00p06n02i02525ent IS
END c07s03b05x00p06n02i02525ent;

ARCHITECTURE c07s03b05x00p06n02i02525arch OF c07s03b05x00p06n02i02525ent IS

BEGIN
  TESTING: PROCESS
    type Apples  is range 0 to 75;
    type Oranges is range 0 to 75;
    variable Macintosh : Apples   := 7;
    variable Seville   : Oranges    := 5;
  BEGIN
    Macintosh := Apples (Seville);
    assert NOT( Macintosh=5 )
      report "***PASSED TEST: c07s03b05x00p06n02i02525"
      severity NOTE;
    assert ( Macintosh=5 )
      report "***FAILED TEST: c07s03b05x00p06n02i02525 - The operand can be of any integer type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b05x00p06n02i02525arch;
