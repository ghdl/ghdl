
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
-- $Id: tc1031.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s04b00x00p02n01i01031ent IS
END c06s04b00x00p02n01i01031ent;

ARCHITECTURE c06s04b00x00p02n01i01031arch OF c06s04b00x00p02n01i01031ent IS

BEGIN
  TESTING: PROCESS
    type    TEN    is range 1 to 10;
    type    ABASE1    is array (TEN range <>) of BOOLEAN;
    subtype    A1    is ABASE1(TEN);
    type    ABASE2    is array (TEN range <>) of A1;
    subtype    A2    is ABASE2(TEN);
    variable Sl_of_sl    : A2 ;
    variable V2      : A2 ; -- := (others=>(others=>TRUE));
  BEGIN
    Sl_of_sl(1 to 8)(7)  := V2(2 to 9)(2);
    assert NOT( Sl_of_sl(1 to 8)(7)=(false,false,false,false,false,false,false,false,false,false))
      report "***PASSED TEST: c06s04b00x00p02n01i01031"
      severity NOTE;
    assert ( Sl_of_sl(1 to 8)(7)=(false,false,false,false,false,false,false,false,false,false))
      report "***FAILED TEST: c06s04b00x00p02n01i01031 - The prefix of an indexed name can be a slice name."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s04b00x00p02n01i01031arch;
