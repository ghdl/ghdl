
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
-- $Id: tc1294.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s04b00x00p06n01i01294ent IS
END c08s04b00x00p06n01i01294ent;

ARCHITECTURE c08s04b00x00p06n01i01294arch OF c08s04b00x00p06n01i01294ent IS
  type   BIT_VECTOR is array (integer range <>) of BIT;
  signal DID : BIT_VECTOR(0 to 7);
BEGIN
  TESTING: PROCESS
    variable NUM1 : BIT_VECTOR(0 to 7) := B"01010101";
  BEGIN
    DID <= NUM1;
    wait on  DID;
    assert NOT( DID = B"01010101" )
      report "***PASSED TEST: c08s04b00x00p06n01i01294"
      severity NOTE;
    assert ( DID = B"01010101" )
      report "***FAILED TEST: c08s04b00x00p06n01i01294 - Type of the right hand and left hand side of the signal assignment statement must be the same"
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s04b00x00p06n01i01294arch;
