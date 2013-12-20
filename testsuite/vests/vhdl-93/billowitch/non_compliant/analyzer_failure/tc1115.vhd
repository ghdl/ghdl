
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
-- $Id: tc1115.vhd,v 1.2 2001-10-26 16:30:06 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p03n01i01115ent IS
END c06s05b00x00p03n01i01115ent;

ARCHITECTURE c06s05b00x00p03n01i01115arch OF c06s05b00x00p03n01i01115ent IS

BEGIN
  TESTING: PROCESS
    type    FIVE    is range 1 to 5;
    type    A1B    is array (FIVE range <>) of BOOLEAN;
    subtype A1    is A1B(FIVE);
    type    A2B    is array (FIVE range <>, FIVE range <>) of A1;
    subtype A2    is A2B(FIVE, FIVE);
    
    function G return A2 is
    begin
      return (others => (others => (others => false)));
    end G;

    variable V1: A1;
    variable V2: A2;
  BEGIN
    V1(2 to 4) := G(3 to 5);   -- ERROR: prefix of a slice name
    -- cannot be a function value
    -- of a multi-dimensional array type
    assert FALSE 
      report "***FAILED TEST: c06s05b00x00p03n01i01115 - Prefix of a slice number must be a one-dimensional array type." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p03n01i01115arch;
