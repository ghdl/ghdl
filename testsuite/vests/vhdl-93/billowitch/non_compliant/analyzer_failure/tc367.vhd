
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
-- $Id: tc367.vhd,v 1.2 2001-10-26 16:30:26 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p03n01i00367ent IS
END c03s02b01x01p03n01i00367ent;

ARCHITECTURE c03s02b01x01p03n01i00367arch OF c03s02b01x01p03n01i00367ent IS
  type MVL is ('0', '1', 'Z') ;
  type MVL_vector is array (positive range <>) of MVL;

  function tristate (X:MVL_vector) return MVL;
  subtype tribit is tristate MVL;
  type tribit_vector is array (positive range <>) of tribit;    
  subtype byte is tribit_vector (0 to 7);
  subtype half_byte is byte (0 to 3); -- Failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c03s02b01x01p03n01i00367 - If an index constraint appears after a type mark in a subtype indication, then the type or subtype denoted by the type mark must not already impose an index constraint." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p03n01i00367arch;
