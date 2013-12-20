
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
-- $Id: tc366.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p03n01i00366ent IS
END c03s02b01x01p03n01i00366ent;

ARCHITECTURE c03s02b01x01p03n01i00366arch OF c03s02b01x01p03n01i00366ent IS
  type MVL is ('0', '1', 'Z') ;
  type tribit is array (natural range <>) of MVL;
  subtype word is tribit (0 to 16); -- Success_here
BEGIN
  TESTING: PROCESS
    variable k : word;
  BEGIN
    k(0)  := '0';
    k(16) := 'Z';
    assert NOT (k(0)='0' and k(16)='Z') 
      report "***PASSED TEST: c03s02b01x01p03n01i00366" 
      severity NOTE;
    assert (k(0)='0' and k(16)='Z') 
      report "***FAILED TEST: c03s02b01x01p03n01i00366 - If an index constraint appears after a type mark in a subtype indication, then the type or subtype denoted by the type mark must not already impose an index constraint." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p03n01i00366arch;
