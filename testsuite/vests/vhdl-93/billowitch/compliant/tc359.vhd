
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
-- $Id: tc359.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p02n01i00359ent IS
END c03s02b01x01p02n01i00359ent;

ARCHITECTURE c03s02b01x01p02n01i00359arch OF c03s02b01x01p02n01i00359ent IS
  type sense is (hear, touch, smell, see);
  type page is array (hear to see) of bit;
BEGIN
  TESTING: PROCESS
    variable k : page;
  BEGIN
    k(hear)   := '0';
    k(touch):= '1';
    k(smell):= '0';
    k(see)  := '1';
    wait for 2 ns;
    assert NOT(   k(hear) = '0' and 
                  k(touch)= '1' and 
                  k(smell)= '0' and
                  k(see)  = '1') 
      report "***PASSED TEST: c03s02b01x01p02n01i00359" 
      severity NOTE;
    assert (   k(hear) = '0' and 
               k(touch)= '1' and 
               k(smell)= '0' and
               k(see)  = '1') 
      report "***FAILED TEST: c03s02b01x01p02n01i00359 - Bounds are of different discrete types." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p02n01i00359arch;
