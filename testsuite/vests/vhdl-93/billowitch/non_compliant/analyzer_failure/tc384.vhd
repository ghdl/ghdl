
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
-- $Id: tc384.vhd,v 1.1.1.1 2001-08-22 18:20:50 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p04n01i00384ent IS
END c03s02b01x01p04n01i00384ent;

ARCHITECTURE c03s02b01x01p04n01i00384arch OF c03s02b01x01p04n01i00384ent IS
        type A1 is array (positive range <>, positive range <>) of bit;
        subtype byte is A1 (0 to 7, -10 to 7); -- Failure_here
BEGIN
   TESTING: PROCESS
   BEGIN
   assert FALSE 
      report "***FAILED TEST: ENTITY c03s02b01x01p04n01i00384ent IS
END c03s02b01x01p04n01i00384ent;

ARCHITECTURE c03s02b01x01p04n01i00384arch OF c03s02b01x01p04n01i00384ent IS - The index constraint values are not compatible with the corresponding subtype."
      severity ERROR;
   wait;
   END PROCESS TESTING;

END ENTITY c03s02b01x01p04n01i00384ent IS
END c03s02b01x01p04n01i00384ent;

ARCHITECTURE c03s02b01x01p04n01i00384arch OF c03s02b01x01p04n01i00384ent ISarch;
