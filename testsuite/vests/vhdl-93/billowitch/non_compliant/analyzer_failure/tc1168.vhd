
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
-- $Id: tc1168.vhd,v 1.2 2001-10-26 16:30:06 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s06b00x00p06n01i01168ent IS
END c06s06b00x00p06n01i01168ent;

ARCHITECTURE c06s06b00x00p06n01i01168arch OF c06s06b00x00p06n01i01168ent IS

BEGIN
  TESTING: PROCESS
    type II is range 1    to 1000;
    type RR is range 0.0001 to 10000.01;
    function F1 (A:II;B:RR) return BOOLEAN is
      variable G1 : II;
      variable G2 : RR;
    begin
      if (G1'HIGH(A) <= 0) then   -- ERROR: attribute does not have a
                                          -- generic expression assoc. with it.
        return FALSE;
      end if;
    end F1;
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c06s06b00x00p06n01i01168 - Arrtribute does not have generic expression associated with it."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s06b00x00p06n01i01168arch;
