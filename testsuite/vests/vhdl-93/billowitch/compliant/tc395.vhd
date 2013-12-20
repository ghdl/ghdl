
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
-- $Id: tc395.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p06n02i00395ent IS
END c03s02b01x01p06n02i00395ent;

ARCHITECTURE c03s02b01x01p06n02i00395arch OF c03s02b01x01p06n02i00395ent IS
  type A1 is array (positive range 1 to 2) of BOOLEAN;

  type R1 is record
               RE1: A1;  -- no_failure_here
             end record;
BEGIN
  TESTING: PROCESS
    variable k : R1;
  BEGIN
    k.RE1(1) := TRUE;
    k.RE1(2) := FALSE;
    assert NOT(    k.RE1(1) = TRUE and 
                   k.RE1(2) = FALSE )
      report "***PASSED TEST: c03s02b01x01p06n02i00395"
      severity NOTE;
    assert (    k.RE1(1) = TRUE and 
                k.RE1(2) = FALSE )
      report "***FAILED TEST: c03s02b01x01p06n02i00395 - Record element cannot be an unconstrained array."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p06n02i00395arch;
