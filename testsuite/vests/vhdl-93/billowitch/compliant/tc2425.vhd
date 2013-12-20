
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
-- $Id: tc2425.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x01p01n02i02425ent IS
END c07s03b02x01p01n02i02425ent;

ARCHITECTURE c07s03b02x01p01n02i02425arch OF c07s03b02x01p01n02i02425ent IS
  type rec is record
                a: integer;
                b: integer;
                c: integer;
                d: integer;
              end record;
  constant y: rec := (a => 12, others => 10); -- No_failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(y.a=12 and y.b=10 and y.c=10 and y.d=10) 
      report "***PASSED TEST: c07s03b02x01p01n02i02425" 
      severity NOTE;
    assert (y.a=12 and y.b=10 and y.c=10 and y.d=10) 
      report "***FAILED TEST: c07s03b02x01p01n02i02425 - If the choice others is given as a choice of a record aggregate, it must represent at least one element." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x01p01n02i02425arch;
