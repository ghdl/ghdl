
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
-- $Id: tc80.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b01x02p12n01i00080ent IS
END c04s03b01x02p12n01i00080ent;

ARCHITECTURE c04s03b01x02p12n01i00080arch OF c04s03b01x02p12n01i00080ent IS
  type arrbit    is array (1 to 3) of bit;
  type comp_vect    is array (positive range <>) of arrbit;
  
  function F(BB: comp_vect) return arrbit is
  begin
    return "111";
  end;
  
  signal X : F arrbit ;
  signal P : bit := '1';
  signal Q : bit := '1';
  signal R : bit := '1';
BEGIN
  TESTING: PROCESS(P,Q,R)
  BEGIN
    X(1) <= P;   -- No_failure_here
    X(2) <= Q;   -- No_failure_here
    X(3) <= R;   -- No_failure_here
    assert NOT(X="111") 
      report "***PASSED TEST: c04s03b01x02p12n01i00080"
      severity NOTE;
    assert (X="111") 
      report "***FAILED TEST:c04s03b01x02p12n01i00080 - All of the subelements of the signal should have a driver in a process."
      severity ERROR;
  END PROCESS TESTING;

END c04s03b01x02p12n01i00080arch;
