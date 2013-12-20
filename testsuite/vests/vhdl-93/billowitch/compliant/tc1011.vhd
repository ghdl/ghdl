
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
-- $Id: tc1011.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p10n01i01011ent IS
END c06s03b00x00p10n01i01011ent;

ARCHITECTURE c06s03b00x00p10n01i01011arch OF c06s03b00x00p10n01i01011ent IS
  procedure check (x: in integer; y: in boolean; signal z :out integer) is
  begin
    z <= 5;
  end;
  signal p: integer ;
  signal q: boolean ;
  signal k: integer ;
BEGIN
  TESTING: PROCESS
  BEGIN
    check(c06s03b00x00p10n01i01011arch.p, c06s03b00x00p10n01i01011arch.q, k);
    wait for 10 ns;
    assert NOT(k=5)
      report "***PASSED TEST: c06s03b00x00p10n01i01011"
      severity NOTE;
    assert ( k=5 )
      report "***FAILED TEST: c06s03b00x00p10n01i01011 - An expanded name with the prefix of an architecture name and the suffix of signal names declared in the architecture can be used in a statement (in this test, procedure call statement) within the architecture body."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p10n01i01011arch;
