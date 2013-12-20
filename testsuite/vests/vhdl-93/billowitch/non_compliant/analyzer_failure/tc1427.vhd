
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
-- $Id: tc1427.vhd,v 1.2 2001-10-26 16:30:09 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s06b00x00p06n01i01427ent IS
END c08s06b00x00p06n01i01427ent;

ARCHITECTURE c08s06b00x00p06n01i01427arch OF c08s06b00x00p06n01i01427ent IS
  procedure check(x : in integer; y : in boolean) is
  begin
  end;
  signal k : real;
  signal q : boolean;
BEGIN
  TESTING: PROCESS
  BEGIN
    L1 : check(k,q);
    assert FALSE 
      report "***FAILED TEST: c08s06b00x00p06n01i01427 - The parameters in the procedure declaration and the corresponding arguments in the procedure call are not of the same type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s06b00x00p06n01i01427arch;
