
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
-- $Id: tc2896.vhd,v 1.2 2001-10-26 16:30:23 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s01b01x00p06n01i02896ent IS
END c02s01b01x00p06n01i02896ent;

ARCHITECTURE c02s01b01x00p06n01i02896arch OF c02s01b01x00p06n01i02896ent IS
  function func1 (signal a1 : real) return integer is
  begin
    null;
  end func1;
BEGIN
  TESTING: PROCESS
    variable x: real := 1.2;
    variable y: integer;
  BEGIN
    y := func1 (x); -- Failure_here
    assert FALSE
      report "***FAILED TEST: c02s01b01x00p06n01i02896 - In a subprogram call the actual designator associated with a formal parameter of class signal cannot be of type variable."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s01b01x00p06n01i02896arch;
