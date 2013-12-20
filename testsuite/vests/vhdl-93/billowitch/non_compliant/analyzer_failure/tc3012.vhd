
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
-- $Id: tc3012.vhd,v 1.2 2001-10-26 16:30:24 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c02s06b00x00p08n01i03012pkg is
  constant X1 : integer;
  constant X2 : integer;
end c02s06b00x00p08n01i03012pkg;

package body c02s06b00x00p08n01i03012pkg is
  constant X1: integer := X2;  --Failure_here
  constant X2: integer := 1;
end c02s06b00x00p08n01i03012pkg;


ENTITY c02s06b00x00p08n01i03012ent IS
END c02s06b00x00p08n01i03012ent;

ARCHITECTURE c02s06b00x00p08n01i03012arch OF c02s06b00x00p08n01i03012ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE
      report "***FAILED TEST: c02s06b00x00p08n01i03012 - A name that denotes the name of a deferred constant can appear, before the full declaration only in the default expression for a local generic, local port, or a formal parameter."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s06b00x00p08n01i03012arch;
