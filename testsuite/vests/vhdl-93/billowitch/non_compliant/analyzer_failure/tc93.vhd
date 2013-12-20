
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
-- $Id: tc93.vhd,v 1.2 2001-10-26 16:30:28 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x00p08n01i00093ent IS
END c04s03b02x00p08n01i00093ent;

ARCHITECTURE c04s03b02x00p08n01i00093arch OF c04s03b02x00p08n01i00093ent IS

  procedure proc1 (x1 : integer; y1 :real; z1 : boolean) is
    variable x12 : integer;
    variable z12 : boolean;
  begin
    x12    := 12;
    z12    := (x1 < 2);
    z1     := z12;
    y1    := y1 - 1.0;
    x1    := x12;
  end proc1;
  
BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c04s03b02x00p08n01i00093 - Object of mode in may not be updated."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x00p08n01i00093arch;
