
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
-- $Id: tc56.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b01x01p05n01i00056ent IS
END c04s03b01x01p05n01i00056ent;

ARCHITECTURE c04s03b01x01p05n01i00056arch OF c04s03b01x01p05n01i00056ent IS
  procedure PRO (constant C1 : in BIT := '1') is  --- No_failure_here
  begin
    assert NOT( C1= '1' )
      report "***PASSED TEST:c04s03b01x01p05n01i00056"
      severity NOTE;
    assert ( C1= '1' )
      report "***FAILED TEST: c04s03b01x01p05n01i00056 - The formal parameters of subprogram are of mode in may be constant."
      severity ERROR;
  end;
BEGIN
  PRO;
END c04s03b01x01p05n01i00056arch;
