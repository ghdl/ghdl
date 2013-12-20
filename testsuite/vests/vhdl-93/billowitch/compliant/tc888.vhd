
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
-- $Id: tc888.vhd,v 1.2 2001-10-26 16:30:01 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c10s02b00x00p10n01i00888ent IS
END c10s02b00x00p10n01i00888ent;

ARCHITECTURE c10s02b00x00p10n01i00888arch OF c10s02b00x00p10n01i00888ent IS
  procedure xyz ( a : integer; b : real ) is
  begin
    assert NOT( b = 2.0 * real(a) ) 
      report "***PASSED TEST: c10s02b00x00p10n01i00888"
      severity NOTE;
    assert ( b = 2.0 * real(a) ) 
      report "***FAILED TEST: c10s02b00x00p10n01i00888 - When in the absence of a separate subprogram declaration, the subprogram specification given in the subprogram body acts as the declaration." 
      severity ERROR;
  end xyz;
BEGIN
  TESTING: PROCESS
  BEGIN
    xyz ( a => 20, b => 40.0 );
    wait;
  END PROCESS TESTING;

END c10s02b00x00p10n01i00888arch;
