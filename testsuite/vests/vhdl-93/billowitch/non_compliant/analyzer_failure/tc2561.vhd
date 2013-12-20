
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
-- $Id: tc2561.vhd,v 1.2 2001-10-26 16:30:19 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b06x00p05n03i02561ent IS
END c07s03b06x00p05n03i02561ent;

ARCHITECTURE c07s03b06x00p05n03i02561arch OF c07s03b06x00p05n03i02561ent IS

BEGIN
  TESTING: PROCESS
    type ONETWO is range 1 to 2;
    type more_bad_sig_drivers is array (positive range <>) of ONETWO;
    function F2( candidate : more_bad_sig_drivers  )  return ONETWO is
      variable R : ONETWO;
    begin
      return R;
    end F2;
    type LINK is access ONETWO;
    variable HEAD : LINK :=  new F2 ONETWO;      --- Failure_here
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c07s03b06x00p05n03i02561 - Subtype indication should not include a resolution function."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b06x00p05n03i02561arch;
