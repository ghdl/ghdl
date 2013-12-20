
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
-- $Id: tc3057.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s03b01x04p10n01i03057ent IS
END c12s03b01x04p10n01i03057ent;

ARCHITECTURE c12s03b01x04p10n01i03057arch OF c12s03b01x04p10n01i03057ent IS

  function f1(constant sb : in bit) return bit is
    constant b : bit := sb;
  begin
    assert (b=sb)
      report "Constant B in function F1 set to non-static variable failed"
      severity failure;
    assert NOT( b=sb )
      report "***PASSED TEST: c12s03b01x04p10n01i03057"
      severity NOTE;
    assert ( b=sb )
      report "***FAILED TEST: c12s03b01x04p10n01i03057 - Non-static expression initializing a constant failed."
      severity ERROR;
    return '1';
  end;

BEGIN
  TESTING: PROCESS
    variable vbi,vbr : bit;
  BEGIN
    vbi:='1';
    vbr:=f1(vbi);
    wait;
  END PROCESS TESTING;

END c12s03b01x04p10n01i03057arch;
