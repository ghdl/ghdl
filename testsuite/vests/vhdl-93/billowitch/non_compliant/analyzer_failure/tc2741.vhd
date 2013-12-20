
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
-- $Id: tc2741.vhd,v 1.2 2001-10-26 16:30:21 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s06b00x00p03n02i02741ent IS
  --ERROR: need 2 quotation characters as input for a string literal
  constant c: string:=""";   -- failure_here
END c13s06b00x00p03n02i02741ent;

ARCHITECTURE c13s06b00x00p03n02i02741arch OF c13s06b00x00p03n02i02741ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert   FALSE 
      report "***FAILED TEST: c13s06b00x00p03n02i02741 - Single quotation mark cannot be a string literal."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s06b00x00p03n02i02741arch;
