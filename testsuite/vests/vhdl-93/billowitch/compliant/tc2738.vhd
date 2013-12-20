
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
-- $Id: tc2738.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s06b00x00p03n02i02738ent IS
END c13s06b00x00p03n02i02738ent;

ARCHITECTURE c13s06b00x00p03n02i02738arch OF c13s06b00x00p03n02i02738ent IS
  constant   c : string := ('"',' ');
  constant   s : string := """ ";
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT( c=s )
      report "***PASSED TEST: c13s06b00x00p03n02i02738"
      severity NOTE;
    assert ( c=s )
      report "***FAILED TEST: c13s06b00x00p03n02i02738 - A string literal that includes two adjacent quotation characters is interpreted as one quotation character."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s06b00x00p03n02i02738arch;
