
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
-- $Id: tc991.vhd,v 1.2 2001-10-26 16:30:29 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p07n02i00991ent IS
END c06s03b00x00p07n02i00991ent;

ARCHITECTURE c06s03b00x00p07n02i00991arch OF c06s03b00x00p07n02i00991ent IS

BEGIN
  TESTING: PROCESS
    type A1 is array (BOOLEAN) of BOOLEAN;
    function F return BOOLEAN is
    begin
      return TRUE;
    end F;
    variable B1 : BOOLEAN;
    variable V1 : BOOLEAN;
    variable V2 : A1 ;
  BEGIN
    V2 := V2(F.B1);     -- ERROR: the prefix of an expanded name
    -- cannot be a functon call.
    assert FALSE 
      report "***FAILED TEST: c06s03b00x00p07n02i00991 - The prefix of an expanded name cannot be a function call.(Expanded name used as array index)"
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p07n02i00991arch;
