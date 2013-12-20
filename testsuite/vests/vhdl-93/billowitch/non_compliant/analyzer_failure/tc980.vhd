
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
-- $Id: tc980.vhd,v 1.2 2001-10-26 16:30:29 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p05n01i00980ent IS
END c06s03b00x00p05n01i00980ent;

ARCHITECTURE c06s03b00x00p05n01i00980arch OF c06s03b00x00p05n01i00980ent IS

BEGIN
  TESTING: PROCESS
    type R1 is record
                 RE1: BOOLEAN;
               end record;
    type R2 is record
                 RE2: BOOLEAN;
               end record;
    function F2 return R2 is
    begin
      return (RE2=>TRUE);
    end F2;

    variable V1: R1 ;
    variable V10: BOOLEAN;

  BEGIN
    V10 := F2.RE1;
    -- SEMANTIC ERROR: NO SUCH RECORD ELEMENT;
    assert FALSE 
      report "***FAILED TEST: c06s03b00x00p05n01i00980 - Illegal record element name." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p05n01i00980arch;
