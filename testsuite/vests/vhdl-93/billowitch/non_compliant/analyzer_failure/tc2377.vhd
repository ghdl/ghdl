
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
-- $Id: tc2377.vhd,v 1.2 2001-10-26 16:30:18 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x00p02n01i02377ent IS
END c07s03b02x00p02n01i02377ent;

ARCHITECTURE c07s03b02x00p02n01i02377arch OF c07s03b02x00p02n01i02377ent IS

BEGIN
  TESTING: PROCESS
    type ENUM is ( ONE, TWO, THREE, FOUR );
    function F_ENUM (       A : ENUM := ONE;
                            B : ENUM := TWO ) return ENUM is
    begin
      return A;
    end F_ENUM;
    variable V : ENUM := F_ENUM(,); -- Failure_here
    -- SYNTAX ERROR:  null parameter association not legal.
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c07s03b02x00p02n01i02377 - Null association element is not allowed." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x00p02n01i02377arch;
