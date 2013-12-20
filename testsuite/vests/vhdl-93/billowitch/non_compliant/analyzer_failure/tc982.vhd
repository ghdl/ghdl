
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
-- $Id: tc982.vhd,v 1.2 2001-10-26 16:30:29 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p05n01i00982ent IS
  port (signal a,b : in integer; c,d : out integer);
END c06s03b00x00p05n01i00982ent;

ARCHITECTURE c06s03b00x00p05n01i00982arch OF c06s03b00x00p05n01i00982ent IS

BEGIN
  TESTING: PROCESS
    type some_record is
      record
        x1,x2,x3,x4,x5,x6,x7,x8 : integer;
        y : boolean;
      end record;

    variable rec1,rec2,rec3 : some_record;
  BEGIN
    rec1.x5 := 5;
    rec1.x7 := a;
    rec1.y := true;

    WAIT for 1 ns;
    rec2 := rec1.all;
    WAIT for 1 ns;
    assert FALSE 
      report "***FAILED TEST: c06s03b00x00p05n01i00982 - Illegal record selected name." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p05n01i00982arch;
