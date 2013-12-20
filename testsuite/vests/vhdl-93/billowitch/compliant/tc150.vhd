
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
-- $Id: tc150.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p14n01i00150ent IS
END c04s03b02x02p14n01i00150ent;

ARCHITECTURE c04s03b02x02p14n01i00150arch OF c04s03b02x02p14n01i00150ent IS

  FUNCTION FLOAT ( ival : in integer) return real is
    VARIABLE v1 : real := 543.0;
  begin
    RETURN v1;
  end FLOAT;

  FUNCTION ROUND ( rval : in real) return integer is
    VARIABLE v1 : integer := 543;
  begin
    RETURN v1;
  end ROUND;
  
  PROCEDURE test_bed
    ( in1  : in  integer;
      out1 : out real )
  is
  begin
    out1 := FLOAT (in1);
  end test_bed;

BEGIN
  TESTING: PROCESS
    VARIABLE var1 : real;
    VARIABLE var2 : real := 543.2;
  BEGIN
    test_bed ( ROUND (var2), var1 );
    assert NOT( var1 = 543.0 )
      report "***PASSED TEST:c04s03b02x02p14n01i00150"
      severity NOTE;
    assert ( var1 = 543.0 )
      report "***FAILED TEST:c04s03b02x02p14n01i00150 - The actual part of a named element association may be in the form of a function call."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p14n01i00150arch;
