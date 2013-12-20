
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
-- $Id: tc147.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p14n01i00147ent IS
  PORT ( ii: INOUT integer);
  FUNCTION addup (i1,i2,i3:INTEGER:=5) RETURN INTEGER IS
  BEGIN
    IF (i1 = 0) THEN
      RETURN (i2+i3);
    ELSE
      RETURN addup(i2=>i1*3,i1=>0,i3=>0)+i2+i3;
    END IF;
  END;
END c04s03b02x02p14n01i00147ent;

ARCHITECTURE c04s03b02x02p14n01i00147arch OF c04s03b02x02p14n01i00147ent IS
  SIGNAL a1 : INTEGER := 57;
  SIGNAL a2 : INTEGER := 68;
  SIGNAL a3 : INTEGER := 77;
BEGIN
  TESTING: PROCESS
  BEGIN
    WAIT FOR 1 ns;
    ii <= addup(
      i3=>addup(i3=>-8,i1=>addup(4,2,2),i2=>addup(i1=>0,i2=>0,i3=>0)),
      i1=>addup(
        i2=>addup(
          i2=>addup(i2=>6,i3=>7),
          i1=>addup(i3=>3,i1=>1,i2=>2),
          i3=>addup(i2=>8,i3=>9,i1=>7)
          ),
        i1=>addup(i2=>2,i1=>1,i3=>3),
        i3=>addup(i2=>a2,i3=>a3,i1=>a1)
        ),
      i2=>addup(i3=>-8,i2=>0,i1=>8)
      );
    WAIT FOR 1 ns;
    assert NOT(ii=1346) 
      report "***PASSED TEST: c04s03b02x02p14n01i00147"
      severity NOTE;
    assert (ii=1346) 
      report "***FAILED TEST: c04s03b02x02p14n01i00147 - Function call does not use function call in parameter list." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p14n01i00147arch;
