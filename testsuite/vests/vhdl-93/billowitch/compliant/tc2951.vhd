
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
-- $Id: tc2951.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s02b00x00p12n01i02951ent IS
END c02s02b00x00p12n01i02951ent;

ARCHITECTURE c02s02b00x00p12n01i02951arch OF c02s02b00x00p12n01i02951ent IS

  function Concat(
    constant in1 : in STRING;
    constant in2 : in STRING
    ) return STRING is
    variable result : STRING(1 to (in1'LENGTH + in2'LENGTH));
  begin
    for i in in1'RANGE loop
      result(result'left + i - in1'left) := in1(i);
    end loop;
    for i in in2'RANGE loop
      result(result'left + in1'length + i - in2'left) := in2(i);
    end loop;
    return result;
  end;

BEGIN
  TESTING: PROCESS
  BEGIN

    assert NOT( Concat( Concat("Let's ","try "), Concat("multiple ", "levels!")) =
                "Let's try multiple levels!" )
      report "***PASSED TEST: c02s02b00x00p12n01i02951"
      severity NOTE;
    assert ( Concat( Concat("Let's ","try "), Concat("multiple ", "levels!")) =
             "Let's try multiple levels!" )
      report "***FAILED TEST: c02s02b00x00p12n01i02951 - The execution of a subprogram test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s02b00x00p12n01i02951arch;
