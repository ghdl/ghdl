
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
-- $Id: tc2949.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s02b00x00p12n01i02949ent IS
END c02s02b00x00p12n01i02949ent;

ARCHITECTURE c02s02b00x00p12n01i02949arch OF c02s02b00x00p12n01i02949ent IS

  function CreateN(constant size : in INTEGER) return STRING is
    variable result : STRING(1 to size);
    variable ch : CHARACTER;
  begin
    ch := 'A';
    for i in result'RANGE loop
      result(i) := ch;
      ch := CHARACTER'SUCC(ch);
      if ch > 'Z' then
        ch := 'Z';
      end if;
    end loop;
    return result;
  end;

BEGIN
  TESTING: PROCESS
  BEGIN
    assert "A" = CreateN(1) report CreateN(1);
    assert "AB" = CreateN(2) report CreateN(2);
    assert "ABCDEFGHIJ" = CreateN(10) report CreateN(10);

    assert NOT(    "A" = CreateN(1)   and
                   "AB" = CreateN(2)   and
                   "ABCDEFGHIJ" = CreateN(10))
      report "***PASSED TEST: c02s02b00x00p12n01i02949"
      severity NOTE;
    assert (    "A" = CreateN(1)   and
                "AB" = CreateN(2)   and
                "ABCDEFGHIJ" = CreateN(10))
      report "***FAILED TEST: c02s02b00x00p12n01i02949 - The execution of a subprogram test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s02b00x00p12n01i02949arch;
