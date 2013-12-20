
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
-- $Id: tc1027.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s04b00x00p01n01i01027ent IS
END c06s04b00x00p01n01i01027ent;

ARCHITECTURE c06s04b00x00p01n01i01027arch OF c06s04b00x00p01n01i01027ent IS

BEGIN
  TESTING: PROCESS
    variable    V1    : BIT_VECTOR(1 to 2);
    variable    V2    : BIT_VECTOR(3 to 4);
    variable   pass   : integer := 0;
  BEGIN
    v1(1) := '1';
    v1(2) := '0';
    v2(3) := '0';
    v2(4) := '1';

    assert v1(1) = '1' report "v1(1) initial value is wrong.";
    assert v1(2) = '0' report "v1(2) initial value is wrong.";
    assert v2(3) = '0' report "v2(3) initial value is wrong.";
    assert v2(4) = '1' report "v2(4) initial value is wrong.";
    if (   V1(1) /= '1' or V1(2) /= '0' or
           V2(3) /= '0' or V2(4) /= '1'   ) then   
      pass := 1;
    end if;
    v1 := v2;         -- composite variable assignment
    assert v1(1) = '0' report "v1(1) final value is wrong.";
    assert v1(2) = '1' report "v1(2) final value is wrong.";
    assert v2(3) = '0' report "v2(3) final value is wrong.";
    assert v2(4) = '1' report "v2(4) final value is wrong.";
    if (   V1(1) /= '0' or V1(2) /= '1' or
           V2(3) /= '0' or V2(4) /= '1'   ) then   
      pass := 1;
    end if;
    v1 := ('1', '1');      -- composite variable assignment
    -- aggregate value
    assert v1(1) = '1' report "v1(1) final value is wrong.";
    assert v1(2) = '1' report "v1(2) final value is wrong.";
    assert v2(3) = '0' report "v2(3) final value is wrong.";
    assert v2(4) = '1' report "v2(4) final value is wrong.";
    if (   V1(1) /= '1' or V1(2) /= '1' or
           V2(3) /= '0' or V2(4) /= '1'   ) then   
      pass := 1;
    end if;
    wait for 5 ns;
    assert NOT(   pass = 0   )
      report "***PASSED TEST: c06s04b00x00p01n01i01027"
      severity NOTE;
    assert (   pass = 0   )
      report "***FAILED TEST: c06s04b00x00p01n01i01027 - Indexed reference test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s04b00x00p01n01i01027arch;
