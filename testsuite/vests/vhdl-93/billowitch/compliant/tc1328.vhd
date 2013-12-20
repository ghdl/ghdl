
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
-- $Id: tc1328.vhd,v 1.2 2001-10-26 16:29:40 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c08s04b01x00p03n03i01328pkg is
  
  type MVL    is ('0', '1', 'Z');
  type TVECT    is array (INTEGER RANGE <>) of MVL;
  
  function BUSFUNC(INPUT: TVECT) return MVL;

  subtype TS1    is BUSFUNC MVL;
  type    TSV    is array (INTEGER RANGE <>) of TS1;
  subtype WORD    is TSV(1 downto 0);
end c08s04b01x00p03n03i01328pkg;

package body c08s04b01x00p03n03i01328pkg is
  
  function BUSFUNC(INPUT: TVECT) return MVL is
    variable RESOLVED_VALUE: MVL := 'Z';
  begin
    for I in INPUT'RANGE loop
      if INPUT(I) /= 'Z' then
        RESOLVED_VALUE := INPUT(I);
        exit;
      end if;
    end loop;
    return RESOLVED_VALUE;
  end BUSFUNC;

end c08s04b01x00p03n03i01328pkg;

use WORK.c08s04b01x00p03n03i01328pkg.all;
ENTITY c08s04b01x00p03n03i01328ent IS
END c08s04b01x00p03n03i01328ent;

ARCHITECTURE c08s04b01x00p03n03i01328arch OF c08s04b01x00p03n03i01328ent IS
  signal S1 : BIT :='1';
  signal X  : BUSFUNC MVL BUS; 
BEGIN

  TESTING: PROCESS
  BEGIN

    S1 <= transport '0' after 15 ns;

    wait on S1;

    if (S1 = '1') then
      X <= '1';
    else
      X <= null after 5 ns;
    end if;

    wait for 6 ns;
    assert NOT( X='Z' )
      report "***PASSED TEST: c08s04b01x00p03n03i01328"
      severity NOTE;
    assert ( X='Z' )
      report "***FAILED TEST: c08s04b01x00p03n03i01328 - The driver of the signal of the signal is turned off when the waveform element consists of the reserved word 'null' and an optional after clasuse."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s04b01x00p03n03i01328arch;
