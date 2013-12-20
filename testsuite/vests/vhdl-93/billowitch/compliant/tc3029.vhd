
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
-- $Id: tc3029.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c11s04b00x00p02n01i03029pkg is
  type MVL is ('0', '1', 'X', 'Z') ;
  function F1 (INPUT : Bit) return Bit;
end c11s04b00x00p02n01i03029pkg;

package body c11s04b00x00p02n01i03029pkg is
  constant C1 : MVL := '1' ;
  function F1 (INPUT : Bit) return Bit is
  begin
    if Input = '1' then
      return '0' ;
    else
      return '1' ;
    end if;
  end F1;
end c11s04b00x00p02n01i03029pkg;

use work.c11s04b00x00p02n01i03029pkg.all;
ENTITY c11s04b00x00p02n01i03029ent IS
END c11s04b00x00p02n01i03029ent;

ARCHITECTURE c11s04b00x00p02n01i03029arch OF c11s04b00x00p02n01i03029ent IS
  signal S1  : MVL;
  signal S2  : Bit;
BEGIN
  TESTING: PROCESS
  BEGIN
    S1 <= 'Z'    after 20 ns;
    S2 <= F1('1')    after 50 ns;
    wait for 60 ns;
    assert NOT(S1 = 'Z' and S2 = '0')
      report "***PASSED TEST: c11s04b00x00p02n01i03029" 
      severity NOTE;
    assert (S1 = 'Z' and S2 = '0')
      report "***FAILED TEST: c11s04b00x00p02n01i03029 - Primary unit must be analyzed before the analysis of the unit that references it."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c11s04b00x00p02n01i03029arch;
