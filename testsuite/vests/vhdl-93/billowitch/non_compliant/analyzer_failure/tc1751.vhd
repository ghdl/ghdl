
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
-- $Id: tc1751.vhd,v 1.2 2001-10-26 16:30:12 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b00x00p25n01i01751ent IS
END c09s05b00x00p25n01i01751ent;

ARCHITECTURE c09s05b00x00p25n01i01751arch OF c09s05b00x00p25n01i01751ent IS
  type a       is array (1 to 4) of boolean;
  type arr_bvec    is array (positive range <>) of a;

  function F (AB: arr_bvec) return a is
  begin
    return (true,true,true,true);
  end;
  
  signal G    : bit;
  signal i       : F a bus;
  signal m   : a := (true, false, true, false);
  constant c1, c2 : integer := 1;
BEGIN
  G <= '1' after 10 ns;

  B1: block(G = '1')
  begin
    (i(1), i(2), i(3), i(4))    <= guarded a'(true, false, false, true);
    (i(c1), i(c2), i(3), i(4))    <= guarded a'(true, false, false, true);
    -- Failure_here :  i(c1) and i(c2) are same signal names        
    (i(1), i(2), i(3), i(1))    <= guarded a'(true, false, false, true);
    -- Failure_here : i(1) appears twice
  end block;

  TESTING: PROCESS
  BEGIN
    wait for 50 ns;
    assert FALSE 
      report "***FAILED TEST: c09s05b00x00p25n01i01751 - No two signal names may identify the same object."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c09s05b00x00p25n01i01751arch;
