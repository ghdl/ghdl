
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
-- $Id: tc3183.vhd,v 1.2 2001-10-26 16:29:52 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c14s01b00x00p122n01i03183ent IS
END c14s01b00x00p122n01i03183ent;

ARCHITECTURE c14s01b00x00p122n01i03183arch OF c14s01b00x00p122n01i03183ent IS

  constant C : INTEGER := 1;
--
  type t2 is array(c to c + c, 1 to 10) of integer;
  
-- transitive cases
  type t3 is array(t2'range(1), t2'reverse_range(2)) of integer;
  
-- 'Reverse_Range (of two-dimensional array type)
  type rt321 is range t3'reverse_range(1);
  type rt322 is range t3'reverse_range(2);

BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 10 ns;
    assert NOT(    rt321'LEFT = rt321(c+c)   and
                   rt321'RIGHT= rt321(c)   and
                   rt322'LEFT = rt322(1)   and
                   rt322'RIGHT= rt322(10)   )
      report "***PASSED TEST: c14s01b00x00p122n01i03183"
      severity NOTE;
    assert (    rt321'LEFT = rt321(c+c)   and
                rt321'RIGHT= rt321(c)   and
                rt322'LEFT = rt322(1)   and
                rt322'RIGHT= rt322(10)   )
      report "***FAILED TEST: c14s01b00x00p122n01i03183 - Predefined attribute reverse_range test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c14s01b00x00p122n01i03183arch;
