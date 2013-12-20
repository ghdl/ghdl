
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
-- $Id: tc489.vhd,v 1.2 2001-10-26 16:29:55 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b02x00p01n01i00489ent IS
  type small is
    record
      bt : bit;
      bv : bit_vector (11 downto 0);
      r  : real range 0.0 to real'high;
      bb : boolean;
      i  : integer range 1 to 20;
    end record;
END c03s02b02x00p01n01i00489ent;

ARCHITECTURE c03s02b02x00p01n01i00489arch OF c03s02b02x00p01n01i00489ent IS

BEGIN
  TESTING: PROCESS
    variable A1 : small;
    alias A1_bv : bit_vector (11 downto 0) is A1.bv;
    alias A1_bt : bit is A1.bt;
    alias A1_i  : integer is A1.i;
    alias A1_r  : real is A1.r;
    alias A1_bb : boolean is A1.bb;
  BEGIN

    assert NOT(   ( A1.bv = x"000")   and
                  ( A1.bt = '0')      and
                  ( A1.bb = false)   and
                  ( A1.i  = 1)      and
                  ( A1.r  = 0.0)      and
                  ( A1_bv = x"000")   and
                  ( A1_bt = '0')      and
                  ( A1_bb = false)   and
                  ( A1_i  = 1)      and
                  ( A1_r  = 0.0)      )
      report "***PASSED TEST: c03s02b02x00p01n01i00489"
      severity NOTE;
    assert (   ( A1.bv = x"000")   and
               ( A1.bt = '0')      and
               ( A1.bb = false)   and
               ( A1.i  = 1)      and
               ( A1.r  = 0.0)      and
               ( A1_bv = x"000")   and
               ( A1_bt = '0')      and
               ( A1_bb = false)   and
               ( A1_i  = 1)      and
               ( A1_r  = 0.0)      )
      report "***FAILED TEST: c03s02b02x00p01n01i00489 - Values of a record object consist of the value of its elements." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b02x00p01n01i00489arch;
