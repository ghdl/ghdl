
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
-- $Id: tc1763.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b02x00p01n01i01763ent IS
END c09s05b02x00p01n01i01763ent;

ARCHITECTURE c09s05b02x00p01n01i01763arch OF c09s05b02x00p01n01i01763ent IS
  type   t_wlogic  is (U, D, Z0, Z1, ZDX, DZX, ZX);
  signal count : integer ;
  signal ECLK  : t_wlogic;
  signal ECLK2 : t_wlogic;
  signal ECL   : integer := 1;
BEGIN
  count <=    0 after  0 ns,   
              1 after 10 ns,
              2 after 20 ns,
              3 after 30 ns,
              4 after 40 ns,
              5 after 50 ns,
              6 after 60 ns;
  ----------------------------------------------------------------------
  WITH count SELECT
    ECLK <=    transport 
    U   after 1 ns WHEN 0,
    D   after 1 ns WHEN 1,
    Z0  after 1 ns WHEN 2,
    Z1  after 1 ns WHEN 3,
    ZDX after 1 ns WHEN 4,
    DZX after 1 ns WHEN 5,
    ZX  after 1 ns WHEN OTHERS;
  TESTING: PROCESS(count)
  BEGIN
    case count is
      WHEN 0      => ECLK2 <= transport U   after 1 ns;
      WHEN 1      => ECLK2 <= transport D   after 1 ns;
      WHEN 2      => ECLK2 <= transport Z0  after 1 ns;
      WHEN 3      => ECLK2 <= transport Z1  after 1 ns;
      WHEN 4      => ECLK2 <= transport ZDX after 1 ns;
      WHEN 5      => ECLK2 <= transport DZX after 1 ns;
      WHEN OTHERS   => ECLK2 <= transport ZX  after 1 ns;
    end case;
  END PROCESS TESTING;
  PROCESS(ECLK,ECLK2)
  BEGIN
    if    now =  0 ns then
      NULL;
    elsif   (now =  1 ns) and (ECLK /= ECLK2) then
      assert FALSE
        report "FAILED TEST" 
        severity ERROR;
      ECL <= 0;
    elsif   (now = 11 ns) and (ECLK /= ECLK2) then
      assert FALSE
        report "FAILED TEST" 
        severity ERROR;
      ECL <= 0;
    elsif    (now = 21 ns) and (ECLK /= ECLK2) then
      assert FALSE
        report "FAILED TEST" 
        severity ERROR;
      ECL <= 0;
    elsif   (now = 31 ns) and (ECLK /= ECLK2) then
      assert FALSE
        report "FAILED TEST" 
        severity ERROR;
      ECL <= 0;
    elsif   (now = 41 ns) and (ECLK /= ECLK2) then
      assert FALSE
        report "FAILED TEST" 
        severity ERROR;
      ECL <= 0;
    elsif   (now = 51 ns) and (ECLK /= ECLK2) then
      assert FALSE
        report "FAILED TEST" 
        severity ERROR;
      ECL <= 0;
    elsif   (now = 61 ns) and (ECLK /= ECLK2) then
      assert FALSE
        report "FAILED TEST" 
        severity ERROR;
      ECL <= 0;
    end if;
  END PROCESS;
  PROCESS(ECLK,ECLK2)
  BEGIN
    if    (now > 60 ns) and (ECL = 1) then
      assert FALSE 
        report "***PASSED TEST: c09s05b02x00p01n01i01763"
        severity NOTE;
    elsif   (now > 60 ns) and (ECL = 0) then
      assert FALSE 
        report "***FAILED TEST: c09s05b02x00p01n01i01763 - The transport selected signal assignment represents a process statement in which the signal transform is a case statement."
        severity ERROR;
    end if;
  END PROCESS;

END c09s05b02x00p01n01i01763arch;
