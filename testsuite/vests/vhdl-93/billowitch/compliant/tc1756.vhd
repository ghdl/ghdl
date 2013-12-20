
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
-- $Id: tc1756.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b01x00p01n01i01756ent IS
END c09s05b01x00p01n01i01756ent;

ARCHITECTURE c09s05b01x00p01n01i01756arch OF c09s05b01x00p01n01i01756ent IS
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
  ECLK <=  U   after 1 ns WHEN count=0 ELSE
           D   after 1 ns WHEN count=1 ELSE
           Z0  after 1 ns WHEN count=2 ELSE
           Z1  after 1 ns WHEN count=3 ELSE
           ZDX after 1 ns WHEN count=4 ELSE
           DZX after 1 ns WHEN count=5 ELSE
           ZX  after 1 ns ;
  TESTING: PROCESS(count)
  BEGIN
    if   count = 0 then
      ECLK2 <= U   after 1 ns;
    elsif   count = 1 then
      ECLK2 <= D   after 1 ns;
    elsif   count = 2 then
      ECLK2 <= Z0  after 1 ns;
    elsif   count = 3 then
      ECLK2 <= Z1  after 1 ns;
    elsif   count = 4 then
      ECLK2 <= ZDX after 1 ns;
    elsif   count = 5 then
      ECLK2 <= DZX after 1 ns;
    else
      ECLK2 <= ZX  after 1 ns;
    end if;   
  END PROCESS TESTING;
  PROCESS(ECLK,ECLK2)
  BEGIN
    if       now =  0 ns then
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
    elsif   (now = 21 ns) and (ECLK /= ECLK2) then
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
    if      (now > 60 ns) and (ECL = 1) then
      assert FALSE
        report "***PASSED TEST: c09s05b01x00p01n01i01756"
        severity NOTE;
    elsif   (now > 60 ns) and (ECL = 0) then
      assert FALSE
        report "***FAILED TEST: c09s05b01x00p01n01i01756 - The conditional signal assignment represents a process statement in which the signal transform is an if statement."
        severity ERROR;
    end if;
  END PROCESS;

END c09s05b01x00p01n01i01756arch;
