
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
-- $Id: tc1077.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p01n01i01077ent IS
END c06s05b00x00p01n01i01077ent;

ARCHITECTURE c06s05b00x00p01n01i01077arch OF c06s05b00x00p01n01i01077ent IS

BEGIN
  TESTING: PROCESS
    variable A    : bit_vector (1 to 32);
    constant AA    : bit_vector (1 to 32) := x"0000ffff";
    variable B    : bit_vector (32 downto 1);
    variable C    : bit_vector (15 downto 0);
    variable D, DD    : bit_vector (0 to 15);
    variable E    : bit_vector (0 to 47);
    variable F    : bit_vector (47 downto 0);
    alias FF    : bit_vector (47 downto 0) is F;
    alias FH    : bit_vector (0 to 31) is F (47 downto 16);
  BEGIN
    A := x"0000ffff";
    B := x"00ff00ff";
    C := x"00ff";
    D := x"0f0f";
    E := x"000000ffffff";
    FF := x"000fff000fff";
    assert NOT(   ( A(1 to 32) = x"0000ffff")   and
                  ( A(1 to 20) = x"0000f")   and
                  ( A(9 to 32) = x"00ffff")   and
                  ( A(9 to 28) = x"00fff")   and
                  ( C(15 downto 0) = x"00ff")   and
                  ( C(11 downto 0) = x"0ff")   and
                  ( C(15 downto 4) = x"00f")   and
                  ( C(11 downto 4) = x"0f")   and
                  ( F(47 downto 0) = x"000fff000fff")   and
                  ( F(39 downto 0) = x"0fff000fff")   and
                  ( F(47 downto 8) = x"000fff000f")   and
                  ( F(39 downto 8) = x"0fff000f")   and
                  ( F(47 downto 36) = x"000")   and
                  ( F(11 downto 0) = x"fff")   and
                  ( F(35 downto 20) = x"fff0")   and
                  ( FF(47 downto 0) = x"000fff000fff")   and
                  ( FF(39 downto 0) = x"0fff000fff")   and
                  ( FF(47 downto 8) = x"000fff000f")   and
                  ( FF(39 downto 8) = x"0fff000f")   and
                  ( FF(47 downto 36) = x"000")   and
                  ( FF(11 downto 0) = x"fff")   and
                  ( FF(35 downto 20) = x"fff0")   and
                  ( FH(0 to 31) = x"000fff00")   and
                  ( FH(8 to 31) = x"0fff00")   and
                  ( FH(0 to 11) = x"000")      and
                  ( FH(12 to 27) = x"fff0")   ) 
      report "***PASSED TEST: c06s05b00x00p01n01i01077"
      severity NOTE;
    assert (   ( A(1 to 32) = x"0000ffff")   and
               ( A(1 to 20) = x"0000f")   and
               ( A(9 to 32) = x"00ffff")   and
               ( A(9 to 28) = x"00fff")   and
               ( C(15 downto 0) = x"00ff")   and
               ( C(11 downto 0) = x"0ff")   and
               ( C(15 downto 4) = x"00f")   and
               ( C(11 downto 4) = x"0f")   and
               ( F(47 downto 0) = x"000fff000fff")   and
               ( F(39 downto 0) = x"0fff000fff")   and
               ( F(47 downto 8) = x"000fff000f")   and
               ( F(39 downto 8) = x"0fff000f")   and
               ( F(47 downto 36) = x"000")   and
               ( F(11 downto 0) = x"fff")   and
               ( F(35 downto 20) = x"fff0")   and
               ( FF(47 downto 0) = x"000fff000fff")   and
               ( FF(39 downto 0) = x"0fff000fff")   and
               ( FF(47 downto 8) = x"000fff000f")   and
               ( FF(39 downto 8) = x"0fff000f")   and
               ( FF(47 downto 36) = x"000")   and
               ( FF(11 downto 0) = x"fff")   and
               ( FF(35 downto 20) = x"fff0")   and
               ( FH(0 to 31) = x"000fff00")   and
               ( FH(8 to 31) = x"0fff00")   and
               ( FH(0 to 11) = x"000")      and
               ( FH(12 to 27) = x"fff0")   ) 
      report "***FAILED TEST: c06s05b00x00p01n01i01077 - A slice name denotes a one-dimensional array composed of a sequence of consecutive elements of another one-dimensional array test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p01n01i01077arch;
