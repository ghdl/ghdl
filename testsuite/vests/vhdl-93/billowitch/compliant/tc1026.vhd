
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
-- $Id: tc1026.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s04b00x00p01n01i01026ent IS
END c06s04b00x00p01n01i01026ent;

ARCHITECTURE c06s04b00x00p01n01i01026arch OF c06s04b00x00p01n01i01026ent IS

BEGIN
  TESTING: PROCESS
    variable E    : bit_vector (0 to 47);
    variable F    : bit_vector (47 downto 0);
    alias FF    : bit_vector (47 downto 0) is F;
    variable G    : bit_vector (3 downto 0);
    variable H    : bit_vector (0 to 3);
  BEGIN

    F    := x"555555555555";
    E    := x"555555555555";
    G    := b"1111";
    G(1)    := '0';
    H    := b"1111";
    H(1)    := '0';

    assert NOT(   ( F(47) = '0')   and
                  ( F(42) = '1')   and
                  ( F(37) = '0')   and
                  ( F(32) = '1')   and
                  ( F(27) = '0')   and
                  ( F(22) = '1')   and
                  ( F(17) = '0')   and
                  ( F(12) = '1')   and
                  ( F(7) = '0')   and
                  ( F(2) = '1')   and
                  ( FF(47) = '0')   and
                  ( FF(42) = '1')   and
                  ( FF(37) = '0')   and
                  ( FF(32) = '1')   and
                  ( FF(27) = '0')   and
                  ( FF(22) = '1')   and
                  ( FF(17) = '0')   and
                  ( FF(12) = '1')   and
                  ( FF(7) = '0')   and
                  ( FF(2) = '1')   and
                  ( E(47) = '1')   and
                  ( E(42) = '0')   and
                  ( E(37) = '1')   and
                  ( E(32) = '0')   and
                  ( E(27) = '1')   and
                  ( E(22) = '0')   and
                  ( E(17) = '1')   and
                  ( E(12) = '0')   and
                  ( E(7) = '1')   and
                  ( E(2) = '0')   and
                  ( E = F)   and 
                  ( G = b"1101")   and
                  ( H = b"1011")   )
      report "***PASSED TEST: c06s04b00x00p01n01i01026"
      severity NOTE;
    assert (   ( F(47) = '0')   and
               ( F(42) = '1')   and
               ( F(37) = '0')   and
               ( F(32) = '1')   and
               ( F(27) = '0')   and
               ( F(22) = '1')   and
               ( F(17) = '0')   and
               ( F(12) = '1')   and
               ( F(7) = '0')   and
               ( F(2) = '1')   and
               ( FF(47) = '0')   and
               ( FF(42) = '1')   and
               ( FF(37) = '0')   and
               ( FF(32) = '1')   and
               ( FF(27) = '0')   and
               ( FF(22) = '1')   and
               ( FF(17) = '0')   and
               ( FF(12) = '1')   and
               ( FF(7) = '0')   and
               ( FF(2) = '1')   and
               ( E(47) = '1')   and
               ( E(42) = '0')   and
               ( E(37) = '1')   and
               ( E(32) = '0')   and
               ( E(27) = '1')   and
               ( E(22) = '0')   and
               ( E(17) = '1')   and
               ( E(12) = '0')   and
               ( E(7) = '1')   and
               ( E(2) = '0')   and
               ( E = F)   and 
               ( G = b"1101")   and
               ( H = b"1011")   )
      report "***FAILED TEST: c06s04b00x00p01n01i01026 - Indexed reference test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s04b00x00p01n01i01026arch;
