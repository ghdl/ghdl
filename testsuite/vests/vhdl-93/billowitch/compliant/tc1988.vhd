
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
-- $Id: tc1988.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b02x00p02n01i01988ent IS
END c07s02b02x00p02n01i01988ent;

ARCHITECTURE c07s02b02x00p02n01i01988arch OF c07s02b02x00p02n01i01988ent IS

BEGIN
  TESTING: PROCESS
    variable A : bit_vector (1 to 32);
    variable B : bit_vector (32 downto 1);
    variable C : bit_vector (15 downto 0);
    variable D : bit_vector (0 to 15);
    variable E : bit_vector (0 to 47);
    variable F : bit_vector (47 downto 0);
  BEGIN

    A := x"ffffffff";
    B := x"00000000";
    C := x"ffff";
    D := x"0000";
    E := x"ffffffffffff";
    F := x"000000000000";

    assert NOT(    A = A      and
                   A /= B      and
                   A /= C      and
                   A /= D      and
                   A /= E      and
                   A /= F      and
                   B /= A      and
                   B = B      and
                   B /= C      and
                   B /= D      and
                   B /= E      and
                   B /= F      and
                   C /= A      and
                   C /= B      and
                   C = C      and
                   C /= D      and
                   C /= E      and
                   C /= F      and
                   D /= A      and
                   D /= B      and
                   D /= C      and
                   D = D      and
                   D /= E      and
                   D /= F      and
                   E /= A      and
                   E /= B      and
                   E /= C      and
                   E /= D      and
                   E = E      and
                   E /= F      and
                   F /= A      and
                   F /= B      and
                   F /= C      and
                   F /= D      and
                   F /= E      and
                   F = F      and
                   A <= A      and
                   A > B      and
                   A > C      and
                   A > D      and
                   A < E      and
                   A > F      and
                   B < A      and
                   B <= B      and
                   B < C      and
                   B > D      and
                   B < E      and
                   B < F      and
                   C < A      and
                   C > B      and
                   C <= C      and
                   C > D      and
                   C < E      and
                   C > F      and
                   D < A      and
                   D < B      and
                   D < C      and
                   D <= D      and
                   D < E      and
                   D < F      and
                   E > A      and
                   E > B      and
                   E > C      and
                   E > D      and
                   E <= E      and
                   E > F      and
                   F < A      and
                   F > B      and
                   F < C      and
                   F > D      and
                   F < E      and
                   F <= F)
      report "***PASSED TEST: c07s02b02x00p02n01i01988"
      severity NOTE;
    assert (    A = A      and
                A /= B      and
                A /= C      and
                A /= D      and
                A /= E      and
                A /= F      and
                B /= A      and
                B = B      and
                B /= C      and
                B /= D      and
                B /= E      and
                B /= F      and
                C /= A      and
                C /= B      and
                C = C      and
                C /= D      and
                C /= E      and
                C /= F      and
                D /= A      and
                D /= B      and
                D /= C      and
                D = D      and
                D /= E      and
                D /= F      and
                E /= A      and
                E /= B      and
                E /= C      and
                E /= D      and
                E = E      and
                E /= F      and
                F /= A      and
                F /= B      and
                F /= C      and
                F /= D      and
                F /= E      and
                F = F      and
                A <= A      and
                A > B      and
                A > C      and
                A > D      and
                A < E      and
                A > F      and
                B < A      and
                B <= B      and
                B < C      and
                B > D      and
                B < E      and
                B < F      and
                C < A      and
                C > B      and
                C <= C      and
                C > D      and
                C < E      and
                C > F      and
                D < A      and
                D < B      and
                D < C      and
                D <= D      and
                D < E      and
                D < F      and
                E > A      and
                E > B      and
                E > C      and
                E > D      and
                E <= E      and
                E > F      and
                F < A      and
                F > B      and
                F < C      and
                F > D      and
                F < E      and
                F <= F)
      report "***FAILED TEST: c07s02b02x00p02n01i01988 - Relational operators truth table test for data type of Bit_vector failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b02x00p02n01i01988arch;
