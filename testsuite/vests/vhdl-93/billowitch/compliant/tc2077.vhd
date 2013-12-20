
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
-- $Id: tc2077.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p01n02i02077ent IS
END c07s02b04x00p01n02i02077ent;

ARCHITECTURE c07s02b04x00p01n02i02077arch OF c07s02b04x00p01n02i02077ent IS

BEGIN
  TESTING: PROCESS
    variable A : bit_vector (1 to 32);
    constant AA : bit_vector (1 to 32) := x"0000ffff";
    variable B : bit_vector (32 downto 1);
    variable C : bit_vector (15 downto 0);
    variable D, DD : bit_vector (0 to 15);
    variable E : bit_vector (0 to 47);
    variable F : bit_vector (47 downto 0);
    alias FF : bit_vector (47 downto 0) is F;
    variable Q, R : bit;
  BEGIN

    A := x"ffffffff";
    B := x"00000000";
    C := x"ffff";
    D := x"0000";
    E := x"ffffffffffff";
    FF := x"000000000000";
    Q := '1';
    R := '0';

    assert NOT(   ( C & Q = b"11111111111111111")   and
                  ( C & R = b"11111111111111110")   and
                  ( D & Q = b"00000000000000001")   and
                  ( D & R = b"00000000000000000")   and
                  ( Q & C = b"11111111111111111")   and
                  ( R & C = b"01111111111111111")   and
                  ( Q & D = b"10000000000000000")   and
                  ( R & D = b"00000000000000000")   and
                  ( A & Q = Q & A)   and
                  ( B & R = R & B)   and
                  ( A & R = C & (C & R))   and
                  ( R & A = (R & C) & C)   and
                  ( R & R & R & R  & C = x"0ffff")   and
                  ( C & R & R & R & R = x"ffff0")      and
                  ( E & Q = Q & E)   and
                  ( F & Q = not (E & R))   and
                  ( A & A = x"ffffffffffffffff")   and
                  ( A & B = x"ffffffff00000000")   and
                  ( A & C = x"ffffffffffff")   and
                  ( A & D = x"ffffffff0000")   and
                  ( A & E = x"ffffffffffffffffffff")   and
                  ( A & F = x"ffffffff000000000000")   and
                  ( B & A = x"00000000ffffffff")   and
                  ( B & B = x"0000000000000000")   and
                  ( B & C = x"00000000ffff")   and
                  ( B & D = x"000000000000")   and
                  ( B & E = x"00000000ffffffffffff")   and
                  ( B & F = x"00000000000000000000")   and
                  ( C & A = x"ffffffffffff")   and
                  ( C & B = x"ffff00000000")   and
                  ( C & C = x"ffffffff")      and
                  ( C & D = x"ffff0000")      and
                  ( C & E = x"ffffffffffffffff")   and
                  ( C & F = x"ffff000000000000")   and
                  ( D & A = x"0000ffffffff")   and
                  ( D & B = x"000000000000")   and
                  ( D & C = x"0000ffff")      and
                  ( D & D = x"00000000")      and
                  ( D & E = x"0000ffffffffffff")   and
                  ( D & F = x"0000000000000000")   and
                  ( E & A = x"ffffffffffffffffffff")   and
                  ( E & B = x"ffffffffffff00000000")   and
                  ( E & C = x"ffffffffffffffff")      and
                  ( E & D = x"ffffffffffff0000")      and
                  ( E & E = x"ffffffffffffffffffffffff")   and
                  ( E & F = x"ffffffffffff000000000000")   and
                  ( F & A = x"000000000000ffffffff")   and
                  ( F & B = x"00000000000000000000")   and
                  ( F & C = x"000000000000ffff")      and
                  ( F & D = x"0000000000000000")      and
                  ( F & E = x"000000000000ffffffffffff")   and
                  ( F & F = x"000000000000000000000000")   )    
      report "***PASSED TEST: c07s02b04x00p01n02i02077" 
      severity NOTE;
    assert (   ( C & Q = b"11111111111111111")   and
               ( C & R = b"11111111111111110")   and
               ( D & Q = b"00000000000000001")   and
               ( D & R = b"00000000000000000")   and
               ( Q & C = b"11111111111111111")   and
               ( R & C = b"01111111111111111")   and
               ( Q & D = b"10000000000000000")   and
               ( R & D = b"00000000000000000")   and
               ( A & Q = Q & A)   and
               ( B & R = R & B)   and
               ( A & R = C & (C & R))   and
               ( R & A = (R & C) & C)   and
               ( R & R & R & R  & C = x"0ffff")   and
               ( C & R & R & R & R = x"ffff0")      and
               ( E & Q = Q & E)   and
               ( F & Q = not (E & R))   and
               ( A & A = x"ffffffffffffffff")   and
               ( A & B = x"ffffffff00000000")   and
               ( A & C = x"ffffffffffff")   and
               ( A & D = x"ffffffff0000")   and
               ( A & E = x"ffffffffffffffffffff")   and
               ( A & F = x"ffffffff000000000000")   and
               ( B & A = x"00000000ffffffff")   and
               ( B & B = x"0000000000000000")   and
               ( B & C = x"00000000ffff")   and
               ( B & D = x"000000000000")   and
               ( B & E = x"00000000ffffffffffff")   and
               ( B & F = x"00000000000000000000")   and
               ( C & A = x"ffffffffffff")   and
               ( C & B = x"ffff00000000")   and
               ( C & C = x"ffffffff")      and
               ( C & D = x"ffff0000")      and
               ( C & E = x"ffffffffffffffff")   and
               ( C & F = x"ffff000000000000")   and
               ( D & A = x"0000ffffffff")   and
               ( D & B = x"000000000000")   and
               ( D & C = x"0000ffff")      and
               ( D & D = x"00000000")      and
               ( D & E = x"0000ffffffffffff")   and
               ( D & F = x"0000000000000000")   and
               ( E & A = x"ffffffffffffffffffff")   and
               ( E & B = x"ffffffffffff00000000")   and
               ( E & C = x"ffffffffffffffff")      and
               ( E & D = x"ffffffffffff0000")      and
               ( E & E = x"ffffffffffffffffffffffff")   and
               ( E & F = x"ffffffffffff000000000000")   and
               ( F & A = x"000000000000ffffffff")   and
               ( F & B = x"00000000000000000000")   and
               ( F & C = x"000000000000ffff")      and
               ( F & D = x"0000000000000000")      and
               ( F & E = x"000000000000ffffffffffff")   and
               ( F & F = x"000000000000000000000000")   )    
      report "***FAILED TEST: c07s02b04x00p01n02i02077 - The operation of operator & test failed." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p01n02i02077arch;
