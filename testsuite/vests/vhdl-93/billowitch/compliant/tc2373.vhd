
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
-- $Id: tc2373.vhd,v 1.1.1.1 2001-08-22 18:20:51 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b01x00p08n01i02373ent IS
END c07s03b01x00p08n01i02373ent;

ARCHITECTURE c07s03b01x00p08n01i02373arch OF c07s03b01x00p08n01i02373ent IS

BEGIN
   TESTING: PROCESS
                        -- Redefine the type CHARACTER.
                        type NEW_CHAR is (
                            NUL, SOH, STX, ETX, EOT, ENQ, ACK, BEL,
                            BS,  HT,  LF,  VT,  FF,  CR,  SO,  SI,
                            DLE, DC1, DC2, DC3, DC4, NAK, SYN, ETB,
                            CAN, EM,  SUB, ESC, FSP, GSP, RSP, USP,

                            ' ', '!', '"', '#', '$', '%', '&', ''',
                            '(', ')', '*', '+', ',', '-', '.', '/',
                            '2', '3', '4', '5', '6', '7',
                            '8', '9', ':', ';', '<', '=', '>', '?',

                            '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
                            'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
                            'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
                            'X', 'Y', 'Z', '[', '\', ']', '^', '_' );

                       -- Local declarations.
                       variable S : STRING( 1 to 12 );
                       variable B : BIT_VECTOR( 1 to 2 );
   BEGIN
                       -- Should be OK,  non-overloaded literals.
                       S := "hello, world";
                       B := B"11";
         wait for 5 ns;
   assert NOT(    S = "hello, world"   and
          B = B"11"      )
      report "***PASSED TEST: c07s03b01x00p08n01i02373"
      severity NOTE;
   assert (    S = "hello, world"   and
          B = B"11"      )
      report "***FAILED TEST: c07s03b01x00p08n01i02373 - The graphic characters contained within a string literal should be visible."
      severity ERROR;
   wait;
   END PROCESS TESTING;

END c07s03b01x00p08n01i02373arch;
