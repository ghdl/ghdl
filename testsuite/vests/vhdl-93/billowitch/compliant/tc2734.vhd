
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
-- $Id: tc2734.vhd,v 1.1.1.1 2001-08-22 18:20:52 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

ENTITY c13s05b00x00p01n01i02734ent IS
END c13s05b00x00p01n01i02734ent;

ARCHITECTURE c13s05b00x00p01n01i02734arch OF c13s05b00x00p01n01i02734ent IS

BEGIN
   TESTING: PROCESS
    type      grph is array (1 to 95) of character;
    variable   k : grph;
   BEGIN
      k(1)  := 'A';
      k(2)  := 'B';
      k(3)  := 'C';
      k(4)  := 'D';
      k(5)  := 'E';
      k(6)  := 'F';
      k(7)  := 'G';
      k(8)  := 'H';
      k(9)  := 'I';
      k(10) := 'J';
      k(11) := 'K';
      k(12) := 'L';
      k(13) := 'M';
      k(14) := 'N';
      k(15) := 'O';
      k(16) := 'P';
      k(17) := 'Q';
      k(18) := 'R';
      k(19) := 'S';
      k(20) := 'T';
      k(21) := 'U';
      k(22) := 'V';
      k(23) := 'W';
      k(24) := 'X';
      k(25) := 'Y';
      k(26) := 'Z';
      k(27) := '0';
      k(28) := '1';
      k(29) := '2';
      k(30) := '3';
      k(31) := '4';
      k(32) := '5';
      k(33) := '6';
      k(34) := '7';
      k(35) := '8';
      k(36) := '9';
      k(37) := '"';
      k(38) := '#';
      k(39) := '&';
      k(40) := ''';
      k(41) := '(';
      k(42) := ')';
      k(43) := '*';
      k(44) := '+';
      k(45) := ',';
      k(46) := '-';
      k(47) := '.';
      k(48) := '/';
      k(49) := ':';
      k(50) := ';';
      k(51) := '<';
      k(52) := '=';
      k(53) := '>';
      k(54) := '_';
      k(55) := '|';
      k(56) := ' ';
      k(57) := 'a';
      k(58) := 'b';
      k(59) := 'c';
      k(60) := 'd';
      k(61) := 'e';
      k(62) := 'f';
      k(63) := 'g';
      k(64) := 'h';
      k(65) := 'i';
      k(66) := 'j';
      k(67) := 'k';
      k(68) := 'l';
      k(69) := 'm';
      k(70) := 'n';
      k(71) := 'o';
      k(72) := 'p';
      k(73) := 'q';
      k(74) := 'r';
      k(75) := 's';
      k(76) := 't';
      k(77) := 'u';
      k(78) := 'v';
      k(79) := 'w';
      k(80) := 'x';
      k(81) := 'y';
      k(82) := 'z';
      k(83) := '!';
      k(84) := '$';
      k(85) := '%';
      k(86) := '@';
      k(87) := '?';
      k(88) := '[';
      k(89) := '\';
      k(90) := ']';
      k(91) := '^';
      k(92) := '`';
      k(93) := '{';
      k(94) := '}';
      k(95) := '~';
   assert NOT(   k(1)  = 'A'   and
         k(2)  = 'B'   and 
         k(3)  = 'C'   and
         k(4)  = 'D'   and
         k(5)  = 'E'   and
         k(6)  = 'F'   and
         k(7)  = 'G'   and
         k(8)  = 'H'   and
         k(9)  = 'I'   and
         k(10) = 'J'   and
         k(11) = 'K'   and
         k(12) = 'L'   and
         k(13) = 'M'   and
         k(14) = 'N'   and
         k(15) = 'O'   and
         k(16) = 'P'   and
         k(17) = 'Q'   and
         k(18) = 'R'   and
         k(19) = 'S'   and
         k(20) = 'T'   and
         k(21) = 'U'   and
         k(22) = 'V'   and
         k(23) = 'W'   and
         k(24) = 'X'   and
         k(25) = 'Y'   and
         k(26) = 'Z'   and
         k(27) = '0'   and
         k(28) = '1'   and
         k(29) = '2'   and
         k(30) = '3'   and
         k(31) = '4'   and
         k(32) = '5'   and
         k(33) = '6'   and
         k(34) = '7'   and
         k(35) = '8'   and
         k(36) = '9'   and
         k(37) = '"'   and
         k(38) = '#'   and
         k(39) = '&'   and
         k(40) = '''   and
         k(41) = '('   and
         k(42) = ')'   and
         k(43) = '*'   and
         k(44) = '+'   and
         k(45) = ','   and
         k(46) = '-'   and
         k(47) = '.'   and
         k(48) = '/'   and
         k(49) = ':'   and
         k(50) = ';'   and
         k(51) = '<'   and
         k(52) = '='   and
         k(53) = '>'   and
         k(54) = '_'   and
         k(55) = '|'   and
         k(56) = ' '   and
         k(57) = 'a'   and
         k(58) = 'b'   and
         k(59) = 'c'   and
         k(60) = 'd'   and
         k(61) = 'e'   and
         k(62) = 'f'   and
         k(63) = 'g'   and
         k(64) = 'h'   and
         k(65) = 'i'   and
         k(66) = 'j'   and
         k(67) = 'k'   and
         k(68) = 'l'   and
         k(69) = 'm'   and
         k(70) = 'n'   and
         k(71) = 'o'   and
         k(72) = 'p'   and
         k(73) = 'q'   and
         k(74) = 'r'   and
         k(75) = 's'   and
         k(76) = 't'   and
         k(77) = 'u'   and
         k(78) = 'v'   and
         k(79) = 'w'   and
         k(80) = 'x'   and
         k(81) = 'y'   and
         k(82) = 'z'   and
         k(83) = '!'   and
         k(84) = '$'   and
         k(85) = '%'   and
         k(86) = '@'   and
         k(87) = '?'   and
         k(88) = '['   and
         k(89) = '\'   and
         k(90) = ']'   and
         k(91) = '^'   and
         k(92) = '`'   and
         k(93) = '{'   and
         k(94) = '}'   and
         k(95) = '~'   )
      report "***PASSED TEST: /src/ch13/sc05/p001-002/s010107.vhd" 
      severity NOTE;
   assert (   k(1)  = 'A'   and
         k(2)  = 'B'   and 
         k(3)  = 'C'   and
         k(4)  = 'D'   and
         k(5)  = 'E'   and
         k(6)  = 'F'   and
         k(7)  = 'G'   and
         k(8)  = 'H'   and
         k(9)  = 'I'   and
         k(10) = 'J'   and
         k(11) = 'K'   and
         k(12) = 'L'   and
         k(13) = 'M'   and
         k(14) = 'N'   and
         k(15) = 'O'   and
         k(16) = 'P'   and
         k(17) = 'Q'   and
         k(18) = 'R'   and
         k(19) = 'S'   and
         k(20) = 'T'   and
         k(21) = 'U'   and
         k(22) = 'V'   and
         k(23) = 'W'   and
         k(24) = 'X'   and
         k(25) = 'Y'   and
         k(26) = 'Z'   and
         k(27) = '0'   and
         k(28) = '1'   and
         k(29) = '2'   and
         k(30) = '3'   and
         k(31) = '4'   and
         k(32) = '5'   and
         k(33) = '6'   and
         k(34) = '7'   and
         k(35) = '8'   and
         k(36) = '9'   and
         k(37) = '"'   and
         k(38) = '#'   and
         k(39) = '&'   and
         k(40) = '''   and
         k(41) = '('   and
         k(42) = ')'   and
         k(43) = '*'   and
         k(44) = '+'   and
         k(45) = ','   and
         k(46) = '-'   and
         k(47) = '.'   and
         k(48) = '/'   and
         k(49) = ':'   and
         k(50) = ';'   and
         k(51) = '<'   and
         k(52) = '='   and
         k(53) = '>'   and
         k(54) = '_'   and
         k(55) = '|'   and
         k(56) = ' '   and
         k(57) = 'a'   and
         k(58) = 'b'   and
         k(59) = 'c'   and
         k(60) = 'd'   and
         k(61) = 'e'   and
         k(62) = 'f'   and
         k(63) = 'g'   and
         k(64) = 'h'   and
         k(65) = 'i'   and
         k(66) = 'j'   and
         k(67) = 'k'   and
         k(68) = 'l'   and
         k(69) = 'm'   and
         k(70) = 'n'   and
         k(71) = 'o'   and
         k(72) = 'p'   and
         k(73) = 'q'   and
         k(74) = 'r'   and
         k(75) = 's'   and
         k(76) = 't'   and
         k(77) = 'u'   and
         k(78) = 'v'   and
         k(79) = 'w'   and
         k(80) = 'x'   and
         k(81) = 'y'   and
         k(82) = 'z'   and
         k(83) = '!'   and
         k(84) = '$'   and
         k(85) = '%'   and
         k(86) = '@'   and
         k(87) = '?'   and
         k(88) = '['   and
         k(89) = '\'   and
         k(90) = ']'   and
         k(91) = '^'   and
         k(92) = '`'   and
         k(93) = '{'   and
         k(94) = '}'   and
         k(95) = '~'   )
      report "***FAILED TEST: c13s05b00x00p01n01i02734 - Any one of the 95 graphic characters should be a character literal." 
      severity ERROR;
   wait;
   END PROCESS TESTING;

END c13s05b00x00p01n01i02734arch;
