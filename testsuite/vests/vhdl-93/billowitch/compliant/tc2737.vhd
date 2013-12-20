
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
-- $Id: tc2737.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s06b00x00p03n01i02737ent IS
END c13s06b00x00p03n01i02737ent;

ARCHITECTURE c13s06b00x00p03n01i02737arch OF c13s06b00x00p03n01i02737ent IS
  constant s01: string:="A ";   --  1
  constant s02: string:="B ";   --  2
  constant s03: string:="C ";   --  3
  constant s04: string:="D ";   --  4
  constant s05: string:="E ";   --  5
  constant s06: string:="F ";   --  6
  constant s07: string:="G ";   --  7
  constant s08: string:="H ";   --  8
  constant s09: string:="I ";   --  9
  constant s10: string:="J ";   -- 10
  constant s11: string:="K ";   -- 11
  constant s12: string:="L ";   -- 12
  constant s13: string:="M ";   -- 13
  constant s14: string:="N ";   -- 14
  constant s15: string:="O ";   -- 15
  constant s16: string:="P ";   -- 16
  constant s17: string:="Q ";   -- 17
  constant s18: string:="R ";   -- 18
  constant s19: string:="S ";   -- 19
  constant s20: string:="T ";   -- 20
  constant s21: string:="U ";   -- 21
  constant s22: string:="V ";   -- 22
  constant s23: string:="W ";   -- 23
  constant s24: string:="X ";   -- 24
  constant s25: string:="Y ";   -- 25
  constant s26: string:="Z ";   -- 26
  constant s27: string:="0 ";   -- 27
  constant s28: string:="1 ";   -- 28
  constant s29: string:="2 ";   -- 29
  constant s30: string:="3 ";   -- 30
  constant s31: string:="4 ";   -- 31
  constant s32: string:="5 ";   -- 32
  constant s33: string:="6 ";   -- 33
  constant s34: string:="7 ";   -- 34
  constant s35: string:="8 ";   -- 35
  constant s36: string:="9 ";   -- 36
  constant s37: string:=""" ";   -- 37
  constant s38: string:="# ";   -- 38
  constant s39: string:="& ";   -- 39
  constant s40: string:="' ";   -- 40
  constant s41: string:="( ";   -- 41
  constant s42: string:=") ";   -- 42
  constant s43: string:="* ";   -- 43
  constant s44: string:="+ ";   -- 44
  constant s45: string:=", ";   -- 45
  constant s46: string:="- ";   -- 46
  constant s47: string:=". ";   -- 47
  constant s48: string:="/ ";   -- 48
  constant s49: string:=": ";   -- 49
  constant s50: string:="; ";   -- 50
  constant s51: string:="< ";   -- 51
  constant s52: string:="= ";   -- 52
  constant s53: string:="> ";   -- 53
  constant s54: string:="_ ";   -- 54
  constant s55: string:="| ";   -- 55
  constant s56: string:="  ";   -- 56
  constant s57: string:="a ";   -- 57
  constant s58: string:="b ";   -- 58
  constant s59: string:="c ";   -- 59
  constant s60: string:="d ";   -- 60
  constant s61: string:="e ";   -- 61
  constant s62: string:="f ";   -- 62
  constant s63: string:="g ";   -- 63
  constant s64: string:="h ";   -- 64
  constant s65: string:="i ";   -- 65
  constant s66: string:="j ";   -- 66
  constant s67: string:="k ";   -- 67
  constant s68: string:="l ";   -- 68
  constant s69: string:="m ";   -- 69
  constant s70: string:="n ";   -- 70
  constant s71: string:="o ";   -- 71
  constant s72: string:="p ";   -- 72
  constant s73: string:="q ";   -- 73
  constant s74: string:="r ";   -- 74
  constant s75: string:="s ";   -- 75
  constant s76: string:="t ";   -- 76
  constant s77: string:="u ";   -- 77
  constant s78: string:="v ";   -- 78
  constant s79: string:="w ";   -- 79
  constant s80: string:="x ";   -- 80
  constant s81: string:="y ";   -- 81
  constant s82: string:="z ";   -- 82
  constant s83: string:="! ";   -- 83
  constant s84: string:="$ ";   -- 84
  constant s85: string:="% ";   -- 85
  constant s86: string:="@ ";   -- 86
  constant s87: string:="? ";   -- 87
  constant s88: string:="[ ";   -- 88
  constant s89: string:="\ ";   -- 89
  constant s90: string:="] ";   -- 90
  constant s91: string:="^ ";   -- 91
  constant s92: string:="` ";   -- 92
  constant s93: string:="{ ";   -- 93
  constant s94: string:="} ";   -- 94
  constant s95: string:="~ ";   -- 95
  
------------------------------------------------------------
  
  constant c01: string:=('A',' ');   --  1
  constant c02: string:=('B',' ');   --  2
  constant c03: string:=('C',' ');   --  3
  constant c04: string:=('D',' ');   --  4
  constant c05: string:=('E',' ');   --  5
  constant c06: string:=('F',' ');   --  6
  constant c07: string:=('G',' ');   --  7
  constant c08: string:=('H',' ');   --  8
  constant c09: string:=('I',' ');   --  9
  constant c10: string:=('J',' ');   -- 10
  constant c11: string:=('K',' ');   -- 11
  constant c12: string:=('L',' ');   -- 12
  constant c13: string:=('M',' ');   -- 13
  constant c14: string:=('N',' ');   -- 14
  constant c15: string:=('O',' ');   -- 15
  constant c16: string:=('P',' ');   -- 16
  constant c17: string:=('Q',' ');   -- 17  
  constant c18: string:=('R',' ');   -- 18
  constant c19: string:=('S',' ');   -- 19
  constant c20: string:=('T',' ');   -- 20
  constant c21: string:=('U',' ');   -- 21
  constant c22: string:=('V',' ');   -- 22
  constant c23: string:=('W',' ');   -- 23
  constant c24: string:=('X',' ');   -- 24
  constant c25: string:=('Y',' ');   -- 25
  constant c26: string:=('Z',' ');   -- 26
  constant c27: string:=('0',' ');   -- 27
  constant c28: string:=('1',' ');   -- 28
  constant c29: string:=('2',' ');   -- 29
  constant c30: string:=('3',' ');   -- 30
  constant c31: string:=('4',' ');   -- 31
  constant c32: string:=('5',' ');   -- 32
  constant c33: string:=('6',' ');   -- 33
  constant c34: string:=('7',' ');   -- 34
  constant c35: string:=('8',' ');   -- 35
  constant c36: string:=('9',' ');   -- 36
  constant c37: string:=('"',' ');   -- 37
  constant c38: string:=('#',' ');   -- 38
  constant c39: string:=('&',' ');   -- 39
  constant c40: string:=(''',' ');   -- 40
  constant c41: string:=('(',' ');   -- 41
                         constant c42: string:=(')',' ');   -- 42
  constant c43: string:=('*',' ');   -- 43
  constant c44: string:=('+',' ');   -- 44
  constant c45: string:=(',',' ');   -- 45
  constant c46: string:=('-',' ');   -- 46
  constant c47: string:=('.',' ');   -- 47
  constant c48: string:=('/',' ');   -- 48
  constant c49: string:=(':',' ');   -- 49
  constant c50: string:=(';',' ');   -- 50
  constant c51: string:=('<',' ');   -- 51
  constant c52: string:=('=',' ');   -- 52
  constant c53: string:=('>',' ');   -- 53
  constant c54: string:=('_',' ');   -- 54
  constant c55: string:=('|',' ');   -- 55
  constant c56: string:=(' ',' ');   -- 56
  constant c57: string:=('a',' ');   -- 57
  constant c58: string:=('b',' ');   -- 58
  constant c59: string:=('c',' ');   -- 59
  constant c60: string:=('d',' ');   -- 60
  constant c61: string:=('e',' ');   -- 61
  constant c62: string:=('f',' ');   -- 62
  constant c63: string:=('g',' ');   -- 63
  constant c64: string:=('h',' ');   -- 64
  constant c65: string:=('i',' ');   -- 65
  constant c66: string:=('j',' ');   -- 66
  constant c67: string:=('k',' ');   -- 67
  constant c68: string:=('l',' ');   -- 68
  constant c69: string:=('m',' ');   -- 69
  constant c70: string:=('n',' ');   -- 70
  constant c71: string:=('o',' ');   -- 71
  constant c72: string:=('p',' ');   -- 72
  constant c73: string:=('q',' ');   -- 73
  constant c74: string:=('r',' ');   -- 74  
  constant c75: string:=('s',' ');   -- 75
  constant c76: string:=('t',' ');   -- 76
  constant c77: string:=('u',' ');   -- 77
  constant c78: string:=('v',' ');   -- 78
  constant c79: string:=('w',' ');   -- 79
  constant c80: string:=('x',' ');   -- 80
  constant c81: string:=('y',' ');   -- 81
  constant c82: string:=('z',' ');   -- 82
  constant c83: string:=('!',' ');   -- 83
  constant c84: string:=('$',' ');   -- 84
  constant c85: string:=('%',' ');   -- 85
  constant c86: string:=('@',' ');   -- 86
  constant c87: string:=('?',' ');   -- 87
  constant c88: string:=('[',' ');   -- 88
                         constant c89: string:=('\',' ');   -- 89
                         constant c90: string:=(']',' ');   -- 90
  constant c91: string:=('^',' ');   -- 91
  constant c92: string:=('`',' ');   -- 92
  constant c93: string:=('{',' ');   -- 93
                         constant c94: string:=('}',' ');   -- 94
  constant c95: string:=('~',' ');   -- 95

BEGIN
  TESTING: PROCESS
    variable k : integer := 0;
  BEGIN
    assert (s01=c01) report "problem with  1  - 'A' " severity failure;
    assert (s02=c02) report "problem with  2  - 'B' " severity failure;
    assert (s03=c03) report "problem with  3  - 'C' " severity failure;
    assert (s04=c04) report "problem with  4  - 'D' " severity failure;
    assert (s05=c05) report "problem with  5  - 'E' " severity failure;
    assert (s06=c06) report "problem with  6  - 'F' " severity failure;
    assert (s07=c07) report "problem with  7  - 'G' " severity failure;
    assert (s08=c08) report "problem with  8  - 'H' " severity failure;
    assert (s09=c09) report "problem with  9  - 'I' " severity failure;
    assert (s10=c10) report "problem with  10 - 'J' " severity failure;
    assert (s11=c11) report "problem with  11 - 'K' " severity failure;
    assert (s12=c12) report "problem with  12 - 'L' " severity failure;
    assert (s13=c13) report "problem with  13 - 'M' " severity failure;
    assert (s14=c14) report "problem with  14 - 'N' " severity failure;
    assert (s15=c15) report "problem with  15 - 'O' " severity failure;
    assert (s16=c16) report "problem with  16 - 'P' " severity failure;
    assert (s17=c17) report "problem with  17 - 'Q' " severity failure;
    assert (s18=c18) report "problem with  18 - 'R' " severity failure;
    assert (s19=c19) report "problem with  19 - 'S' " severity failure;
    assert (s20=c20) report "problem with  20 - 'T' " severity failure;
    assert (s21=c21) report "problem with  21 - 'U' " severity failure;
    assert (s22=c22) report "problem with  22 - 'V' " severity failure;
    assert (s23=c23) report "problem with  23 - 'W' " severity failure;
    assert (s24=c24) report "problem with  24 - 'X' " severity failure;
    assert (s25=c25) report "problem with  25 - 'Y' " severity failure;
    assert (s26=c26) report "problem with  26 - 'Z' " severity failure;
    assert (s27=c27) report "problem with  27 - '0' " severity failure;
    assert (s28=c28) report "problem with  28 - '1' " severity failure;
    assert (s29=c29) report "problem with  29 - '2' " severity failure;
    assert (s30=c30) report "problem with  30 - '3' " severity failure;
    assert (s31=c31) report "problem with  31 - '4' " severity failure;
    assert (s32=c32) report "problem with  32 - '5' " severity failure;
    assert (s33=c33) report "problem with  33 - '6' " severity failure;
    assert (s34=c34) report "problem with  34 - '7' " severity failure;
    assert (s35=c35) report "problem with  35 - '8' " severity failure;
    assert (s36=c36) report "problem with  36 - '9' " severity failure;
    assert (s37=c37) report "problem with  37 - '""' " severity failure;
    assert (s38=c38) report "problem with  38 - '#' " severity failure;
    assert (s39=c39) report "problem with  39 - '&' " severity failure;
    assert (s40=c40) report "problem with  40 - ''' " severity failure;
    assert (s41=c41) report "problem with  41 - '(' " severity failure;
    assert (s42=c42) report "problem with  42 - ')' " severity failure;
    assert (s43=c43) report "problem with  43 - '*' " severity failure;
    assert (s44=c44) report "problem with  44 - '+' " severity failure;
    assert (s45=c45) report "problem with  45 - ',' " severity failure;
    assert (s46=c46) report "problem with  46 - '-' " severity failure;
    assert (s47=c47) report "problem with  47 - '.' " severity failure;
    assert (s48=c48) report "problem with  48 - '/' " severity failure;
    assert (s49=c49) report "problem with  49 - ':' " severity failure;
    assert (s50=c50) report "problem with  50 - ';' " severity failure;
    assert (s51=c51) report "problem with  51 - '<' " severity failure;
    assert (s52=c52) report "problem with  52 - '=' " severity failure;
    assert (s53=c53) report "problem with  53 - '>' " severity failure;
    assert (s54=c54) report "problem with  54 - '_' " severity failure;
    assert (s55=c55) report "problem with  55 - '|' " severity failure;
    assert (s56=c56) report "problem with  56 - ' ' " severity failure;
    assert (s57=c57) report "problem with  57 - 'a' " severity failure;
    assert (s58=c58) report "problem with  58 - 'b' " severity failure;    
    assert (s59=c59) report "problem with  59 - 'c' " severity failure;
    assert (s60=c60) report "problem with  60 - 'd' " severity failure;
    assert (s61=c61) report "problem with  61 - 'e' " severity failure;
    assert (s62=c62) report "problem with  62 - 'f' " severity failure;
    assert (s63=c63) report "problem with  63 - 'g' " severity failure;
    assert (s64=c64) report "problem with  64 - 'h' " severity failure;
    assert (s65=c65) report "problem with  65 - 'i' " severity failure;
    assert (s66=c66) report "problem with  66 - 'j' " severity failure;
    assert (s67=c67) report "problem with  67 - 'k' " severity failure;
    assert (s68=c68) report "problem with  68 - 'l' " severity failure;
    assert (s69=c69) report "problem with  69 - 'm' " severity failure;
    assert (s70=c70) report "problem with  70 - 'n' " severity failure;
    assert (s71=c71) report "problem with  71 - 'o' " severity failure;
    assert (s72=c72) report "problem with  72 - 'p' " severity failure;
    assert (s73=c73) report "problem with  73 - 'q' " severity failure;
    assert (s74=c74) report "problem with  74 - 'r' " severity failure;
    assert (s75=c75) report "problem with  75 - 's' " severity failure;
    assert (s76=c76) report "problem with  76 - 't' " severity failure;
    assert (s77=c77) report "problem with  77 - 'u' " severity failure;
    assert (s78=c78) report "problem with  78 - 'v' " severity failure;
    assert (s79=c79) report "problem with  79 - 'w' " severity failure;
    assert (s80=c80) report "problem with  80 - 'x' " severity failure;
    assert (s81=c81) report "problem with  81 - 'y' " severity failure;
    assert (s82=c82) report "problem with  82 - 'z' " severity failure;
    assert (s83=c83) report "problem with  83 - '!' " severity failure;
    assert (s84=c84) report "problem with  84 - '$' " severity failure;
    assert (s85=c85) report "problem with  85 - '%' " severity failure;
    assert (s86=c86) report "problem with  86 - '@' " severity failure;
    assert (s87=c87) report "problem with  87 - '?' " severity failure;
    assert (s88=c88) report "problem with  88 - '[' " severity failure;
    assert (s89=c89) report "problem with  89 - '\' " severity failure;
    assert (s90=c90) report "problem with  90 - ']' " severity failure;
    assert (s91=c91) report "problem with  91 - '^' " severity failure;
    assert (s92=c92) report "problem with  92 - '`' " severity failure;
    assert (s93=c93) report "problem with  93 - '{' " severity failure;
    assert (s94=c94) report "problem with  94 - '}' " severity failure;
    assert (s95=c95) report "problem with  95 - '~' " severity failure;

    assert NOT(   (s01=c01) and 
                  (s02=c02) and 
                  (s03=c03) and 
                  (s04=c04) and 
                  (s05=c05) and 
                  (s06=c06) and 
                  (s07=c07) and 
                  (s08=c08) and 
                  (s09=c09) and 
                  (s10=c10) and 
                  (s11=c11) and 
                  (s12=c12) and 
                  (s13=c13) and 
                  (s14=c14) and 
                  (s15=c15) and 
                  (s16=c16) and 
                  (s17=c17) and 
                  (s18=c18) and 
                  (s19=c19) and 
                  (s20=c20) and 
                  (s21=c21) and 
                  (s22=c22) and 
                  (s23=c23) and 
                  (s24=c24) and 
                  (s25=c25) and 
                  (s26=c26) and 
                  (s27=c27) and 
                  (s28=c28) and 
                  (s29=c29) and 
                  (s30=c30) and 
                  (s31=c31) and 
                  (s32=c32) and 
                  (s33=c33) and 
                  (s34=c34) and 
                  (s35=c35) and 
                  (s36=c36) and 
                  (s37=c37) and 
                  (s38=c38) and 
                  (s39=c39) and 
                  (s40=c40) and 
                  (s41=c41) and 
                  (s42=c42) and 
                  (s43=c43) and 
                  (s44=c44) and 
                  (s45=c45) and 
                  (s46=c46) and 
                  (s47=c47) and 
                  (s48=c48) and 
                  (s49=c49) and 
                  (s50=c50) and 
                  (s51=c51) and 
                  (s52=c52) and 
                  (s53=c53) and 
                  (s54=c54) and 
                  (s55=c55) and 
                  (s56=c56) and 
                  (s57=c57) and 
                  (s58=c58) and 
                  (s59=c59) and 
                  (s60=c60) and 
                  (s61=c61) and 
                  (s62=c62) and 
                  (s63=c63) and 
                  (s64=c64) and 
                  (s65=c65) and 
                  (s66=c66) and 
                  (s67=c67) and 
                  (s68=c68) and 
                  (s69=c69) and 
                  (s70=c70) and 
                  (s71=c71) and 
                  (s72=c72) and 
                  (s73=c73) and 
                  (s74=c74) and 
                  (s75=c75) and 
                  (s76=c76) and 
                  (s77=c77) and 
                  (s78=c78) and 
                  (s79=c79) and 
                  (s80=c80) and 
                  (s81=c81) and 
                  (s82=c82) and 
                  (s83=c83) and 
                  (s84=c84) and 
                  (s85=c85) and 
                  (s86=c86) and 
                  (s87=c87) and 
                  (s88=c88) and 
                  (s89=c89) and 
                  (s90=c90) and 
                  (s91=c91) and 
                  (s92=c92) and 
                  (s93=c93) and 
                  (s94=c94) and 
                  (s95=c95) ) 
      report "***PASSED TEST: c13s06b00x00p03n01i02737"
      severity NOTE;
    assert (   (s01=c01) and 
               (s02=c02) and 
               (s03=c03) and 
               (s04=c04) and 
               (s05=c05) and 
               (s06=c06) and 
               (s07=c07) and 
               (s08=c08) and 
               (s09=c09) and 
               (s10=c10) and 
               (s11=c11) and 
               (s12=c12) and 
               (s13=c13) and 
               (s14=c14) and 
               (s15=c15) and 
               (s16=c16) and 
               (s17=c17) and 
               (s18=c18) and 
               (s19=c19) and 
               (s20=c20) and 
               (s21=c21) and 
               (s22=c22) and 
               (s23=c23) and 
               (s24=c24) and 
               (s25=c25) and 
               (s26=c26) and 
               (s27=c27) and 
               (s28=c28) and 
               (s29=c29) and 
               (s30=c30) and 
               (s31=c31) and 
               (s32=c32) and 
               (s33=c33) and 
               (s34=c34) and 
               (s35=c35) and 
               (s36=c36) and 
               (s37=c37) and 
               (s38=c38) and 
               (s39=c39) and 
               (s40=c40) and 
               (s41=c41) and 
               (s42=c42) and 
               (s43=c43) and 
               (s44=c44) and 
               (s45=c45) and 
               (s46=c46) and 
               (s47=c47) and 
               (s48=c48) and 
               (s49=c49) and 
               (s50=c50) and 
               (s51=c51) and 
               (s52=c52) and 
               (s53=c53) and 
               (s54=c54) and 
               (s55=c55) and 
               (s56=c56) and 
               (s57=c57) and 
               (s58=c58) and 
               (s59=c59) and 
               (s60=c60) and 
               (s61=c61) and 
               (s62=c62) and 
               (s63=c63) and 
               (s64=c64) and 
               (s65=c65) and 
               (s66=c66) and 
               (s67=c67) and 
               (s68=c68) and 
               (s69=c69) and 
               (s70=c70) and 
               (s71=c71) and 
               (s72=c72) and 
               (s73=c73) and 
               (s74=c74) and 
               (s75=c75) and 
               (s76=c76) and 
               (s77=c77) and 
               (s78=c78) and 
               (s79=c79) and 
               (s80=c80) and 
               (s81=c81) and 
               (s82=c82) and 
               (s83=c83) and 
               (s84=c84) and 
               (s85=c85) and 
               (s86=c86) and 
               (s87=c87) and 
               (s88=c88) and 
               (s89=c89) and 
               (s90=c90) and 
               (s91=c91) and 
               (s92=c92) and 
               (s93=c93) and 
               (s94=c94) and 
               (s95=c95) ) 
      report "***FAILED TEST: c13s06b00x00p03n01i02737 - All string literal of length 1 are equal in value to their corresponding character values."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s06b00x00p03n01i02737arch;
