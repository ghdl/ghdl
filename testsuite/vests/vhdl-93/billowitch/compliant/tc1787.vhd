
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
-- $Id: tc1787.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity c09s06b00x00p04n05i01787ent_a is
  generic (
    g0  :   Boolean        ;
    g1  :   Bit            ;
    g2  :   Character      ;
    g3  :   SEVERITY_LEVEL ;
    g4  :   Integer        ;
    g5  :   Real           ;
    g6  :   TIME           ;
    g7  :   Natural        ;
    g8  :   Positive       ;
    g9  :   String         ;
    gA  :   Bit_vector
    );
  port    (
    port0  : out  Boolean        ;
    port1  : out  Bit            ;
    port2  : out  Character      ;
    port3  : out  SEVERITY_LEVEL ;
    port4  : out  Integer        ;
    port5  : out  Real           ;
    port6  : out  TIME           ;
    port7  : out  Natural        ;
    port8  : out  Positive       ;
    port9  : out  String         ;
    portA  : out  Bit_vector
    );
end c09s06b00x00p04n05i01787ent_a;

architecture c09s06b00x00p04n05i01787arch_a of c09s06b00x00p04n05i01787ent_a is
begin
  port0 <= g0 after 11 ns;
  port1 <= g1 after 11 ns;
  port2 <= g2 after 11 ns;
  port3 <= g3 after 11 ns;
  port4 <= g4 after 11 ns;
  port5 <= g5 after 11 ns;
  port6 <= g6 after 11 ns;
  port7 <= g7 after 11 ns;
  port8 <= g8 after 11 ns;
  port9 <= g9 after 11 ns;
  portA <= gA after 11 ns;
end c09s06b00x00p04n05i01787arch_a;

ENTITY c09s06b00x00p04n05i01787ent IS
END c09s06b00x00p04n05i01787ent;

ARCHITECTURE c09s06b00x00p04n05i01787arch OF c09s06b00x00p04n05i01787ent IS
  component MultiType
    generic (
      g0  :   Boolean        ;
      g1  :   Bit            ;
      g2  :   Character      ;
      g3  :   SEVERITY_LEVEL ;
      g4  :   Integer        ;
      g5  :   Real           ;
      g6  :   TIME           ;
      g7  :   Natural        ;
      g8  :   Positive       ;
      g9  :   String         ;
      gA  :   Bit_vector
      );
    port    (
      port0  : out  Boolean        ;
      port1  : out  Bit            ;
      port2  : out  Character      ;
      port3  : out  SEVERITY_LEVEL ;
      port4  : out  Integer        ;
      port5  : out  Real           ;
      port6  : out  TIME           ;
      port7  : out  Natural        ;
      port8  : out  Positive       ;
      port9  : out  String         ;
      portA  : out  Bit_vector
      );
  end component;
  for u1 : MultiType use entity work.c09s06b00x00p04n05i01787ent_a(c09s06b00x00p04n05i01787arch_a);

  subtype reg32    is Bit_vector ( 31 downto 0 );
  subtype string16    is String ( 1 to 16 );
  
  signal signal0  :   Boolean        ;
  signal signal1  :   Bit            ;
  signal signal2  :   Character      ;
  signal signal3  :   SEVERITY_LEVEL ;
  signal signal4  :   Integer        ;
  signal signal5  :   Real           ;
  signal signal6  :   TIME           ;
  signal signal7  :   Natural        ;
  signal signal8  :   Positive       ;
  signal signal9  :   String16       ;
  signal signalA  :   Reg32          ;
  

BEGIN
  u1 : MultiType
    generic map (
      True,
      '0',
      '@',
      NOTE,
      123456789,
      987654321.5,
      110 ns,
      12312,
      3423,
      "16 characters OK",
      B"0101_0010_1001_0101_0010_1010_0101_0100"
      )
    port map (
      signal0  ,
      signal1  ,
      signal2  ,
      signal3  ,
      signal4  ,
      signal5  ,
      signal6  ,
      signal7  ,
      signal8  ,
      signal9  ,
      signalA
      );

  TESTING: PROCESS
  BEGIN
    wait on signal0,signal1,signal2,signal3,signal4,signal5,signal6,signal7,signal8;
    assert NOT(   signal0 = True                  and 
                  signal1 = '0'                   and    
                  signal2 = '@'                  and 
                  signal3 = NOTE                and 
                  signal4 = 123456789          and 
                  signal5 = 987654321.5          and 
                  signal6 = 110 ns              and 
                  signal7 = 12312              and 
                  signal8 = 3423              and 
                  signal9 = "16 characters OK"    and 
                  signalA = B"01010010100101010010101001010100")
      report "***PASSED TEST: c09s06b00x00p04n05i01787"
      severity NOTE;
    assert (   signal0 = True                  and 
               signal1 = '0'                   and    
               signal2 = '@'                  and 
               signal3 = NOTE                and 
               signal4 = 123456789          and 
               signal5 = 987654321.5          and 
               signal6 = 110 ns              and 
               signal7 = 12312              and 
               signal8 = 3423              and 
               signal9 = "16 characters OK"    and 
               signalA = B"01010010100101010010101001010100")
      report "***FAILED TEST: c09s06b00x00p04n05i01787 - The generic map aspect, if present, should associate a single actual with each local generic in the corresponding component declaration."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c09s06b00x00p04n05i01787arch;
