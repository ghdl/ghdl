
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
-- $Id: tc1781.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

Package c09s06b00x00p04n05i01781pkg is
  type info is record
                 field_1 : integer;
                 field_2 : real;
               end record;
  type stuff is array (Integer range 1 to 2) of info;
end c09s06b00x00p04n05i01781pkg;

use work.c09s06b00x00p04n05i01781pkg.all;
entity c09s06b00x00p04n05i01781ent_a is
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
    gA  :   Bit_vector     ;
    gB  :   stuff
    );
end c09s06b00x00p04n05i01781ent_a;

use work.c09s06b00x00p04n05i01781pkg.all;
architecture c09s06b00x00p04n05i01781arch_a of c09s06b00x00p04n05i01781ent_a is
  -- Check that the data was passed...
begin
  TESTING: PROCESS
  BEGIN
    assert NOT(   g0 = True                   and 
                  g1 = '0'                      and  
                  g2 = '@'                      and 
                  g3 = NOTE                     and 
                  g4 = 123456789                and 
                  g5 = 987654321.5              and 
                  g6 = 110  ns                   and 
                  g7 = 12312                    and 
                  g8 = 3423                     and 
                  g9 = "16 characters OK"       and 
                  gA = B"01010010100101010010101001010100"and 
                  gB = ((123, 456.7 ), (890, 135.7))) 
      report "***PASSED TEST: c09s06b00x00p04n05i01781"
      severity NOTE;
    assert (   g0 = True                   and 
               g1 = '0'                      and  
               g2 = '@'                      and 
               g3 = NOTE                     and 
               g4 = 123456789                and 
               g5 = 987654321.5              and 
               g6 = 110  ns                   and 
               g7 = 12312                    and 
               g8 = 3423                     and 
               g9 = "16 characters OK"       and 
               gA = B"01010010100101010010101001010100"and 
               gB = ((123, 456.7 ), (890, 135.7))) 
      report "***FAILED TEST: c09s06b00x00p04n05i01781 - The generic map aspect, if present, should associate a single actual with each local generic in the corresponding component declaration."
      severity ERROR;
    wait;
  END PROCESS TESTING;
end c09s06b00x00p04n05i01781arch_a;

-------------------------------------------------------------------------

ENTITY c09s06b00x00p04n05i01781ent IS
END c09s06b00x00p04n05i01781ent;

use work.c09s06b00x00p04n05i01781pkg.all;
ARCHITECTURE c09s06b00x00p04n05i01781arch OF c09s06b00x00p04n05i01781ent IS
  subtype reg32    is Bit_vector ( 31 downto 0 );
  subtype string16    is String ( 1 to 16 );
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
      gA  :   Bit_vector     ;
      gB  :   stuff
      );
  end component;
  for u1 : MultiType use entity work.c09s06b00x00p04n05i01781ent_a(c09s06b00x00p04n05i01781arch_a);

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
      B"0101_0010_1001_0101_0010_1010_0101_0100",
      gB(2) => ( 890, 135.7 ),
      gB(1) => ( 123, 456.7 )
      );

END c09s06b00x00p04n05i01781arch;
