
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
-- $Id: tc1785.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

Package c09s06b00x00p04n07i01785pkg is
  type info is record
                 field_1 : integer;
                 field_2 : real;
               end record;
  type stuff is array (Integer range 1 to 2) of info;
end c09s06b00x00p04n07i01785pkg;

use work.c09s06b00x00p04n07i01785pkg.all;
entity c09s06b00x00p04n07i01785ent_a is
  port    (
    port_0  : in Boolean        ;
    port_1  : in Bit            ;
    port_2  : in Character      ;
    port_3  : in SEVERITY_LEVEL ;
    port_4  : in Integer        ;
    port_5  : in Real           ;
    port_6  : in TIME           ;
    port_7  : in Natural        ;
    port_8  : in Positive       ;
    port_9  : in String         ; 
    port_A  : in Bit_vector     ;
    port_B  : in stuff
    );
end c09s06b00x00p04n07i01785ent_a;


use work.c09s06b00x00p04n07i01785pkg.all;
architecture c09s06b00x00p04n07i01785arch_a of c09s06b00x00p04n07i01785ent_a is
  -- Check that the data was passed...
begin
  TESTING: PROCESS(port_0,port_1,port_2,port_3,port_4,port_5,port_6,port_7,port_8)
  BEGIN
    assert NOT(   port_0 = True                and 
                  port_1 = '0'                and 
                  port_2 = '@'                  and 
                  port_3 = NOTE                and 
                  port_4 = 123456789                 and
                  port_5 = 987654321.5              and
                  port_6 = 110 ns                  and
                  port_7 = 12312                  and 
                  port_8 = 3423                  and 
                  port_9 = "16 characters OK"          and 
                  port_A = B"01010010100101010010101001010100"    and 
                  port_B = ((123, 456.7), (890, 135.7))) 
      report "***PASSED TEST: c09s06b00x00p04n07i01785"
      severity NOTE;
    assert (   port_0 = True                and 
               port_1 = '0'                and 
               port_2 = '@'                  and 
               port_3 = NOTE                and 
               port_4 = 123456789                 and
               port_5 = 987654321.5              and
               port_6 = 110 ns                  and
               port_7 = 12312                  and 
               port_8 = 3423                  and 
               port_9 = "16 characters OK"          and 
               port_A = B"01010010100101010010101001010100"    and 
               port_B = ((123, 456.7), (890, 135.7))) 
      report "***FAILED TEST: c09s06b00x00p04n07i01785 - Port map aspect associates a single actual with each local port in the corresponding component declaration test failed."
      severity ERROR;
  END PROCESS TESTING;
end c09s06b00x00p04n07i01785arch_a;

-----------------------------------------------------------------------

ENTITY c09s06b00x00p04n07i01785ent IS
END c09s06b00x00p04n07i01785ent;


use work.c09s06b00x00p04n07i01785pkg.all;
ARCHITECTURE c09s06b00x00p04n07i01785arch OF c09s06b00x00p04n07i01785ent IS
  subtype reg32    is Bit_vector ( 31 downto 0 );
  subtype string16    is String ( 1 to 16 );
  
  signal sig_0  :   Boolean        := TRUE;
  signal sig_1  :   Bit            := '0';
  signal sig_2  :   Character      := '@';
  signal sig_3  :   SEVERITY_LEVEL := NOTE;
  signal sig_4  :   Integer        := 123456789;
  signal sig_5  :   Real           := 987654321.5;
  signal sig_6  :   TIME           := 110 NS;
  signal sig_7  :   Natural        := 12312;
  signal sig_8  :   Positive       := 3423;
  signal sig_9  :   String16       := "16 characters OK";
  signal sig_A  :   REG32          := B"0101_0010_1001_0101_0010_1010_0101_0100";
  signal sig_B  :   stuff          := (( 123, 456.7 ), ( 890, 135.7 ));

  component MultiType
    port    (
      port_0  : in Boolean        ;
      port_1  : in Bit            ;
      port_2  : in Character      ;
      port_3  : in SEVERITY_LEVEL ;
      port_4  : in Integer        ;
      port_5  : in Real           ;
      port_6  : in TIME           ;
      port_7  : in Natural        ;
      port_8  : in Positive       ;
      port_9  : in String         ;
      port_A  : in Bit_vector     ;
      port_B  : in stuff
      );
  end component;
  for u1 : MultiType use entity work.c09s06b00x00p04n07i01785ent_a (c09s06b00x00p04n07i01785arch_a);

BEGIN
  u1 : MultiType
    port map (
      port_0 => sig_0,
      port_1 => sig_1,
      port_2 => sig_2,
      port_3 => sig_3,
      port_4 => sig_4,
      port_5 => sig_5,
      port_6 => sig_6,
      port_7 => sig_7,
      port_8 => sig_8,
      port_9 => sig_9,
      port_A => sig_A,
      port_B => sig_B
      );

END c09s06b00x00p04n07i01785arch;
