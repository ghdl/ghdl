
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
-- $Id: tc669.vhd,v 1.3 2001-10-29 02:12:46 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------


--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Tue Nov  5 16:37:58 1996  --
--                 ****************************               --



--                 ****************************                   --
-- Reversed to VHDL 87 by reverse87.pl - Tue Nov  5 11:26:27 1996  --
--                 ****************************                    --



--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Mon Nov  4 17:36:37 1996  --
--                 ****************************               --



ENTITY c03s04b01x00p01n01i00669ent IS
END c03s04b01x00p01n01i00669ent;

ARCHITECTURE c03s04b01x00p01n01i00669arch OF c03s04b01x00p01n01i00669ent IS

  type boolean_vector     is array (natural range <>) of boolean;
  type severity_level_vector is array (natural range <>) of severity_level;
  type integer_vector     is array (natural range <>) of integer;
  type real_vector        is array (natural range <>) of real;
  type time_vector        is array (natural range <>) of time;
  type natural_vector     is array (natural range <>) of natural;
  type positive_vector    is array (natural range <>) of positive;
  
  subtype boolean_vector_st       is boolean_vector(0 to 15);
  subtype severity_level_vector_st is  severity_level_vector(0 to 15);
  subtype integer_vector_st       is  integer_vector(0 to 15);
  subtype real_vector_st          is  real_vector(0 to 15);
  subtype time_vector_st          is  time_vector(0 to 15);
  subtype natural_vector_st       is  natural_vector(0 to 15);
  subtype positive_vector_st      is  positive_vector(0 to 15);
  
  type boolean_cons_vector        is array (15 downto 0) of boolean;
  type severity_level_cons_vector is array (15 downto 0) of severity_level;
  type integer_cons_vector        is array (15 downto 0) of integer;
  type real_cons_vector           is array (15 downto 0) of real;
  type time_cons_vector           is array (15 downto 0) of time;
  type natural_cons_vector        is array (15 downto 0) of natural;
  type positive_cons_vector       is array (15 downto 0) of positive;
  
  type record_std_package is record
                               a:boolean;
                               b:bit;
                               c:character;
                               d:severity_level;
                               e:integer;
                               f:real;
                               g:time;
                               h:natural;
                               i:positive;
                             end record;
  
  type record_array_st is record
                            a:boolean_vector_st;
                            b:severity_level_vector_st;
                            c:integer_vector_st;
                            d:real_vector_st;
                            e:time_vector_st;
                            f:natural_vector_st;
                            g:positive_vector_st;
                          end record;
  
  type record_cons_array is record
                              a:boolean_cons_vector;
                              b:severity_level_cons_vector;
                              c:integer_cons_vector;
                              d:real_cons_vector;
                              e:time_cons_vector;
                              f:natural_cons_vector;
                              g:positive_cons_vector;
                            end record;
  
  type record_of_records is record
                              a: record_std_package;
                              c: record_cons_array;
                              i: record_array_st;
                            end record;
  
  type    array_rec_rec           is array (integer range <>) of record_of_records;
  type    array_rec_rec_file      is file of array_rec_rec;
  
  constant C1 : boolean           := true;
  constant C2 : bit               := '1';
  constant C3 : character         := 's';
  constant C4 : severity_level    := note;
  constant C5 : integer           := 3;
  constant C6 : real              := 3.0;
  constant C7 : time              := 3 ns;
  constant C8 : natural           := 3;
  constant C9 : positive          := 3;
  constant C10 : string           := "shishir";
  constant C11 : bit_vector               := B"0011";
  constant C12 : boolean_vector           := (true,false);
  constant C13 : severity_level_vector    := (note,error);
  constant C14 : integer_vector           := (1,2,3,4);
  constant C15 : real_vector              := (1.0,2.0,3.0,4.0);
  constant C16 : time_vector              := (1 ns, 2 ns, 3 ns, 4 ns);
  constant C17 : natural_vector           := (1,2,3,4);
  constant C18 : positive_vector          := (1,2,3,4);
  constant C19 : boolean_cons_vector      := (others => C1);
  constant C20 : severity_level_cons_vector := (others => C4);
  constant C21 : integer_cons_vector      := (others => C5);
  constant C22 : real_cons_vector         := (others => C6);
  constant C23 : time_cons_vector         := (others => C7);
  constant C24 : natural_cons_vector      := (others => C8);
  constant C25 : positive_cons_vector     := (others => C9);
  constant C26 : record_std_package       := (C1,C2,C3,C4,C5,C6,C7,C8,C9);
  constant C27 : record_cons_array        := (C19,C20,C21,C22,C23,C24,C25);
  constant C28 : boolean_vector_st        := (others => C1);
  constant C29 : severity_level_vector_st := (others => C4);
  constant C30 : integer_vector_st        := (others => C5);
  constant C31 : real_vector_st           := (others => C6);
  constant C32 : time_vector_st           := (others => C7);
  constant C33 : natural_vector_st        := (others => C8);
  constant C34 : positive_vector_st       := (others => C9);
  constant C35 : record_array_st          := (C28,C29,C30,C31,C32,C33,C34);
  
  constant C37 : record_of_records        := (C26,C27,C35);
  
  constant C59: array_rec_rec(0 to 7)     :=(others => C37);

  signal     k : integer := 0;

BEGIN
  TESTING: PROCESS
    file filein    : array_rec_rec_file open read_mode is "iofile.15";
    variable   v    : array_rec_rec(0 to 7);
    variable   len   : natural;
  BEGIN
    for i in 1 to 100 loop
      assert(endfile(filein) = false) report"end of file reached before expected";
      read(filein,v,len);
      assert(len = 8) report "wrong length passed during read operation";
      if (v /= C59) then
        k <= 1;
      end if;
    end loop;
    wait for 1 ns;
    assert NOT(k = 0)
      report "***PASSED TEST: c03s04b01x00p01n01i00669"
      severity NOTE;
    assert (k = 0)
      report "***FAILED TEST: c03s04b01x00p01n01i00669 - File reading of array_rec_rec_file operation failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s04b01x00p01n01i00669arch;
