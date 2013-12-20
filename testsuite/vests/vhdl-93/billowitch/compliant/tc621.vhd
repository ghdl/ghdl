
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
-- $Id: tc621.vhd,v 1.3 2001-10-29 02:12:45 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------


--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Tue Nov  5 16:37:45 1996  --
--                 ****************************               --



--                 ****************************                   --
-- Reversed to VHDL 87 by reverse87.pl - Tue Nov  5 11:26:07 1996  --
--                 ****************************                    --



--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Mon Nov  4 17:36:24 1996  --
--                 ****************************               --



ENTITY c03s04b01x00p01n01i00621ent IS
END c03s04b01x00p01n01i00621ent;

ARCHITECTURE c03s04b01x00p01n01i00621arch OF c03s04b01x00p01n01i00621ent IS
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
  
  type record_array_st is record
                            a:boolean_vector_st;
                            b:severity_level_vector_st;
                            c:integer_vector_st;
                            d:real_vector_st;
                            e:time_vector_st;
                            f:natural_vector_st;
                            g:positive_vector_st;
                          end record;
  type record_array_st_file is file of record_array_st;
  
  constant C1 : boolean   := true;
  constant C4 : severity_level    := note;
  constant C5 : integer   := 3;
  constant C6 : real      := 3.0;
  constant C7 : time      := 3 ns;
  constant C8 : natural   := 3;
  constant C9 : positive  := 3;
  
  constant C28 : boolean_vector_st        :=(others => C1);
  constant C29 : severity_level_vector_st :=(others => C4);
  constant C30 : integer_vector_st        :=(others => C5);
  constant C31 : real_vector_st           :=(others => C6);
  constant C32 : time_vector_st           :=(others => C7);
  constant C33 : natural_vector_st        :=(others => C8);
  constant C34 : positive_vector_st       :=(others => C9);
  
  constant C35 : record_array_st  := (C28,C29,C30,C31,C32,C33,C34);
  signal       k    : integer := 0;
BEGIN
  TESTING: PROCESS
    file filein    : record_array_st_file open read_mode is "iofile.34";
    variable  v    : record_array_st;
  BEGIN
    for i in 1 to 100 loop
      assert(endfile(filein) = false) report"end of file reached before expected";
      read(filein,v);
      if (v /= C35) then
        k <= 1;
      end if;
    end loop;
    wait for 1 ns;
    assert NOT(k = 0)
      report "***PASSED TEST: c03s04b01x00p01n01i00621"
      severity NOTE;
    assert (k = 0)
      report "***FAILED TEST: c03s04b01x00p01n01i00621 - File reading operation (record_array_st file type) failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s04b01x00p01n01i00621arch;
