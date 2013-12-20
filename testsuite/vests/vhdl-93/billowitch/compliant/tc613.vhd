
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
-- $Id: tc613.vhd,v 1.3 2001-10-29 02:12:45 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------


--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Tue Nov  5 16:37:43 1996  --
--                 ****************************               --



--                 ****************************                   --
-- Reversed to VHDL 87 by reverse87.pl - Tue Nov  5 11:26:03 1996  --
--                 ****************************                    --



--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Mon Nov  4 17:36:22 1996  --
--                 ****************************               --



ENTITY c03s04b01x00p01n01i00613ent IS
END c03s04b01x00p01n01i00613ent;

ARCHITECTURE c03s04b01x00p01n01i00613arch OF c03s04b01x00p01n01i00613ent IS
  constant C1 : boolean   := true;
  constant C4 : severity_level    := note;
  constant C5 : integer   := 3;
  constant C6 : real      := 3.0;
  constant C7 : time      := 3 ns;
  constant C8 : natural   := 3;
  constant C9 : positive  := 3;
  type boolean_cons_vector    is array (15 downto 0) of boolean;
  type severity_level_cons_vector is array (15 downto 0) of severity_level;
  type integer_cons_vector    is array (15 downto 0) of integer;
  type real_cons_vector       is array (15 downto 0) of real;
  type time_cons_vector       is array (15 downto 0) of time;
  type natural_cons_vector    is array (15 downto 0) of natural;
  type positive_cons_vector    is array (15 downto 0) of positive;
  constant C19 : boolean_cons_vector      := (others => C1);
  constant C20 : severity_level_cons_vector       := (others => C4);
  constant C21 : integer_cons_vector      := (others => C5);
  constant C22 : real_cons_vector         := (others => C6);
  constant C23 : time_cons_vector         := (others => C7);
  constant C24 : natural_cons_vector      := (others => C8);
  constant C25 : positive_cons_vector     := (others => C9);
  type record_cons_array is record
                              a:boolean_cons_vector;
                              b:severity_level_cons_vector;
                              c:integer_cons_vector;
                              d:real_cons_vector;
                              e:time_cons_vector;
                              f:natural_cons_vector;
                              g:positive_cons_vector;
                            end record;
  type record_cons_array_file is file of record_cons_array;
  constant C27 : record_cons_array := (C19,C20,C21,C22,C23,C24,C25);
  signal    k : integer := 0;
BEGIN
  TESTING: PROCESS
    file filein    : record_cons_array_file open read_mode is "iofile.34";
    variable  v    : record_cons_array;
  BEGIN
    for i in 1 to 100 loop
      assert(endfile(filein) = false) report"end of file reached before expected";
      read(filein,v);
      if (v /= C27) then
        k <= 1;
      end if;
    end loop;
    wait for 1 ns;
    assert NOT(k = 0)
      report "***PASSED TEST: c03s04b01x00p01n01i00613"
      severity NOTE;
    assert (k = 0)
      report "***FAILED TEST: c03s04b01x00p01n01i00613 - File reading operation (record_cons_array file type) failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s04b01x00p01n01i00613arch;
