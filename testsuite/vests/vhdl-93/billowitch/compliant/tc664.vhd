
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
-- $Id: tc664.vhd,v 1.3 2001-10-29 02:12:46 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------


--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Tue Nov  5 16:37:57 1996  --
--                 ****************************               --



ENTITY c03s04b01x00p01n01i00664ent IS
END c03s04b01x00p01n01i00664ent;

ARCHITECTURE c03s04b01x00p01n01i00664arch OF c03s04b01x00p01n01i00664ent IS

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

  type    array_rec_std       is array (integer range <>) of record_std_package;
  type    array_rec_std_file    is file of array_rec_std;

  constant   C26 : record_std_package    := (true,'1','s',note,3,3.0,3 ns,3,3);
  constant   C57 : array_rec_std(0 to 7)    := (others => C26);

BEGIN
  TESTING: PROCESS
    file filein : array_rec_std_file open write_mode is "iofile.11";
  BEGIN
    for i in 1 to 100 loop
      write(filein,C57);
    end loop;
    assert FALSE
      report "***PASSED TEST: c03s04b01x00p01n01i00664 - The output file will be verified by test s010114.vhd"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c03s04b01x00p01n01i00664arch;
