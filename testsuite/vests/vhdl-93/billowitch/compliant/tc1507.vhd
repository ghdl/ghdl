
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
-- $Id: tc1507.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s08b00x00p16n01i01507ent IS
END c08s08b00x00p16n01i01507ent;

ARCHITECTURE c08s08b00x00p16n01i01507arch OF c08s08b00x00p16n01i01507ent IS

BEGIN
  TESTING: PROCESS
    variable s1, s2, s3, s4, s5, s6 : INTEGER := 0;
  BEGIN

    for i in 1 to 1000 loop
      case i is
        when 1 =>                           s1 := s1 + 1;
        when 2 | 3 =>                       s2 := s2 + 1;
        when 4 to 100 =>                    s3 := s3 + 1;
        when 200 to 201 | 300 to 350 =>     s4 := s4 + 1;
        when 400 to 450 =>                  s5 := s5 + 1;
        when others =>                      s6 := s6 + 1;
      end case;
    end loop;
    wait for 5 ns;
    assert NOT(    s1 = 1   and
                   s2 = 2   and
                   s3 = 97   and
                   s4 = 2 + 51   and
                   s5 = 51   and
                   s6 = 1000 - (s1+s2+s3+s4+s5)   )
      report "***PASSED TEST: c08s08b00x00p16n01i01507"
      severity NOTE;
    assert (    s1 = 1   and
                s2 = 2   and
                s3 = 97   and
                s4 = 2 + 51   and
                s5 = 51   and
                s6 = 1000 - (s1+s2+s3+s4+s5)   )
      report "***FAILED TEST: c08s08b00x00p16n01i01507 - Case statement execution test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s08b00x00p16n01i01507arch;
