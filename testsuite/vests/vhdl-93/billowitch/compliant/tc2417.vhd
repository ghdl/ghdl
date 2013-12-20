
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
-- $Id: tc2417.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x00p10n02i02417ent IS
END c07s03b02x00p10n02i02417ent;

ARCHITECTURE c07s03b02x00p10n02i02417arch OF c07s03b02x00p10n02i02417ent IS

BEGIN
  TESTING: PROCESS
    type rec is record
                  ele_1 : integer;
                  ele_2 : real;
                  ele_3 : boolean;
                  ele_4 : character;
                  ele_5 : bit;
                  ele_6 : time;
                  ele_7 : severity_level;
                end record;
    variable v24 : rec;
  BEGIN
    v24 := (ele_1=>23,ele_2=>1.4,ele_3=>True,ele_4=>'C',ele_5=>'1',ele_6=>1 ns,ele_7=>error);      
    assert NOT(   v24.ele_1 = 23      and
                  v24.ele_2 = 1.4      and
                  v24.ele_3 = True   and
                  v24.ele_4 = 'C'      and
                  v24.ele_5 = '1'      and
                  v24.ele_6 = 1 ns   and
                  v24.ele_7 = error   )
      report "***PASSED TEST: c07s03b02x00p10n02i02417"
      severity NOTE;
    assert (   v24.ele_1 = 23      and
               v24.ele_2 = 1.4      and
               v24.ele_3 = True   and
               v24.ele_4 = 'C'      and
               v24.ele_5 = '1'      and
               v24.ele_6 = 1 ns   and
               v24.ele_7 = error   )
      report "***FAILED TEST: c07s03b02x00p10n02i02417 - Elements of an aggregate should have the same type as that determined by the aggregate."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x00p10n02i02417arch;
