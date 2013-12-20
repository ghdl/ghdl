
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
-- $Id: tc1776.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b02x00p11n01i01776ent IS
END c09s05b02x00p11n01i01776ent;

ARCHITECTURE c09s05b02x00p11n01i01776arch OF c09s05b02x00p11n01i01776ent IS
  SUBTYPE string_30 is STRING(1 to 30);
  SUBTYPE string_4  is STRING(1 to 4);
  CONSTANT    str    : string_30 := "1234567890abcdefghijlkmnopqrst";
  SIGNAL    s    : bit;
BEGIN

  -- test point
  with string_4'(str(1 to 4)) select
    s <=    '1' after 10 ns when "1234",
    '0' after 10 ns when OTHERS;

  TESTING : PROCESS(s)
  BEGIN
    if (now = 10 ns) then
      assert NOT(s='1') 
        report "***PASSED TEST: c09s05b02x00p11n01i01776" 
        severity NOTE;
      assert (s='1')
        report "***FAILED TEST: c09s05b02x00p11n01i01776 - Qualified expression used as the expression in a selected signal assignment fialed." 
        severity ERROR;
    end if;
  END PROCESS TESTING;

END c09s05b02x00p11n01i01776arch;
