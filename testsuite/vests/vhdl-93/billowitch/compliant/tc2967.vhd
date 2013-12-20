
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
-- $Id: tc2967.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s03b01x00p02n01i02967ent IS
END c02s03b01x00p02n01i02967ent;

ARCHITECTURE c02s03b01x00p02n01i02967arch OF c02s03b01x00p02n01i02967ent IS
  function "not" (I1:Bit) return bit is
  begin
    if (I1 = '1') then
      return '1';
    else
      return '0';
    end if;
  end;
BEGIN
  TESTING: PROCESS
    variable k : bit;
  BEGIN
    k := not('1');
    assert NOT(k='1')
      report "***PASSED TEST: c02s03b01x00p02n01i02967" 
      severity NOTE;
    assert (k='1')
      report "***FAILED TEST: c02s03b01x00p02n01i02967 - The subprogram specification of a unary operator must have only a single parameter."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s03b01x00p02n01i02967arch;
