
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
-- $Id: tc2874.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s01b00x00p06n05i02874ent IS
  function "+" (I1:Bit) return bit;  --- No_Failure_here
  
  function "+" (I1:Bit) return bit is
  begin
    if (I1 = '1') then
      return '1';
    else
      return '0';
    end if;
  end;
END c02s01b00x00p06n05i02874ent;

ARCHITECTURE c02s01b00x00p06n05i02874arch OF c02s01b00x00p06n05i02874ent IS
BEGIN
  TESTING: PROCESS
    variable k : bit := '0';
  BEGIN
    k := "+"('1');
    assert NOT(k='1')
      report "***PASSED TEST: c02s01b00x00p06n05i02874" 
      severity NOTE;
    assert (k='1')
      report "***FAILED TEST: c02s01b00x00p06n05i02874 - Operator symbol as the function designator test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s01b00x00p06n05i02874arch;
