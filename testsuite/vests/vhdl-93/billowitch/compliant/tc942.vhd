
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
-- $Id: tc942.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c10s05b00x00p05n01i00942ent IS
END c10s05b00x00p05n01i00942ent;

ARCHITECTURE c10s05b00x00p05n01i00942arch OF c10s05b00x00p05n01i00942ent IS
  type three_state is ( '0','1','X');

  function "and" (l,r : three_state) return three_state is
  begin
    if    (l = '0') or  (r = '0') then return('0');
    elsif (l = '1') and (r = '1') then return('1');
    else  return ('X');
    end if;
  end;

BEGIN
  TESTING:PROCESS
  BEGIN
    assert NOT( ('1' and 'X') = 'X' )
      report "***PASSED TEST: c10s05b00x00p05n01i00942"
      severity NOTE;
    assert ( ('1' and 'X') = 'X' )
      report "***FAILED TEST: c10s05b00x00p05n01i00942 - A name or expression have a certain type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s05b00x00p05n01i00942arch;
