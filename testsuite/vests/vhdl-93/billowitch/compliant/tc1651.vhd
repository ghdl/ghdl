
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
-- $Id: tc1651.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s13b00x00p03n01i01651ent IS
  procedure passive is
  begin
    null;                             -- or is that "dull"?
  end passive;
begin
  passive;
END c08s13b00x00p03n01i01651ent;

ARCHITECTURE c08s13b00x00p03n01i01651arch OF c08s13b00x00p03n01i01651ent IS
  function troo return boolean is
  begin
    null;
    return true;
  end troo;
BEGIN
  TESTING: PROCESS
    variable v1 : integer := 1;
    variable v2 : integer := 0;
  BEGIN
    if v1 > v2 then
      null;
    elsif v1 < v2 then
      null;
    else
      null;
    end if;

    case troo is
      when false => null;
      when true => null;
    end case;

    loop
      null;
      exit;                         -- jump out of the infinite loop
    end loop;

    null;
    assert FALSE 
      report "***PASSED TEST: c08s13b00x00p03n01i01651"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c08s13b00x00p03n01i01651arch;
