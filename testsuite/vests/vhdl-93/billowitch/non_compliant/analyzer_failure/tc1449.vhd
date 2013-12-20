
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
-- $Id: tc1449.vhd,v 1.2 2001-10-26 16:30:10 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s07b00x00p02n01i01449ent IS
END c08s07b00x00p02n01i01449ent;

ARCHITECTURE c08s07b00x00p02n01i01449arch OF c08s07b00x00p02n01i01449ent IS

  type boolean_enum is (less_than, equal_to, greater_than);

  function be_compare ( constant i1, i2 : integer ) return boolean_enum is
  begin
    --
    -- This if statement has two else clauses; the second one
    -- is illegal.
    --
    if i1 < i2 then
      return less_than;
    else                              -- This 'else' is OK
      return greater_than;
    else                              -- This 'else' is illegal
      return equal_to;
    end if;
  end be_compare;

begin
  TESTING: process
    variable be_val : boolean_enum;   -- function return value
    variable v1, v2 : integer := 0;   -- equal test values
  begin
    --
    -- This first function call should get an error message
    -- if it even gets that far.
    --
    be_val := be_compare(v1,v2);

    assert FALSE 
      report "***FAILED TEST: c08s07b00x00p02n01i01449 - If statement can only have one else clause." 
      severity ERROR;

    wait;
  end process TESTING;

END c08s07b00x00p02n01i01449arch;
