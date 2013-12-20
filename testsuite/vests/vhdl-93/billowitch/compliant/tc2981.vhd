
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
-- $Id: tc2981.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s04b00x00p04n04i02981ent IS
END c02s04b00x00p04n04i02981ent;

ARCHITECTURE c02s04b00x00p04n04i02981arch OF c02s04b00x00p04n04i02981ent IS
  -- Define an array of integers.
  type INT_ARRAY is array( NATURAL range <> ) of INTEGER;

  -- Define a resolution function.
  function RESFUNC( S : INT_ARRAY ) return INTEGER is
    -- local variables.
    variable MAX : INTEGER := INTEGER'LOW;

  begin
    if  (S'LENGTH = 0) then
      return INTEGER'LOW;
    else
      for I in S'RANGE loop
        if  (S(I) > MAX) then
          MAX := S(I);
        end if;
      end loop;
      return MAX;
    end if;
  end;

  -- Define a subtype of integer.
  subtype SINT is RESFUNC INTEGER;

  -- Define a bus signal.
  signal B : SINT bus := 0;
BEGIN
  -- One process driving B.
  process
  begin
    -- Verify initial conditions.
    assert( B = 0 );
    
    -- Go to NULL.
    B <=       null  after 10 ns;
    
    -- Wait until B gets updated.
    wait on B;
    
    -- Verify that both drivers have been turned off.
    assert( B = INTEGER'LOW );
    
    -- Turn a driver on again.
    B <= 47 after 10 ns;
    
    -- Wait for B to be updated.
    wait on B;
    
    -- Verify that B was updated accordingly.
    assert( B = 47 );
    
    -- End of test.
    wait;
  end process;
  
  -- Second process driving B.
  process
  begin
    -- Verify initial conditions.
    assert( B = 0 );
    
    -- Go to NULL.
    B <=       null  after 10 ns;
    
    -- Wait until B gets updated.
    wait on B;
    
    -- Verify that both drivers have been turned off.
    assert( B = INTEGER'LOW );
    
    -- End of test.
    wait;
  end process;

  TESTING: PROCESS
  BEGIN
    wait for 50 ns;
    assert FALSE
      report "***PASSED TEST: c02s04b00x00p04n04i02981 - This test needs manual check to make sure there is no other ERROR or FAILURE assertion notice."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c02s04b00x00p04n04i02981arch;
