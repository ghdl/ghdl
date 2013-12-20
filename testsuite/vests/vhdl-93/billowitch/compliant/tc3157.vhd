
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
-- $Id: tc3157.vhd,v 1.2 2001-10-26 16:29:52 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c05s03b00x00p16n01i03157ent IS
  -- Define resolution function for SIG:
  function RESFUNC( S : BIT_VECTOR ) return BIT is
  begin
    for I in S'RANGE loop
      if (S(I) = '1') then
        return '1';
      end if;
    end loop;
    return '0';
  end RESFUNC;

  -- Define the signal.
  subtype RBIT is RESFUNC BIT;
  signal SIG : RBIT bus;

  -- Use the implicit disconnect specification here.

  -- Define the GUARD signal.
  signal GUARD : BOOLEAN := FALSE;
begin
END c05s03b00x00p16n01i03157ent;

ARCHITECTURE c05s03b00x00p16n01i03157arch OF c05s03b00x00p16n01i03157ent IS

BEGIN

  -- Define the guarded signal assignment.
  L1: block
  begin
    SIG <= guarded '1';
  end block L1;


  TESTING: PROCESS
    variable ShouldBeTime : TIME;
  BEGIN
    -- 1. Turn on the GUARD, verify that SIG gets toggled.
    GUARD <= TRUE;
    ShouldBeTime := NOW;
    wait on SIG;
    assert( SIG = '1' ) severity FAILURE;
    assert( ShouldBeTime = NOW ) severity FAILURE;
    
    -- 2. Turn off the GUARD, verify that SIG gets turned OFF.
    GUARD <= FALSE;
    ShouldBeTime := NOW;
    wait on SIG;
    assert( SIG = '0' ) severity FAILURE;
    assert( ShouldBeTime = NOW ) severity FAILURE;

    assert NOT( SIG = '0' and ShouldBeTime = NOW )
      report "***PASSED TEST: c05s03b00x00p16n01i03157"
      severity NOTE;
    assert ( SIG = '0' and ShouldBeTime = NOW )
      report "***FAILED TEST: c05s03b00x00p16n01i03157 - Default disconnect specification test failed."
      severity ERROR;
    
    -- Define a second driver for SIG, just for kicks.
    -- Should never get invoked.  Not have an effect on the value.
    SIG <= '0' after 10 ns;
    wait;
  END PROCESS TESTING;

END c05s03b00x00p16n01i03157arch;
