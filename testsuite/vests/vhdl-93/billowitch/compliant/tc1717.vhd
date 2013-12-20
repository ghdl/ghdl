
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
-- $Id: tc1717.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s02b00x00p13n01i01717ent IS
END c09s02b00x00p13n01i01717ent;

ARCHITECTURE c09s02b00x00p13n01i01717arch OF c09s02b00x00p13n01i01717ent IS
  -- Local signals.
  signal A, B : BIT := '0';
BEGIN
  TESTING: PROCESS
    -- Local variables.
    variable STARTED: BOOLEAN    := FALSE;
    variable OldTime: TIME    := 250 ns;
    variable OldInt : INTEGER    := 13;
    variable OldA,
      OldB   : BIT;
    variable I      : INTEGER;
  BEGIN
    -- Initialize variables for this first pass.
    if  (NOT(STARTED)) then
      OldTime := NOW;
      OldInt  := 47;
      OldA    := A;
      OldB    := B;
      I       := 0;
      STARTED := TRUE;
    elsif (I > 15) then
      assert NOT(I = 16) 
        report "***PASSED TEST: c09s02b00x00p13n01i01717"
        severity NOTE;
      assert (I = 16) 
        report "***FAILED TEST: c09s02b00x00p13n01i01717 - The execution of a process statement consists of the repetitive execution of its sequence of statements."
        severity ERROR;
      wait;
    end if;
    -- Verify that no variables, time or signals have changed.
    assert( OldInt = 47 )   severity ERROR;
    assert( OldTime = NOW ) severity ERROR;
    assert( OldA = A )      severity ERROR;
    assert( OldB = B )      severity ERROR;
    I := I + 1;
  END PROCESS TESTING;

  -- This process merely makes assignments to the signals A and B.
  ASSIGN_PROCESS:   process
  begin
    A <= '1' ;
    B <= '1';
    wait;
  end process;

END c09s02b00x00p13n01i01717arch;
