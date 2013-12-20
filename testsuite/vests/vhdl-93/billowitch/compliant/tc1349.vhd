
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
-- $Id: tc1349.vhd,v 1.2 2001-10-26 16:29:40 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s04b01x00p10n01i01349ent IS
END c08s04b01x00p10n01i01349ent;

ARCHITECTURE c08s04b01x00p10n01i01349arch OF c08s04b01x00p10n01i01349ent IS
  -- Local signals.
  signal S : BIT := '0';
BEGIN
  TESTING: PROCESS

    -- local variables.
    variable S_INITIAL : BIT;
    variable ShouldBeTime : TIME;

    variable k : integer := 0;

  BEGIN
    -- 0. Keep around the initial value of S.
    S_INITIAL := S;

    -- 1. When no preemption necessary, verify the results.  INERTIAL SAME AS TRANSPORT.
    S <= (not S) after 10 ns, (S) after 20 ns;
    -- a. Wait for first transaction.
    ShouldBeTime := NOW + 10 ns;
    wait on S;
    if (ShouldBeTime /= now and S /= not S_INITIAL) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (S = (not S_INITIAL));

    -- b. Wait for second transaction.
    ShouldBeTime := NOW + 10 ns;
    wait on S;
    if (ShouldBeTime /= now and S /= S_INITIAL) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (S = S_INITIAL);

    -- 2. Preempt a transaction which is to occur at the same time as second one.
    --    INERTIAL SAME AS TRANSPORT.
    S_INITIAL := S;
    S <= (S) after 10 ns;
    S <= (not S) after 10 ns;     -- Should preempt first transaction.
    -- a. Verify that the second transaction comes as expected.
    ShouldBeTime := NOW + 10 ns;
    wait on S;
    if (ShouldBeTime /= now and S /= not S_INITIAL) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (S = (not S_INITIAL));
    
    -- b. Verify that the first transaction has been preempted.
    ShouldBeTime := NOW + 10 ns;
    wait on S for 10 ns;
    if (ShouldBeTime /= now ) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    
    -- 3. Preempt a transaction which is to occur at a later time than second one.
    --   INERTIAL SAME AS TRANSPORT.
    S_INITIAL := S;
    S <= (S) after 15 ns;
    S <= (not S) after 10 ns;     -- Should preempt first transaction.
    -- a. Verify that the second transaction comes as expected.
    ShouldBeTime := NOW + 10 ns;
    wait on S;
    if (ShouldBeTime /= now and S /= not S_INITIAL) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (S = (not S_INITIAL));
    
    -- b. Verify that the first transaction has been preempted.
    ShouldBeTime := NOW + 10 ns;
    wait on S for 10 ns;
    if (ShouldBeTime /= now ) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    
    -- 4. Preempt multiple transactions.  INERTIAL SAME AS TRANSPORT.
    S_INITIAL := S;
    S <= (S) after 15 ns, (not S) after 30 ns;
    S <= (not S) after 10 ns, (S) after 20 ns;
    -- a. Verify that the second transactions come as expected.
    ShouldBeTime := NOW + 10 ns;
    wait on S;
    if (ShouldBeTime /= now and S /= not S_INITIAL) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (S = (not S_INITIAL));
    ShouldBeTime := NOW + 10 ns;
    wait on S;
    if (ShouldBeTime /= now and S /= S_INITIAL) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (S = S_INITIAL);
    
    -- b. Verify that the first transactions have been preempted.
    ShouldBeTime := NOW + 40 ns;
    wait on S for 40 ns;
    if (ShouldBeTime /= now ) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    
    -- 5. Preempt transactions which occur before the second inertial assignment.
    S_INITIAL := S;
    S <= (S) after 5 ns;
    S <= (not S) after 10 ns, (S) after 20 ns;
    -- a. Verify that the second transactions come as expected.
    ShouldBeTime := NOW + 10 ns;
    wait on S;
    if (ShouldBeTime /= now and S /= not S_INITIAL) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (S = (not S_INITIAL));
    ShouldBeTime := NOW + 10 ns;
    wait on S;
    if (ShouldBeTime /= now and S /= S_INITIAL) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (S = S_INITIAL);
    
    -- b. Verify that the first transactions have been preempted.
    ShouldBeTime := NOW + 40 ns;
    wait on S for 40 ns;
    if (ShouldBeTime /= now ) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);

    -- 6. Don't preempt transactions which occur before the second inertial assignment.
    S_INITIAL := S;
    S <= (not S) after 5 ns;
    S <= (not S) after 10 ns, (S) after 20 ns;
    -- a. Verify that the first transaction was NOT preempted.
    ShouldBeTime := NOW + 5 ns;
    wait on S;
    if (ShouldBeTime /= now and S /= not S_INITIAL) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (S = (not S_INITIAL));
    ShouldBeTime := NOW + 15 ns;
    wait on S;
    if (ShouldBeTime /= now and S /= S_INITIAL) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (S = S_INITIAL);
    
    -- b. Verify that there are no more transactions.
    ShouldBeTime := NOW + 40 ns;
    wait on S for 40 ns;
    if (ShouldBeTime /= now ) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);

    assert NOT( k=0 )
      report "***PASSED TEST: c08s04b01x00p10n01i01349"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c08s04b01x00p10n01i01349 - Interial signal assignment test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s04b01x00p10n01i01349arch;
