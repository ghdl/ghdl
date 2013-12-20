
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
-- $Id: tc1327.vhd,v 1.2 2001-10-26 16:29:40 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s04b01x00p03n02i01327ent IS
END c08s04b01x00p03n02i01327ent;

ARCHITECTURE c08s04b01x00p03n02i01327arch OF c08s04b01x00p03n02i01327ent IS
  -- enumerated types.
  type    SWITCH_LEVEL is ('0', '1', 'X');
  subtype LOGIC_SWITCH is SWITCH_LEVEL range '0' to '1';

  -- integer types.
  type POSITIVE        is range 0 to INTEGER'HIGH;

  -- user defined physical types.
  type DISTANCE is range 0 to 1E9
    units
      -- Base units.
      A;                    -- angstrom

      -- Metric lengths.
      nm       = 10 A;      -- nanometer
      um       = 1000 nm;   -- micrometer (or micron)
      mm       = 1000 um;   -- millimeter
      cm       = 10 mm;     -- centimeter

      -- English lengths.
      mil      = 254000 A;  -- mil
      inch     = 1000 mil;  -- inch
    end units;

  -- floating point types.
  type POSITIVE_R    is range 0.0 to REAL'HIGH;

  -- array types.
  type MEMORY is array(INTEGER range <>) of BIT;
  type WORD   is array(0 to 31) of BIT;
  type BYTE   is array(7 downto 0) of BIT;
  
  -- record types.
  type DATE is
    record
      DAY           : INTEGER range 1 to 31;
      MONTH         : INTEGER range 1 to 12;
      YEAR          : INTEGER range -10000 to 1988;
    end record;
  
  -- Signals with no resolution function.
  signal SWITCHSIG : SWITCH_LEVEL;
  signal LOGICSIG  : LOGIC_SWITCH;
  signal CHARSIG   : CHARACTER;
  signal BOOLSIG   : BOOLEAN;
  signal SEVERSIG  : SEVERITY_LEVEL;
  signal INTSIG    : INTEGER;
  signal POSSIG    : POSITIVE;
  signal DISTSIG   : DISTANCE;
  signal TIMESIG   : TIME;
  signal REALSIG   : REAL;
  signal POSRSIG   : POSITIVE_R;
  signal BYTESIG   : BYTE;
  signal RECSIG    : DATE;

  -- Composite signals with resolution functions on the scalar subelements.

BEGIN
  TESTING: PROCESS
    -- local variables
    variable ShouldBeTime : TIME := 0 ns;

    variable k : integer := 0;
  BEGIN
    -- Test each signal assignment.
    SWITCHSIG <= '1' after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on SWITCHSIG;
    if (ShouldBeTime /= now or switchsig /= '1') then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (SWITCHSIG = '1');
    
    LOGICSIG <= '1' after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on LOGICSIG;
    if (ShouldBeTime /= now or logicsig /= '1') then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (LOGICSIG = '1');
    
    CHARSIG <= '1' after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on CHARSIG;
    if (ShouldBeTime /= now or charsig /= '1') then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (CHARSIG = '1');
    
    BOOLSIG <= TRUE after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on BOOLSIG;
    if (ShouldBeTime /= now or boolsig /= true) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (BOOLSIG = TRUE);
    
    SEVERSIG <= ERROR after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on SEVERSIG;
    if (ShouldBeTime /= now or seversig /= error) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (SEVERSIG = ERROR);
    
    INTSIG <= 47 after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on INTSIG;
    if (ShouldBeTime /= now or intsig /= 47) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (INTSIG = 47);
    
    POSSIG <= 47 after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on POSSIG;
    if (ShouldBeTime /= now or possig /= 47) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (POSSIG = 47);
    
    DISTSIG <= 1 A after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on DISTSIG;
    if (ShouldBeTime /= now or distsig /= 1 A) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (DISTSIG = 1 A);
    
    TIMESIG <= 10 ns after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on TIMESIG;
    if (ShouldBeTime /= now or timesig /= 10 ns) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (TIMESIG = 10 ns);
    
    REALSIG <= 47.0 after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on REALSIG;
    if (ShouldBeTime /= now or realsig /= 47.0) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (REALSIG = 47.0);
    
    POSRSIG <= 47.0 after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on POSRSIG;
    if (ShouldBeTime /= now or posrsig /= 47.0) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (POSRSIG = 47.0);
    
    BYTESIG <= B"10101010" after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on BYTESIG;
    if (ShouldBeTime /= now or bytesig /= B"10101010") then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (BYTESIG = B"10101010");
    
    RECSIG <= ( DAY => 14, MONTH => 2, YEAR => 1988 ) after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on RECSIG;
    if (ShouldBeTime /= now or recsig.day /= 14 or recsig.month /= 2 or recsig.year /= 1988) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (RECSIG.DAY = 14);
    assert (RECSIG.MONTH = 2);
    assert (RECSIG.YEAR = 1988);

    assert NOT( k=0 )
      report "***PASSED TEST: c08s04b01x00p03n02i01327"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c08s04b01x00p03n02i01327 - Evaluation of waveform elements is used to specify that driver is to assign a particular value to a target at the specified time."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s04b01x00p03n02i01327arch;
