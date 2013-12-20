
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
-- $Id: tc1748.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b00x00p16n01i01748ent IS
END c09s05b00x00p16n01i01748ent;

ARCHITECTURE c09s05b00x00p16n01i01748arch OF c09s05b00x00p16n01i01748ent IS
  signal gate_1    : BIT;
  signal gate_2    : BIT;
  signal data_in    : BIT;
  signal data_pass    : BIT;
  signal data_latch    : BIT;
BEGIN

  gate_1    <= '1' after 10 ns, '0' after 20 ns, '1' after 30 ns, '0' after 40 ns;
  gate_2    <= gate_1 after 1 ns;
  data_in    <=    '1' after  5 ns, '0' after 25 ns,
                   '1' after 35 ns, '0' after 36 ns,
                   '1' after 37 ns, '0' after 38 ns,
                   '1' after 39 ns, '0' after 40 ns,
                   '1' after 41 ns, '0' after 42 ns;

  B: block ((gate_1 and gate_2) = '1')
  begin
    data_pass    <=       data_in;
    data_latch    <= guarded    data_in;
  end block;

  TESTING: PROCESS(data_pass,data_latch)
    variable ok : integer := 1;
  BEGIN
    if (now = 5 ns) then
      if not(data_pass'event and data_pass = '1' and data_latch'quiet and data_latch = '0') then
        ok := 0;
      end if;
    elsif (now = 11 ns) then
      if not(data_latch'event and data_latch = '1' and data_pass'quiet and data_pass = '1') then
        ok := 0;
      end if;
    elsif (now = 25 ns) then
      if not(data_latch'quiet and data_latch = '1' and data_pass'event and data_pass = '0') then
        ok := 0;
      end if;
    elsif (now = 31 ns) then
      if not(data_latch'event and data_latch = '0' and data_pass'quiet and data_pass = '0') then
        ok := 0;
      end if;
    elsif (now = 35 ns) then
      if not(data_latch'event and data_latch = '1' and data_pass'event and data_pass = '1') then
        ok := 0;
      end if;
    elsif (now = 36 ns) then
      if not(data_latch'event and data_latch = '0' and data_pass'event and data_pass = '0') then
        ok := 0;
      end if;
    elsif (now = 37 ns) then
      if not(data_latch'event and data_latch = '1' and data_pass'event and data_pass = '1') then
        ok := 0;
      end if;
    elsif (now = 38 ns) then
      if not(data_latch'event and data_latch = '0' and data_pass'event and data_pass = '0') then
        ok := 0;
      end if;
    elsif (now = 39 ns) then
      if not(data_latch'event and data_latch = '1' and data_pass'event and data_pass = '1') then
        ok := 0;
      end if;
    elsif (now = 40 ns) then
      if not(data_latch'quiet and data_latch = '1' and data_pass'event and data_pass = '0') then
        ok := 0;
      end if;
    elsif (now = 41 ns) then
      if not(data_latch'quiet and data_latch = '1' and data_pass'event and data_pass = '1') then
        ok := 0;
      end if;
    end if;

    if (now > 41 ns) then   
      assert NOT( ok=1 )
        report "***PASSED TEST: c09s05b00x00p16n01i01748"
        severity NOTE;
      assert ( ok=1 )
        report "***FAILED TEST: c09s05b00x00p16n01i01748 - Concurrent signal assignment test failed."
        severity ERROR;
    end if;
  END PROCESS TESTING;

END c09s05b00x00p16n01i01748arch;
