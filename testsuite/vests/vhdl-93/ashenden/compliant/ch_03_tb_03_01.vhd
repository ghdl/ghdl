
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

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
-- $Id: ch_03_tb_03_01.vhd,v 1.2 2001-10-24 23:30:59 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity test_bench_03_01 is
end entity test_bench_03_01;

architecture test_thermostat_example of test_bench_03_01 is

  signal desired_temp, actual_temp : integer := 25;
  signal heater_on : boolean := false;

begin

  dut : entity work.thermostat(example)
    port map ( desired_temp => desired_temp, actual_temp => actual_temp,
               heater_on => heater_on );

  stimulus : process is
  begin
    wait for 5 sec;
    actual_temp <= 24;		wait for 5 sec;
    actual_temp <= 23;		wait for 5 sec;
    actual_temp <= 22;		wait for 5 sec;
    actual_temp <= 21;		wait for 5 sec;
    actual_temp <= 22;		wait for 5 sec;
    actual_temp <= 23;		wait for 5 sec;
    actual_temp <= 24;		wait for 5 sec;
    actual_temp <= 25;		wait for 5 sec;
    actual_temp <= 26;		wait for 5 sec;
    actual_temp <= 27;		wait for 5 sec;
    actual_temp <= 28;		wait for 5 sec;
    actual_temp <= 29;		wait for 5 sec;
    actual_temp <= 28;		wait for 5 sec;
    actual_temp <= 27;		wait for 5 sec;
    actual_temp <= 26;		wait for 5 sec;
    actual_temp <= 25;		wait for 5 sec;
    actual_temp <= 24;		wait for 5 sec;
    actual_temp <= 23;		wait for 5 sec;
    actual_temp <= 22;		wait for 5 sec;
    actual_temp <= 21;		wait for 5 sec;
    actual_temp <= 22;		wait for 5 sec;
    actual_temp <= 23;		wait for 5 sec;
    actual_temp <= 24;		wait for 5 sec;
    actual_temp <= 25;		wait for 5 sec;
    actual_temp <= 26;		wait for 5 sec;
    actual_temp <= 27;		wait for 5 sec;
    actual_temp <= 28;		wait for 5 sec;
    actual_temp <= 29;		wait for 5 sec;
    actual_temp <= 28;		wait for 5 sec;
    actual_temp <= 27;		wait for 5 sec;
    actual_temp <= 26;		wait for 5 sec;

    desired_temp <= 30;		wait for 5 sec;
    actual_temp <= 25;		wait for 5 sec;
    actual_temp <= 26;		wait for 5 sec;
    actual_temp <= 27;		wait for 5 sec;
    actual_temp <= 28;		wait for 5 sec;
    actual_temp <= 29;		wait for 5 sec;
    actual_temp <= 30;		wait for 5 sec;
    actual_temp <= 31;		wait for 5 sec;
    actual_temp <= 32;		wait for 5 sec;
    actual_temp <= 33;		wait for 5 sec;
    actual_temp <= 34;		wait for 5 sec;
    actual_temp <= 35;		wait for 5 sec;
    actual_temp <= 34;		wait for 5 sec;
    actual_temp <= 33;		wait for 5 sec;
    actual_temp <= 32;		wait for 5 sec;
    actual_temp <= 31;		wait for 5 sec;
    actual_temp <= 30;		wait for 5 sec;

    wait;
  end process stimulus;

end architecture test_thermostat_example;
