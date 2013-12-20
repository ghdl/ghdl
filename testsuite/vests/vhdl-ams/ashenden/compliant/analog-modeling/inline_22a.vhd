
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

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

library ieee;  use ieee.std_logic_1164.all;
library ieee_proposed;  use ieee_proposed.electrical_systems.all;
                        
entity inline_22a is

end entity inline_22a;


architecture test of inline_22a is

  signal clock : bit;
  quantity q : real;
  signal sample : integer;
  signal average : real;

  quantity v_in : real;
  constant v_il : real := 0.8;
  constant v_ih : real := 2.0;
  signal data : std_ulogic;

begin

  -- code from book

  sampler : process ( clock ) is
    constant num_levels : real := 64.0;
    constant max_val : real := 5.0;
  begin
    if clock = '1' then
      sample <= integer(q * num_levels / max_val) after 5 ns;
    end if;
  end process sampler;

  --

  compute_running_average : process (clock) is
    variable num_samples : integer := 0;
    variable total : real := 0.0;
    variable running_average : real := 0.0;
  begin
    if clock = '1' then
      total := total + q;
      num_samples := num_samples + 1;
      running_average := total / real(num_samples);
      average <= running_average after 5 ns;
    end if;
  end process compute_running_average;

  --

  analog_to_std_logic : process (v_in'above(v_il), v_in'above(v_ih)) is
  begin
    if not v_in'above(v_il) then
      data <= '0';
    elsif v_in'above(v_ih) then
      data <= '1';
    else
      data <= 'X';
    end if;
  end process analog_to_std_logic;
  
  -- end code from book

end architecture test;
