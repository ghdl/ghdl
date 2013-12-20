
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

library ieee_proposed;  use ieee_proposed.electrical_systems.all;
                        
entity inline_11a is

end entity inline_11a;


architecture test of inline_11a is

  constant v_pos : voltage := 15.0;
  constant v_neg : voltage := -15.0;
  terminal input : electrical;
  quantity v_in across input;
  quantity v_amplified : voltage;
  constant gain : real := 1.0;

  constant threshold_voltage : voltage := 0.6;
  constant k : real := 0.0125;
  terminal gate, source, drain : electrical;
  quantity vds across ids through drain to source;
  quantity vsd across source to drain;
  quantity vgs across gate to source;
  quantity vgd across gate to drain;
  
  constant r_charge : resistance := 10_000.0;
  constant r_discharge : resistance := 10_000.0;
  constant charging : boolean := true;
  terminal cap, plus, minus : electrical;
  quantity v_plus := 10.0 across plus;
  quantity v_minus := 0.0 across minus;
  quantity v_cap across cap;
  quantity i_charge through plus to cap;
  quantity i_discharge through cap to minus;

begin

  -- code from book

  if v_in * gain > v_pos use -- incorrect
    v_amplified == v_pos;
  elsif v_in * gain < v_neg use -- incorrect
    v_amplified == v_neg;
  else
    v_amplified == gain * v_in;
  end use;

  --

  if vds'above(0.0) use -- transistor is forward biased
    if not vgs'above(threshold_voltage) use -- cutoff region
      ids == 0.0;
    elsif vds'above(vgs - threshold_voltage) use -- saturation region
      ids == 0.5 * k * (vgs - threshold_voltage)**2;
    else -- linear/triode region
      ids == k * (vgs - threshold_voltage - 0.5*vds) * vds;
    end use;
  else -- transistor is reverse biased
    if not vgd 'above(threshold_voltage) use -- cutoff region
      ids == 0.0;
    elsif vsd'above(vgd - threshold_voltage) use -- saturation region
      ids == -0.5 * k * (vgd - threshold_voltage)**2;
    else -- linear/triode region
      ids == -k * (vgd - threshold_voltage - 0.5*vsd) * vsd;
    end use;
  end use;

  --

  if charging use
    i_charge == ( v_plus - v_cap ) / r_charge;
    i_discharge == 0.0;
  else
    i_charge == 0.0;
    i_discharge == ( v_cap - v_minus ) / r_discharge;
  end use;

  -- end code from book

end architecture test;
