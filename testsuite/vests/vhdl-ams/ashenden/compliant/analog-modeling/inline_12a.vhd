
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
                        
entity inline_12a is

end entity inline_12a;


architecture test of inline_12a is

  -- code from book

  type biases is (forward, reverse);
  type regions is (cutoff, saturation, linear);
  
  signal bias : biases;
  signal region : regions;

  -- end code from book

  constant threshold_voltage : voltage := 0.6;
  constant k : real := 0.0125;
  terminal gate, source, drain : electrical;
  quantity vds across ids through drain to source;
  quantity vsd across source to drain;
  quantity vgs across gate to source;
  quantity vgd across gate to drain;
  
begin

  -- code from book

  case bias use
    when forward =>
      case region use
        when cutoff =>
          ids == 0.0;
        when saturation =>
          ids == 0.5 * k * (vgs - threshold_voltage)**2;
        when linear =>
          ids == k * (vgs - threshold_voltage - 0.5*vds) * vds;
      end case;
    when reverse =>
      case region use
        when cutoff =>
          ids == 0.0;
        when saturation =>
          ids == -0.5 * k * (vgd - threshold_voltage)**2;
        when linear =>
          ids == -k * (vgd - threshold_voltage - 0.5*vsd) * vsd;
      end case;
  end case;

  -- end code from book

end architecture test;
