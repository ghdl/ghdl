
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
                        
entity triangle_waveform_wa is
  port ( terminal pos, neg : electrical );
end entity triangle_waveform_wa;

----------------------------------------------------------------

architecture ideal of triangle_waveform_wa is
  
  constant freq : real := 10_000.0;  -- in Hz
  constant period : real := 1.0 / freq;
  constant amplitude : voltage := 5.0;
  constant offset : voltage := 0.0;
  signal square_wave : real := 0.0;
  quantity v across i through pos to neg;
--  limit v : voltage with period / 10.0;
        
begin
  
  process is
    variable state : bit := '0';
  begin
    if state = '1' then
      square_wave <= 1.0;
    else
      square_wave <= 0.0;
    end if;
    state := not state;
    wait for period / 2.0;
  end process;
        
  v == offset + amplitude * square_wave'ramp(period / 2.0);
        
end architecture ideal;
