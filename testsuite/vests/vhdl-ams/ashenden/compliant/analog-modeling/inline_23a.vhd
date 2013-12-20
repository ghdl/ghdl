
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
                        
entity inline_23a is

end entity inline_23a;


architecture test of inline_23a is

  signal digital_level : integer;
  constant num_levels : integer := 63;
  constant max_voltage : real := 10.0;
  
begin

  block_1 : block is

    quantity analog_voltage : real;

  begin
    
    -- code from book

    analog_voltage == real(digital_level) / real(num_levels) * max_voltage;

    -- end code from book

  end block block_1;


  block_2 : block is

    signal real_digital_level : real;
    quantity analog_voltage : real;

  begin
    
    -- code from book

    real_digital_level <= real(digital_level);
    analog_voltage == real_digital_level'ramp(1.0E-6) / real(num_levels) * max_voltage;

    -- end code from book

  end block block_2;


end architecture test;
