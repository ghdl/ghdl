
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
                        
entity multiple_opamp is
  generic ( size : positive;
            gains : real_vector );
  port ( terminal inputs, outputs : electrical_vector(1 to size) );
end entity multiple_opamp;

----------------------------------------------------------------

architecture ideal of multiple_opamp is
  
  quantity v_in across i_in through inputs to electrical_ref;
  quantity v_out across outputs to electrical_ref;
  alias gains_alias : real_vector(1 to size) is gains;
        
begin
  
  assert gains'length = size
    report "gains vector size differs from input/output size";
        
  amplify : procedural is
  begin
    for index in 1 to size loop
      v_out(index) := v_in(index) * gains_alias(index);
    end loop;
  end procedural amplify;
        
end architecture ideal;
