
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

entity MeasFreq is
  generic ( thres : real := 0.0 );
  port ( terminal input : electrical;
         signal f_out : out real := 0.0 );
end entity MeasFreq;

----------------------------------------------------------------

architecture ThresDetect of MeasFreq is

  quantity vin across input;

begin

  detect : process ( vin'above(thres) ) is
    variable t_old : real := real'low;
  begin
    if vin'above(thres) then
      f_out <= 1.0 / (now - t_old);
      t_old := now;
    end if;
  end process detect;

end ThresDetect;
