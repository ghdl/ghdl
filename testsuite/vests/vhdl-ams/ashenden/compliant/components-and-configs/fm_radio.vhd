
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

-- not in book

library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity fm_radio is
end entity fm_radio;

-- end not in book



architecture top_level of fm_radio is

  terminal left_decoded, left_filtered : electrical;
  terminal right_decoded, right_filtered : electrical;
  -- ...

begin

  left_pilot_filter : configuration work.notch_filter_down_to_device_level
    port map ( input => left_decoded, output => left_filtered,
               vdd => vdd, vss => vss, gnd => gnd );

  -- ...

end architecture top_level;
