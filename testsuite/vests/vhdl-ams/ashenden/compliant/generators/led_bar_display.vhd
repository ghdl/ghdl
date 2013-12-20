
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

-- analyze into resource library device_lib

library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity resistor is
  port ( terminal p1, p2 : electrical );
end entity resistor;

architecture ideal of resistor is
begin
end architecture ideal;


library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity led is
  port ( terminal anode, cathode : electrical );
end entity led;

architecture ideal of led is
begin
end architecture ideal;



-- code from book

library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity led_bar_display is
  generic ( width : positive );
  port ( terminal anodes : electrical_vector(1 to width);
         terminal common_cathode : electrical );
end entity led_bar_display;

----------------------------------------------------------------

architecture device_level of led_bar_display is

  component resistor is
    port ( terminal p1, p2 : electrical );
  end component resistor;

  component led is
    port ( terminal anode, cathode : electrical );
  end component led;

begin

  device_array : for segment in 1 to width generate

    terminal led_anode : electrical;

  begin

    limiting_resistor : component resistor
      port map ( p1 => anodes(segment), p2 => led_anode );

    segment_led : component led
      port map ( anode => led_anode, cathode => common_cathode );

  end generate device_array;

end architecture device_level;

-- end code from book
