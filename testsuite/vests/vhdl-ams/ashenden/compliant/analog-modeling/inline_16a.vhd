
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

package inline_16a_types is

   subtype ILLUMINANCE is REAL tolerance "DEFAULT_ILLUMINANCE";
   subtype OPTIC_FLUX  is REAL tolerance "DEFAULT_OPTIC_FLUX";

   nature RADIANT is
      ILLUMINANCE across
      OPTIC_FLUX  through
      RADIANT_REF reference;

   subtype VOLTAGE     is REAL tolerance "DEFAULT_VOLTAGE";
   subtype CURRENT     is REAL tolerance "DEFAULT_CURRENT";

   nature ELECTRICAL is
      VOLTAGE        across
      CURRENT        through
      ELECTRICAL_REF reference;

  -- code from book

  type illuminance_vector is array ( natural range <> ) of illuminance;
  nature electrical_vector is array ( natural range <> ) of electrical;

  -- end code from book

end package inline_16a_types;



use work.inline_16a_types.all;

-- code from book

entity seven_segment_led is
  port ( terminal segment_anodes : electrical_vector ( 1 to 7 );
         terminal common_cathode : electrical;
         quantity segment_illuminances : out illuminance_vector ( 1 to 7 ) );
end entity seven_segment_led;

-- end code from book



architecture basic_optics of seven_segment_led is
begin
end architecture basic_optics;


                        
use work.inline_16a_types.all;

entity inline_16a is

end entity inline_16a;


architecture test of inline_16a is

  -- code from book

  terminal hour_anode_2, hour_anode_3 : electrical;
  terminal anodes_unused : electrical_vector(1 to 5);
  terminal hour_display_source_2, hour_display_source_3 : radiant;
  quantity hour_illuminance_2 across hour_display_source_2;
  quantity hour_illuminance_3 across hour_display_source_3;
  quantity illuminances_unused : illuminance_vector(1 to 5);

  -- end code from book

begin

  -- code from book

  hour_digit : entity work.seven_segment_led(basic_optics)
    port map ( segment_anodes(2) => hour_anode_2,
               segment_anodes(3) => hour_anode_3,
               segment_anodes(1) => anodes_unused(1),
               segment_anodes(4 to 7) => anodes_unused(2 to 5),
               common_cathode => electrical_ref,
               segment_illuminances(2) => hour_illuminance_2,
               segment_illuminances(3) => hour_illuminance_3,
               segment_illuminances(1) => illuminances_unused(1),
               segment_illuminances(4 to 7) => illuminances_unused(2 to 5) );

  -- end code from book

end architecture test;
