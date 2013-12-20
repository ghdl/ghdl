
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

entity inline_04a is
end entity inline_04a;


architecture test of inline_04a is

  component opamp is
    port ( terminal plus_in, minus_in, output, vdd, vss, gnd : electrical );
  end component opamp;

  terminal plus_in, minus_in, output, vdd, vss, gnd : electrical;

begin

  voltage_amp : component opamp
    port map ( plus_in => plus_in, minus_in => minus_in, output => output,
               vdd => vdd, vss => vss, gnd => gnd );

end architecture test;


configuration inline_04a_test of inline_04a is

  for test

    -- code from book (in text)

    for voltage_amp : opamp
      use configuration work.opamp_mosfets;
    end for;

    -- end code from book

  end for;

end configuration inline_04a_test;
