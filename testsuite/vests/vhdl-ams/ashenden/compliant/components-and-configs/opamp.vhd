
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

entity bulk_cmos_nfet is
  generic ( Vt : real;
            transconductance : real );
  port ( terminal gate, drain, source : electrical );
end entity bulk_cmos_nfet;


architecture basic of bulk_cmos_nfet is
begin
end architecture basic;


architecture detailed of bulk_cmos_nfet is
begin
end architecture detailed;


-- code from book

library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity opamp is
  port ( terminal plus_in, minus_in, output, vdd, vss, gnd : electrical );
end entity opamp;

----------------------------------------------------------------

architecture struct of opamp is

  component nfet is
    generic ( Vt : real;
              transconductance : real );
    port ( terminal gate, drain, source : electrical );
  end component nfet;

  terminal int_1, int_2, int_3, -- ...
    -- not in book
    other_terminal
    -- end not in book
    : electrical;

begin

  m1 : component nfet
    generic map ( Vt => 0.026, transconductance => 1.0 )
    port map ( gate => plus_in, drain => int_1, source => int_2 );

  m2 : component nfet
    generic map ( Vt => 0.026, transconductance => 1.0 )
    port map ( gate => minus_in, drain => int_1, source => int_3 );

  -- other component instances
  -- ...

end architecture struct;

-- end code from book
