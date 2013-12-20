
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

library ieee;  use ieee.std_logic_1164.all;

entity phase_locked_clock_gen is
  port ( ref_clock : in std_ulogic;
         phi1, phi2 : out std_ulogic );
end entity phase_locked_clock_gen;


architecture std_cell of phase_locked_clock_gen is

  use work.clock_power_pkg.Tpw;

begin

  phi1_gen : phi1 <= '1', '0' after Tpw when rising_edge(ref_clock);

  phi2_gen : phi2 <= '1', '0' after Tpw when falling_edge(ref_clock);

end architecture std_cell;


library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity regulator is
  port ( terminal plus_in, minus_in, plus_out, minus_out : electrical );
end entity regulator;


architecture device_level of regulator is
begin
end architecture device_level;




library ieee_proposed;  use ieee_proposed.electrical_systems.all;

-- end not in book



library ieee;  use ieee.std_logic_1164.all;

entity io_controller is
  port ( signal ref_clock : in std_ulogic;
         terminal ext_supply, ext_ground : electrical;  -- . . . );
         -- not in book
         other_port : in std_ulogic );
         -- end not in book
end entity io_controller;

--------------------------------------------------

architecture top_level of io_controller is

  -- . . .

  -- not in book
  signal rd, wr, sel, width, burst : std_ulogic;
  signal addr : std_ulogic_vector(3 downto 0);
  signal ready : std_ulogic;
  signal control_reg_wr, status_reg_rd, data_fifo_wr, data_fifo_rd,
         other_signal : std_ulogic;

  signal analog_out_wr_0 : std_ulogic;
  signal internal_data : std_ulogic_vector(7 downto 0);
  terminal analog_out_0 : electrical;
  -- end not in book

begin

  internal_clock_gen : entity work.phase_locked_clock_gen(std_cell)
    port map ( ref_clock => ref_clock,
               phi1 => work.clock_power_pkg.clock_phase1,
               phi2 => work.clock_power_pkg.clock_phase2 );
  
  internal_analog_regulator : entity work.regulator(device_level)
    port map ( plus_in => ext_supply, minus_in => ext_ground,
               plus_out => work.clock_power_pkg.analog_plus_supply,
               minus_out => work.clock_power_pkg.analog_ground );

  the_bus_sequencer : entity work.bus_sequencer(fsm)
    port map ( rd, wr, sel, width, burst, addr(3 downto 0), ready,
               control_reg_wr, status_reg_rd, data_fifo_wr, data_fifo_rd,
               analog_out_wr_0, -- . . . );
               -- not in book
               other_signal );
               -- not in book

  analog_output_interface_0 : entity work.analog_output_interface(structural)
    port map ( analog_out_wr_0, internal_data(7 downto 0), analog_out_0 );
  
  -- . . .

end architecture top_level;
