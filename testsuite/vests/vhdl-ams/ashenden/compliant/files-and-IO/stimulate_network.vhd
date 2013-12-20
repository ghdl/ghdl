
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

entity stimulate_network_write_data is
end entity stimulate_network_write_data;


architecture writer of stimulate_network_write_data is
begin

  process is
    type packet_file is file of bit_vector;
    file stimulus_file : packet_file open write_mode is "test packets";
  begin
    write(stimulus_file, X"6C");
    write(stimulus_file, X"05");
    write(stimulus_file, X"3");

    wait;
  end process;

end architecture writer;



entity stimulate_network is
end entity stimulate_network;


architecture test of stimulate_network is

  signal stimulus_network, stimulus_clock : bit;

begin

  clock_gen : stimulus_clock <= not stimulus_clock after 10 ns;

  -- code from book

  stimulate_network : process is

    type packet_file is file of bit_vector;
    file stimulus_file : packet_file open read_mode is "test packets";

    -- variable packet : bit_vector(1 to 2048);
    -- not in book (for testing only)
    variable packet : bit_vector(1 to 8);
    -- end not in book
    variable packet_length : natural;

  begin

    while not endfile(stimulus_file) loop

      read(stimulus_file, packet, packet_length);
      if packet_length > packet'length then
        report "stimulus packet too long - ignored" severity warning;
      else
        for bit_index in 1 to packet_length loop
          wait until stimulus_clock = '1';
          stimulus_network <= not stimulus_network;
          wait until stimulus_clock = '0';
          stimulus_network <= stimulus_network xor packet(bit_index);
        end loop;
      end if;

    end loop;

    wait;  -- end of stimulation: wait forever

  end process stimulate_network;

  -- code from book

end architecture test;
