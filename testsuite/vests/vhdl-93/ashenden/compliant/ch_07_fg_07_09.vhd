
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

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

-- ---------------------------------------------------------------------
--
-- $Id: ch_07_fg_07_09.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity receiver is
end entity receiver;


-- code from book

architecture behavioral of receiver is

  -- . . .    -- type declarations, etc

  -- not in book

  subtype packet_index_range is integer range 1 to 8;
  type packet_array is array (packet_index_range) of bit;

  -- end not in book

  signal recovered_data : bit;
  signal recovered_clock : bit;
  -- . . .

  procedure receive_packet ( signal rx_data : in bit;
                             signal rx_clock : in bit;
                             data_buffer : out packet_array ) is
  begin
    for index in packet_index_range loop
      wait until rx_clock = '1';
      data_buffer(index) := rx_data;
    end loop;
  end procedure receive_packet;

begin

  packet_assembler : process is
                               variable packet : packet_array;
  begin
    -- . . .
    receive_packet ( recovered_data, recovered_clock, packet );
    -- . . .
  end process packet_assembler;

  -- . . .


  -- not in book

  data_generator : recovered_data <= '1' after 5 ns,
                                     '0' after 15 ns,
                                     '1' after 25 ns,
                                     '0' after 35 ns,
                                     '0' after 45 ns,
                                     '1' after 55 ns,
                                     '0' after 65 ns,
                                     '1' after 75 ns;

  clock_generator : process is
  begin
    recovered_clock <= '0' after 2 ns, '1' after 10 ns;
    wait for 10 ns;
  end process clock_generator;

  -- end not in book

end architecture behavioral;

-- end code from book
