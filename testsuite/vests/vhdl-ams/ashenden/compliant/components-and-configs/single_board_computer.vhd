
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
entity single_board_computer is
end entity single_board_computer;
-- end not in book


architecture structural of single_board_computer is

  -- . . .    -- type and signal declarations

  -- not in book

  subtype word is bit_vector(31 downto 0);
  signal sys_clk : bit;
  signal cpu_a_d, latched_addr : word;

  -- end not in book

  component processor is
    port ( clk : in bit;  a_d : inout word; -- . . . );
    -- not in book
           other_port : in bit := '0' );
    -- end not in book
  end component processor;

  component memory is
    port ( addr : in bit_vector(25 downto 0); -- . . . );
    -- not in book
           other_port : in bit := '0' );
    -- end not in book
  end component memory;

  component serial_interface is
    port ( clk : in bit;  address : in bit_vector(3 downto 0); -- . . . );
    -- not in book
           other_port : in bit := '0' );
    -- end not in book
  end component serial_interface;

begin

  cpu : component processor
    port map ( clk => sys_clk, a_d => cpu_a_d, -- . . . );
    -- not in book
               other_port => open );
    -- end not in book

  main_memory : component memory
    port map ( addr => latched_addr(25 downto 0), -- . . . );
    -- not in book
               other_port => open );
    -- end not in book

  serial_interface_a : component serial_interface
    port map ( clk => sys_clk, address => latched_addr(3 downto 0), -- . . . );
    -- not in book
               other_port => open );
    -- end not in book

  -- . . .

end architecture structural;
