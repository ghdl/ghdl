
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

entity tb_address_decoder is
end entity tb_address_decoder;


architecture test of tb_address_decoder is

  use work.cpu_types.all;

  signal addr : address := X"000000";
  signal status : status_value := idle;
  signal mem_sel, int_sel, io_sel : bit;

begin

  dut : entity work.address_decoder
    port map ( addr => addr, status => status,
               mem_sel => mem_sel, int_sel => int_sel, io_sel => io_sel );

  stimulus : process is
  begin
    wait for 10 ns;

    status <= fetch;      wait for 10 ns;
    status <= mem_read;   wait for 10 ns;
    status <= mem_write;  wait for 10 ns;
    status <= io_read;    wait for 10 ns;
    status <= io_write;   wait for 10 ns;
    status <= int_ack;    wait for 10 ns;
    status <= idle;       wait for 10 ns;

    addr <= X"EFFFFF";    wait for 10 ns;
    status <= fetch;      wait for 10 ns;
    status <= mem_read;   wait for 10 ns;
    status <= mem_write;  wait for 10 ns;
    status <= io_read;    wait for 10 ns;
    status <= io_write;   wait for 10 ns;
    status <= int_ack;    wait for 10 ns;
    status <= idle;       wait for 10 ns;

    addr <= X"F00000";    wait for 10 ns;
    status <= fetch;      wait for 10 ns;
    status <= mem_read;   wait for 10 ns;
    status <= mem_write;  wait for 10 ns;
    status <= io_read;    wait for 10 ns;
    status <= io_write;   wait for 10 ns;
    status <= int_ack;    wait for 10 ns;
    status <= idle;       wait for 10 ns;

    addr <= X"FFFFFF";    wait for 10 ns;
    status <= fetch;      wait for 10 ns;
    status <= mem_read;   wait for 10 ns;
    status <= mem_write;  wait for 10 ns;
    status <= io_read;    wait for 10 ns;
    status <= io_write;   wait for 10 ns;
    status <= int_ack;    wait for 10 ns;
    status <= idle;       wait for 10 ns;

    wait;
  end process stimulus;

end architecture test;
