
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

entity bus_monitor is
end entity bus_monitor;



architecture test of bus_monitor is

  subtype byte is bit_vector(7 downto 0);
  type byte_array is array (natural range <>) of byte;

  function resolve_bytes ( drivers : in byte_array ) return byte is
  begin
    return drivers(drivers'left);
  end function resolve_bytes;

  function resolve_bits ( drivers : in bit_vector ) return bit is
  begin
    return drivers(drivers'left);
  end function resolve_bits;

  -- code from book (in text)

  signal address : bit_vector(15 downto 0);
  signal data : resolve_bytes byte;
  signal rd, wr, io : bit;    -- read, write, io/mem select
  signal ready : resolve_bits bit;

  -- end code from book

begin

-- code from book

bus_monitor : process is

  constant header : string(1 to 44)
    := FF & "      Time   R/W I/M  Address          Data";

  use std.textio.all;

  file log : text open write_mode is "buslog";
  variable trace_line : line;
  variable line_count : natural := 0;

begin

  if line_count mod 60 = 0 then
    write ( trace_line, header );
    writeline ( log, trace_line );
    writeline ( log, trace_line );    -- empty line
  end if;
  wait until (rd = '1' or wr = '1') and ready = '1';
  write ( trace_line, now, justified => right, field => 10, unit => us );
  write ( trace_line, string'("    ") );
  if rd = '1' then
    write ( trace_line, 'R' );
  else
    write ( trace_line, 'W' );
  end if;
  write ( trace_line, string'("   ") );
  if io = '1' then
    write ( trace_line, 'I' );
  else
    write ( trace_line, 'M' );
  end if;
  write ( trace_line, string'("   ") );
  write ( trace_line, address );
  write ( trace_line, ' ');
  write ( trace_line, data );
  writeline ( log, trace_line );
  line_count := line_count + 1;

end process bus_monitor;

-- end code from book

  stimulus : process is
  begin
    wait for 0.4 us - now;
    rd <= '1', '0' after 10 ns;
    address <= X"0000";
    data <= B"10011110";
    ready <= '1', '0' after 10 ns;

    wait for 0.9 us - now;
    rd <= '1', '0' after 10 ns;
    address <= X"0001";
    data <= B"00010010";
    ready <= '1', '0' after 10 ns;

    wait for 2.0 us - now;
    rd <= '1', '0' after 10 ns;
    address <= X"0014";
    data <= B"11100111";
    ready <= '1', '0' after 10 ns;

    wait for 2.7 us - now;
    wr <= '1', '0' after 10 ns;
    io <= '1', '0' after 10 ns;
    address <= X"0007";
    data <= X"00";
    ready <= '1', '0' after 10 ns;

    wait;
  end process stimulus;

end architecture test;
