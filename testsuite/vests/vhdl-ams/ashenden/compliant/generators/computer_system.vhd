
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

-- code from book (in text)

entity computer_system is
  generic ( instrumented : boolean := false );
  port ( -- . . . );
  -- not in book
         other_port : in bit := '0' );
  -- end not in book
end entity computer_system;

-- end code from book


-- code from book

architecture block_level of computer_system is

  -- . . .    -- type and component declarations for cpu and memory, etc

  signal clock : bit;    -- the system clock
  signal mem_req : bit;  -- cpu access request to memory
  signal ifetch : bit;   -- indicates access is to fetch an instruction
  signal write : bit;    -- indicates access is a write
  -- . . .                  -- other signal declarations

begin

  -- . . .    -- component instances for cpu and memory, etc

  instrumentation : if instrumented generate

    signal ifetch_freq, write_freq, read_freq : real := 0.0;

  begin

    access_monitor : process is
      variable access_count, ifetch_count,
               write_count, read_count : natural := 0;
    begin
      wait until mem_req = '1';
      if ifetch = '1' then
        ifetch_count := ifetch_count + 1;
      elsif write = '1' then
        write_count := write_count + 1;
      else
        read_count := read_count + 1;
      end if;
      access_count := access_count + 1;
      ifetch_freq <= real(ifetch_count) / real(access_count);
      write_freq <= real(write_count) / real(access_count);
      read_freq <= real(read_count) / real(access_count);
    end process access_monitor;

  end generate instrumentation;

  -- not in book

  stimulus : process is
  begin
    ifetch <= '1';  write <= '0';
    mem_req <= '1', '0' after 10 ns;
    wait for 20 ns;

    mem_req <= '1', '0' after 10 ns;
    ifetch <= '1';  write <= '0';
    wait for 20 ns;

    mem_req <= '1', '0' after 10 ns;
    ifetch <= '1';  write <= '0';
    wait for 20 ns;

    mem_req <= '1', '0' after 10 ns;
    ifetch <= '0';  write <= '1';
    wait for 20 ns;

    mem_req <= '1', '0' after 10 ns;
    ifetch <= '1';  write <= '0';
    wait for 20 ns;

    mem_req <= '1', '0' after 10 ns;
    ifetch <= '0';  write <= '0';
    wait for 20 ns;

    mem_req <= '1', '0' after 10 ns;
    ifetch <= '1';  write <= '0';
    wait for 20 ns;

    mem_req <= '1', '0' after 10 ns;
    ifetch <= '0';  write <= '0';
    wait for 20 ns;

    mem_req <= '1', '0' after 10 ns;
    ifetch <= '1';  write <= '0';
    wait for 20 ns;

    mem_req <= '1', '0' after 10 ns;
    ifetch <= '0';  write <= '0';
    wait for 20 ns;

    wait;
  end process stimulus;

  -- end not in book

end architecture block_level;

-- end code from book
