
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
-- $Id: ch_14_fg_14_09.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package bus_monitor_pkg is

  type stats_type is record
                       ifetch_freq, write_freq, read_freq : real;
                     end record stats_type;

  component bus_monitor is
                          generic ( verbose, dump_stats : boolean := false );
                        port ( mem_req, ifetch, write : in bit;
                               bus_stats : out stats_type );
  end component bus_monitor;

end package bus_monitor_pkg;


use work.bus_monitor_pkg.all;

entity bus_monitor is
  generic ( verbose, dump_stats : boolean := false );
  port ( mem_req, ifetch, write : in bit;
         bus_stats : out stats_type );
end entity bus_monitor;


architecture general_purpose of bus_monitor is
begin

  access_monitor : process is

                             variable access_count, ifetch_count,
                           write_count, read_count : natural := 0;
                           use std.textio;
                           variable L : textio.line;

  begin
    wait until mem_req = '1';
    if ifetch = '1' then
      ifetch_count := ifetch_count + 1;
      if verbose then
        textio.write(L, string'("Ifetch"));
        textio.writeline(textio.output, L);
      end if;
    elsif write = '1' then
      write_count := write_count + 1;
      if verbose then
        textio.write(L, string'("Write"));
        textio.writeline(textio.output, L);
      end if;
    else
      read_count := read_count + 1;
      if verbose then
        textio.write(L, string'("Read"));
        textio.writeline(textio.output, L);
      end if;
    end if;
    access_count := access_count + 1;
    bus_stats.ifetch_freq <= real(ifetch_count) / real(access_count);
    bus_stats.write_freq <= real(write_count) / real(access_count);
    bus_stats.read_freq <= real(read_count) / real(access_count);
    if dump_stats and access_count mod 5 = 0 then
      textio.write(L, string'("Ifetch frequency = "));
      textio.write(L, real(ifetch_count) / real(access_count));
      textio.writeline(textio.output, L);
      textio.write(L, string'("Write frequency = "));
      textio.write(L, real(write_count) / real(access_count));
      textio.writeline(textio.output, L);
      textio.write(L, string'("Read frequency = "));
      textio.write(L, real(read_count) / real(access_count));
      textio.writeline(textio.output, L);
    end if;
  end process access_monitor;

end architecture general_purpose;



-- code from book

architecture block_level of computer_system is

  -- . . .    -- type and component declarations for cpu and memory, etc.

  signal clock : bit;    -- the system clock
  signal mem_req : bit;  -- cpu access request to memory
  signal ifetch : bit;   -- indicates access is to fetch an instruction
  signal write : bit;    -- indicates access is a write
  -- . . .                  -- other signal declarations

begin

  -- . . .    -- component instances for cpu and memory, etc.

  instrumentation : if instrumented generate

    use work.bus_monitor_pkg;
    signal bus_stats : bus_monitor_pkg.stats_type;

  begin

    cpu_bus_monitor : component bus_monitor_pkg.bus_monitor
      port map ( mem_req, ifetch, write, bus_stats );

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
