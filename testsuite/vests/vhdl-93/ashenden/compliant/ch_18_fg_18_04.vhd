
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
-- $Id: ch_18_fg_18_04.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity cache is
  generic ( cache_size, block_size, associativity : positive;
            benchmark_name : string(1 to 10) );
  port ( halt : in bit );
end entity cache;



architecture instrumented of cache is

begin

  -- code from book

  cache_monitor : process is

                            type measurement_record is
                          record
                            cache_size, block_size, associativity : positive;
                            benchmark_name : string(1 to 10);
                            miss_rate : real;
                            ave_access_time : delay_length;
                          end record;
                          type measurement_file is file of measurement_record;
                          file measurements : measurement_file
                            open append_mode is "cache-measurements";
                          -- . . .

                          -- not in book
                          constant miss_count : natural := 100;
                          constant total_accesses : natural := 1000;
                          constant total_delay : delay_length := 2400 ns;
                          -- end not in book

  begin
    -- . . .
    loop
      -- . . .
      -- not in book
      wait on halt;
      -- end not in book
      exit when halt = '1';
      -- . . .
    end loop;

    write ( measurements,
            measurement_record'(
              -- write values of generics for this run
              cache_size, block_size, associativity, benchmark_name,
              -- calculate performance metrics
              miss_rate => real(miss_count) / real(total_accesses),
              ave_access_time => total_delay / total_accesses ) );
    wait;

  end process cache_monitor;

  -- end code from book

end architecture instrumented;



entity fg_18_04 is
end entity fg_18_04;



architecture test of fg_18_04 is

  signal halt : bit := '0';

begin

  dut : entity work.cache(instrumented)
    generic map ( cache_size => 128*1024, block_size => 16,
                  associativity => 2, benchmark_name => "dhrystone " )
    port map ( halt => halt );

  halt <= '1' after 10 ns;

end architecture test;



entity fg_18_04_a is
end entity fg_18_04_a;


architecture reader of fg_18_04_a is
begin

  process is

            type measurement_record is
          record
            cache_size, block_size, associativity : positive;
            benchmark_name : string(1 to 10);
            miss_rate : real;
            ave_access_time : delay_length;
          end record;
          type measurement_file is file of measurement_record;
          file measurements : measurement_file open read_mode is "cache-measurements";
          variable measurement : measurement_record;

          use std.textio.all;
          variable L : line;

  begin
    while not endfile(measurements) loop
      read(measurements, measurement);
      write(L, measurement.cache_size);
      write(L, ' ');
      write(L, measurement.block_size);
      write(L, ' ');
      write(L, measurement.associativity);
      write(L, ' ');
      write(L, measurement.benchmark_name);
      write(L, ' ');
      write(L, measurement.miss_rate);
      write(L, ' ');
      write(L, measurement.ave_access_time);
      writeline(output, L);

    end loop;

    wait;
  end process;

end architecture reader;
