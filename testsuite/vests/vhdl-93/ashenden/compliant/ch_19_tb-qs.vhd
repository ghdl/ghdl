
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
-- $Id: ch_19_tb-qs.vhd,v 1.3 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library qsim;
library random;

use std.textio.all;

architecture queue_server of test_bench is

  use qsim.qsim_types.all;
  use random.random.all;

  signal source_arc, queue_arc, server_arc : arc_type;
  signal server_ready : boolean;
  signal info_detail : info_detail_type := trace;

begin

  source1 : entity qsim.source(behavior)
    generic map ( name => "source1",
                  distribution => fixed,  mean_inter_arrival_time => 100 ns,
                  seed => sample_seeds(1),
                  time_unit => ns,
                  info_file_name => "source1.dat" )
    port map ( out_arc => source_arc,
               info_detail => info_detail );

  queue1 : entity qsim.queue(behavior)
    generic map ( name => "queue1",
                  time_unit => ns,
                  info_file_name => "queue1.dat" )
    port map ( in_arc => source_arc,
               out_arc => queue_arc,  out_ready => server_ready,
               info_detail => info_detail );

  server1 : entity qsim.server(behavior)
    generic map ( name => "server1",
                  distribution => fixed,  mean_service_time => 120 ns,
                  seed => sample_seeds(2),
                  time_unit => ns,
                  info_file_name => "server1.dat" )
    port map ( in_arc => queue_arc,  in_ready => server_ready,
               out_arc => server_arc,
               info_detail => info_detail );

  sink1 : entity qsim.sink(behavior)
    generic map ( name => "sink1",
                  time_unit => ns,
                  info_file_name => "sink1.dat" )
    port map ( in_arc => server_arc,
               info_detail => info_detail );


  source_monitor : process is
                             variable L : line;
  begin
    wait on source_arc;
    write(L, string'("source_monitor: at "));
    write(L, now, unit => ns);
    write(L, string'(", "));
    write(L, source_arc.token, ns);
    writeline(output, L);
  end process source_monitor;

  queue_monitor : process is
                            variable L : line;
  begin
    wait on queue_arc;
    write(L, string'("queue_monitor: at "));
    write(L, now, unit => ns);
    write(L, string'(", "));
    write(L, queue_arc.token, ns);
    writeline(output, L);
  end process queue_monitor;

  server_monitor : process is
                             variable L : line;
  begin
    wait on server_arc;
    write(L, string'("server_monitor: at "));
    write(L, now, unit => ns);
    write(L, string'(", "));
    write(L, server_arc.token, ns);
    writeline(output, L);
  end process server_monitor;


end architecture queue_server;
