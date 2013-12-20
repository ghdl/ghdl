
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
-- $Id: ch_19_tb-frk.vhd,v 1.3 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library qsim;
library random;

use std.textio.all;

architecture fork of test_bench is

  use qsim.qsim_types.all;
  use random.random.all;

  constant num_outputs : positive := 4;
  constant probabilities : probability_vector(1 to num_outputs - 1)
    := ( 0.2, 0.4, 0.1 );

  signal source_arc : arc_type;
  signal fork_arc : arc_vector(1 to num_outputs);
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

  fork1 : entity qsim.fork(behavior)
    generic map ( name => "fork1",
                  probabilities => probabilities,
                  seed => sample_seeds(2),
                  time_unit => ns,
                  info_file_name => "fork1.dat" )
    port map ( in_arc => source_arc,
	       out_arc => fork_arc,
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


  sinks : for index in 1 to num_outputs generate

    constant index_string : string := integer'image(index);

  begin

    sink : entity qsim.sink(behavior)
      generic map ( name => "sink" & index_string,
                    time_unit => ns,
                    info_file_name => "sink" & index_string & ".dat" )
      port map ( in_arc => fork_arc(index),
		 info_detail => info_detail );

    sink_monitor : process
      variable L : line;
    begin
      wait on fork_arc(index);
      write(L, string'("sink_monitor(" & index_string & "): at "));
      write(L, now, unit => ns);
      write(L, string'(", "));
      write(L, fork_arc(index).token, ns);
      writeline(output, L);
    end process sink_monitor;

  end generate sinks;


end architecture fork;
