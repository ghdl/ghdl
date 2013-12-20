
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
-- $Id: ch_19_tb-jnsth.vhd,v 1.2 2001-10-24 22:18:13 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library qsim;

use std.textio.all;

architecture join_synth of test_bench is

  use qsim.qsim_types.all;

  constant num_outputs : positive := 4;

  signal fork_arc : arc_vector(1 to num_outputs);
  signal join_arc : arc_type;
  signal info_detail : info_detail_type := trace;

begin

  generator : process is
  begin
    fork_arc(1) <= (true,  ("generator           ", 9,  0, now));
    fork_arc(2) <= (true,  ("generator           ", 9,  1, now));    wait for 0 ns;
    fork_arc(3) <= (true,  ("generator           ", 9,  2, now));
    fork_arc(4) <= (true,  ("generator           ", 9,  3, now));    wait for 10 ns;

    fork_arc(1) <= (false, ("generator           ", 9,  4, now));
    fork_arc(2) <= (false, ("generator           ", 9,  5, now));    wait for 0 ns;
    fork_arc(3) <= (false, ("generator           ", 9,  6, now));
    fork_arc(4) <= (false, ("generator           ", 9,  7, now));    wait for 0 ns;
    fork_arc(1) <= (true,  ("generator           ", 9,  8, now));
    fork_arc(2) <= (true,  ("generator           ", 9,  9, now));    wait for 0 ns;
    fork_arc(3) <= (true,  ("generator           ", 9, 10, now));
    fork_arc(4) <= (true,  ("generator           ", 9, 11, now));    wait for 0 ns;
    fork_arc(1) <= (false, ("generator           ", 9, 12, now));
    fork_arc(2) <= (false, ("generator           ", 9, 13, now));    wait for 0 ns;
    fork_arc(3) <= (false, ("generator           ", 9, 14, now));
    fork_arc(4) <= (false, ("generator           ", 9, 15, now));    wait for 10 ns;

    wait;
  end process generator;

  join1 : entity qsim.join(behavior)
    generic map ( name => "join1",
                  time_unit => ns,
                  info_file_name => "join1.dat" )
    port map ( in_arc => fork_arc,
               out_arc => join_arc,
               info_detail => info_detail );


  sink1 : entity qsim.sink(behavior)
    generic map ( name => "sink1",
                  time_unit => ns,
                  info_file_name => "sink1.dat" )
    port map ( in_arc => join_arc,
               info_detail => info_detail );


  forks : for index in 1 to num_outputs generate

    constant index_string : string := integer'image(index);

  begin

    fork_monitor : process
      variable L : line;
    begin
      wait on fork_arc(index);
      write(L, string'("fork_monitor(" & index_string & "): at "));
      write(L, now, unit => ns);
      write(L, string'(", "));
      write(L, fork_arc(index).token, ns);
      writeline(output, L);
    end process fork_monitor;

  end generate forks;


  sink_monitor : process
    variable L : line;
  begin
    wait on join_arc;
    write(L, string'("sink_monitor: at "));
    write(L, now, unit => ns);
    write(L, string'(", "));
    write(L, join_arc.token, ns);
    writeline(output, L);
  end process sink_monitor;


end architecture join_synth;
