
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
-- $Id: ch_19_tb-snk.vhd,v 1.3 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library qsim;
library random;

use std.textio.all;

architecture sink of test_bench is

  use qsim.qsim_types.all;
  use random.random.all;

  signal a : arc_type;
  signal info_detail : info_detail_type := trace;

begin

  source1 : entity qsim.source(behavior)
    generic map ( name => "source1",
                  distribution => fixed,  mean_inter_arrival_time => 100 ns,
                  seed => sample_seeds(1),
                  time_unit => ns,
                  info_file_name => "source1.dat" )
    port map ( out_arc => a,
               info_detail => info_detail );

  sink1 : entity qsim.sink(behavior)
    generic map ( name => "sink1",
                  time_unit => ns,
                  info_file_name => "sink1.dat" )
    port map ( in_arc => a,
               info_detail => info_detail );

  monitor : process is

                      variable L : line;

  begin
    wait on a;
    write(L, string'("monitor: at "));
    write(L, now, unit => ns);
    write(L, string'(" received "));
    write(L, a.token, ns);
    writeline(output, L);
  end process monitor;

end architecture sink;
