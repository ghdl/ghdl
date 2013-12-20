
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
-- $Id: ch_15_dlxstsv.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

configuration dlx_test_verifier of dlx_test is

  for verifier

    for cg : clock_gen
      use entity work.clock_gen(behavior)
        generic map ( Tpw => 8 ns, Tps => 2 ns );
    end for;

    for mem : memory
      use entity work.memory(preloaded)
        generic map ( mem_size => 65536,
                      Tac_first => 95 ns, Tac_burst => 35 ns, Tpd_clk_out => 2 ns );
    end for;

    for proc_behav : dlx
      use entity work.dlx(behavior)
        generic map ( Tpd_clk_out => 2 ns, debug => trace_each_step );
    end for;

    for proc_rtl : dlx
      use configuration work.dlx_rtl
        generic map ( Tpd_clk_out => 2 ns, debug => trace_each_step );
    end for;

  end for;  -- verifier of dlx_test

end configuration dlx_test_verifier;
