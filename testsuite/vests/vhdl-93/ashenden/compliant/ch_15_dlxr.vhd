
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
-- $Id: ch_15_dlxr.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

configuration dlx_rtl of dlx is

  for rtl

    for alu_s1_reg : latch
      use entity work.latch(behavior)
        generic map ( Tpd => 2 ns );
    end for;

    for alu_s2_reg : latch
      use entity work.latch(behavior)
        generic map ( Tpd => 2 ns );
    end for;

    for the_alu : alu
      use entity work.alu(behavior)
        generic map ( Tpd => 4 ns );
    end for;

    for the_reg_file : reg_file
      use entity work.reg_file(behavior)
        generic map ( Tac => 4 ns );
    end for;

    for c_reg : latch
      use entity work.latch(behavior)
        generic map ( Tpd => 2 ns );
    end for;

    for a_reg : reg_multiple_out
      use entity work.reg_multiple_out(behavior)
        generic map ( num_outputs => num_outputs, Tpd => 2 ns );
    end for;

    for b_reg : reg_multiple_out
      use entity work.reg_multiple_out(behavior)
        generic map ( num_outputs => num_outputs, Tpd => 2 ns );
    end for;

    for temp_reg : reg_multiple_out
      use entity work.reg_multiple_out(behavior)
        generic map ( num_outputs => num_outputs, Tpd => 2 ns );
    end for;

    for iar_reg : reg_multiple_out
      use entity work.reg_multiple_out(behavior)
        generic map ( num_outputs => num_outputs, Tpd => 2 ns );
    end for;

    for pc_reg :reg_multiple_plus_one_out_reset
      use entity work.reg_multiple_plus_one_out_reset(behavior)
        generic map ( num_outputs => num_outputs, Tpd => 2 ns );
    end for;

    for mar_reg : reg_multiple_plus_one_out
      use entity work.reg_multiple_plus_one_out(behavior)
        generic map ( num_outputs => num_outputs, Tpd => 2 ns );
    end for;

    for mem_addr_mux : mux2
      use entity work.mux2(behavior)
        generic map ( Tpd => 1 ns );
    end for;

    for mdr_reg : reg_multiple_out
      use entity work.reg_multiple_out(behavior)
        generic map ( num_outputs => num_outputs, Tpd => 2 ns );
    end for;

    for mdr_mux : mux2
      use entity work.mux2(behavior)
        generic map ( Tpd => 1 ns );
    end for;

    for instr_reg : latch
      use entity work.latch(behavior)
        generic map ( Tpd => 2 ns );
    end for;

    for ir_extender1 : ir_extender
      use entity work.ir_extender(behavior)
        generic map ( Tpd => 2 ns );
    end for;

    for ir_extender2 : ir_extender
      use entity work.ir_extender(behavior)
        generic map ( Tpd => 2 ns );
    end for;

    for the_controller : controller
      use entity work.controller(behavior)
        generic map ( Tpd_clk_ctrl => 2 ns, Tpd_clk_const => 4 ns,
                      debug => debug );
    end for;

  end for;  -- rtl of dlx

end configuration dlx_rtl;
