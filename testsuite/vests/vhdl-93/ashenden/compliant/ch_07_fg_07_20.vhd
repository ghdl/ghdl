
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
-- $Id: ch_07_fg_07_20.vhd,v 1.3 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

-- code from book

library ieee;  
use ieee.std_logic_1164.all;

entity reg_ctrl is
  port ( reg_addr_decoded, rd, wr, io_en, cpu_clk : in std_ulogic;
         reg_rd, reg_wr : out std_ulogic );
end entity reg_ctrl;

architecture bool_eqn of reg_ctrl is
begin

  rd_ctrl : reg_rd <= reg_addr_decoded and rd and io_en;

  rw_ctrl : reg_wr <= reg_addr_decoded and wr and io_en
                      and not cpu_clk;

end architecture bool_eqn;

-- end code from book

entity fg_07_20 is

end entity fg_07_20;

library ieee;  
use ieee.std_logic_1164.all;
library stimulus;

architecture test of fg_07_20 is

  signal reg_addr_decoded, rd, wr, io_en,
    cpu_clk, reg_rd, reg_wr : std_ulogic := '0';
  signal test_vector : std_ulogic_vector(1 to 5);

  use stimulus.stimulus_generators.all;

begin

  dut : entity work.reg_ctrl
    port map ( reg_addr_decoded, rd, wr, io_en, cpu_clk, reg_rd, reg_wr );

  stimulus_proc : process is
  begin
    all_possible_values( bv => test_vector,
                         delay_between_values => 10 ns );
    wait;
  end process stimulus_proc;

  (reg_addr_decoded, rd, wr, io_en, cpu_clk) <= test_vector;

end architecture test;
