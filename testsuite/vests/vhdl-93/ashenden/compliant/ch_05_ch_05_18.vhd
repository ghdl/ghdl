
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
-- $Id: ch_05_ch_05_18.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- code from book:

entity DRAM_controller is
  port ( rd, wr, mem : in bit;
         ras, cas, we, ready : out bit  );
end entity DRAM_controller;

-- end of code from book


----------------------------------------------------------------


architecture fpld of DRAM_controller is
begin
end architecture fpld;


----------------------------------------------------------------


entity ch_05_18 is

end entity ch_05_18;


----------------------------------------------------------------


architecture test of ch_05_18 is



begin


  block_05_4_a : block is
                         signal cpu_rd, cpu_wr, cpu_mem,
                       mem_ras, mem_cas, mem_we, cpu_rdy : bit;
  begin

    -- code from book:

    main_mem_controller : entity work.DRAM_controller(fpld)
      port map ( cpu_rd, cpu_wr, cpu_mem,
                 mem_ras, mem_cas, mem_we, cpu_rdy );

    -- end of code from book

  end block block_05_4_a;


  ----------------


  block_05_4_b : block is
                         signal cpu_rd, cpu_wr, cpu_mem,
                       mem_ras, mem_cas, mem_we, cpu_rdy : bit;
  begin

    -- code from book:

    main_mem_controller : entity work.DRAM_controller(fpld)
      port map ( rd => cpu_rd, wr => cpu_wr,
                 mem => cpu_mem, ready => cpu_rdy,
                 ras => mem_ras, cas => mem_cas, we => mem_we );

    -- end of code from book

  end block block_05_4_b;


end architecture test;
