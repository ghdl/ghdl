
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
-- $Id: ch_15_mem.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee. std_logic_1164.all;

               use work.dlx_types.all;

               entity memory is

                 generic ( mem_size : positive;
                           Tac_first : delay_length;
                           Tac_burst : delay_length;
                           Tpd_clk_out : delay_length;
                           load_file_name : string := "dlx.out" );

                 port ( phi1, phi2 : in std_logic;
                        a : in dlx_address;
                        d : inout dlx_word;
                        width : in dlx_mem_width;
                        write_enable : in std_logic;
                        burst : in std_logic := '0';
                        mem_enable : in std_logic;
                        ready : out std_logic );

               end entity memory;
