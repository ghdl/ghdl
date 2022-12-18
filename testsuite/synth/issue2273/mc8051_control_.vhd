-------------------------------------------------------------------------------
--                                                                           --
--          X       X   XXXXXX    XXXXXX    XXXXXX    XXXXXX      X          --
--          XX     XX  X      X  X      X  X      X  X           XX          --
--          X X   X X  X         X      X  X      X  X          X X          --
--          X  X X  X  X         X      X  X      X  X         X  X          --
--          X   X   X  X          XXXXXX   X      X   XXXXXX      X          --
--          X       X  X         X      X  X      X         X     X          --
--          X       X  X         X      X  X      X         X     X          --
--          X       X  X      X  X      X  X      X         X     X          --
--          X       X   XXXXXX    XXXXXX    XXXXXX    XXXXXX      X          --
--                                                                           --
--                                                                           --
--                       O R E G A N O   S Y S T E M S                       --
--                                                                           --
--                            Design & Consulting                            --
--                                                                           --
-------------------------------------------------------------------------------
--                                                                           --
--         Web:           http://www.oregano.at/                             --
--                                                                           --
--         Contact:       mc8051@oregano.at                                  --
--                                                                           --
-------------------------------------------------------------------------------
--                                                                           --
--  MC8051 - VHDL 8051 Microcontroller IP Core                               --
--  Copyright (C) 2001 OREGANO SYSTEMS                                       --
--                                                                           --
--  This library is free software; you can redistribute it and/or            --
--  modify it under the terms of the GNU Lesser General Public               --
--  License as published by the Free Software Foundation; either             --
--  version 2.1 of the License, or (at your option) any later version.       --
--                                                                           --
--  This library is distributed in the hope that it will be useful,          --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of           --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        --
--  Lesser General Public License for more details.                          --
--                                                                           --
--  Full details of the license can be found in the file LGPL.TXT.           --
--                                                                           --
--  You should have received a copy of the GNU Lesser General Public         --
--  License along with this library; if not, write to the Free Software      --
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA  --
--                                                                           --
-------------------------------------------------------------------------------
--
--
--         Author:                 Helmut Mayrhofer
--
--         Filename:               mc8051_control_.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.7 $
--
--         Date of Latest Version: $Date: 2002-01-07 12:17:45 $
--
--
--         Description: Connects the units control_fsm and control_mem. This
--                      unit manages the whole microcontroller core.
--
--
--
--
-------------------------------------------------------------------------------
library IEEE;
library work;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use work.mc8051_p.all;


------------------------ ENTITY DECLARATION -------------------------
entity mc8051_control is

  port (pc_o       : out std_logic_vector(15 downto 0);  -- Programmcounter =
  							 -- ROM-adress
        rom_data_i : in  std_logic_vector(7 downto 0);   -- data input from ROM
        ram_data_o : out std_logic_vector(7 downto 0);   -- data output to
  							 -- internal RAM
        ram_data_i : in  std_logic_vector(7 downto 0);   -- data input from
  							 -- internal RAM
        ram_adr_o  : out std_logic_vector(6 downto 0);   -- internal RAM-adress
        reg_data_o : out std_logic_vector(7 downto 0);   -- data for ALU
        ram_wr_o   : out std_logic;  	-- read (0) / write (1)
  					-- internal RAM
        cy_o       : out std_logic_vector(1 downto 0);   -- Carry Flag
        ov_o       : out std_logic;  	-- Overflow Flag
        ram_en_o   : out std_logic;  	-- RAM-block enable
        alu_cmd_o  : out std_logic_vector (5 downto 0);  -- ALU operationscode
        aludata_i  : in  std_logic_vector (7 downto 0);  -- ALU result
        aludatb_i  : in  std_logic_vector (7 downto 0);  -- 2nd ALU result
        acc_o      : out std_logic_vector (7 downto 0);  -- ACC register
        new_cy_i   : in  std_logic_vector(1 downto 0);   -- CY result of ALU
        new_ov_i   : in  std_logic;  	-- OV result of ALU
        reset      : in  std_logic;  	-- reset signal
        clk        : in  std_logic;  	-- clock signal
        cen        : in  std_logic;     -- clock enable signal
        int0_i     : in  std_logic_vector(C_IMPL_N_EXT-1 downto 0);  -- ext.Int
        int1_i     : in  std_logic_vector(C_IMPL_N_EXT-1 downto 0);  -- ext.Int

        datax_i : in  std_logic_vector (7 downto 0);   -- ext. RAM
        datax_o : out std_logic_vector (7 downto 0);   -- ext. RAM
        adrx_o  : out std_logic_vector (15 downto 0);  -- ext. RAM
        wrx_o   : out std_logic;  		       -- ext. RAM
        memx_o  : out std_logic;             -- ext. RAM

        p0_i : in std_logic_vector(7 downto 0);  -- IO-port0
        p1_i : in std_logic_vector(7 downto 0);  -- IO-port1
        p2_i : in std_logic_vector(7 downto 0);  -- IO-port2
        p3_i : in std_logic_vector(7 downto 0);  -- IO-port3

        p0_o : out std_logic_vector(7 downto 0);  -- IO-port0
        p1_o : out std_logic_vector(7 downto 0);  -- IO-port1
        p2_o : out std_logic_vector(7 downto 0);  -- IO-port2
        p3_o : out std_logic_vector(7 downto 0);  -- IO-port3

        -- Signals to and from the SIUs

        all_trans_o : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
        all_scon_o  : out std_logic_vector(6*C_IMPL_N_SIU-1 downto 0);
        all_sbuf_o  : out std_logic_vector(8*C_IMPL_N_SIU-1 downto 0);
        all_smod_o  : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
        all_scon_i  : in  std_logic_vector(3*C_IMPL_N_SIU-1 downto 0);
        all_sbuf_i  : in  std_logic_vector(8*C_IMPL_N_SIU-1 downto 0);

        -- signals to and from the timer/counters

        all_tcon_tr0_o : out std_logic_vector(C_IMPL_N_TMR-1 downto 0);
        all_tcon_tr1_o : out std_logic_vector(C_IMPL_N_TMR-1 downto 0);
        all_tmod_o     : out std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
        all_reload_o   : out std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
        all_wt_o       : out std_logic_vector(2*C_IMPL_N_TMR-1 downto 0);
        all_wt_en_o    : out std_logic_vector(C_IMPL_N_TMR-1 downto 0);

        all_tf0_i : in std_logic_vector(C_IMPL_N_TMR-1 downto 0);
        all_tf1_i : in std_logic_vector(C_IMPL_N_TMR-1 downto 0);
        all_tl0_i : in std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
        all_tl1_i : in std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
        all_th0_i : in std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
        all_th1_i : in std_logic_vector(8*C_IMPL_N_TMR-1 downto 0));

end mc8051_control;

