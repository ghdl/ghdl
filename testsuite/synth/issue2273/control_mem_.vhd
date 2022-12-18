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
--         Filename:               control_mem_.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.6 $
--
--         Date of Latest Version: $Date: 2006-09-07 09:59:09 $
--
--
--         Description: Describe all sequential funcitonality like read from
--                      special function registers, observe interrupt sources,
--                      write to special function registers, and read or write
--                      to the bit addressable memory area.
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


entity control_mem is

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
        aludata_i  : in  std_logic_vector (7 downto 0);  -- ALU result
        aludatb_i  : in  std_logic_vector (7 downto 0);  -- 2nd ALU result
        acc_o      : out std_logic_vector (7 downto 0);  -- ACC register
        new_cy_i   : in  std_logic_vector(1 downto 0);   -- CY result of ALU
        new_ov_i   : in  std_logic;  	-- OV result of ALU
        reset      : in  std_logic;  	-- reset signal
        clk        : in  std_logic;  	-- clock signal
        cen        : in  std_logic;     -- clock enable
        int0_i     : in  std_logic_vector(C_IMPL_N_EXT-1 downto 0);  -- ext.Int
        int1_i     : in  std_logic_vector(C_IMPL_N_EXT-1 downto 0);  -- ext.Int

        p0_i : in std_logic_vector(7 downto 0);  -- IO-port0
        p1_i : in std_logic_vector(7 downto 0);  -- IO-port1
        p2_i : in std_logic_vector(7 downto 0);  -- IO-port2
        p3_i : in std_logic_vector(7 downto 0);  -- IO-port3

        p0_o : out std_logic_vector(7 downto 0);  -- IO-port0
        p1_o : out std_logic_vector(7 downto 0);  -- IO-port1
        p2_o : out std_logic_vector(7 downto 0);  -- IO-port2
        p3_o : out std_logic_vector(7 downto 0);  -- IO-port3

        -- Signals to and from the SIUs

        -- "1" starts serial transmission in SIU
        all_trans_o : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
        -- RI,SM0,SM1,SM2,REN,TB8
        all_scon_o  : out std_logic_vector(6*C_IMPL_N_SIU-1 downto 0);
        -- data buffer for SIU
        all_sbuf_o  : out std_logic_vector(8*C_IMPL_N_SIU-1 downto 0);
        -- baud rate for SIU in PCON
        all_smod_o  : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
        -- RB8, TI, RI of SIU
        all_scon_i  : in  std_logic_vector(3*C_IMPL_N_SIU-1 downto 0);
        -- int. data buffer of SIU
        all_sbuf_i  : in  std_logic_vector(8*C_IMPL_N_SIU-1 downto 0);

        -- signals to and from the timer/counters

        -- timer run flag0 of T/C
        all_tcon_tr0_o : out std_logic_vector(C_IMPL_N_TMR-1 downto 0);
        -- timer run flag1 of T/C
        all_tcon_tr1_o : out std_logic_vector(C_IMPL_N_TMR-1 downto 0);
        -- TMOD for T/C
        all_tmod_o     : out std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
        -- user reload value for T/C
        all_reload_o   : out std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
        -- reload enable for T/C
        all_wt_o       : out std_logic_vector(2*C_IMPL_N_TMR-1 downto 0);
        -- reload target for T/C
        all_wt_en_o    : out std_logic_vector(C_IMPL_N_TMR-1 downto 0);
        -- timer OF flag0 of T/C
        all_tf0_i      : in  std_logic_vector(C_IMPL_N_TMR-1 downto 0);
        -- timer OF flag1 of T/C
        all_tf1_i      : in  std_logic_vector(C_IMPL_N_TMR-1 downto 0);
        -- count value of T/C
        all_tl0_i      : in  std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
        -- count value of T/C
        all_tl1_i      : in  std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
        -- count value of T/C
        all_th0_i      : in  std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
        -- count value of T/C
        all_th1_i      : in  std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);

        -- signals from/to the state-machine

        state_o    : out t_state;  	-- actual state
        help_o     : out std_logic_vector(7 downto 0);   -- general help-reg
        bit_data_o : out std_logic;  	-- bitdata from regs
        command_o  : out std_logic_vector (7 downto 0);  -- actual command
        inthigh_o  : out std_logic;  	-- high priority int is running
        intlow_o   : out std_logic;  	-- low priority int is running
        intpre_o   : out std_logic;  	-- an interrupt must start
        intpre2_o  : out std_logic;  	-- prepare for interrupt
        intblock_o : out std_logic;     -- interrupt delay at RETI, IE, IP
        ti_o       : out std_logic;
        ri_o       : out std_logic;
        ie0_o      : out std_logic;
        ie1_o      : out std_logic;
        tf0_o      : out std_logic;
        tf1_o      : out std_logic;
        psw_o      : out std_logic_vector(7 downto 0);
        ie_o       : out std_logic_vector(7 downto 0);
        ip_o       : out std_logic_vector(7 downto 0);
        adrx_o     : out std_logic_vector(15 downto 0);  -- ext. RAM
        datax_o    : out std_logic_vector(7 downto 0);   -- ext. RAM
        wrx_o      : out std_logic;  	-- ext. RAM
        memx_o     : out std_logic;

        datax_i      : in std_logic_vector(7 downto 0);  -- ext. RAM
        pc_inc_en_i  : in std_logic_vector (3 downto 0);
        nextstate_i  : in t_state;  	-- enable signal for state
        adr_mux_i    : in std_logic_vector (3 downto 0);
        adrx_mux_i   : in std_logic_vector (1 downto 0);
        wrx_mux_i    : in std_logic;
        data_mux_i   : in std_logic_vector (3 downto 0);
        bdata_mux_i  : in std_logic_vector (3 downto 0);
        regs_wr_en_i : in std_logic_vector (2 downto 0);
        help_en_i    : in std_logic_vector (3 downto 0);
        help16_en_i  : in std_logic_vector (1 downto 0);
        helpb_en_i   : in std_logic;
        inthigh_en_i : in std_logic;
        intlow_en_i  : in std_logic;
        intpre2_en_i : in std_logic;
        inthigh_d_i  : in std_logic;
        intlow_d_i   : in std_logic;
        intpre2_d_i  : in std_logic;
        ext0isr_d_i   : in std_logic;
        ext1isr_d_i   : in std_logic;
        ext0isrh_d_i  : in std_logic;
        ext1isrh_d_i  : in std_logic;
        ext0isr_en_i  : in std_logic;
        ext1isr_en_i  : in std_logic;
        ext0isrh_en_i : in std_logic;
        ext1isrh_en_i : in std_logic);

end control_mem;
