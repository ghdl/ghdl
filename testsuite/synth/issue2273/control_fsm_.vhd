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
--         Filename:               control_fsm_.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.6 $
--
--         Date of Latest Version: $Date: 2006-09-07 09:57:24 $
--
--
--         Description: Decode instruction and execute it. Pure combinational
--                      descripton of the finite state machine.
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
entity control_fsm is

  port (state_i    : in t_state;  	-- actual state
        help_i     : in std_logic_vector (7 downto 0);  -- general help-reg
        bit_data_i : in std_logic;  	-- bitdata from regs
        aludata_i  : in std_logic_vector (7 downto 0);  -- ALU result
        command_i  : in std_logic_vector (7 downto 0);  -- actual command
        inthigh_i  : in std_logic;  	-- high priority int is running
        intlow_i   : in std_logic;  	-- low priority int is running
        intpre_i   : in std_logic;  	-- an interrupt must start
        intpre2_i  : in std_logic;  	-- prepare for interrupt
        intblock_i : in std_logic;      -- interrupt delay at RETI, IE, IP
        ti_i       : in std_logic;
        ri_i       : in std_logic;
        ie0_i      : in std_logic;
        ie1_i      : in std_logic;
        tf0_i      : in std_logic;
        tf1_i      : in std_logic;
        acc        : in std_logic_vector(7 downto 0);
        psw        : in std_logic_vector(7 downto 0);
        ie         : in std_logic_vector(7 downto 0);
        ip         : in std_logic_vector(7 downto 0);

        alu_cmd_o    : out std_logic_vector (5 downto 0);  -- ALU code
        pc_inc_en_o  : out std_logic_vector (3 downto 0);
        nextstate_o  : out t_state;  	-- enable signal for state
        adr_mux_o    : out std_logic_vector (3 downto 0);
        adrx_mux_o   : out std_logic_vector (1 downto 0);
        wrx_mux_o    : out std_logic;
        data_mux_o   : out std_logic_vector (3 downto 0);
        bdata_mux_o  : out std_logic_vector (3 downto 0);
        regs_wr_en_o : out std_logic_vector (2 downto 0);
        help_en_o    : out std_logic_vector (3 downto 0);
        help16_en_o  : out std_logic_vector (1 downto 0);
        helpb_en_o   : out std_logic;
        inthigh_en_o : out std_logic;
        intlow_en_o  : out std_logic;
        intpre2_en_o : out std_logic;
        inthigh_d_o  : out std_logic;
        intlow_d_o   : out std_logic;
        intpre2_d_o  : out std_logic;
        ext0isr_d_o   : out std_logic;
        ext1isr_d_o   : out std_logic;
        ext0isrh_d_o  : out std_logic;
        ext1isrh_d_o  : out std_logic;
        ext0isr_en_o  : out std_logic;
        ext1isr_en_o  : out std_logic;
        ext0isrh_en_o : out std_logic;
        ext1isrh_en_o : out std_logic);

end control_fsm;
