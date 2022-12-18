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
--         Filename:               mc8051_core_.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.8 $
--
--         Date of Latest Version: $Date: 2002-01-07 12:17:45 $
--
--
--         Description: Connect the mc8051_control and mc8051_alu modules.
--                      Generate and connect a certain number of mc8051_tmrctr
--                      and mc8051_siu units.
--
--
--
--
-------------------------------------------------------------------------------
library IEEE; 
use IEEE.std_logic_1164.all; 
use IEEE.std_logic_arith.all; 
library work;
use work.mc8051_p.all;
  
-----------------------------ENTITY DECLARATION--------------------------------

entity mc8051_core is
    
  port (clk        : in std_logic;   -- system clock
        cen        : in std_logic;   -- system clock enable
        reset      : in std_logic;   -- system reset
        rom_data_i : in std_logic_vector(7 downto 0);  -- data input from ROM 
        ram_data_i : in std_logic_vector(7 downto 0);  -- data input from
                                                       -- internal RAM
        int0_i     : in std_logic_vector(C_IMPL_N_EXT-1 downto 0);  -- ext.Int
        int1_i     : in std_logic_vector(C_IMPL_N_EXT-1 downto 0);  -- ext.Int
        -- counter input 0 for T/C
        all_t0_i   : in std_logic_vector(C_IMPL_N_TMR-1 downto 0);
        -- counter input 1 for T/C
        all_t1_i   : in std_logic_vector(C_IMPL_N_TMR-1 downto 0);
        -- serial input for SIU
        all_rxd_i  : in std_logic_vector(C_IMPL_N_SIU-1 downto 0);
        p0_i       : in std_logic_vector(7 downto 0);  -- IO-port0 input
        p1_i       : in std_logic_vector(7 downto 0);  -- IO-port1 input
        p2_i       : in std_logic_vector(7 downto 0);  -- IO-port2 input
        p3_i       : in std_logic_vector(7 downto 0);  -- IO-port3 input 

        p0_o        : out std_logic_vector(7 downto 0);  -- IO-port0 output
        p1_o        : out std_logic_vector(7 downto 0);  -- IO-port1 output
        p2_o        : out std_logic_vector(7 downto 0);  -- IO-port2 output
        p3_o        : out std_logic_vector(7 downto 0);  -- IO-port3 output
        -- M0 serial output for SIU
        all_rxd_o   : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
        -- serial output for SIU 
        all_txd_o   : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
        -- rxd direction signal
        all_rxdwr_o : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
        
        rom_adr_o   : out std_logic_vector(15 downto 0);  -- Programmcounter =
                                                          -- ROM-adress
        ram_data_o  : out std_logic_vector(7 downto 0); -- data output to
                                                        -- internal RAM
        ram_adr_o   : out std_logic_vector(6 downto 0); -- internal RAM-address
        ram_wr_o    : out std_logic;                    -- read (0) / write (1)
        ram_en_o    : out std_logic;                    -- RAM-block enable

        datax_i : in  std_logic_vector (7 downto 0);   -- ext. RAM data input
        datax_o : out std_logic_vector (7 downto 0);   -- ext. RAM data output
        adrx_o  : out std_logic_vector (15 downto 0);  -- ext. RAM address
        memx_o  : out std_logic;                       -- ext. RAM access
        wrx_o   : out std_logic);                      -- ext. RAM write enable

end mc8051_core;

