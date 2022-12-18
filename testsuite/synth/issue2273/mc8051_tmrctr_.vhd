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
--         Author:                 Roland Höller
--
--         Filename:               mc8051_tmrctr_.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.4 $
--
--         Date of Latest Version: $Date: 2002-01-07 12:17:45 $
--
--
--         Description: Timer/Counter unit of the mc8051 microcontroller.
--
--
--
--
-------------------------------------------------------------------------------
library IEEE; 
use IEEE.std_logic_1164.all; 
use IEEE.std_logic_arith.all; 
  
-----------------------------ENTITY DECLARATION--------------------------------

entity mc8051_tmrctr is

  port (clk        : in  std_logic;  			--< system clock
        cen        : in  std_logic;  			--< system clock enable
        reset      : in  std_logic;             --< system reset
        int0_i     : in  std_logic;  			--< interrupt 0
        int1_i     : in  std_logic;  			--< interrupt 1
        t0_i       : in  std_logic;  			--< external clock for
  							--  timer/counter0
        t1_i       : in  std_logic;  			--< external clock for
  							--  timer/counter1
        tmod_i     : in  std_logic_vector(7 downto 0);  --< from SFR register
        tcon_tr0_i : in  std_logic;  			--< timer run 0
        tcon_tr1_i : in  std_logic;  			--< timer run 1
        reload_i   : in  std_logic_vector(7 downto 0);  --< to load counter
        wt_en_i    : in  std_logic;  			--< indicates reload
        wt_i       : in  std_logic_vector(1 downto 0);  --< reload which reg.
        th0_o      : out std_logic_vector(7 downto 0);  --< contents of th0 
        tl0_o      : out std_logic_vector(7 downto 0);  --< contents of tl0 
        th1_o      : out std_logic_vector(7 downto 0);  --< contents of th1 
        tl1_o      : out std_logic_vector(7 downto 0);  --< contents of tl1 
        tf0_o      : out std_logic;  			--< interrupt flag 0
        tf1_o      : out std_logic);  			--< interrupt flag 1
      
end mc8051_tmrctr;

