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
--         Filename:               mc8051_siu_.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.5 $
--
--         Date of Latest Version: $Date: 2002-01-07 12:17:45 $
--
--
--         Description: Serial interface unit for the mc8051 microcontroller.
--
--
--
--
-------------------------------------------------------------------------------
library IEEE; 
use IEEE.std_logic_1164.all; 
use IEEE.std_logic_arith.all; 
  
-----------------------------ENTITY DECLARATION--------------------------------

entity mc8051_siu is

  port (clk     : in std_logic;  		    --< system clock
        cen     : in std_logic;  		    --< system clock enable
        reset   : in std_logic;             --< system reset
        tf_i    : in std_logic;  		    --< timer1 overflow flag
        trans_i : in std_logic;  		    --< 1 activates transm.
        rxd_i   : in std_logic;  		    --< serial data input
        scon_i  : in std_logic_vector(5 downto 0);  --< from SFR register
  						    --< bits 7 to 3
        sbuf_i  : in std_logic_vector(7 downto 0);  --< data for transm.
        smod_i  : in std_logic;  		    --< low(0)/high baudrate

        sbuf_o  : out std_logic_vector(7 downto 0);  --< received data 
        scon_o  : out std_logic_vector(2 downto 0);  --< to SFR register 
  						     --< bits 0 to 2
        rxdwr_o : out std_logic;  	             --< rxd direction signal
        rxd_o   : out std_logic;  		     --< mode0 data output
        txd_o   : out std_logic);  		     --< serial data output
  
end mc8051_siu;

