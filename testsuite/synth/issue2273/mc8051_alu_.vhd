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
--         Filename:               mc8051_alu_.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.7 $
--
--         Date of Latest Version: $Date: 2002-01-07 12:17:44 $
--
--
--         Description: Connects the units alumux, alucore, addsub_core,
--                      comb_mltplr, comb_divider, and dcml_adjust together.
--                      The whole design is made up of combinational logic.
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

entity mc8051_alu is

  generic (DWIDTH : integer := 8);             -- Data width of the ALU

  port (rom_data_i : in std_logic_vector(DWIDTH-1 downto 0);
        ram_data_i : in std_logic_vector(DWIDTH-1 downto 0);
        acc_i      : in std_logic_vector(DWIDTH-1 downto 0);
        cmd_i      : in std_logic_vector(5 downto 0);
        cy_i       : in std_logic_vector((DWIDTH-1)/4 downto 0);
        ov_i       : in std_logic;
  
        new_cy_o   : out std_logic_vector((DWIDTH-1)/4 downto 0);
        new_ov_o   : out std_logic;
        result_a_o : out std_logic_vector(DWIDTH-1 downto 0);
        result_b_o : out std_logic_vector(DWIDTH-1 downto 0));
  
end mc8051_alu;
--Inputs:
-- rom_data_i...... data input from ROM
-- ram_data_i...... data input from RAM
-- acc_i........... the contents of the accumulator register
-- cmd_i........... command from the control unit
-- cy_i............ CY-Flags of the SFR
-- ov_i............ OV-Flag of the SFR
--Outputs:
-- new_cy_o........ new CY-Flags for SFR
-- new_ov_o........ new OV-Flag for SFR
-- result_a_o...... result
-- result_b_o...... result
