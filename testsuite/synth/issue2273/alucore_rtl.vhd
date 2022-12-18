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
--         Filename:               alucore_rtl.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.5 $
--
--         Date of Latest Version: $Date: 2002-01-07 12:17:44 $
--
--
--         Description: This unit performs simple logical operations.
--
--
--
--
-------------------------------------------------------------------------------
architecture rtl of alucore is

  constant LAND : std_logic_vector(3 downto 0) := "0011";
  constant LOR  : std_logic_vector(3 downto 0) := "0101";
  constant LXOR : std_logic_vector(3 downto 0) := "0110";
  constant RL   : std_logic_vector(3 downto 0) := "0111";
  constant RLC  : std_logic_vector(3 downto 0) := "1000";
  constant RR   : std_logic_vector(3 downto 0) := "1001";
  constant RRC  : std_logic_vector(3 downto 0) := "1010";
  constant COMP : std_logic_vector(3 downto 0) := "1011";
  constant INV  : std_logic_vector(3 downto 0) := "1100";

begin                 -- architecture structural

  p_alu: process (alu_cmd_i, op_a_i, op_b_i, cy_i)

  begin
  
  case alu_cmd_i is
-------------------------------------------------------------------------------
    when LAND =>  -- op_a_i and op_b_i
      result_o <= op_a_i and op_b_i;
      cy_o <= cy_i;      
-------------------------------------------------------------------------------
    when LOR =>  -- op_a_i or op_b_i
      result_o <= op_a_i or op_b_i;
      cy_o <= cy_i;      
-------------------------------------------------------------------------------
    when LXOR =>  -- op_a_i xor op_b_i
      result_o <= op_a_i xor op_b_i;
      cy_o <= cy_i;      
-------------------------------------------------------------------------------
    when RL =>  -- rotate left op_a_i
      if DWIDTH > 1 then
        result_o(DWIDTH-1 downto 1) <= op_a_i(DWIDTH-2 downto 0);
	result_o(0) <= op_a_i(DWIDTH-1);
      else
        result_o <= op_a_i;
      end if;
      cy_o <= cy_i;      
-------------------------------------------------------------------------------
    when RLC =>  -- rotate left op_a_i with CY
      if DWIDTH > 1 then
        result_o(DWIDTH-1 downto 1) <= op_a_i(DWIDTH-2 downto 0);
	result_o(0) <= cy_i((DWIDTH-1)/4);
      else
        result_o(0) <= cy_i((DWIDTH-1)/4);
      end if;
      cy_o <= cy_i;      
      cy_o((DWIDTH-1)/4) <= op_a_i(DWIDTH-1);      
-------------------------------------------------------------------------------
    when RR =>  -- rotate right op_a_i
      if DWIDTH > 1 then
        result_o(DWIDTH-2 downto 0) <= op_a_i(DWIDTH-1 downto 1);
	result_o(DWIDTH-1) <= op_a_i(0);
      else
        result_o <= op_a_i;
      end if;
      cy_o <= cy_i;      
-------------------------------------------------------------------------------
    when RRC =>  -- rotate right op_a_i with CY
      if DWIDTH > 1 then
        result_o(DWIDTH-2 downto 0) <= op_a_i(DWIDTH-1 downto 1);
	result_o(DWIDTH-1) <= cy_i((DWIDTH-1)/4);
      else
        result_o(0) <= cy_i((DWIDTH-1)/4);
      end if;
      cy_o <= cy_i;      
      cy_o((DWIDTH-1)/4) <= op_a_i(0);      
-------------------------------------------------------------------------------
    when COMP =>  -- Compare op_a_i with op_b_i
      if op_a_i = op_b_i then
        result_o <= (others => '0');
      else
        result_o <= (others => '1');
      end if;
      cy_o <= cy_i;         
      if op_a_i < op_b_i then
        cy_o((DWIDTH-1)/4) <= '1';
      else
        cy_o((DWIDTH-1)/4) <= '0';          
      end if;
-------------------------------------------------------------------------------
    when INV =>  -- invert op_a_i
      result_o <= not(op_a_i);
      cy_o <= cy_i;      
-------------------------------------------------------------------------------
    when others =>  -- turn unit off
      result_o <= (others => '0');      
      cy_o <= (others => '0');
-------------------------------------------------------------------------------
  end case;
  
 end process p_alu;

end rtl;

