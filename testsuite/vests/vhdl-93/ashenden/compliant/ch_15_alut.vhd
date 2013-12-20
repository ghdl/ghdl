
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
-- $Id: ch_15_alut.vhd,v 1.3 2001-11-03 23:19:37 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package alu_types is

  subtype alu_func is std_logic_vector(3 downto 0);

  constant alu_add :     alu_func := "0000";
  constant alu_addu :    alu_func := "0001";
  constant alu_sub :     alu_func := "0010";
  constant alu_subu :    alu_func := "0011";
  constant alu_and :     alu_func := "0100";
  constant alu_or :      alu_func := "0101";
  constant alu_xor :     alu_func := "0110";
  constant alu_sll :     alu_func := "1000";
  constant alu_srl :     alu_func := "1001";
  constant alu_sra :     alu_func := "1010";
  constant alu_pass_s1 : alu_func := "1100";
  constant alu_pass_s2 : alu_func := "1101";

end package alu_types;
