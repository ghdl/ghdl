
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
-- $Id: ch_10_alu-b.vhd,v 1.3 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library bv_utilities;
use bv_utilities.bv_arithmetic.all;

architecture behavior of alu is
begin

  alu_op: process (s1, s2, func) is

                                   constant Tpd : delay_length := 10 ns;

                                 variable bv_s1 : bit_vector(s1'range) := To_bitvector(s1);
                                 variable bv_s2 : bit_vector(s2'range) := To_bitvector(s2);
                                 variable temp_result : bit_vector(result'range);
                                 constant zero_result : bit_vector(result'range) := (others => '0');
                                 variable temp_overflow : boolean;

                                 type boolean_to_X01_table is array (boolean) of X01;
                                 constant boolean_to_X01 : boolean_to_X01_table
                                   := ( false => '0', true => '1' );

  begin
    case func is
      when alu_add =>
        bv_add(bv_s1, bv_s2, temp_result, temp_overflow);
      when alu_addu =>
        bv_addu(bv_s1, bv_s2, temp_result, temp_overflow);
      when alu_sub =>
        bv_sub(bv_s1, bv_s2, temp_result, temp_overflow);
      when alu_subu =>
        bv_subu(bv_s1, bv_s2, temp_result, temp_overflow);
      when others =>
        report "alu: illegal function code" severity error;
        temp_result := X"0000_0000";
    end case;
    result <= To_X01(temp_result) after Tpd;
    zero <= boolean_to_X01(temp_result = zero_result) after Tpd;
    negative <= To_X01(temp_result(temp_result'left)) after Tpd;
    overflow <= boolean_to_X01(temp_overflow) after Tpd;
  end process alu_op;

end architecture behavior;
