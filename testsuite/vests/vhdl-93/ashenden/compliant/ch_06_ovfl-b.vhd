
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
-- $Id: ch_06_ovfl-b.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

architecture behavioral of overflow_logic is

  constant Tpd_in_out : time := 3 ns;

begin

  ovf <= real_accumulator_ovf or imag_accumulator_ovf
	 or ( real_sum(21) xor real_sum(20) )
	 or ( real_sum(21) xor real_sum(19) )
	 or ( real_sum(21) xor real_sum(18) )
	 or ( real_sum(21) xor real_sum(17) )
	 or ( imag_sum(21) xor imag_sum(20) )
	 or ( imag_sum(21) xor imag_sum(19) )
	 or ( imag_sum(21) xor imag_sum(18) )
	 or ( imag_sum(21) xor imag_sum(17) ) after Tpd_in_out;

end architecture behavioral;
