--!
--! Copyright (C) 2011 - 2014 Creonic GmbH
--!
--! This file is part of the Creonic Viterbi Decoder, which is distributed
--! under the terms of the GNU General Public License version 2.
--!
--! @file
--! @brief  Helper package with useful functions
--! @author Markus Fehrenz
--! @date   2011/12/02
--!

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


package pkg_helper is

	--!
	--! Return the log_2 of an natural value, i.e. the number of bits required
	--! to represent this unsigned value.
	--!
	function no_bits_natural(value_in : natural) return natural;

	--! Return maximum of two input values
	function max(value_in_a, value_in_b : natural) return natural;

end pkg_helper;


package body pkg_helper is

	function no_bits_natural(value_in: natural) return natural is
		variable v_n_bit : unsigned(31 downto 0);
	begin
		if value_in = 0 then
			return 0;
		end if;
		v_n_bit := to_unsigned(value_in, 32);
		for i in 31 downto 0 loop
			if v_n_bit(i) = '1' then
				return i + 1;
			end if;
		end loop;
		return 1;
	end no_bits_natural;

	function max(value_in_a, value_in_b : natural) return natural is
	begin
		if value_in_a > value_in_b then
			return value_in_a;
		else
			return value_in_b;
		end if;
	end function;

end pkg_helper;
