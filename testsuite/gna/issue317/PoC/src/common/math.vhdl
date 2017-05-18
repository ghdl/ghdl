-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- =============================================================================
-- Authors:					Patrick Lehmann
--
-- Package:					Math extension package.
--
-- Description:
-- -------------------------------------
--		This package provides additional math functions.
--
-- License:
-- =============================================================================
-- Copyright 2007-2015 Technische Universitaet Dresden - Germany,
--										 Chair of VLSI-Design, Diagnostics and Architecture
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--		http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- =============================================================================

library IEEE;
use			IEEE.std_logic_1164.all;
use			IEEE.numeric_std.all;

library PoC;
use			PoC.utils.all;


package math is
	-- figurate numbers
	function squareNumber(N : natural) return natural;
	function cubicNumber(N : natural) return natural;
	function triangularNumber(N : natural) return natural;

	-- coefficients
	-- binomial coefficient (N choose K)
	function binomialCoefficient(N : positive; K : positive) return positive;

	-- greatest common divisor (gcd)
	function greatestCommonDivisor(N1 : positive; N2 : positive) return positive;
	-- least common multiple (lcm)
	function leastCommonMultiple(N1 : positive; N2 : positive) return positive;
end package;

package body math is
	-- figurate numbers
	function squareNumber(N : natural) return natural is
	begin
		return N*N;
	end function;

	function cubicNumber(N : natural) return natural is
	begin
		return N*N*N;
	end function;

	function triangularNumber(N : natural) return natural is
		variable T	: natural;
	begin
		return (N * (N + 1) / 2);
	end function;

	-- coefficients
	function binomialCoefficient(N : positive; K : positive) return positive is
		variable Result		: positive;
	begin
		Result		:= 1;
		for i in 1 to K loop
			Result := Result * (((N + 1) - i) / i);
		end loop;
		return Result;
	end function;

	-- greatest common divisor (gcd)
	function greatestCommonDivisor(N1 : positive; N2 : positive) return positive is
		variable M1				: positive;
		variable M2				: natural;
		variable Remainer	: natural;
	begin
		M1	:= imax(N1, N2);
		M2	:= imin(N1, N2);
		while M2 /= 0 loop
			Remainer	:= M1 mod M2;
			M1				:= M2;
			M2				:= Remainer;
		end loop;
		return M1;
	end function;

	-- least common multiple (lcm)
	function leastCommonMultiple(N1 : positive; N2 : positive) return positive is
	begin
		return ((N1 * N2) / greatestCommonDivisor(N1, N2));
	end function;
end package body;
