-- Author: Patrick Lehmann
--
-- Constrained array types: the type declaration was translated to a stub named '????'.
package ConstrainedArray is
	type state_t is (idle, run);

	type ranged_t     is array (0 to 3) of bit;
	type enumerated_t is array (state_t) of integer;
	type matrix_t     is array (7 downto 0, state_t) of bit_vector(1 downto 0);
	type subtyped_t   is array (natural range 0 to 7) of bit;
end package;
