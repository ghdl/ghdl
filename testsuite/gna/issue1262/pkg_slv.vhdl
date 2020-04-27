library ieee;
use ieee.std_logic_1164.all;

package slv is
	generic(
		N: integer
	);
	subtype slv_t is std_logic_vector(N-1 downto 0);
end package;
