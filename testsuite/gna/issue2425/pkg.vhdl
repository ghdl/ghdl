library IEEE;
use IEEE.STD_LOGIC_1164.all;

package pkg is

	constant MY_CONSTANT    : std_logic_vector(15 downto 0) := (others => '0');
	constant MY_CONSTANT2   : std_logic_vector(15 downto 0) := to_slv(MY_CONSTANT);

end package;
