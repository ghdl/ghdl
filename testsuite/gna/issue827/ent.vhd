library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity B is
	generic(
		GENERIC_VALUE : positive := 2
	);
	port(
		output : out std_ulogic_vector(31 downto 0)
	);
end B;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity A is
	port(
		out_a : out unsigned(15 downto 0);
		out_b : out unsigned(15 downto 0)
	);
end A;

architecture struct of A is
begin
	out_a <= (others => '0');
	out_b <= (others => '1');
end architecture;

architecture struct of B is
begin
	a_0 : entity work.A
	port map(
		std_ulogic_vector(out_a) => output(output'high downto output'high-GENERIC_VALUE*8+1),
		std_ulogic_vector(out_b) => output(15 downto 0)
	);

end architecture;
