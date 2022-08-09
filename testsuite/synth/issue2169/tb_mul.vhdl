library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity tb_mul is
end entity;

architecture tb of tb_mul is
	signal a : unsigned( 7 downto 0);
	signal b : unsigned(15 downto 0);
	signal r : unsigned(23 downto 0);
begin
	
	u0 : entity work.mul
	port map(
		a => a,
		b => b,
		r => r
	);
	
	process
	begin
		a <= to_unsigned(243,8);
		b <= to_unsigned(34560,16);
		wait for 1 ns;
		report integer'image(to_integer(r));
		assert r = to_unsigned(8398080, 24);
		wait;
	end process;

end architecture;
