library IEEE;
use IEEE.std_logic_1164.all;

entity sub is
	generic(
		WIDTH : positive := 32
	);
	port (
		data : out std_ulogic_vector(WIDTH-1 downto 0)
	);
end sub;

library IEEE;
use IEEE.std_logic_1164.all;

entity bug is
	generic(
		WIDTH : positive := 32
	);
	port (
		data : out std_ulogic_vector(WIDTH-1 downto 0)
	);
end bug;

architecture struct of sub is
begin
	data <= (others => '0');
end architecture;

architecture struct of bug is
begin
	base: entity work.sub
	generic map(
		WIDTH => WIDTH
	)
	port map(
		data => data
	);
end architecture;
