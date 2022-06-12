library ieee;
use ieee.std_logic_1164.all;

entity tb_ent is
end entity;

architecture a of tb_ent is
	signal a, b : std_logic_vector(7 downto 0);
begin
	uut: entity work.ent port map (a => a, b => b);

	process
	begin
		a <= x"42";
		wait for 1 ns;
		assert b = x"00";

		wait;
	end process;
end architecture;
