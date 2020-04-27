library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb2 is
end entity;

architecture test of tb2 is
	constant w : integer := 4;
	signal s_out : std_logic_vector(w-1 downto 0);
begin
	e : entity work.ent
		generic map(
			WIDTH => w
		)
		port map(
			o_slv => s_out
		);

	process
	begin
		wait for 1 ns;
		report integer'image(to_integer(unsigned(s_out)));
		wait;
	end process;
end architecture test;
