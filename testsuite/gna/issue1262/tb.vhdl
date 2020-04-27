library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb is
end entity;

architecture test of tb is
	constant w : integer := 3;

	package slv_tb_pkg is new work.slv generic map(N => w);

	component ent
		generic(
			WIDTH: integer
		);
		port (
			o_slv: out slv_tb_pkg.slv_t
		);
	end component ent;

	signal s_out : std_logic_vector(w-1 downto 0);
begin
	e : ent
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
