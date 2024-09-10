library ieee;
use ieee.std_logic_1164.all;

package slv is
	generic(
		N: integer
	);
	subtype slv_t is std_logic_vector(N-1 downto 0);

	function foo return integer;

end package;

package body slv is

	function foo return integer is
	begin
		return 12345;
	end function foo;

end package body slv;

library ieee;
use ieee.std_logic_1164.all;

entity ent is
	generic(
		 WIDTH: integer;
		 package slv_pkg is new work.slv
		 	generic map(N => WIDTH)
	);
	port (
		o_slv: out slv_pkg.slv_t
	);
end ent;

architecture beh of ent is
	constant ones : std_logic_vector(WIDTH-1 downto 0) := (others => '1');
begin
	o_slv <= ones;
end architecture beh;

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
