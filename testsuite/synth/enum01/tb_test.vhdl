library ieee;
use ieee.std_logic_1164.all;

use work.test_pkg.all;

entity tb_test is
end entity;

architecture a of tb_test is
	type reference_arr_t is array(number_t, number_t) of std_logic_vector(1 to 6);
	signal reference_arr : reference_arr_t;

	signal x, y : number_t;
	signal eq, neq, lt, lte, gt, gte : boolean;
	signal result : std_logic_vector(1 to 6);
begin
	reference_arr(ONE,   ONE)   <= "100101";
	reference_arr(ONE,   TWO)   <= "011100";
	reference_arr(ONE,   THREE) <= "011100";
	reference_arr(TWO,   ONE)   <= "010011";
	reference_arr(TWO,   TWO)   <= "100101";
	reference_arr(TWO,   THREE) <= "011100";
	reference_arr(THREE, ONE)   <= "010011";
	reference_arr(THREE, TWO)   <= "010011";
	reference_arr(THREE, THREE) <= "100101";

	process
	begin
		for a in ONE to THREE loop
			for b in ONE to THREE loop
				x <= a;
				y <= b;

				wait for 10 ns;

				assert result = reference_arr(a, b);
			end loop;
		end loop;

		wait;
	end process;

	result(1) <= '1' when eq  else '0';
	result(2) <= '1' when neq else '0';
	result(3) <= '1' when lt  else '0';
	result(4) <= '1' when lte else '0';
	result(5) <= '1' when gt  else '0';
	result(6) <= '1' when gte else '0';

	test_inst: entity work.test
		port map (
			x => x,
			y => y,

			eq  => eq,
			neq => neq,
			lt  => lt,
			lte => lte,
			gt  => gt,
			gte => gte
		);
end architecture;
