library ieee;
use ieee.std_logic_1164.all;

entity ent is
	port (
		reset : in std_logic;
		clk : in std_logic
	);
end ent;

architecture rtl of ent is
	function const return natural is
	begin
		return 1;
	end const;

	constant MAX_COUNT : natural := const;
	signal countdown : natural;

	signal x : std_logic;
	signal y : std_logic;
begin
	x <= '1';
	y <= '1';

	process(reset, clk)
	begin
		if reset then
			countdown <= MAX_COUNT;
		elsif rising_edge(clk) then
			if x then
				if y then
					countdown <= MAX_COUNT;
				end if;
			end if;
		end if;
	end process;
end rtl;
