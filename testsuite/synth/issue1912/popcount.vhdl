library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.all;

entity popcount is
	generic (
		SIZE  : natural := 32
	);
	port (
		-- Input data
		data : in std_logic_vector(SIZE-1 downto 0);
		-- Results
		num  : out std_logic
	);
end entity;

architecture synth of popcount is

	-- Component declaration of itself, for recursive instantiation

	component popcount is
		generic (
			SIZE  : natural := 32
		);
		port (
			-- Input data
			data : in std_logic_vector(SIZE-1 downto 0);
			-- Results
			num  : out std_logic
		);
	end component;

begin

	n5 : if SIZE <= 5 generate

		num <= '1';

	end generate;

	more : if SIZE >= 6 generate

		signal num_1 : std_logic;
		signal num_2 : std_logic;

		constant SIZE_LOWER : natural := 4 * (SIZE / 5);

	begin

		inst1 : popcount
			generic map (
				SIZE  => SIZE_LOWER
			)
			port map (
				data => data(SIZE_LOWER-1 downto 0),
				num  => num_1
			);

		inst2 : popcount
			generic map (
				SIZE  => SIZE - SIZE_LOWER
			)
			port map (
				data => data(SIZE-1 downto SIZE_LOWER),
				num  => num_2
			);

		num <= num_1 or num_2;

	end generate;

end architecture;
