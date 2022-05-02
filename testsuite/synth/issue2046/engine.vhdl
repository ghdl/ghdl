library ieee;
use ieee.std_logic_1164.all;

entity phony is
	generic (
		INDEX : natural := 0
	);
	port (
		i : in  std_logic;
		o : out std_logic
	);
end entity;

architecture synth of phony is

	signal o6 : std_logic := '0';

	component LUT6 is
		generic (
			INIT : bit_vector
		);
		port (
			O  : out std_logic;
			I0 : in  std_logic;
			I1 : in  std_logic;
			I2 : in  std_logic;
			I3 : in  std_logic;
			I4 : in  std_logic;
			I5 : in  std_logic
		);
	end component;

begin

	lutA : LUT6
		generic map (
			INIT => x"0123456789012345"
		)
		port map (
			O  => o6,
			I0 => i,
			I1 => i,
			I2 => i,
			I3 => i,
			I4 => i,
			I5 => i
		);

	o <= o6;

end architecture;

library ieee;
use ieee.std_logic_1164.all;

library work;
use work.all;

entity engine is
	generic (
		SIZE : natural := 1000
	);
	port (
		i : in  std_logic;
		o : out std_logic
	);
end entity;

architecture synth of engine is

begin

	chunks : for b in 0 to SIZE-1 generate

		ph : entity phony
			generic map (
				INDEX => b
			)
			port map (
				i => i,
				o => open
			);

	end generate;

	o <= i;

end architecture;
