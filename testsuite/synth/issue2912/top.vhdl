library ieee;
use ieee.std_logic_1164.all;

entity top is
	port (
		clk : in  std_logic
	);
end top;

architecture synth of top is

	signal sig : std_logic := '0';

	component compOut
		port (
			po : out std_logic
		);
	end component;

	component compIn is
		port (
			pi : in std_logic
		);
	end component;

begin

	instO1 : compOut
		port map (
			po => sig
		);

	instO2 : compOut
		port map (
			po => sig
		);

	instIn : compIn
		port map (
			pi => sig
		);

end architecture;
