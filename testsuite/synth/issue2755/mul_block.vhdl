library ieee;
use ieee.std_logic_1164.all;

entity top is
	port(
		data_in  : in  std_logic;
		data_out : out std_logic
	);
end top;

architecture synth of top is

	component comp is
		generic (
			param_str : string := "WANNABE_STRING_DEFAULT"
		);
		port (
			data_in  : in  std_logic;
			data_out : out std_logic
		);
	end component;

begin

	comp_i : comp
		generic map (
			param_str => "WANNABE_STRING"
		)
		port map (
			data_in  => data_in,
			data_out => data_out
		);

end architecture;
