library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.t_config;

entity tb_dut is
end tb_dut;

architecture behaviour of tb_dut is
	component dut is
		port(
			i_config : in t_config(
				param_a(31 downto 0),
				param_b(31 downto 0)
			)
		);
	end component;

	constant s_config : t_config(
		param_a(31 downto 0),
		param_b(31 downto 0)
	) := (
		std_logic_vector(to_unsigned(34,32)),
		std_logic_vector(to_unsigned(55,32))
	);
begin
	dutlbl : dut
		port map(
			i_config => s_config
		);
end architecture;
