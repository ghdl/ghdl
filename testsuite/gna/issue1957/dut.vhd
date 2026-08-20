library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.t_config;

entity dut is
	port(
		i_config : in t_config(
			param_a(31 downto 0),
			param_b(31 downto 0)
		)
	);
end entity;

architecture arch of dut is
begin
end architecture arch;
