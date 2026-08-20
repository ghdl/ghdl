library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package config_pkg is
	  -- Configuration
	type t_config is record
		param_a   : std_logic_vector;
		param_b   : std_logic_vector;
	end record t_config;
end package config_pkg;
