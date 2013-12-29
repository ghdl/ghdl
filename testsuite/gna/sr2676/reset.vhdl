library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.reset_types.all;


entity power_on_reset is
	generic(
		clk_period_ns: unsigned := X"14"
	);
	port (
		clk: in std_logic;
		reset: in std_logic;

		core_en: out std_logic;
		sys_clken: out std_logic;
		clk_33mhz_en: out std_logic;
		cfg_drv: out std_logic;
		rd_pmbrd_rev : out std_logic;
		

		cur_state: out reset_state
	);
end power_on_reset;

