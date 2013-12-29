library ieee;
use ieee.std_logic_1164.all;
use std.textio.all;
use ieee.numeric_std.all;

use work.reset_types.all;

entity reset_testbench is
end reset_testbench;

architecture behavior of reset_testbench is

component power_on_reset
	port (
		clk: in std_logic;
		reset: in std_logic;

		core_en: out std_logic;
		sys_clken: out std_logic;
		cfg_drv: out std_logic;
		rd_pmbrd_rev : out std_logic;

		clk_33mhz_en: out std_logic;

		cur_state: out reset_state
	);
end component;

--for por_0: power_on_reset use entity work.power_on_reset;

signal clk: std_logic;
signal reset: std_logic;
signal core_en: std_logic;
signal sys_clken: std_logic;
signal cfg_drv: std_logic;
signal rd_pmbrd_rev: std_logic;
signal clk_33mhz_en: std_logic;
signal current_state: reset_state;

constant CLK50MHZ_period: time := 20 ns;  
constant CLK33_period: time := 33 ns;
constant LB_LCLK0_period: time := 16 ns; 

begin

	por_0: power_on_reset port map (
		clk => clk,
		reset => reset,

		core_en => core_en,
		sys_clken => sys_clken,
		cfg_drv => cfg_drv,
		rd_pmbrd_rev => rd_pmbrd_rev,

		clk_33mhz_en => clk_33mhz_en,

		cur_state => current_state
	);


CLK50MHZ_process :process
begin
	clk <= '0';
	wait for CLK50MHZ_period/2;
	clk <= '1';
	wait for CLK50MHZ_period/2;
end process;

stim: process
variable l: line;
begin
	report "asserting reset" severity note;
	reset <= '1';

	report "current_state is " & reset_state'image(current_state);
	wait until current_state = reset_state'value("RST");	
	report "saw state RST" severity note;
	
	wait for CLK50MHZ_period * 3;
	report "de-asserting reset" severity note;
	reset <= '0';

	wait until core_en = '1';
	report "saw core_en" severity note;

	-- personality module id
	wait until rd_pmbrd_rev = '1';
	report "saw rd_pmbrd_rev assert" severity note;
	wait until rd_pmbrd_rev = '0';
	report "saw rd_pmbrd_rev de-assert" severity note;



	wait for CLK50MHZ_period * 100;
	report "end of test" severity note;
	wait;
end process;


end behavior;
