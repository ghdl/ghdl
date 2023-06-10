library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

entity bug_test is
end entity;

architecture tb of bug_test is
	constant clk_period : time := 10 ns;
	signal clk : std_logic;
	signal test : ufixed(1 downto -5);
	signal test_s : sfixed(1 downto -5);
begin

	clk_p : process
	begin
		clk <= '1';
		wait for clk_period / 2;
		clk <= '0';
		wait for clk_period / 2;
	end process;

	main : process
	begin
		test <= (others => '0'); --0ns
		wait until falling_edge(clk);
		test <= (others => '1'); -- 5ns
		wait until falling_edge(clk); -- 15ns
		test <= to_ufixed(0.017452406, test);
		wait until rising_edge(clk); -- 20ns
		assert test = to_ufixed(0.03125, test) report "Check ufixed failed: 0.017452406";
		wait until falling_edge(clk); -- 25ns
		test <= to_ufixed(0.019531249999, test);
		wait until rising_edge(clk); -- 30ns
		assert test = to_ufixed(0.03125, test) report "Check ufixed failed: 0.019531249999";
		wait until falling_edge(clk); -- 35ns
		test <= to_ufixed(0.01953125, test);
		wait until rising_edge(clk); -- 40ns
		assert test = to_ufixed(0.03125, test) report "Check ufixed failed: 0.01953125";
		wait until falling_edge(clk); -- 45ns
		test_s <= to_sfixed(0.017452406, test_s);
		wait until rising_edge(clk); -- 50ns
		assert test_s = to_sfixed(0.03125, test_s) report "Check sfixed failed: 0.017452406";
		wait until falling_edge(clk); -- 55ns
		test_s <= to_sfixed(0.019531249999, test_s);
		wait until rising_edge(clk); -- 60ns
		assert test_s = to_sfixed(0.03125, test_s) report "Check sfixed failed: 0.019531249999";
		wait until falling_edge(clk); -- 65ns
		test_s <= to_sfixed(0.01953125, test_s);
		wait until rising_edge(clk); -- 70ns
		assert test_s = to_sfixed(0.03125, test_s) report "Check failed: 0.01953125";
		wait;
	end process;
end architecture;
