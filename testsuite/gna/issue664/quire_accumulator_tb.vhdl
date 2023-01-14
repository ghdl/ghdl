library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.all;
use work.reset_synchronizer;
--use work.quire_accumulator;

entity quire_accumulator_tb is
end entity;

architecture behavior of quire_accumulator_tb is
	component reset_synchronizer
		port(
			clk         : in  std_logic;
			async_rst_n : in  std_logic;
			rst_n       : out std_logic
		);
	end component reset_synchronizer;

	--	component quire_accumulator
	--		port(
	--			clk   : in  std_logic;
	--			rst_n : in  std_logic;
	--			q_in  : in  quire_in_type;
	--			reg   : in  quire_in_reg;
	--			add   : in  quire_in_add;
	--			mul   : in  quire_in_mul;
	--			q_out : out quire_out_type
	--		);
	--	end component quire_accumulator;

	signal clk         : std_logic := '0';
	signal async_rst_n : std_logic := '0';
	signal rst_n       : std_logic := '0';
	signal finished    : std_logic := '0';

	--	signal q_in     : quire_in_type := (cmd => CLEAR, d_in => QUIRE_ZERO);
	--	signal q_in_reg : quire_in_reg  := (
	--		isNaR    => '0',
	--		isZero   => '1',
	--		sign     => '0',
	--		scale    => (others => '0'),
	--		fraction => (others => '0')
	--	);
	--	signal q_in_add : quire_in_add  := (
	--		valid       => '0',
	--		isNaR       => '0',
	--		isZero      => '1',
	--		sign        => '0',
	--		scale       => (others => '0'),
	--		significant => (others => '0')
	--	);
	--	signal q_in_mul : quire_in_mul  := (
	--		valid       => '0',
	--		isNaR       => '0',
	--		isZero      => '1',
	--		sign        => '0',
	--		scale       => (others => '0'),
	--		significant => (others => '0')
	--	);

	--	signal q_out : quire_out_type := (
	--		state => QS_ZERO, 
	--		q_out => QUIRE_ZERO
	--	);
	signal q_out : quire_out_type;
begin
	CLKGEN : process(clk, finished)
	begin
		clk <= not clk after 2500 ps when finished /= '1' else '0';
	end process;

	RESET_PULSE : process
	begin
		reportConfiguration;

		wait for 5 ns;
		async_rst_n <= '1';
		wait;
	end process;

	END_OF_SIM : process
	begin
		wait for 100 ns;
		finished <= '1';
		wait;
	end process;

	RESET_SYNC : reset_synchronizer
		port map(
			clk         => clk,
			async_rst_n => async_rst_n,
			rst_n       => rst_n
		);

		--	SUT_PROC : quire_accumulator
		--		port map(
		--			clk   => clk,
		--			rst_n => rst_n,
		--			q_in  => q_in,
		--			reg   => q_in_reg,
		--			add   => q_in_add,
		--			mul   => q_in_mul,
		--			q_out => q_out
		--		);

end architecture behavior;
