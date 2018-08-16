library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity testbench is
end testbench;

architecture test of testbench is
	constant clkPeriod : time := 100 ns;
	signal simulationFinished : std_logic := '0';

	component Function_Z3fooi is
		port
		(
			clk : in std_logic;
			reset : in std_logic;
			input1 : in signed(31 downto 0);
			output : out signed(31 downto 0);
			ready : out std_logic
		);
	end component;

	signal clk : std_logic;
	signal reset : std_logic;
	signal input1 : signed(31 downto 0);
	signal output : signed(31 downto 0);
	signal ready : std_logic;
begin
	uut : Function_Z3fooi port map
	(
		clk => clk,
		reset => reset,
		input1 => input1,
		output => output,
		ready => ready
	);

	clkGeneration : process
	begin
		if not simulationFinished
		then
			clk <= '1';
			wait for clkPeriod / 2;
			clk <= '0';
			wait for clkPeriod / 2;
		else
			wait;
		end if;
	end process clkGeneration;

	simulation : process
		procedure check
		(
			constant in1 : in integer;
			constant outputExpected : in integer
		) is
			variable result : integer;
		begin
			input1 <= to_signed(in1, input1'length);

			reset <= '1';

			wait until rising_edge(clk);

			reset <= '0';

			wait until rising_edge(clk) and ready = '1';

			result := to_integer(output);

			assert result = outputExpected
			report
				"Unexpected result: " &
				"intput1 = " & integer'image(in1) & "; " &
				"output = " & integer'image(result) & "; " &
				"outputExpected = " & integer'image(outputExpected)
			severity failure;
		end procedure check;
	begin
		check(10, 59);
		check(0, 49);

		simulationFinished <= '1';
		wait;
	end process simulation;
end architecture test;
