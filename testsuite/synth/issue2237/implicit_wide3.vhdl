library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity implicit_wide3 is
port (
	clk : in std_logic;
	gate : in std_logic;

	d1 : in std_logic_vector(1 downto 0);
	q1 : out std_logic_vector(1 downto 0);
	q2 : out std_logic_vector(1 downto 0);
	q3 : out std_logic_vector(1 downto 0);

	d2 : in unsigned(1 downto 0);
	q4 : out unsigned(1 downto 0);
	q5 : out unsigned(1 downto 0);
	q6 : out unsigned(1 downto 0);

	d3 : in signed(1 downto 0);
	q7 : out signed(1 downto 0);
	q8 : out signed(1 downto 0);
	q9 : out signed(1 downto 0)
);
end entity;

architecture behav of implicit_wide3 is
begin

	process(clk)
	begin
		if rising_edge(clk) then
			q1 <= d1 nand gate;
			q2 <= d1 nor gate;
			q3 <= d1 xnor gate;

			q4 <= d2 nand gate;
			q5 <= d2 nor gate;
			q6 <= d2 xnor gate;

			q7 <= d3 nand gate;
			q8 <= d3 nor gate;
			q9 <= d3 xnor gate;
		end if;
	end process;

end architecture;

