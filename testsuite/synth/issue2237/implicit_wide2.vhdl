library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity implicit_wide2 is
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

architecture behav of implicit_wide2 is
begin

	process(clk)
	begin
		if rising_edge(clk) then
			q1 <= gate and d1;
			q2 <= gate or d1;
			q3 <= gate xor d1;

			q4 <= gate and d2;
			q5 <= gate or d2;
			q6 <= gate xor d2;

			q7 <= gate and d3;
			q8 <= gate or d3;
			q9 <= gate xor d3;
		end if;
	end process;

end architecture;

