library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity implicit_wide4 is
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

architecture behav of implicit_wide4 is
begin

	process(clk)
	begin
		if rising_edge(clk) then
			q1 <= gate nand d1;
			q2 <= gate nor d1;
			q3 <= gate xnor d1;

			q4 <= gate nand d2;
			q5 <= gate nor d2;
			q6 <= gate xnor d2;

			q7 <= gate nand d3;
			q8 <= gate nor d3;
			q9 <= gate xnor d3;
		end if;
	end process;

end architecture;

