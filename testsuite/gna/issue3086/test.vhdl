library ieee;
use ieee.std_logic_1164.all;

entity test is
	generic(
		C_A : boolean := true;
		C_B : boolean := true
	       );
	port(
		a : in std_logic;
		x : out std_logic
	    );
begin
end entity;

architecture rtl of test is
begin
	g1: if C_A and C_B generate
		x <= a;
	end generate;
	g2: if C_A nand C_B generate
		x <= not a;
	end generate;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity tb_test is
begin
end entity;

architecture behav of tb_test is
	signal clk : std_logic := '0';
	signal a : std_logic := '0';
	signal x : std_logic;
begin
	timeout: process is
	begin
		for i in 0 to 20 loop
			wait until rising_edge(clk);
		end loop;
	end process;

	clk_p: process is
	begin
		clk <= not clk;
		wait for 5 ns;
	end process;

	dut: entity work.test
	 generic map(
	    C_A => true,
	    C_B => true
	)
	 port map(
	    a => a,
	    x => x
	);
end architecture;
