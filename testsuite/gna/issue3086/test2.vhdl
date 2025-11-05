library ieee;
use ieee.std_logic_1164.all;

entity test2 is
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

architecture rtl of test2 is
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

entity tb_test2 is
begin
end entity;

architecture behav of tb_test2 is
	signal a : std_logic := '0';
	signal x : std_logic;
begin
	clk_p: process is
	begin
          wait for 1 ns;
          assert x = a severity failure;
          wait;
	end process;

	dut: entity work.test2
	 generic map(
	    C_A => true,
	    C_B => true
	)
	 port map(
	    a => a,
	    x => x
	);
end architecture;
