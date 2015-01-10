library ieee;
use ieee.std_logic_1164.all;

entity mwe is
end mwe;

architecture lulz of mwe is
type sig_t is array (0 to 1) of std_logic_vector(1 downto 0);
signal sigw 	: sig_t := (others => (others => '0'));
signal sigf 	: sig_t := (others => (others => '0'));
signal clk	: std_logic := '0';

begin
	clk <= not clk after 50 ns;

	sigw(0) <= (others => '1');
	sigf(0) <= (others => '1');

	fail : process
		variable i : integer range 0 to 0 := 0;
		variable j : integer range 1 to 1 := 1;
	begin
		wait until rising_edge(clk);
		sigf(j) <= sigf(i);
	end process;

	work : process
	begin
		wait until rising_edge(clk);
		sigw(1) <= sigw(0);
	end process;

        process (sigf)
        begin
          report "sigf(1) = " & std_logic'image (sigf (0)(1));
          assert now = 0 ns or sigf (0) = "XX" severity failure;
        end process;
end lulz;
