entity tb_ent is
end tb_ent;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_ent is
	-- Interrupt mapping register:
	signal iar	 : unsigned(15 downto 0) := "0000000010010000";
	signal ipend : unsigned(3 downto 0) := (others => '0');
	signal irq : unsigned(3 downto 0);
	signal clk : std_logic := '0';
begin
	dut: entity work.ent
	generic map (NUM_CHANNELS => 4)
		port map (iar => iar, ipend => ipend, irq => irq, clk => clk);

	process
	begin
		clk <= not clk;
		wait for 1 ns;
	end process;

stim:
	process
	begin
		wait for 10 ns;
		ipend(0) <= '1';
		wait for 10 ns;
		ipend(1) <= '1';
		wait for 10 ns;
		ipend(2) <= '1';
		wait for 10 ns;
		ipend <= (others => '0');

		wait for 10 ns;
		ipend(3) <= '1';
		wait for 10 ns;
		ipend(2) <= '1';
		wait for 10 ns;
		ipend(1) <= '1';
		wait for 10 ns;
		ipend <= (others => '0');

		wait;
	end process;

end behav;
