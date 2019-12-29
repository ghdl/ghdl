entity tb_ent is
end tb_ent;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent is
  signal clk : std_logic;
  signal v : std_logic_vector (31 downto 0);
begin
  dut: entity work.ent
    port map (clk => clk, o => v);

  process
  begin
    clk <= '0';
    wait for 1 ns;
    clk <= '1';
    wait for 1 ns;
    assert v = x"8000_0000" severity failure;
    wait;
  end process;
end behav;
